# Using PEGs to make RVSDGs more egglog-Friendly

This document covers some technical issues around RVSDGs and how to use tricks
from PEGs to solve them. It assumes some familiarity with RVSDGs, eggcc, and
PEGs, but I'd like for the notation to be relatively self-contained.  It
probably includes some material that is "review" for experts; feel free to skip
around as needed.

The encoding suggested in this document is very similar to PEGs, but it keeps
some structure from RVSDGs to keep everything egglog-friendly. To differentiate
from the initial encoding, I've been calling them "acyclic PEGs."  This document
often refers to both the original and new encodings as PEGs, though.

## RVSDGs

RVSDGs give us a "dataflow-oriented" IR for for imperative programs. In
pseudo-egglog, we are representing them as follows:

```clojure
(datatype Value
    ; (Arg i) is the ith argument to the enclosing block
    (Arg i64)
    ; (Proj i r) is the ith output of the region r.
    (Proj i64 Region)
    ; Abridged list of constant literals. Predicates are the kinds of values
    ; used in γ and θ predicates.
    (Const-int i64)
    (Const-pred i64))

(datatype Region
    ; Shorthand for most operations: (Proj 0 (Builtin "+" (vec-of a b))) is the
    ; result of adding a and b.
    (Builtin String (Vec Value))
    ; Multi-way conditional branches.
    (γ 
        ; The predicate determining which branch's outputs to use.
        Value 
        ; The inputs to the inner blocks.
        (Vec Value)
        ; The output blocks, one per predicate value. Run in their own scope.
        (Vec (Vec Value)))
    ; A "do-while" loop
    (θ 
        ; The loop predicate, run in the scope of the loop body.
        Value
        ; The loop inputs, in the scope of the outer region.
        (Vec Value)
        ; The loop outputs, run in the scope of the inner region. (Arg i) here
        ; refers to the value of the ith input as of the previous iteration of
        ; the loop.
        (Vec Value)))
```

> Note: eggcc currently has separate data constructors for each supported
builtin, rather than the "dynamically typed" variant here where the primitive is
described as a string. I'm doing that here purely to keep the encoding concise,
though having fewer variants here would make things like substitution easier.

## Advantages of RVSDGs

**Dataflow** The nice thing about RVSDGs compared to a standard CFG is that it is
"dataflow-oriented." Things like constant folding and strength reduction are
fairly straightforward:

```clojure
; Constant folding "+"
(rewrite (Proj 0 (Builtin "+" (vec-of (Const-int a) (Const-int b))))
    (Const-int (+ a b)))

; Strengh reduction *2 => <<1
(rewrite (Builtin "*" (vec-of v (Const-int 2)))
    (Builtin "<<" (vec-of v (Const-int 1))))
```

In a CFG-oriented IRs, even basic operations like this must keep "context" in
mind: we have to decide not just _what_ to do but also _when_ to do it in a
stream of instructions. This is particularly difficult when doing equality
saturation: the two constants `a` and `b` could be "defined" anywhere in the
instruction schedule, the new schedule (the RHS of the rewrite) doesn't have
them, but it's not obvious how much of the old and new contexts need to be
equated when doing a rewrite. What it even means for two instruction streams to
be equal is much more complex than it is for two expression like `x * 2` and 
`x << 1`.

**Explicit, Structured Control-flow** Other IRs like "Sea of Nodes" also
makes constant folding and strengh reduction fairly straightforward. However,
Sea of Nodes has unstructured control flow, and a mix of "control flow" and
"data flow" edges in its graph. This makes equality saturation on the
control-flow operations of the graph non-obvious: it's not clear what the
semantics of certain control-flow edges should be. By contrast, all control flow
in RVSDGs takes the form of its own "region" that behaves like a multi-output
function.

Still, it seems like you can get pretty far in "Sea of Nodes + dataflow-only
rewrites": at a high level this is more or less what the ægraphs folks seem to
be doing: they only put the dataflow edges in the e-graph, and they keep the
control-flow edges around outside; they use those during extraction.

## Disadvantages of RVSDGs

While RVSDGs make it possible to do rewrites for both control-flow and dataflow
operations, the representation has a number of issues in egglog.

### Vectors are Awkward

Egglog vectors are not particularly well-optimized and basic operations have
unexpected runtime overheads over what vector or sequence data-structures have
in other languages. Any update to a vector is linear-time, and iteration over
vectors requires $O(n)$ applications of a rule. RVSDGs use vectors _a lot_.

Beyond efficiency concerns, vectors require lots of maintenance operations that
aren't needed with standard egglog terms. Values in RVSDGs are essentially
indexes into vectors. It is common for different optimization passes to remove
indexes from vectors and when that happens all projections into that vector must
potentially be rewritten to a new index. We see this in the numerous `shift`
calls that we need for rules over the RVSDG.

> Note: a lower-tech fix for this could be to phrase RVSDGs in terms of 
> `(Map i64 Value)`s rather than vectors.

### Oversharing

RVSDGs have a less restrictive notion of sharing than egglog has. The term 
`(Builtin "+" (vec-of (Arg 0) (Arg 1)))` will only appear _once_ in an extracted
egglog term-DAG, but in an RVSDG such a term may appear once _per region_. While
we can duplicate these nodes as a post-processing pass the fact that terms do
not "mention" important context like this can skew the cost estimates that the
egraph extraction algorithm makes when it searches for an optimal term. 

> Note: a lower-tech fix for this could be to generate a unique id per region
and tag each `Arg` with the enclosing region itself.

### Dead Node Elimination Is Very Difficult

The RVSDG paper describes a *Dead Node Elimination* (DNE) pass that removes
portions of an RVSDG that aren't used in computing the result of a block or
function. DNE is essentially a DFS of the RVSDG starting at the return value
(i.e. the 'root' node we are trying to extract; probably the return value of a
function).

This sort of DFS sounds very related to e-graph extraction. However, extraction
of an RVSDG _does not_ implement DNE. To see why, consider the following code:

```C
int foo(int x) {
    int y = 0;
    if (x < 0) {
        y += 1;
        x += 2;
    }
    return x;
}
```

In this code, `y` is unused. But by extracting the `x` that is returned from
`foo`, `y` will come along for the ride.

The RVSDG we get out of this will look like:

```clojure
(Proj 0
  (γ (Proj 0 (Builtin "<" (vec-of (Arg 0) (Const-int 0))))
   ;          x          y
   (vec-of (Arg 0) (const-int 0))
   (vec-of
        ; `else`: x=x,   y=y
        (vec-of (Arg 0) (Arg 1))
        ; `if`:  x=x+2
        (vec-of (Proj 0 (Builtin "+" (vec-of (Arg 0) (Const-int 2)))) 
        ;        y=y+1
                (Proj 0 (Builtin "+" (vec-of (Arg 1) (Const-int 1))))))))
```

By extracting `(Proj 0 <γ node>)` we extract _all_ of that γ's outputs, even
though we only use the first output. We need to somehow remove the `y` output
from the γ region. The options for doing that we have aren't great:

* **Fancy scheduling** We could compute _unused_ sets for each region.
  Maintaining these could be expensive, and we would rely on the rules computing
  these sets saturating before we could the sets to remove outputs.
* **Fission** We could attempt to _narrow_ regions by rewriting calls to `Proj`
to instead project from the minimal region needed to compute the output in
question. In this example, that would mean factoring the program into two γ
regions: one for `x` and one for `y`.  That would work, but it would require
very complex logic to properly handle cases where `x` and `y` depend on one
another. On top of all of that, if we also added fusion for γ nodes this would
create $O(2^n)$ e-nodes for a γ region with $n$ outputs.

PEGs sidestep this problem entirely.
 
## PEGs

PEGs are the "original" IR used in equality saturation and compilers. They are a
lot like RVSDGs, but they lack a lot of the administrative issues around vectors
that we see in RVSDGs. Instead of identifying a portion of a region explicitly
with a `Proj` expression, PEGs use the _expressions themselves_ as a means of
projecting out of a region. This ideally lets them share more than RVSDGs do,
while also avoiding oversharing.

The original equality saturation / PEGs paper (aka the "peggy" paper) predates
egg and egglog. The authors developed a custom e-matching engine to implement
the compiler, with custom scheduling, term representations and extraction.  I
think we'd have a lot of trouble adapting PEGs wholesale in eggcc if we want to
avoid importing all of the special cases from the PEGs paper.

### PEG extraction

egg and egglog both provide APIs for encoding terms, rewrites, and cost
functions. Given those three things, users automatically get extraction
algorithms the find the "best" term from an equivalence class. egglog takes this
a step forward and _only_ provides users with the builtin mechanism for
extraction, egg users can still traverse the raw e-graph if they want to and do
their own thing.

PEGs as described in the Peggy paper lean very hard towards "doing their own
thing." In particular, all loops rely on cyclic terms. Given that most
extraction algorithms used by egg and egglog today can _only_ extract terms that
are acyclic, this is a problem.

We could break this abstraction layer if we wanted and implement a custom
extraction algorithm, but that would essentially opt us out of any new research
on e-graph extraction and also make further changes to the encoding harder.

> Note: there are other issues around extracting from PEGs that we can probably
> use some of the existing RVSDG algorithms for. As we'll see: we can take a
> PEG-like representation and turn it back into an RVSDG fairly easily. Once we do
> that, there are further operations such as loop fusion and code motion that we
> can write in Rust, rather than egglog.

The following sections go over how to "merge" RVSDGs and PEGs to get an
egglog-friendly encoding.

## Combining RVSDGs and PEGs

PEGs and RVSDGs are pretty similar: indeed, the current PEG support that eggcc
has is based on RVSDGs already. While PEGs predate RVSDGs in the literature we
can think of them as a version of RVSDGs that are "easy to encode" in an
equality saturation framework, though there are some differences.

This section explains how to incrementally move from an RVSDG-style
representation to a PEG-style one. We'll do this in a way that avoids vectors
(like PEGs) but maintains the acyclic structure of RVSDGs.

### From γ to φ

PEGs use φ (similar to the `?:` operator in C) to encode conditional values,
rather than γ. The two constructs are different, but they are closely related.
For any RVSDG γ node we can instead have a series of φ nodes, one per output.

```clojure
; (φ pred vals) evaluates to vals[pred].
(function (φ Value (Vec Value)) Value)
; NB: this rule is a more cumbersome and less efficient for RVSDGs.
(rewrite (φ (Const-int i) choices) (vec-get i choices))
```

Any reference to the output of a gamma node:

```clojure
(Proj i (γ pred inputs outputs))
```

Can be transformed into a φ node of the form:

```clojure
(φ pred (subst-all inputs (vec-get i outputs)))
```

Where `(subst-all vec expr)` recursively replaces all instances of `(Arg i)` in
`expr` with `(vec-get i vec)`. In this way, φ nodes are _almost_ just "better
notation" for γ nodes. The fact that they are "pre-substituted" with inputs
allows us to only reference input terms when an output that depends on them is
used. In other words, extraction and DNE are _the same_ for PEGs.

**One subtelty** RVSDG γ nodes have explicit inputs. This allows RVSDGs to
 express the difference between code like:

```C
int foo(int x, int y) {
    if (x < 3) {
        y *= 2;
    } else {
        ++y;
    }
    return y;
}
```

and code like:

```C
int foo(int x, int y) {
    int y1 = y * 2;
    int y2 = y + 1;
    return (x < 3) ? y1 : y2;
}
```

The PEGs for these two functions are the same: there's a separate code motion
algorithm that decides whether to multiply `y` by 2 unconditionally, or _only_
if `x < 3`: The Peggy paper aims to compute values at most once but also as
infrequently as possible, similar to the "Global Code Motion" algorithm used in
Sea of Nodes compilers; in this case it would pick the first `foo`. I think we
should be fine doing something similar.

### Loops, per-output, without cycles.

PEG-style φ nodes give us "dead node elimination" for free. Any extraction of a
term containing a φ node will only extract the data dependencies needed _for
that output_, rather than for the entire γ node. PEGs have a similar notation
for loops, but _any loop output term_ is going to be cyclic, meaning that
standard extraction algorithms will not work to extract terms with loops in
them. For example, the value of `x` in the loop:

```C
int x = 0;
do {
    x += 1;
} while (x < 10);
```

Is encoded in as a PEG in psuedo-egglog as:

```clojure
(letrec x (θ 0 (+ x 1)))
```

egglog doesn't have `letrec` but it still supports cycles. We could faithfully
encode this term in egglog by doing something like the following:

```clojure
(function (placeholder i64) Value :no_extract)
(function (peg-θ Value Value) Value)
(let x (placeholder 0))
(union x (peg-θ (Const-int 0) (Builtin "+" (vec-of x (Const-int 1)))))
```

The problem (aside from rigging up these `placeholder`s, which is annoying) is
that attempting to extract `x` directly will fail: the only terms associated
with `x` are either `:no_extract` or cyclic.

> Could we just allow ourselves to extract from `placeholder` ? We could, but
> setting its cost correctly (so we don't _just_ extract placeholders) would be
> tricky, and we would still have to have some extra information to disambiguate
> different placeholders in the setting where there are multiple loop variables
> that depend on one another. 

We can borrow a trick from RVSDG "projections" to avoid this fate. First, let's
define our PEGs in pseudo-egglog:

```clojure
(datatype Index ...)
(datatype Value
    (Const-int i64)
    (Const-pred i64)
    (Builtin String (Vec Value))
    (φ Value (Vec Value))
    ; (θ-input i base) is _either_ 'base' or the value of loop variable 'i' as
    ; of the previous loop iteration. This is akin to the first argument to peg-θ.
    (θ-input Index Value)
    ; (θ-output i pred v) is the value of the ith loop variable after the first
    ; iteration  where 'pred' is made false by iteratively applying 'v'. This is
    ; akin to the second argument to peg-θ as well as a call to the `pass`
    ; construct in PEGs.
    (θ-output Index Value Value))
```
> We are ignoring multi-output primitives for now. We can handle them with a
> separate "projection" variant to `Value`

This `Index` sort is essentially a source of fresh identifiers. Every θ region
in an RVSDG in Rust will have a unique "space" of `Index` values that it uses to
mark inputs and outputs. Different passes may need to create new loops, and our
egglog sort lets us generate new indexes from old ones.

```clojure
(datatype Index
    (Base i64 :cost 0)
    ; We can add more constructors here depending on what kinds of
    ; transformations we need. For example, we could add a
    ; (Peeled Index :cost 0) constructor for the new θ input/outputs introduced
    ; by loop peeling.
    ...)
```

> Note: these index values must be **globally unique**, not just unique
within a θ node. Otherwise, extracting `(θ i pred x)` and `(θ i pred y)` for
inequivalent `x` and `y` could result in an ambiguous term. This would have been
nice though: it could have given us a lightweight form of loop fusion for ~free.

Indexes are just used for bookkeeping; they will not impact extraction and can
be ignored when generating code. The same loop from before can be encoded as:

```clojure
(let i (Base 0))
(let x-input (θ-input i (Const-int 0)))
(θ-output i (Builtin "<" (vec-of x-input (Const-int 10))) 
            (Builtin "+" (vec-of x-input (Const-int 1))))
```

In other words for loops we separate "input" variables from "output" variables.
Nodes land in the same "region" by having the same loop predicate. Any use of an
output will automatically use the loop predicate and the base value, but the
term itself will be acylic: cost more or less works as expected here.

**Additional Loop Variables** Terms in PEGs are cyclic in order to clarify how
"outputs" of one iteration flow back into "inputs" of another one. For example,
two variables with the same base value still need to appear as separate nodes:
they may not be equal after a single iteration. Consider this C code:

```C
int x = 0;
int y = 0;
do {
    x += 1;
    y += x;
} while (x == y);
```

We can encode that as follows:

```clojure
(let i (Base 0))
(let j (Base 1))
(let x-input (θ-input i (Const-int 0)))
(let y-input (θ-input j (Const-int 0)))
(let pred (Builtin "==" (vec-of x-input y-input)))
(let x (θ-output i pred (Builtin "+" (vec-of x-input (Const-int 1)))))
(let y (θ-output j pred 
    (Builtin "+" (vec-of y-input 
                         (Builtin "+" (vec-of x-input (Const-int 1)))))))
```

Without unique index values `i` and `j`, we would have no way to distinguish
between `x-input` and `y-input` and therefore no way to distinguish between `x`
and `y` when computing new values of `y` in the loop body.

This actually points to a subtle issue with this encoding and how extraction
works in egglog today.

### Feature Request: Dependent Extraction

Take the example from the last section: if we wanted to extract `y` but not `x`,
we would get a value for `x-input` but _not_ `x-output`. That's wrong: to
compute `y`'s value after a loop we _also_ need to compute `x`s. To fix this, I
propose adding _extraction dependencies_ to egglog ids:

```clojure
(rule ((= inp (θ-input i base))
       (= out (θ-output i pred v)))
      ((extract-with inp out)))
```

The new `extract-with` directive instructs egglog to always extract `out`
whenever extracting `inp` . Supporting this in egglog would be straightforward,
both for extraction algorithms and for keeping track of dependencies. The result
of extraction can potentially be disconnected, and the code handling output from
egglog must handle this fact.

With this rule in place, we can ensure that any variable mentioned by a variable
that is used is also computed as part of a loop. This is the only change we need
to allow extraction to do dead node elimination for us.

> Note: one way to think about the classic PEG presentation is that it _avoids_
the need for `extract-with` by making the `θ-output` term a child of the
`θ-input` term. We cannot do that because the input is already (in general) the
child of the output, and we want to avoid cycles.

## Code Motion

Once we extract a set of terms from egglog, we need some way to get back to a
CFG. I think we can reuse the existing conversion code for RVSDGs for most of
that. What remains is taking this encoding of PEGs and transforming them to
RVSDGs. I think this is relatively straightforward:

* Construct a map from predicates to the set of `φ` terms with that predicate.
* For each such mapping, construct a γ region:
  - Each output block has an output port corresponding to one of the φ nodes.
  - If a subterm is used by only one φ, move that subterm to that φ's block,
    otherwise add it as an input to the γ region.
* Construct a map from predicates to the set of `θ-output` terms with that predicate.
* For each such mapping, construct a θ region:
  - For each `θ-output` add an output and an input port to the region.
  - `θ-input`s are transformed to relevant `Arg`s. Any other invariant subterm
    is passed as an input to the loop.

All other constructs (builtins, function arguments) have direct analogs in
RVSDGs. This is a good deal simpler than the extraction algorithm described in
the Peggy paper for PEGs: it's analogous to the "Global Code Motion" algorithm
used for Sea-of-Nodes, but again we can simplify some steps due to the added
structure of RVSDGs.

### Adding Code Motion to PEGs

It's worth noting that we are essentially relegating "code motion" for γ regions
to a post-processing step on the extracted PEG. I think that's a good place to
start, personally, but we can imagine adding it back by introducing a `φ-input`
constructor analogous to the `θ-input` variant used for loops. This would let us
explicitly encode code motion questions into the PEG term itself. I think we
should probably keep code motion in Rust until we have a good reason to move it
back to egglog (this is all complicated enough already).

## Worked Example: Invariant Value Redirection

Beyond the benefits around extraction / oversharing, I think this version of
PEGs would make a lot of rules easier to write. This section uses the example of
"invariant value redirection." This is an optimization that takes a value that
doesn't change within a block and hoists references to it out of that block. For
example, loops like:

```C
int x = 0; int y = 0;
do {
    x += 1;
    y = y;
} while (x > y);
```

Can "hoist" `y` out of the loop entirely:

```C
int x = 0; int y = 0;
do {
    x += 1;
} while (x > y);
```

This is an important optimization because the RVSDG conversion algorithm moves
variables like `y` into the loop fairly aggressively.

This optimization is much more concise for our encoding of PEGs than it is for RVSDGs.

### RVSDGs

```clojure
; θ nodes
(relation to-redirect Region i64)
(rule ((= r (θ pred ins outs))
       (= p (Proj i r))
       (= (Arg i) (vec-get outs i)))
     ((to-redirect r i)))

(rule ((= r (θ pred ins outs))
       (to-redirect r i))
      ((union (Proj i r) (vec-get ins i))))

(rule ((= r (θ pred ins outs))
       (to-redirect r i)
       (= proj (Proj j r))
       (!= i j))
     ((union proj (Proj (shifted i j) (shift i r)))))
; where `shifted` returns j if i>j, and j-1 otherwise.
; and `shift` returns a new region without an ith input or ith output.

; γ nodes

; `(all-equal v vs)`  has an entry if out[i] = v for every out in vs.
(relation all-equal i64 Value (Vec (Vec Value)))
(rule ((= r (γ pred ins outs))
       (Arg i)
       (all-equal i v outs)
       (= proj (Proj i r)))
      ((union proj v)))

(rule ((= r (γ pred ins outs))
       (Arg i)
       (all-equal i v outs)
       (= proj (Proj j r))
       (!= i j))
       ((union proj (Proj (shifted i j) (shift-γ i r)))))

; where shift-γ calls 'shift' on each of the outputs and removes 'i' from the
; inputs.

(function (all-equal-up-to i64 Value (Vec (Vec Value))) i64 :merge (max old new))

(rule ((= r (γ pred ins outs))
       (Arg i)
       (< i (vec-length (vec-get 0 outs))))
      ((set (all-equal-up-to i (vec-get i (vec-get 0 outs)) outs) 0)))

(rule ((= (vec-length outs) (all-equal-up-to i v outs)))
      ((all-equal i v outs)))

(rule ((= n (all-equal-up-to i v outs))
       (< n (vec-length outs))
       (= v (vec-get i (vec-get (+ n 1) outs))))
      ((set (all-equal-up-to i v outs) (+ n 1))))
```


### Acyclic PEGs

```clojure
; θs are a one-liner.
(rewrite (θ-output i pred (θ-input i base)) base)

; The binary case is almost a one-liner, but we need to do some iteration
; to get multi-way φs to work.
; binary would be:
;   (rewrite (φ pred v v) v)
; Instead we have
(relation all-same (Vec Value)) 
(rule ((= phi (φ pred vs)) (all-same vs))
      ((union phi (vec-get 0 vs))))
; Where 'all-same' could be implemented as follows.
(function all-same-up-to ((Vec Value)) i64 :merge (max old new)) 
(rule ((= phi (φ pred vs))) (set (all-same-up-to vs) 0))
(rule ((= i (all-same-up-to vs))
       (< i (vec-length vs))
       (= (vec-get 0 vs) (vec-get i vs)))
      ((set (all-same-up-to vs) (+ i 1))))
(rule ((= (vec-length vs) (all-same-up-to vs))) 
      ((all-same vs)))
```

## Notes on Loop Fusion

The peggy paper's extraction procedure handles loop fusion as part of its code
motion procedure. We could do something similar after translating the extracted
PEG to an RVSDG in Rust: at that point we can syntactically check if two loops
have the same predicate. Furthermore, we are unlikely to run into the issues
Alex found with implementing loop fusion in egglog; Rust doesn't have the same
limitations.

I initially thought that we would get "loop fusion for free", but different
loops have different `Index` values for their inputs, so only trivial predicates
will be equal.

> We'll need to "split" such loops that accidentally get fused where one loop's
inputs depend on another's outputs.

Cyclic terms have this issue too, though we could think about some sort of
"coinductive hash-consing" that could unify two cyclic terms. The process of
doing this feels a lot like Prolog-style unification: there could be a useful
new egglog feature in there somewhere.

## Notes on Monotone Cost Functions

Another puzzle shared by both PEGs and RVSDGs is how to assign costs to loops.
egglog only supports what egg calls `LpCost`s, where e-nodes are assigned a
constant cost.

The Peggy paper assigns e-nodes costs that are exponential in their loop nesting
depth. In other words, loops are linear functions of the cost of the loop body.
If egglog supported something like the following:

```clojure
(function (foo X Y) Z :cost_function (+ 5 (* 10 child-cost)))
```

Then we could encode PEG's loop costs directly in RVSDGs as a `:cost_function`
attached to θ regions. Because the function is monotone, the existing greedy
algorithms that we have should continue to work with only superficial changes.
It could complicate the extraction gym though.

PEGs, on the other hand, do not have a single node that can "see" the entire
body of a loop. It's not as clear where we'd add a `:cost_function`, maybe we
want a different abstraction to handle PEGs.

# References

* The Peggy paper ["Equality Saturation: A New Approach to Optimization"](https://rosstate.org/publications/eqsat/)
* The RVSDG paper ["RVSDG: An Intermediate Representation for Optimizing Compilers"](https://www.sjalander.com/research/pdf/sjalander-tecs2020.pdf)