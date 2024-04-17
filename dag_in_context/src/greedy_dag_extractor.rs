use egglog::*;
use egraph_serialize::{ClassId, EGraph, NodeId};
use indexmap::*;
use ordered_float::NotNan;
use rustc_hash::FxHashMap;
use std::collections::{HashMap, HashSet, VecDeque};

use crate::{
    from_egglog::FromEgglog,
    schema::{RcExpr, TreeProgram, Type},
    typechecker::TypeChecker,
};

pub(crate) struct Extractor<'a> {
    pub(crate) egraph: &'a EGraph,
    // For a particular region root, find the set of reachable nodes
    #[allow(dead_code)]
    pub(crate) reachable_from: HashMap<ClassId, HashSet<ClassId>>,
    // For a particular region root, we are done extracting the region
    #[allow(dead_code)]
    pub(crate) done: HashSet<NodeId>,
    pub(crate) cm: &'a dyn CostModel,
    pub(crate) termdag: &'a mut TermDag,
    // Each term must correspond to a node in the egraph. We store that here
    pub(crate) correspondence: HashMap<Term, NodeId>,
    // use to get the expression corresponding to the term
    pub(crate) term_to_expr: HashMap<Term, RcExpr>,
    // use to get the type of an expression
    pub(crate) typechecker: TypeChecker<'a>,
    costs: FxHashMap<ClassId, CostSet>,
    /// A set of names of functions that are unextractable
    unextractables: HashSet<String>,
}

impl<'a> Extractor<'a> {
    fn is_region_node(&self, node_id: NodeId) -> bool {
        enode_regions(self.egraph, &self.egraph[&node_id]).is_some()
    }

    #[allow(dead_code)]
    fn node_to_eclass(&self, node_id: NodeId) -> ClassId {
        self.egraph.nid_to_cid(&node_id).clone()
    }

    pub(crate) fn term_to_prog(&mut self, term: &Term) -> TreeProgram {
        let mut temp_term_to_expr = Default::default();
        std::mem::swap(&mut self.term_to_expr, &mut temp_term_to_expr);
        // convert the term to an expression using a converter
        // converter has to own the termdag, so we swap it with the current one
        let mut converter = FromEgglog {
            termdag: self.termdag,
            conversion_cache: temp_term_to_expr,
        };

        let expr = converter.program_from_egglog(term.clone());
        self.term_to_expr = converter.conversion_cache;
        expr
    }

    pub(crate) fn term_to_expr(&mut self, term: &Term) -> RcExpr {
        let mut temp_term_to_expr = Default::default();
        std::mem::swap(&mut self.term_to_expr, &mut temp_term_to_expr);
        // convert the term to an expression using a converter
        // converter has to own the termdag, so we swap it with the current one
        let mut converter = FromEgglog {
            termdag: self.termdag,
            conversion_cache: temp_term_to_expr,
        };

        let expr = converter.expr_from_egglog(term.clone());
        self.term_to_expr = converter.conversion_cache;
        expr
    }

    pub(crate) fn term_to_type(&mut self, term: &Term) -> Type {
        let expr = self.term_to_expr(term);
        self.typechecker.add_arg_types_to_expr(expr, &None).0
    }

    pub(crate) fn expr_to_type(&mut self, expr: &RcExpr) -> Type {
        self.typechecker
            .add_arg_types_to_expr(expr.clone(), &None)
            .0
    }

    pub(crate) fn eclass_of(&self, term: &Term) -> ClassId {
        let term_enode = self.node_of(term);
        self.egraph.nodes.get(&term_enode).unwrap().eclass.clone()
    }

    /// Checks if an expressions is effectful by checking if it returns something of type state.
    pub(crate) fn is_effectful(&mut self, expr: &RcExpr) -> bool {
        let ty = self.expr_to_type(expr);
        ty.contains_state()
    }

    /// Checks if an already extracted node is effectful.
    pub(crate) fn is_node_effectful(&mut self, node_id: NodeId) -> bool {
        let eclass = self.node_to_eclass(node_id);
        let term = self.costs.get(&eclass).unwrap().term.clone();
        let expr = self.term_to_expr(&term);
        self.is_effectful(&expr)
    }

    pub(crate) fn node_of(&self, term: &Term) -> NodeId {
        self.correspondence
            .get(term)
            .unwrap_or_else(|| panic!("Failed to find correspondence for term {:?}", term))
            .clone()
    }

    pub(crate) fn new(
        original_prog: &'a TreeProgram,
        cm: &'a dyn CostModel,
        termdag: &'a mut TermDag,
        correspondence: HashMap<Term, NodeId>,
        egraph: &'a EGraph,
        unextractables: HashSet<String>,
    ) -> Self {
        let mut region_roots = HashSet::new();
        for node in egraph.classes().values().flat_map(|c| &c.nodes) {
            if let Some(regions) = enode_regions(egraph, &egraph[node]) {
                for root in regions {
                    region_roots.insert(root);
                }
            }
        }

        let reachable_from = region_roots
            .iter()
            .map(|root| (root.clone(), reachable_classes(egraph, root.clone())))
            .collect();

        Extractor {
            cm,
            termdag,
            correspondence,
            egraph,
            costs: FxHashMap::<ClassId, CostSet>::with_capacity_and_hasher(
                egraph.classes().len(),
                Default::default(),
            ),
            unextractables,
            term_to_expr: Default::default(),
            typechecker: TypeChecker::new(original_prog, true),
            reachable_from,
            done: Default::default(),
        }
    }
}

fn get_root(egraph: &egraph_serialize::EGraph) -> NodeId {
    let mut root_nodes = egraph
        .nodes
        .iter()
        .filter(|(_nid, node)| node.op == "Program");
    let res = root_nodes.next().unwrap();
    assert!(root_nodes.next().is_none());
    res.0.clone()
}

pub fn get_unextractables(egraph: &egglog::EGraph) -> HashSet<String> {
    let unextractables = egraph
        .functions
        .iter()
        .filter_map(|(name, func)| {
            if func.is_extractable() {
                None
            } else {
                Some(name.to_string())
            }
        })
        .collect();
    unextractables
}

pub fn serialized_egraph(
    egglog_egraph: egglog::EGraph,
) -> (egraph_serialize::EGraph, HashSet<String>) {
    let config = SerializeConfig::default();
    let egraph = egglog_egraph.serialize(config);

    (egraph, get_unextractables(&egglog_egraph))
}

type Cost = NotNan<f64>;

#[derive(Clone)]
pub struct CostSet {
    pub total: Cost,
    // TODO perhaps more efficient as
    // persistent data structure?
    pub costs: HashMap<ClassId, Cost>,
    pub term: Term,
}

fn build_parent_index(egraph: &egraph_serialize::EGraph) -> IndexMap<ClassId, Vec<NodeId>> {
    let mut parents = IndexMap::<ClassId, Vec<NodeId>>::with_capacity(egraph.classes().len());
    let n2c = |nid: &NodeId| egraph.nid_to_cid(nid);

    for class in egraph.classes().values() {
        parents.insert(class.id.clone(), Vec::new());
    }

    for class in egraph.classes().values() {
        for node in &class.nodes {
            for c in &egraph[node].children {
                // compute parents of this enode
                parents[n2c(c)].push(node.clone());
            }
        }
    }
    parents
}

fn initialize_worklist(egraph: &egraph_serialize::EGraph) -> UniqueQueue<NodeId> {
    let mut analysis_pending = UniqueQueue::default();
    for class in egraph.classes().values() {
        for node in &class.nodes {
            // start the analysis from leaves
            if egraph[node].is_leaf() {
                analysis_pending.insert(node.clone());
            }
        }
    }
    analysis_pending
}

impl<'a> Extractor<'a> {
    /// Construct a term for this operator with subterms from the cost sets
    /// We also need to add this term to the correspondence map so we can
    /// find its enode id later.
    fn get_term(&mut self, node_id: NodeId, children: Vec<Term>) -> Term {
        let node = &self.egraph[&node_id];
        let op = &node.op;
        let term = if children.is_empty() {
            if op.starts_with('\"') {
                self.termdag
                    .lit(ast::Literal::String(op[1..op.len() - 1].into()))
            } else if let Ok(n) = op.parse::<i64>() {
                self.termdag.lit(ast::Literal::Int(n))
            } else if op == "true" {
                self.termdag.lit(ast::Literal::Bool(true))
            } else if op == "false" {
                self.termdag.lit(ast::Literal::Bool(false))
            } else {
                self.termdag.app(op.into(), children)
            }
        } else {
            self.termdag.app(op.into(), children)
        };

        self.correspondence.insert(term.clone(), node_id);

        term
    }
}

/// Handles the edge case of cycles, then calls get_node_cost
fn calculate_cost_set(
    egraph: &egraph_serialize::EGraph,
    node_id: NodeId,
    extractor: &mut Extractor,
) -> CostSet {
    let node = &egraph[&node_id];
    let cid = egraph.nid_to_cid(&node_id);

    let children_classes = node
        .children
        .iter()
        .map(|c| egraph.nid_to_cid(c).clone())
        .collect::<Vec<ClassId>>();

    let child_cost_sets: Vec<_> = children_classes
        .iter()
        .map(|c| extractor.costs.get(c).unwrap())
        .collect();

    // cycle detection
    if child_cost_sets.iter().any(|cs| cs.costs.contains_key(cid)) {
        return CostSet {
            costs: Default::default(),
            total: std::f64::INFINITY.try_into().unwrap(),
            // returns junk children since this cost set is guaranteed to not be selected.
            term: extractor.termdag.app(node.op.as_str().into(), vec![]),
        };
    }

    let mut shared_total = NotNan::new(0.).unwrap();
    let mut unshared_total = extractor.cm.get_op_cost(&node.op);
    let mut costs = HashMap::default();

    let unshared_children = extractor.cm.unshared_children(&node.op);
    if !extractor.cm.ignore_children(&node.op) {
        for (i, child_set) in child_cost_sets.iter().enumerate() {
            if unshared_children.contains(&i) {
                unshared_total += child_set.total;
            } else {
                for (child_cid, child_cost) in &child_set.costs {
                    // it was already present in the set
                    if let Some(existing) = costs.insert(child_cid.clone(), *child_cost) {
                        assert_eq!(
                            existing, *child_cost,
                            "Two different costs found for the same child enode!"
                        );
                    } else {
                        shared_total += child_cost;
                    }
                }
            }
        }
    }
    costs.insert(cid.clone(), unshared_total);
    let total = unshared_total + shared_total;

    let sub_terms = child_cost_sets.iter().map(|cs| cs.term.clone()).collect();

    let term = extractor.get_term(node_id, sub_terms);

    CostSet { total, costs, term }
}

pub fn extract(
    original_prog: &TreeProgram,
    egraph: &egraph_serialize::EGraph,
    unextractables: HashSet<String>,
    termdag: &mut TermDag,
    cost_model: impl CostModel,
) -> CostSet {
    let extractor_not_linear = &mut Extractor::new(
        original_prog,
        &cost_model,
        termdag,
        Default::default(),
        egraph,
        unextractables,
    );

    let res = extract_without_linearity(extractor_not_linear);
    // TODO implement linearity
    let effectful_nodes_along_path =
        extractor_not_linear.find_effectful_nodes_in_program(&res.term);
    let _effectful_regions_along_path = effectful_nodes_along_path
        .into_iter()
        .filter(|nid| extractor_not_linear.is_region_node(nid.clone()))
        .collect::<HashSet<NodeId>>();

    // TODO loop over effectful regions
    // 1) Find reachable nodes in this region
    // 2) Extract sub-regions
    // 3) Extract this region, banning all nodes in effectful regions not on the state edge path
    // 4) extract current region from scratch, sub-regions get cost from previous extraction
    //    a) mark effectful nodes along the path as extractable (just for this region)
    //    b) extract the region

    // To get the type of an e-node, we use the old extractor and query its type

    /*let mut linear_egraph = egraph.clone();
    remove_invalid_effectful_nodes(&mut linear_egraph, &effectful_regions, todo!());

    let extract = &mut Extractor::new(
        &cost_model,
        termdag,
        Default::default(),
        &linear_egraph,
        unextractables,
    );
    let res = extract_without_linearity(extractor_not_linear);*/

    extract_without_linearity(extractor_not_linear)
}

/// Perform a greedy extraction of the DAG, without considering linearity.
/// This uses the "fast_greedy_dag" algorithm from the extraction gym.
pub fn extract_without_linearity(extractor: &mut Extractor) -> CostSet {
    let n2c = |nid: &NodeId| extractor.egraph.nid_to_cid(nid);
    let parents = build_parent_index(extractor.egraph);
    let mut worklist = initialize_worklist(extractor.egraph);

    while let Some(node_id) = worklist.pop() {
        let class_id = n2c(&node_id);
        let node = &extractor.egraph[&node_id];
        if extractor.unextractables.contains(&node.op) {
            continue;
        }
        if node
            .children
            .iter()
            .all(|c| extractor.costs.contains_key(n2c(c)))
        {
            let lookup = extractor.costs.get(class_id);
            let mut prev_cost: Cost = std::f64::INFINITY.try_into().unwrap();
            if lookup.is_some() {
                prev_cost = lookup.unwrap().total;
            }

            let cost_set = calculate_cost_set(extractor.egraph, node_id.clone(), extractor);
            if cost_set.total < prev_cost {
                extractor.costs.insert(class_id.clone(), cost_set);
                worklist.extend(parents[class_id].iter().cloned());
            }
        }
    }

    let mut root_eclasses = extractor.egraph.root_eclasses.clone();
    root_eclasses.sort();
    root_eclasses.dedup();

    let root = get_root(extractor.egraph);
    extractor.costs.get(n2c(&root)).unwrap().clone()
}

pub trait CostModel {
    /// TODO: we could do better with type info
    fn get_op_cost(&self, op: &str) -> Cost;

    /// if true, the op's children are ignored in calculating the cost
    fn ignore_children(&self, op: &str) -> bool;

    /// returns a slice of indices into the children vec
    /// count the cost of these children, but don't add the nodes they depend on to the set
    fn unshared_children(&self, op: &str) -> &'static [usize];
}

pub struct DefaultCostModel;

impl CostModel for DefaultCostModel {
    fn get_op_cost(&self, op: &str) -> Cost {
        match op {
            // Leaves
            "Const" => 1.,
            "Arg" => 0.,
            _ if op.parse::<i64>().is_ok() || op.starts_with('"') => 0.,
            "true" | "false" | "()" => 0.,
            // Lists
            "Empty" | "Single" | "Concat" | "Get" | "Nil" | "Cons" => 0.,
            // Types
            "IntT" | "BoolT" | "PointerT" | "StateT" => 0.,
            "Base" | "TupleT" | "TNil" | "TCons" => 0.,
            "Int" | "Bool" => 0.,
            // Algebra
            "Add" | "PtrAdd" | "Sub" | "And" | "Or" | "Not" => 10.,
            "Mul" => 30.,
            "Div" => 50.,
            // Comparisons
            "Eq" | "LessThan" | "GreaterThan" | "LessEq" | "GreaterEq" => 10.,
            // Effects
            "Print" | "Write" | "Load" => 50.,
            "Alloc" | "Free" => 100.,
            "Call" => 1000., // TODO: we could make this more accurate
            // Control
            "Program" | "Function" => 1.,
            "DoWhile" => 100., // TODO: we could make this more accurate
            "If" | "Switch" => 50.,
            // Unreachable
            "HasType" | "HasArgType" | "ContextOf" | "NoContext" | "ExpectType" => 0.,
            "ExprIsPure" | "ListExprIsPure" | "BinaryOpIsPure" | "UnaryOpIsPure" => 0.,
            "IsLeaf" | "BodyContainsExpr" | "ScopeContext" => 0.,
            "Region" | "Full" | "IntB" | "BoolB" => 0.,
            // Schema
            "Bop" | "Uop" | "Top" => 0.,
            "InContext" => 0.,
            _ if self.ignore_children(op) => 0.,
            _ => panic!("no cost for {op}"),
        }
        .try_into()
        .unwrap()
    }

    fn ignore_children(&self, op: &str) -> bool {
        matches!(op, "InLoop" | "NoContext" | "InSwitch" | "InIf")
    }

    fn unshared_children(&self, op: &str) -> &'static [usize] {
        match op {
            "DoWhile" => &[1],
            "Function" => &[3],
            "If" => &[2, 3],
            "Switch" => &[2], // TODO: Switch branches can share nodes
            _ => &[],
        }
    }
}

/** A data structure to maintain a queue of unique elements.

Notably, insert/pop operations have O(1) expected amortized runtime complexity.

Thanks @Bastacyclop for the implementation!
*/
#[derive(Clone)]
pub(crate) struct UniqueQueue<T>
where
    T: Eq + std::hash::Hash + Clone,
{
    set: HashSet<T>,
    queue: VecDeque<T>,
}

impl<T> Default for UniqueQueue<T>
where
    T: Eq + std::hash::Hash + Clone,
{
    fn default() -> Self {
        UniqueQueue {
            set: Default::default(),
            queue: Default::default(),
        }
    }
}

impl<T> UniqueQueue<T>
where
    T: Eq + std::hash::Hash + Clone,
{
    pub fn insert(&mut self, t: T) {
        if self.set.insert(t.clone()) {
            self.queue.push_back(t);
        }
    }

    pub fn extend<I>(&mut self, iter: I)
    where
        I: IntoIterator<Item = T>,
    {
        for t in iter.into_iter() {
            self.insert(t);
        }
    }

    pub fn pop(&mut self) -> Option<T> {
        let res = self.queue.pop_front();
        res.as_ref().map(|t| self.set.remove(t));
        res
    }

    #[allow(dead_code)]
    pub fn is_empty(&self) -> bool {
        let r = self.queue.is_empty();
        debug_assert_eq!(r, self.set.is_empty());
        r
    }
}

// For a given enode, if it creates sub-regions
// return the roots for these sub-regions
fn enode_regions(
    egraph: &egraph_serialize::EGraph,
    enode: &egraph_serialize::Node,
) -> Option<Vec<ClassId>> {
    match (enode.op.as_str(), enode.children.as_slice()) {
        ("DoWhile", [_input, body]) => Some(vec![egraph.nid_to_cid(body).clone()]),
        ("If", [_pred, _input, then_branch, else_branch]) => Some(vec![
            egraph.nid_to_cid(then_branch).clone(),
            egraph.nid_to_cid(else_branch).clone(),
        ]),
        ("Switch", [_pred, _input, branchlist]) => Some(get_conslist_children(
            egraph,
            egraph.nid_to_cid(branchlist).clone(),
        )),
        ("Function", [_name, _args, _ret, body]) => Some(vec![egraph.nid_to_cid(body).clone()]),
        _ => None,
    }
}

// For a given enode, if it creates sub-regions
// return the roots for these sub-regions
fn enode_nonregions(
    egraph: &egraph_serialize::EGraph,
    enode: &egraph_serialize::Node,
) -> Vec<ClassId> {
    match (enode.op.as_str(), enode.children.as_slice()) {
        ("DoWhile", [input, _body]) => vec![egraph.nid_to_cid(input).clone()],
        ("If", [pred, input, _then_branch, _else_branch]) => vec![
            egraph.nid_to_cid(pred).clone(),
            egraph.nid_to_cid(input).clone(),
        ],
        ("Switch", [pred, input, _branchlist]) => vec![
            egraph.nid_to_cid(pred).clone(),
            egraph.nid_to_cid(input).clone(),
        ],
        ("Function", [_name, _args, _ret, _body]) => vec![],
        _ => {
            let mut children = vec![];
            for child in &enode.children {
                children.push(egraph.nid_to_cid(child).clone());
            }
            children
        }
    }
}

fn get_conslist_children(egraph: &egraph_serialize::EGraph, class_id: ClassId) -> Vec<ClassId> {
    // assert that there is only one e-node in the eclass
    let class = egraph.classes()[&class_id].clone();
    assert_eq!(class.nodes.len(), 1);
    let node = egraph[&class.nodes[0]].clone();
    match node.op.as_str() {
        "Nil" => vec![],
        "Cons" => {
            let mut children = vec![egraph.nid_to_cid(&node.children[0]).clone()];
            children.extend(get_conslist_children(
                egraph,
                egraph.nid_to_cid(&node.children[1]).clone(),
            ));
            children
        }
        _ => panic!("Expected Cons or Nil, found {:?}", node.op),
    }
}

fn reachable_classes(egraph: &egraph_serialize::EGraph, root: ClassId) -> HashSet<ClassId> {
    let mut visited = HashSet::new();
    let mut queue = UniqueQueue::default();
    queue.insert(root);

    while let Some(eclass) = queue.pop() {
        if visited.contains(&eclass) {
            continue;
        }
        visited.insert(eclass.clone());

        for node in &egraph.classes()[&eclass].nodes {
            for child in enode_nonregions(egraph, &egraph[node]) {
                queue.insert(child);
            }
        }
    }

    visited
}

#[test]
fn test_dag_extract() {
    use crate::ast::*;
    use crate::{print_with_intermediate_vars, prologue};
    let prog = program!(
        function(
            "main",
            tuplet!(intt(), statet()),
            tuplet!(intt(), statet()),
            parallel!(
                add(
                    int(10),
                    get(
                        dowhile(
                            parallel!(getat(0)),
                            push(
                                add(getat(0), int(10)),
                                single(less_than(add(getat(0), int(10)), int(10)))
                            )
                        ),
                        0
                    )
                ),
                getat(1)
            )
        ),
        function(
            "niam",
            tuplet!(intt(), statet()),
            tuplet!(intt(), statet()),
            parallel!(
                add(
                    int(10),
                    get(
                        dowhile(
                            parallel!(get(arg(), 0)),
                            push(
                                add(getat(0), int(10)),
                                single(less_than(add(getat(0), int(10)), int(10)))
                            )
                        ),
                        0
                    )
                ),
                getat(1)
            )
        )
    );

    let string_prog = {
        let (term, termdag) = prog.to_egglog();
        let printed = print_with_intermediate_vars(&termdag, term);
        format!("{}\n{printed}\n", prologue(),)
    };

    let mut egraph = egglog::EGraph::default();
    egraph.parse_and_run_program(&string_prog).unwrap();
    let (serialized_egraph, unextractables) = serialized_egraph(egraph);
    let mut termdag = TermDag::default();
    let cost_model = DefaultCostModel;

    let cost_set = extract(
        &prog,
        &serialized_egraph,
        unextractables,
        &mut termdag,
        DefaultCostModel,
    );
    eprintln!("{}", termdag.to_string(&cost_set.term));

    let cost_of_one_func = cost_model.get_op_cost("Add") * 2.
        + cost_model.get_op_cost("DoWhile")
        + cost_model.get_op_cost("LessThan")
        // while the same const is used three times, it is only counted twice
        + cost_model.get_op_cost("Const") * 2.;
    let expected_cost = cost_of_one_func * 2.
        + cost_model.get_op_cost("Function") * 2.
        + cost_model.get_op_cost("Program");

    assert_eq!(cost_set.total, expected_cost);
}