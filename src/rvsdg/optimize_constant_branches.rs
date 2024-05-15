//! Optimize the output CFG to avoid redundant branches.
//!
//! This postprocessing operation optimizes code sequences that rely on the
//! pattern of assigning to a constant value in a predecessor branch and then
//! branching on that value in the successor. RVSDGs are a structured format and
//! a naive translation of RVSDGs into bril can emit code of this form. CFGs
//! support unstructured control flow, so these kinds of operations can be
//! optimized into direct jumps.
//!
//! This code does exactly that. It is based on the PCFR algorithm from the
//! paper "Perfect Reconstrucability of Control Flow from Demand Dependence
//! Graphs" by Bahmann, Reissmann, Jahre, and Meyer. The two major changes from
//! that algorithm are:
//!
//! * It does not assume the CFG is in 'predicate continuation form', where
//! predicates are immediately "consumed" by a branch. Our translation from
//! RVSDGs back to bril does not preserve this property.
//!
//! * Instead of following predicate values "forward" and rewriting branches in
//! that direction, this algorithm iterates _backward_, starting at branches and
//! then searching for nodes that assign the branch value to a constant in a
//! direct predecessor. This requires us to iterate to a fixed point, which
//! given the lack of a good worklist means that the current algorithm may be a
//! bit slower. In practice, the algorithm iterates less than three times over
//! any programs in the test suite. Still; there is plenty of low-hanging
//! performance fruit here.

use bril_rs::{Instruction, Literal, Type, ValueOps};
use hashbrown::HashMap;
use petgraph::{
    graph::NodeIndex,
    visit::{Dfs, EdgeRef, IntoNodeIdentifiers},
    Direction,
};

use crate::cfg::{BasicBlock, Branch, BranchOp, Identifier, SimpleCfgFunction};

pub(crate) fn optimize_constant_branches(mut func: SimpleCfgFunction) -> SimpleCfgFunction {
    loop {
        let potential = func.potential_branches();
        if func.jump_forwarding(&potential) {
            continue;
        }
        break;
    }
    let mut walker = Dfs::new(&func.graph, func.entry);
    while walker.next(&func.graph).is_some() {}
    let mut to_remove = vec![];
    for node_id in func.graph.node_identifiers() {
        if !walker.discovered.contains(node_id.index()) {
            to_remove.push(node_id);
        }
    }
    for node_id in to_remove {
        func.graph.remove_node(node_id);
    }
    func
}

#[derive(Clone)]
enum BrilValue {
    Const(Literal, Type),
    Ident(Identifier),
    Unknown,
}

impl SimpleCfgFunction {
    fn potential_branches(&self) -> PotentialBranches {
        let mut potential_branches = PotentialBranches::default();

        'outer: for node in self.graph.node_indices() {
            let mut branch_on = None;
            let mut targets = vec![];
            for e in self.graph.edges(node) {
                let branch = e.weight();
                match &branch.op {
                    BranchOp::Jmp => continue 'outer,
                    BranchOp::Cond { arg, val, .. } => {
                        branch_on = Some(arg.clone());
                        targets.resize(val.of as usize, NodeIndex::end());
                        targets[val.val as usize] = e.target();
                    }
                }
            }
            let Some(branch_on) = branch_on else {
                continue;
            };
            let block = &self.graph[node];
            if !block.footer.is_empty() {
                // We shouldn't have nontrivial footers on the way back to bril.
                continue;
            }
            let source_of = execute_simple_block(block);
            let to_watch = match source_of.get(&branch_on) {
                Some(BrilValue::Const(..)) => {
                    // NB: we could rewrite things here too, but we don't seem
                    // to generate code that does this in practice.
                    continue;
                }
                Some(BrilValue::Ident(src)) => src.clone(),
                Some(BrilValue::Unknown) => continue,
                None => branch_on,
            };
            debug_assert!(
                targets.iter().all(|t| t != &NodeIndex::end()),
                "branch targets in a bad state: {targets:?}"
            );
            potential_branches.nodes.insert(node, (to_watch, targets));
        }

        potential_branches
    }

    fn jump_forwarding(&mut self, potential: &PotentialBranches) -> bool {
        let mut to_stitch = vec![];
        for (branch_node, (ident, targets)) in &potential.nodes {
            for incoming in self.graph.edges_directed(*branch_node, Direction::Incoming) {
                // First, check for direct jumps to the branch node.
                if !matches!(incoming.weight().op, BranchOp::Jmp) {
                    continue;
                }
                // Now, see if we bind the branch value to a constant.
                let pred_node = &self.graph[incoming.source()];
                if !pred_node.footer.is_empty() {
                    continue;
                }
                let source_of = execute_simple_block(pred_node);
                if let Some(BrilValue::Const(c, _)) = source_of.get(ident) {
                    let target = targets[literal_to_branch_target(c)];
                    to_stitch.push((incoming.id(), target));
                }
            }
        }

        let changed = !to_stitch.is_empty();

        for (old_edge, new_target) in to_stitch {
            let (src, old_target) = self.graph.edge_endpoints(old_edge).unwrap();
            let old_block = self.graph[old_target].clone();
            self.graph[src].instrs.extend(old_block.instrs.into_iter());
            self.graph.remove_edge(old_edge);
            self.graph.add_edge(
                src,
                new_target,
                Branch {
                    op: BranchOp::Jmp,
                    pos: None,
                },
            );
        }
        changed
    }
}

#[derive(Default)]
struct PotentialBranches {
    nodes: HashMap<NodeIndex, (Identifier /* branched  */, Vec<NodeIndex>)>,
}

fn execute_simple_block(block: &BasicBlock) -> HashMap<Identifier, BrilValue> {
    let mut res = HashMap::<Identifier, BrilValue>::new();
    for instr in &block.instrs {
        match instr {
            Instruction::Constant {
                dest,
                const_type,
                value,
                ..
            } => {
                res.insert(
                    dest.into(),
                    BrilValue::Const(value.clone(), const_type.clone()),
                );
            }
            Instruction::Value {
                args,
                dest,
                op: ValueOps::Id,
                ..
            } => {
                let src = Identifier::from(args[0].clone());
                match res.get(&src) {
                    Some(val) => {
                        res.insert(dest.into(), val.clone());
                    }
                    None => {
                        res.insert(dest.into(), BrilValue::Ident(src));
                    }
                }
            }
            Instruction::Value { dest, .. } => {
                // Only analyze id and constant operations.
                res.insert(dest.into(), BrilValue::Unknown);
            }
            _ => {}
        }
    }
    res
}

fn literal_to_branch_target(lit: &Literal) -> usize {
    match lit {
        Literal::Int(i) => *i as usize,
        Literal::Bool(b) => *b as usize,
        Literal::Float(_) | Literal::Char(_) => unreachable!(),
    }
}
