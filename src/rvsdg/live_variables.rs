//! Basic live variable analysis for bril programs.
//!
//! The structure here follows that of optir, which in turn implements a (less
//! optimized) variant of the live variable analysis described in "Iterative
//! Data-flow Analysis, Revisited", by Keith D. Cooper, Timothy J. Harvey, and
//! Ken Kennedy.
use bril_rs::Instruction;
use fixedbitset::FixedBitSet;
use hashbrown::HashMap;
use indexmap::IndexSet;
use petgraph::{
    stable_graph::NodeIndex,
    visit::{DfsPostOrder, VisitMap, Visitable},
};

use crate::cfg::{Cfg, Identifier, NodeSet};

use super::Annotation;

pub(crate) fn live_variables(cfg: &Cfg) -> LiveVariableAnalysis {
    let mut analysis = LiveVariableAnalysis::default();
    let mut names = Names::default();
    let mut dfs = DfsPostOrder::new(&cfg.graph, cfg.entry);
    let mut worklist = WorkList::new(cfg);
    while let Some(block) = dfs.next(&cfg.graph) {
        let state = analysis.var_state_mut(block);
        let weight = &cfg.graph[block];

        for ann in weight.footer.iter().rev() {
            match ann {
                Annotation::AssignCond { dst, .. } => {
                    let var = names.intern(dst.clone());
                    state.kills.insert(var);
                    state.gen.remove(var);
                }
            }
        }
        for instr in weight.instrs.iter().rev() {
            match instr {
                Instruction::Constant { dest, .. } => {
                    let var = names.intern(dest);
                    state.kills.insert(var);
                    state.gen.remove(var);
                }
                Instruction::Value { args, dest, .. } => {
                    let dest = names.intern(dest);
                    state.kills.insert(dest);
                    state.gen.remove(dest);
                    for arg in args {
                        state.gen.insert(names.intern(arg));
                    }
                }
                Instruction::Effect { args, .. } => {
                    for arg in args {
                        state.gen.insert(names.intern(arg));
                    }
                }
            }
        }
        worklist.push(block);
    }

    while let Some(block) = worklist.pop() {
        let mut changed = false;
        // Update live_in
        let state = analysis.var_state_mut(block);
        changed |= state.live_in.merge(&state.gen);
        for x in state.live_out.vars.difference(&state.kills.vars) {
            changed |= state.live_in.insert(VarId(x as u32));
        }

        // Update live_out
        for succ in cfg.graph.neighbors(block) {
            changed |= analysis.union_out_in(block, succ);
        }

        if changed {
            cfg.graph
                .neighbors(block)
                .for_each(|succ| worklist.push(succ))
        }
    }

    analysis.intern = names;
    analysis
}

/// An opaque Id representing a variable.
#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub(crate) struct VarId(u32);

#[derive(Default)]
pub(crate) struct Names {
    table: IndexSet<Identifier>,
}

impl Names {
    pub(crate) fn get_var(&self, id: VarId) -> &Identifier {
        self.table.get_index(id.0 as usize).unwrap()
    }

    pub(crate) fn intern(&mut self, name: impl Into<Identifier>) -> VarId {
        let name = name.into();
        if let Some(id) = self.table.get_index_of(&name) {
            return VarId(id as u32);
        }
        let id = u32::try_from(self.table.len()).unwrap();
        self.table.insert(name);
        VarId(id)
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub(crate) struct VarSet {
    vars: FixedBitSet,
}

impl VarSet {
    pub(crate) fn iter(&self) -> impl Iterator<Item = VarId> + '_ {
        self.vars.ones().map(|x| VarId(x as u32))
    }
    fn insert(&mut self, var: VarId) -> bool {
        let bit = var.0 as usize;
        if self.vars.len() <= bit {
            self.vars.grow(bit + 1);
        }
        !self.vars.put(bit)
    }

    fn remove(&mut self, var: VarId) {
        let bit = var.0 as usize;
        if self.vars.len() <= bit {
            return;
        }
        self.vars.set(bit, false);
    }

    fn merge(&mut self, other: &VarSet) -> bool {
        if other.vars.is_subset(&self.vars) {
            return false;
        }
        self.vars.union_with(&other.vars);
        true
    }
}

/// The per-basic block state associated with the live variable analysis.
pub(crate) struct LiveVariableState {
    /// The variables live on entry to a given basic block.
    pub(crate) live_in: VarSet,
    /// The variables live on exit from the basic block.
    pub(crate) live_out: VarSet,
    /// The variables written to in the basic block.
    kills: VarSet,
    /// The variables used before they are written to in the basic block.
    gen: VarSet,
}

#[derive(Default)]
pub(crate) struct LiveVariableAnalysis {
    pub(crate) intern: Names,
    analysis: HashMap<NodeIndex, LiveVariableState>,
}

impl LiveVariableAnalysis {
    fn var_state_mut(&mut self, node: NodeIndex) -> &mut LiveVariableState {
        self.analysis.entry(node).or_insert_with(|| {
            let var_set = VarSet {
                vars: FixedBitSet::with_capacity(self.intern.table.len()),
            };
            LiveVariableState {
                live_in: var_set.clone(),
                live_out: var_set.clone(),
                kills: var_set.clone(),
                gen: var_set,
            }
        })
    }

    pub(crate) fn var_state(&self, node: NodeIndex) -> Option<&LiveVariableState> {
        self.analysis.get(&node)
    }

    /// Union pred's `live_out` set with succ's `live_in` set.
    fn union_out_in(&mut self, pred: NodeIndex, succ: NodeIndex) -> bool {
        let Some([pred_state, succ_state]) = self.analysis.get_many_mut([&pred, &succ]) else { return false; };
        pred_state.live_out.merge(&succ_state.live_in)
    }
}

struct WorkList {
    node_set: NodeSet,
    stack: Vec<NodeIndex>,
}

impl WorkList {
    fn new(cfg: &Cfg) -> WorkList {
        WorkList {
            node_set: cfg.graph.visit_map(),
            stack: Default::default(),
        }
    }

    fn push(&mut self, node: NodeIndex) {
        if !self.node_set.is_visited(&node) {
            self.stack.push(node);
        }
    }

    fn pop(&mut self) -> Option<NodeIndex> {
        let res = self.stack.pop()?;
        self.node_set.set(res.index(), false);
        Some(res)
    }
}
