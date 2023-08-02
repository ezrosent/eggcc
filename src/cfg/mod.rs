#![allow(dead_code)] // TODO: remove this once wired in
//! Parse a bril program into a CFG.
//!
//! The methods here largely ignore the instructions in the program: all that we
//! look for here are instructions that may break up basic blocks (`jmp`, `br`,
//! `ret`), and labels. All other instructions are copied into the CFG.
use std::collections::HashMap;
use std::mem;

use bril_rs::{Argument, Code, EffectOps, Function, Instruction, Position};
use petgraph::{graph::NodeIndex, Graph};

#[cfg(test)]
mod tests;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub(crate) enum BlockName {
    Entry,
    Exit,
    Named(String),
}

pub(crate) struct BasicBlock {
    pub(crate) instrs: Vec<Instruction>,
    pub(crate) name: BlockName,
    pub(crate) pos: Option<Position>,
}

impl BasicBlock {
    fn empty(name: BlockName) -> BasicBlock {
        BasicBlock {
            instrs: Default::default(),
            name,
            pos: None,
        }
    }
}

/// An number (`val`) between 0 and `of` (exclusive).
///
/// These are used to represent "switch" blocks in the CFG.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub(crate) struct CondVal {
    pub(crate) val: u32,
    pub(crate) of: u32,
}

impl From<bool> for CondVal {
    fn from(value: bool) -> Self {
        if value {
            CondVal { val: 1, of: 2 }
        } else {
            CondVal { val: 0, of: 2 }
        }
    }
}

/// A branch in the CFG.
pub(crate) struct Branch {
    /// The type of branch.
    pub(crate) op: BranchOp,
    /// The position of the branch in the original program.
    pub(crate) pos: Option<Position>,
}

/// The types of branch.
#[derive(PartialEq, Eq)]
pub(crate) enum BranchOp {
    /// An unconditional branch to a block.
    Jmp,
    /// A conditional branch to a block.
    Cond { arg: String, val: CondVal },
    /// A return statement carrying a value.
    RetVal { arg: String },
}

/// The control-flow graph for a single function.
pub(crate) struct Cfg {
    /// The arguments to the function.
    pub(crate) args: Vec<Argument>,
    /// The graph itself.
    pub(crate) graph: Graph<BasicBlock, Branch>,
    /// The entry node for the CFG.
    pub(crate) entry: NodeIndex,
    /// The (single) exit node for the CFG.
    pub(crate) exit: NodeIndex,
}

/// Get the underyling CFG corresponding to the function `func`.
///
/// The structure is reproduced exactly, aside from the addition of a single
/// exit node branched to from all return statements.
pub(crate) fn to_cfg(func: &Function) -> Cfg {
    let mut builder = CfgBuilder::new(func);
    let mut block = Vec::new();
    let mut current = builder.cfg.entry;
    let mut had_branch = false;
    for inst in &func.instrs {
        match inst {
            Code::Label { label, pos } => {
                let next_block = builder.get_index(label);
                builder.finish_block(current, mem::take(&mut block));
                builder.set_pos(next_block, pos.clone());
                if !had_branch {
                    builder.add_edge(
                        current,
                        next_block,
                        Branch {
                            op: BranchOp::Jmp,
                            pos: pos.clone(),
                        },
                    );
                }
                current = next_block;
                had_branch = false;
            }
            Code::Instruction(Instruction::Effect {
                args,
                funcs: _,
                labels,
                op: EffectOps::Branch,
                pos,
            }) => {
                had_branch = true;
                assert_eq!(labels.len(), 2, "unexpected format to branch instruction");
                assert_eq!(args.len(), 1, "unexpected format to branch instruction");
                let true_block = builder.get_index(&labels[0]);
                let false_block = builder.get_index(&labels[1]);
                let arg = &args[0];
                builder.add_edge(
                    current,
                    true_block,
                    Branch {
                        op: BranchOp::Cond {
                            arg: arg.clone(),
                            val: true.into(),
                        },
                        pos: pos.clone(),
                    },
                );
                builder.add_edge(
                    current,
                    false_block,
                    Branch {
                        op: BranchOp::Cond {
                            arg: arg.clone(),
                            val: false.into(),
                        },
                        pos: pos.clone(),
                    },
                );
            }
            Code::Instruction(Instruction::Effect {
                args: _,
                funcs: _,
                labels,
                op: EffectOps::Jump,
                pos,
            }) => {
                had_branch = true;
                assert_eq!(labels.len(), 1, "unexpected format to jump instruction");
                let dest_block = builder.get_index(&labels[0]);
                builder.add_edge(
                    current,
                    dest_block,
                    Branch {
                        op: BranchOp::Jmp,
                        pos: pos.clone(),
                    },
                );
            }
            Code::Instruction(Instruction::Effect {
                args,
                funcs: _,
                labels: _,
                op: EffectOps::Return,
                pos,
            }) => {
                had_branch = true;
                match args.as_slice() {
                    [] => {
                        builder.add_edge(
                            current,
                            builder.cfg.exit,
                            Branch {
                                op: BranchOp::Jmp,
                                pos: pos.clone(),
                            },
                        );
                    }
                    [arg] => {
                        builder.add_edge(
                            current,
                            builder.cfg.exit,
                            Branch {
                                op: BranchOp::RetVal { arg: arg.clone() },
                                pos: pos.clone(),
                            },
                        );
                    }
                    _ => panic!("unexpected format to return instruction"),
                }
            }
            Code::Instruction(i) => block.push(i.clone()),
        }
    }
    builder.finish_block(current, mem::take(&mut block));
    builder.build()
}

struct CfgBuilder {
    cfg: Cfg,
    label_to_block: HashMap<String, NodeIndex>,
}

impl CfgBuilder {
    fn new(func: &Function) -> CfgBuilder {
        let mut graph = Graph::default();
        let entry = graph.add_node(BasicBlock::empty(BlockName::Entry));
        let exit = graph.add_node(BasicBlock::empty(BlockName::Exit));
        CfgBuilder {
            cfg: Cfg {
                args: func.args.clone(),
                graph,
                entry,
                exit,
            },
            label_to_block: HashMap::new(),
        }
    }
    fn build(mut self) -> Cfg {
        // If there are no outgoing edges from the entry block, add a basic one returning to the exit.
        if self
            .cfg
            .graph
            .neighbors_directed(self.cfg.entry, petgraph::Outgoing)
            .next()
            .is_none()
        {
            self.cfg.graph.add_edge(
                self.cfg.entry,
                self.cfg.exit,
                Branch {
                    op: BranchOp::Jmp,
                    pos: None,
                },
            );
        }
        self.cfg
    }
    fn get_index(&mut self, label: &str) -> NodeIndex {
        *self
            .label_to_block
            .entry(label.to_string())
            .or_insert_with(|| {
                self.cfg
                    .graph
                    .add_node(BasicBlock::empty(BlockName::Named(label.into())))
            })
    }
    fn finish_block(&mut self, index: NodeIndex, block: Vec<Instruction>) {
        let BasicBlock { instrs, .. } = self.cfg.graph.node_weight_mut(index).unwrap();
        debug_assert!(instrs.is_empty());
        *instrs = block;
    }

    fn set_pos(&mut self, index: NodeIndex, pos: Option<Position>) {
        self.cfg.graph.node_weight_mut(index).unwrap().pos = pos;
    }

    fn add_edge(&mut self, src: NodeIndex, dst: NodeIndex, branch: Branch) {
        self.cfg.graph.add_edge(src, dst, branch);
    }
}
