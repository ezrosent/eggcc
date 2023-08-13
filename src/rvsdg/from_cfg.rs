//! Conversion from (structured) CFG to RVSDG.

use bril_rs::{ConstOps, EffectOps, Instruction, Literal, Position, Type, ValueOps};
use hashbrown::HashMap;
use petgraph::visit::IntoNeighborsDirected;
use petgraph::Direction;
use petgraph::{algo::dominators::Dominators, stable_graph::NodeIndex};

use crate::cfg::{BranchOp, Cfg, Identifier};
use crate::rvsdg::{Annotation, Result};

use super::live_variables::Names;
use super::{
    live_variables::{LiveVariableAnalysis, VarId},
    Expr, Id, Operand, RvsdgBody, RvsdgError,
};

pub(crate) struct RvsdgBuilder<'a> {
    cfg: &'a mut Cfg,
    expr: Vec<RvsdgBody>,
    analysis: LiveVariableAnalysis,
    dom: Dominators<NodeIndex>,
    // TODO: initialize mapping to arguments
    env: HashMap<VarId, Operand>,
}

impl<'a> RvsdgBuilder<'a> {
    fn try_loop(&mut self, block: NodeIndex) -> Result<NodeIndex> {
        // First, check if this is the head of a loop. There are two cases here:
        //
        // 1. The loop is a single block, in which case this block will have
        // itself as a neighbor.
        let is_self_loop = self
            .cfg
            .graph
            .neighbors_directed(block, Direction::Outgoing)
            .any(|n| n == block);
        // 2. this is the start of a "do-while" loop. We can check this by seeing
        // if `block` dominates any of its incoming edges.
        let loop_tail = if is_self_loop {
            None
        } else {
            self.cfg
                .graph
                .neighbors_directed(block, Direction::Incoming)
                .find(|pred| {
                    let Some(mut dom) = self.dom.dominators(*pred) else { return false; };
                    dom.any(|n| n == block)
                })
        };

        if !is_self_loop && loop_tail.is_none() {
            // This is not a loop! Look at the other cases.
            return self.try_branch(block);
        }

        // First, we need to record the live operands going into the loop. These
        // are the loop inputs.
        let live_vars = self.analysis.var_state(block).unwrap();

        debug_assert_eq!(&live_vars.live_in, &live_vars.live_out);

        let mut inputs = Vec::new();
        let pos = self.cfg.graph[block].pos.clone();
        for (i, input) in live_vars.live_in.iter().enumerate() {
            // Record the initial value of the loop variable
            inputs.push(get_op(input, &pos, &self.env, &self.analysis.intern)?);
            // Mark it as an argument to the loop.
            self.env.insert(input, Operand::Arg(i as u32));
        }

        // Now we "run" the loop until we reach the end:
        let tail = if let Some(tail) = loop_tail {
            let mut next = self.try_branch(block)?;
            while next != tail {
                next = self.try_loop(block)?;
            }
            tail
        } else {
            debug_assert!(is_self_loop);
            block
        };

        self.translate_block(tail)?;

        let live_vars = self.analysis.var_state(block).unwrap();
        let mut outputs = Vec::with_capacity(inputs.len());
        for input in live_vars.live_in.iter() {
            outputs.push(get_op(input, &pos, &self.env, &self.analysis.intern)?);
        }

        // Now to discover the loop predicate:
        // TODO:
        // * We may want to rethink our loop predicates here.
        // * All "native" bril branches are booleans
        // * All "synthesized" branches are switches.
        // * We could have a specialized Condition representation (Theta { pred: Pred(n_opts: 2, op: Operand))
        // let mut op = None;
        for edge in self.cfg.graph.edges_connecting(tail, block) {
            match &edge.weight().op {
                BranchOp::Jmp => {
                    //          assert!(op.is_none());
                    todo!()
                }
                BranchOp::Cond { arg, val } => todo!(),
                BranchOp::RetVal { arg } => todo!(),
            }
            todo!()
        }

        // let theta_node = get_id(&mut self.expr, RvsdgBody::Theta { pred: (), inputs, outputs })

        todo!()
    }

    fn try_branch(&mut self, block: NodeIndex) -> Result<NodeIndex> {
        todo!()
    }

    fn get_op(&self, var: VarId, pos: &Option<Position>) -> Result<Operand> {
        match self.env.get(&var) {
            Some(op) => Ok(*op),
            None => Err(RvsdgError::UndefinedId {
                id: self.analysis.intern.get_var(var).clone(),
                pos: pos.clone(),
            }),
        }
    }
    fn translate_block(&mut self, block: NodeIndex) -> Result<()> {
        let block = &self.cfg.graph[block];

        fn convert_args(
            args: &[String],
            analysis: &mut LiveVariableAnalysis,
            env: &mut HashMap<VarId, Operand>,
            pos: &Option<Position>,
        ) -> Result<Vec<Operand>> {
            let mut ops = Vec::with_capacity(args.len());
            for arg in args {
                let arg_var = analysis.intern.intern(arg);
                let Some(arg_id) = env.get(&arg_var).copied() else {
                    return Err(RvsdgError::UndefinedId {
                        id: arg.into(),
                        pos: pos.clone(),
                    });
                };
                ops.push(arg_id);
            }
            Ok(ops)
        }

        for instr in &block.instrs {
            match instr {
                Instruction::Constant {
                    dest,
                    op,
                    const_type,
                    value,
                    ..
                } => {
                    let dest_var = self.analysis.intern.intern(dest);
                    let const_id = get_id(
                        &mut self.expr,
                        RvsdgBody::PureOp(Expr::Const(*op, const_type.clone(), value.clone())),
                    );
                    self.env.insert(dest_var, Operand::Id(const_id));
                }
                Instruction::Value {
                    args,
                    dest,
                    funcs,
                    labels: _,
                    op,
                    pos,
                    op_type: _,
                } => match op {
                    ValueOps::Alloc | ValueOps::Load | ValueOps::PtrAdd => {
                        return Err(RvsdgError::UnsupportedOperation {
                            op: *op,
                            pos: pos.clone(),
                        });
                    }
                    ValueOps::Id => {
                        let dest_var = self.analysis.intern.intern(dest);
                        let src_var = self.analysis.intern.intern(&args[0]);
                        let Some(arg_id) = self.env.get(&src_var).copied() else {
                            return Err(RvsdgError::UndefinedId {
                                id: self.analysis.intern.get_var(src_var).clone(),
                                pos: pos.clone(),
                            });
                        };
                        self.env.insert(dest_var, arg_id);
                    }
                    _ => {
                        let dest_var = self.analysis.intern.intern(dest);
                        let ops = convert_args(args, &mut self.analysis, &mut self.env, pos)?;
                        let expr = if let ValueOps::Call = op {
                            Expr::Call((&funcs[0]).into(), ops)
                        } else {
                            Expr::Op(*op, ops)
                        };
                        let expr_id = get_id(&mut self.expr, RvsdgBody::PureOp(expr));
                        self.env.insert(dest_var, Operand::Id(expr_id));
                    }
                },
                Instruction::Effect { op, pos, .. } => {
                    if let EffectOps::Nop = op {
                        continue;
                    }
                    // Two notes here:
                    // * Control flow like Return and Jmp _are_ supported, but
                    // the instructions should be eliminated as part of CFG
                    // conversion and they should instead show up as branches.
                    //
                    // * Print isn't supported (yet!) because it would require
                    // some form of "state" plumbing to ensure it is actually
                    // run.
                    return Err(RvsdgError::UnsupportedEffect {
                        op: *op,
                        pos: pos.clone(),
                    });
                }
            }
        }

        for ann in &block.footer {
            match ann {
                Annotation::AssignCond { dst, cond } => {
                    let id = get_id(
                        &mut self.expr,
                        RvsdgBody::PureOp(Expr::Const(
                            ConstOps::Const,
                            Type::Int,
                            Literal::Int(*cond as i64),
                        )),
                    );
                    let dest_var = self.analysis.intern.intern(dst.clone());
                    self.env.insert(dest_var, Operand::Id(id));
                }
            }
        }
        Ok(())
    }
}

fn get_id(exprs: &mut Vec<RvsdgBody>, body: RvsdgBody) -> Id {
    let id = exprs.len();
    exprs.push(body);
    id as Id
}

fn get_op(
    var: VarId,
    pos: &Option<Position>,
    env: &HashMap<VarId, Operand>,
    intern: &Names,
) -> Result<Operand> {
    match env.get(&var) {
        Some(op) => Ok(*op),
        None => Err(RvsdgError::UndefinedId {
            id: intern.get_var(var).clone(),
            pos: pos.clone(),
        }),
    }
}
