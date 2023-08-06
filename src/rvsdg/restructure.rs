//! Convert a potentially irreducible CFG to a reducible one.

use hashbrown::HashMap;
use petgraph::{
    algo::{dominators, tarjan_scc},
    graph::NodeIndex,
    stable_graph::{EdgeIndex, StableDiGraph},
    visit::{NodeFiltered, VisitMap, Visitable},
    Direction,
};

use crate::{
    cfg::{BasicBlock, BlockName, Branch, BranchOp, Cfg, CondVal, Identifier},
    rvsdg::Annotation,
};

type NodeSet = <StableDiGraph<BasicBlock, Branch> as Visitable>::Map;

fn node_set(nodes: impl IntoIterator<Item = NodeIndex>) -> NodeSet {
    let mut set = NodeSet::default();
    for node in nodes {
        if set.len() <= node.index() {
            set.grow(node.index() + 1);
        }
        set.visit(node);
    }
    set
}

struct RestructureState {
    n_names: usize,
}

impl RestructureState {
    fn fresh(&mut self) -> Identifier {
        let n = self.n_names;
        self.n_names += 1;
        Identifier::Num(n)
    }
}

macro_rules! remove_edge {
    ($graph:expr, $edge:expr) => {{
        let weight = $graph.remove_edge($edge).unwrap();
        if let Some(pos) = &weight.pos {
            eprintln!("{}: removing edge with {pos:?}", line!());
        }
        weight
    }};
}

impl Cfg {
    fn fresh_block(&mut self) -> NodeIndex {
        let placeholder = self.graph.node_count();
        self.graph
            .add_node(BasicBlock::empty(BlockName::Placeholder(placeholder)))
    }

    pub(crate) fn restructure(&mut self) {
        let mut state = RestructureState { n_names: 0 };
        let mut all = NodeSet::with_capacity(self.graph.node_count());
        self.graph.node_indices().for_each(|node| {
            all.visit(node);
        });
        self.restructure_inner(&all, &mut state);
        let please_uncomment = 1;
        // self.restructure_branches(&mut state);
    }

    fn branch_if(
        &mut self,
        from: NodeIndex,
        to: NodeIndex,
        id: &Identifier,
        cv: CondVal,
    ) -> EdgeIndex {
        self.graph.add_edge(
            from,
            to,
            Branch {
                op: BranchOp::Cond {
                    arg: id.clone(),
                    val: cv,
                },
                pos: None,
            },
        )
    }

    fn restructure_inner(&mut self, filter: &NodeSet, state: &mut RestructureState) {
        // TODO: pool allocations for filters and node vectors
        let base = NodeFiltered::from_fn(&self.graph, |node| filter.is_visited(&node));
        let sccs = tarjan_scc(&base);
        let please_remove = 1;
        eprintln!(
            "sccs={:?}",
            sccs.iter()
                .map(|x| x
                    .iter()
                    .map(|node| self.graph[*node].name.clone())
                    .collect::<Vec<_>>())
                .collect::<Vec<_>>()
        );

        for scc in sccs {
            if scc.len() < 2 {
                let please_remove = 1;
                eprintln!("skipping {scc:?}");
                // Either a single block or a simple loop.
                continue;
            }

            let please_remove = 1;
            eprintln!("not skipping {scc:?}");

            let in_scc = node_set(scc.iter().copied());
            // Entry nodes are those from outside the SCC pointing in.
            let entry_nodes = node_set(scc.iter().copied().flat_map(|node| {
                self.graph
                    .neighbors_directed(node, petgraph::Incoming)
                    .filter(|neighbor| !in_scc.is_visited(neighbor))
            }));

            let entry_targets = node_set(entry_nodes.ones().map(NodeIndex::new).flat_map(|node| {
                self.graph
                    .neighbors_directed(node, petgraph::Outgoing)
                    .filter(|neighbor| in_scc.is_visited(neighbor))
            }));

            let self_loops = node_set(entry_nodes.ones().map(NodeIndex::new).flat_map(|node| {
                self.graph
                    .neighbors_directed(node, petgraph::Incoming)
                    .filter(|neighbor| in_scc.is_visited(neighbor))
            }));

            let exit_nodes = node_set(scc.iter().copied().flat_map(|node| {
                self.graph
                    .neighbors_directed(node, petgraph::Outgoing)
                    .filter(|neighbor| !in_scc.is_visited(neighbor))
            }));

            let n_entries = entry_nodes.count_ones(..);
            let n_exits = exit_nodes.count_ones(..);

            let please_remove = 1;
            eprintln!("entries={n_entries}, exits={n_exits}");

            // if n_entries == 1 && n_exits == 1 {
            //     continue;
            // }

            self_loops
                .ones()
                .chain(entry_nodes.ones())
                .map(NodeIndex::new)
                .for_each(|x| self.split_edges(x));
            let please_sort_this_out = 1;
            // DUPLICATION DUPLICATION
            // Entry nodes are those from outside the SCC pointing in.
            let entry_nodes = node_set(scc.iter().copied().flat_map(|node| {
                self.graph
                    .neighbors_directed(node, petgraph::Incoming)
                    .filter(|neighbor| !in_scc.is_visited(neighbor))
            }));

            let entry_targets = node_set(entry_nodes.ones().map(NodeIndex::new).flat_map(|node| {
                self.graph
                    .neighbors_directed(node, petgraph::Outgoing)
                    .filter(|neighbor| in_scc.is_visited(neighbor))
            }));

            let self_loops = node_set(entry_nodes.ones().map(NodeIndex::new).flat_map(|node| {
                self.graph
                    .neighbors_directed(node, petgraph::Incoming)
                    .filter(|neighbor| in_scc.is_visited(neighbor))
            }));
            let n_entries = entry_nodes.count_ones(..);
            let n_exits = exit_nodes.count_ones(..);

            let rep = state.fresh();

            assert!(n_entries < u32::MAX as usize);

            let footer = self.fresh_block();

            let exit = self.fresh_block();
            let entry = if entry_targets.count_ones(..) == 1 {
                NodeIndex::new(entry_nodes.ones().next().unwrap())
            } else {
                self.fresh_block()
            };

            if footer.index() == 7 {
                let please_remove = 1;
                eprintln!("entry={entry:?},exit={exit:?},footer={footer:?}");
            }

            self.branch_if(footer, exit, &rep, CondVal { val: 0, of: 1 });
            self.branch_if(footer, entry, &rep, CondVal { val: 1, of: 1 });

            let loop_info = LoopState {
                entry: &entry_nodes,
                rep: &self_loops,
                exit: &exit_nodes,
                scc: &in_scc,
                rep_var: &rep,
                entry_node: entry,
                tail_node: footer,
                exit_node: exit,
            };

            // NB: we don't handle the case where there is >1 entry node but
            // only one exit, like optir. We should potentially fix that.

            if entry_targets.count_ones(..) > 1 {
                self.split_entry_rep(&loop_info, state);
            }

            self.split_exit(&loop_info, state);

            if n_exits == 0 {
                // Infinite loop
                self.graph.remove_node(exit);
            }

            // Recursively restructure the inner loop.
            self.restructure_inner(&in_scc, state);
        }
    }

    fn split_edges(&mut self, node: NodeIndex) {
        let consider_refolding = 1;
        let mut has_jmp = false;
        let mut walker = self
            .graph
            .neighbors_directed(node, Direction::Outgoing)
            .detach();
        let mut edges = Vec::new();
        while let Some(x) = walker.next(&self.graph) {
            edges.push(x);
        }

        for (edge, other) in edges {
            assert!(!has_jmp);
            match &self.graph.edge_weight(edge).unwrap().op {
                BranchOp::Jmp => {
                    has_jmp = true;
                    continue;
                }
                BranchOp::Cond { .. } => {}
                BranchOp::RetVal { .. } => {
                    panic!("exit block in target set")
                }
            }

            // We have a conditional branch. Reroute through a placeholder.
            let weight = remove_edge!(self.graph, edge); // self.graph.remove_edge(edge).unwrap();
            let placeholder = self.fresh_block();

            // We had  node => other
            // We want node => placeholder => other
            assert_ne!(other, self.entry);
            self.graph.add_edge(node, placeholder, weight);
            self.graph.add_edge(placeholder, other, JMP);
        }
    }

    fn make_demux_node(
        &mut self,
        node: NodeIndex,
        targets: impl IntoIterator<Item = NodeIndex>,
        state: &mut RestructureState,
    ) -> (HashMap<NodeIndex, u32>, Identifier) {
        let mut blocks = HashMap::new();
        for node in targets {
            let cur_len = u32::try_from(blocks.len()).unwrap();
            blocks.entry(node).or_insert(cur_len);
        }

        let n_blocks = u32::try_from(blocks.len()).unwrap();
        let cond = state.fresh();
        for (block, val) in blocks.iter() {
            self.branch_if(
                node,
                *block,
                &cond,
                CondVal {
                    val: *val,
                    of: n_blocks,
                },
            );
        }
        (blocks, cond)
    }

    fn split_exit(&mut self, loop_info: &LoopState, state: &mut RestructureState) {
        // Demux in the exit block

        let (exit_blocks, cond) = self.make_demux_node(
            loop_info.exit_node,
            loop_info.exit.ones().map(NodeIndex::new),
            state,
        );

        for node in loop_info.exit.ones().map(NodeIndex::new) {
            // First: split edges coming into this node:
            self.graph
                .neighbors_directed(node, Direction::Incoming)
                .collect::<Vec<_>>()
                .into_iter()
                .for_each(|node| self.split_edges(node));

            let val = exit_blocks[&node];
            // Then branch them all to the tail.
            let mut walker = self
                .graph
                .neighbors_directed(node, Direction::Incoming)
                .detach();

            while let Some((edge, src)) = walker.next(&self.graph) {
                let please_uncomment = 1;
                // if !loop_info.scc.is_visited(&src) {
                //     continue;
                // }
                let weight = self.graph.remove_edge(edge).unwrap();
                debug_assert!(matches!(&weight.op, BranchOp::Jmp));
                let footer = &mut self.graph[node].footer;
                footer.push(Annotation::AssignCond {
                    dst: cond.clone(),
                    cond: val,
                });
                footer.push(Annotation::AssignCond {
                    dst: loop_info.rep_var.clone(),
                    cond: 0,
                });
                self.graph.add_edge(node, loop_info.tail_node, weight);
            }
        }
    }

    fn split_entry_rep(&mut self, loop_info: &LoopState, state: &mut RestructureState) {
        let targets = Vec::from_iter(
            loop_info
                .entry
                .ones()
                .map(NodeIndex::new)
                .flat_map(|n| self.graph.neighbors_directed(n, Direction::Outgoing)),
        );
        let (entry_blocks, cond) = self.make_demux_node(loop_info.entry_node, targets, state);

        // For each node that has a jmp into the scc:
        for node in loop_info
            .entry
            .ones()
            .chain(loop_info.rep.ones())
            .map(NodeIndex::new)
        {
            // Look at its neighbors:
            let mut walker = self
                .graph
                .neighbors_directed(node, Direction::Outgoing)
                .detach();

            while let Some((edge, dst)) = walker.next(&self.graph) {
                if !loop_info.scc.is_visited(&dst) {
                    continue;
                }
                // Given a branch into the SCC, add a footer assigning to the
                // destination block, then add reroute the edge to the entry block.
                let weight = self.graph.remove_edge(edge).unwrap();
                debug_assert!(matches!(&weight.op, BranchOp::Jmp));
                let val = entry_blocks[&dst];
                let footer = &mut self.graph[node].footer;
                footer.push(Annotation::AssignCond {
                    dst: cond.clone(),
                    cond: val,
                });
                if loop_info.rep.is_visited(&node) {
                    footer.push(Annotation::AssignCond {
                        dst: loop_info.rep_var.clone(),
                        cond: 1,
                    });
                    self.graph.add_edge(node, loop_info.tail_node, weight);
                } else {
                    self.graph.add_edge(node, loop_info.entry_node, weight);
                }
            }
        }
    }

    fn restructure_branches(&mut self, state: &mut RestructureState) {
        // Credit to optir for structuring the loop in this way; this is pretty different than the paper.
        let dom = dominators::simple_fast(&self.graph, self.entry);
        let dominates = |x: NodeIndex, y| {
            dom.dominators(y)
                .map(|mut ds| ds.any(|d| x == d))
                .unwrap_or(false)
        };

        // The "Perfect Reconstructability" paper uses "continuation point" to
        // refer to the targets of a branch _not_ part of a structured region.
        //
        // We want to group these continuations by their immediate dominators
        // (called the "Head" in the paper), then add a mux node in front of the
        // continuations if there is more than one.

        let mut tail_continuations = HashMap::<NodeIndex, Vec<NodeIndex>>::new();

        for ix in self.graph.node_indices() {
            if let Some(idom) = dom.immediate_dominator(ix) {
                // Continuations have more than one non-loop incoming edge
                if self
                    .graph
                    .neighbors_directed(ix, Direction::Incoming)
                    .filter(|pred| !dominates(ix, *pred))
                    .nth(1)
                    .is_some()
                {
                    tail_continuations.entry(idom).or_default().push(ix);
                }
            }
        }

        for (_, conts) in tail_continuations
            .into_iter()
            .filter(|(_, cs)| cs.len() > 1)
        {
            let mux = self.fresh_block();
            let (preds, cond_var) = self.make_demux_node(mux, conts.iter().copied(), state);
            for cont in conts {
                let mut walker = self
                    .graph
                    .neighbors_directed(cont, Direction::Incoming)
                    .detach();
                while let Some((_, src)) = walker.next(&self.graph) {
                    self.split_edges(src);
                }
                walker = self
                    .graph
                    .neighbors_directed(cont, Direction::Incoming)
                    .detach();

                // NB: there's some extra filtering that happens here in optir, do we need it?
                while let Some((edge, src)) = walker.next(&self.graph) {
                    let branch = self.graph.remove_edge(edge).unwrap();
                    self.graph.add_edge(src, mux, branch);
                    self.graph[src].footer.push(Annotation::AssignCond {
                        dst: cond_var.clone(),
                        cond: preds[&cont],
                    });
                }
            }
        }
    }
}

const JMP: Branch = Branch {
    op: BranchOp::Jmp,
    pos: None,
};

struct LoopState<'a> {
    entry: &'a NodeSet,
    rep: &'a NodeSet,
    exit: &'a NodeSet,
    scc: &'a NodeSet,
    rep_var: &'a Identifier,
    entry_node: NodeIndex,
    tail_node: NodeIndex,
    exit_node: NodeIndex,
}
