use crate::cfg::{to_cfg, BlockName};
use crate::util::parse_from_string;
use petgraph::dot::Dot;

macro_rules! to_block {
    (ENTRY) => {
        BlockName::Entry
    };
    (EXIT) => {
        BlockName::Exit
    };
    ($name:expr) => {
        BlockName::Named($name.into())
    };
}

// Test that a CFG is wired up correctly.
macro_rules! cfg_test {
    ($name:ident, $prog:expr, [ $($src:tt =($($edge:tt)*)=> $dst:tt,)* ]) => {
        #[test]
        fn $name() {
            use $crate::cfg::BranchOp;
            let prog = parse_from_string($prog);
            let cfg = to_cfg(&prog.functions[0]);
            let mut mentioned = std::collections::HashSet::new();
            let mut block = std::collections::HashMap::new();
            $(
                mentioned.insert(to_block!($src));
                mentioned.insert(to_block!($dst));
            )*
            for i in cfg.graph.node_indices() {
                let node = cfg.graph.node_weight(i).unwrap();
                assert!(mentioned.contains(&node.name), "description does not mention block {:?}", node.name);
                block.insert(node.name.clone(), i);
            }
            $({
                let src_name = to_block!($src);
                let dst_name = to_block!($dst);
                let src = block[&src_name];
                let dst = block[&dst_name];
                let has_edge = cfg.graph.edges_connecting(src, dst).any(|edge| {
                    edge.weight().op == BranchOp::$($edge)*
                });
                assert!(has_edge, "missing edge from {src_name:?} to {dst_name:?}");
            })*
        }
    };
}

cfg_test!(
    fib,
    include_str!("../../data/fib.bril"),
    [
        ENTRY  = (Jmp) => "loop",
        "loop" = (Cond { arg: "cond".into(), val: true.into() }) => "body",
        "loop" = (Cond { arg: "cond".into(), val: false.into() }) => "done",
        "body" = (Jmp) => "loop",
        "done" = (Jmp) => EXIT,
    ]
);

cfg_test!(
    queen,
    include_str!("../../data/queens-func.bril"),
    [
        ENTRY = (Cond { arg: "ret_cond".into(), val: true.into() }) => "next.ret",
        ENTRY = (Cond { arg: "ret_cond".into(), val: false.into()}) => "for.cond",
        "for.cond" = (Cond { arg: "for_cond_0".into(), val: true.into() }) => "for.body",
        "for.cond" = (Cond { arg: "for_cond_0".into(), val: false.into() }) => "next.ret.1",
        "for.body" = (Cond { arg: "is_valid".into(), val: true.into() }) => "rec.func",
        "for.body" = (Cond { arg: "is_valid".into(), val: false.into() }) => "next.loop",
        "rec.func" = (Jmp) => "next.loop",
        "next.loop" = (Jmp) => "for.cond",
        "next.ret" = (Jmp) => EXIT,
        "next.ret.1" = (Jmp) => EXIT,
    ]
);

cfg_test!(
    implicit_return,
    include_str!("../../data/implicit-return.bril"),
    [
        ENTRY = (Jmp) => EXIT,
    ]
);

#[test]
fn restructure_basic() {
    let prog = parse_from_string(include_str!("../../data/unstructured-val.bril"));
    let mut cfg = to_cfg(&prog.functions[0]);
    cfg.restructure();
    // TODO: more tests.
    eprintln!("{:#?}", Dot::new(&cfg.graph));
}

// TODO: graphs to try:
// a -> b, a -> c, b -> c, c -> b
// AND
// a -> b, a-> c, b -> d, c -> e, c -> f, d -> e, e -> f, f -> e , e -> d
