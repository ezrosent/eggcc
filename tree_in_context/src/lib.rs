use interpreter::Value;
use schema::TreeProgram;

use crate::interpreter::interpret_tree_prog;

pub mod ast;
pub mod interpreter;
mod optimizations;
pub mod schema;
pub mod schema_helpers;
mod to_egglog;
pub(crate) mod type_analysis;
pub mod typechecker;
pub(crate) mod utility;

pub type Result = std::result::Result<(), egglog::Error>;

pub fn prologue() -> String {
    [
        include_str!("schema.egg"),
        &optimizations::is_valid::rules().join("\n"),
        &optimizations::body_contains::rules().join("\n"),
        &optimizations::purity_analysis::rules().join("\n"),
        include_str!("utility/in_context.egg"),
        include_str!("utility/subst.egg"),
        include_str!("optimizations/constant_fold.egg"),
        include_str!("type_analysis.egg"),
        include_str!("utility/util.egg"),
    ]
    .join("\n")
}

/// Runs an egglog test.
/// `build` is egglog code that runs before the running rules.
/// `check` is egglog code that runs after the running rules.
/// It is highly reccomended to also provide the programs used in the egglog code
/// so that they can be interpreted on the given value.
pub fn egglog_test(
    build: &str,
    check: &str,
    progs: Vec<TreeProgram>,
    input: Value,
    expected: Value,
    expected_log: Vec<String>,
) -> Result {
    // first interpret the programs on the value
    for prog in progs {
        let (result_val, print_log) = interpret_tree_prog(&prog, input.clone());
        assert_eq!(
            result_val, expected,
            "Program {:?}\nproduced:\n{}\ninstead of expected:\n{}",
            prog, result_val, expected
        );
        assert_eq!(
            print_log, expected_log,
            "Program {:?}\nproduced log:\n{:?}\ninstead of expected log:\n{:?}",
            prog, print_log, expected_log
        );
    }

    let program = format!(
        "{}\n{build}\n{}\n{check}\n",
        prologue(),
        include_str!("schedule.egg"),
    );

    let res = egglog::EGraph::default()
        .parse_and_run_program(&program)
        .map(|lines| {
            for line in lines {
                println!("{}", line);
            }
        });

    if res.is_err() {
        println!("{}", program);
        println!("{:?}", res);
    }

    res
}
