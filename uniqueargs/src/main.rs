// Rust test modules
// If you don't put your Rust file here it won't get compiled!
mod switch_rewrites;

pub type Result = std::result::Result<(), egglog::Error>;

pub fn main() -> () {}

pub fn run_test(build: &str, check: &str) -> Result {
    let program = format!(
        "{}\n{build}\n{}\n{check}\n",
        vec![
            include_str!("schema.egg"),
            // analyses
            include_str!("id_analysis.egg"),
            // repairs
            include_str!("util.egg"),
            include_str!("deep_copy.egg"),
            // optimizations
            include_str!("switch_rewrites.egg"),
        ]
        .join("\n"),
        include_str!("schedule.egg"),
    );

    egglog::EGraph::default()
        .parse_and_run_program(&program)
        .map(|_| ())
}
