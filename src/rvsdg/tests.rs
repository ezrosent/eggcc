use std::{fs::File, io::Write};

use crate::{cfg::to_cfg, rvsdg::from_cfg::to_rvsdg, util::parse_from_string};

#[test]
fn restructure_basic() {
    let prog = parse_from_string(include_str!("../../data/implicit-return.bril"));
    let mut cfg = to_cfg(&prog.functions[0]);
    let rvsdg = to_rvsdg(&mut cfg).unwrap();
    eprintln!("{rvsdg:#?}");
    File::create("/tmp/rvsdg-implicit-return.svg")
        .unwrap()
        .write_all(rvsdg.to_svg().as_bytes())
        .unwrap();
}
