use coexp::parse_program;
use coexp::Expression::*;
use coexp::ProgramContext;
use coexp::Statement::*;
use coexp::Type::Arrow;
use colored::Colorize;
use immutable_chunkmap::map::MapM;
use std::env;
use std::fs;

fn main() {
    let file_name: String = env::args().nth(1).expect("No file name provided");
    let source =
        fs::read_to_string(&file_name).expect(&format!("Unable to read file {:?}", file_name));
    ProgramContext::new().run_program(parse_program(&source).expect("Unable to parse source").0);
}
