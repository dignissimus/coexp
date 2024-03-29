use coexp::parse_expression;
use coexp::parse_program;
use coexp::Expression::*;
use coexp::ParseError;
use coexp::ParseResult;
use coexp::Program;
use coexp::ProgramContext;
use coexp::Statement::*;
use coexp::Type::Arrow;
use coexp::WHITESPACE;
use colored::Colorize;
use immutable_chunkmap::map::MapM;
use rustyline::error::ReadlineError;
use rustyline::{DefaultEditor, Result};
use std::env;
use std::fs;
use std::io;
use std::io::prelude::*;

fn main() {
    match env::args().nth(1) {
        Some(file_name) => run_file(file_name),
        None => repl(),
    }
}

fn repl() {
    let mut rl = DefaultEditor::new().unwrap();
    let mut line = String::new();
    let mut context = ProgramContext::new();
    loop {
        match rl.readline("coexp> ") {
            Ok(data) => {
                let _ = rl.add_history_entry(data.as_str());
                line = data;
            }
            Err(ReadlineError::Interrupted) => break,
            Err(ReadlineError::Eof) => break,
            Err(err) => {
                println!("Error: {:?}", err);
                break;
            }
        }
        print!("coexp> ");
        match parse_program(&line).or_else(|_| {
            let (_, index) = WHITESPACE(&line, 0)?;
            let (expression, index) = parse_expression(&line, index)?;
            let (_, index) = WHITESPACE(&line, index)?;
            (if index == line.len() {
                Ok((
                    vec![
                        Assignment {
                            name: "_result".to_string(),
                            value: expression,
                        },
                        Debug {
                            expression: Name("_result".to_string()),
                        },
                    ],
                    index,
                ))
            } else {
                Err(ParseError {
                    message: "Unable to parse code".to_string(),
                })
            }) as ParseResult<Program>
        }) {
            Ok((program, _)) => context = context.run_program(program),
            Err(err) => {
                println!("Error while parsing input: {:?}", err)
            }
        }
        line.clear();
    }
}

fn run_file(file_name: String) {
    let source =
        fs::read_to_string(&file_name).expect(&format!("Unable to read file {:?}", file_name));
    ProgramContext::new().run_program(parse_program(&source).expect("Unable to parse source").0);
}
