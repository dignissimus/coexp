use coexp::parse_expression;
use coexp::parse_program;
use coexp::Expression::*;
use coexp::ParseError;
use coexp::ParseResult;
use coexp::Program;
use coexp::ProgramContext;
use coexp::Statement::*;
use coexp::Type::Arrow;
use colored::Colorize;
use immutable_chunkmap::map::MapM;
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
    let mut line = String::new();
    let mut context = ProgramContext::new();
    loop {
        print!("coexp> ");
        io::stdout().flush().expect("Unable to flush stdout");
        io::stdin()
            .read_line(&mut line)
            .expect("Unable to read from stdin");
        match parse_program(&line).or_else(|_| {
            let (expression, index) = parse_expression(&line, 0)?;
            (if index == line.len()  - 1 {
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
                Err(ParseError {message: "Unable to parse code".to_string()})
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
