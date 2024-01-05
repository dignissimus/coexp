use crate::Expression::*;
use crate::Statement::*;
use colored::Colorize;
use std::collections::HashMap;

#[derive(Debug)]
enum Expression {
    Function {
        parameter: String,
        body: Box<Expression>,
    },
    Cofunction,
    IntegerLiteral(i64),
    Call {
        name: String,
        argument: Box<Expression>,
    },
    Name(String),
}

#[derive(Debug)]
enum Statement {
    Assignment { name: String, value: Expression },
    Data { name: String, cases: Vec<DataCase> },
    Debug { expression: Expression },
}

#[derive(Debug)]
struct DataCase {
    name: String,
}

type Program = Vec<Statement>;

struct ProgramContext {
    variables: HashMap<String, Expression>,
}

impl ProgramContext {
    fn new() -> ProgramContext {
        ProgramContext {
            variables: HashMap::new(),
        }
    }

    fn run_program(&mut self, program: Program) {
        program
            .iter()
            .for_each(|statement| self.evaluate_statement(&statement));
    }

    fn evaluate_statement(&mut self, statement: &Statement) {
        match statement {
            Assignment { name, value } => {
                println!(
                    "{} {} {} {} {}",
                    "[debug]:".cyan(),
                    "Assigning".cyan(),
                    name.blue(),
                    "=",
                    format!("{:?}", value).red()
                )
            }
            Debug { expression } => {
                println!(
                    "{} {}",
                    "[debug]:".cyan(),
                    format!("{:?}", expression).blue()
                )
            }
            _ => todo!(),
        }
    }

    fn evaluate(&mut self, _expression: &Expression) {}
}

fn main() {
    let program: Program = vec![
        Assignment {
            name: "f".to_string(),
            value: Function {
                parameter: "x".to_string(),
                body: Box::new(Name("x".to_string())),
            },
        },
        Debug {
            expression: IntegerLiteral(5),
        },
    ];

    let mut context = ProgramContext::new();
    context.run_program(program);

    println!("Hello, world!");
}
