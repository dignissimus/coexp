use crate::Expression::*;
use crate::Statement::*;
use colored::Colorize;
use std::collections::HashMap;

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
enum Statement {
    Assignment { name: String, value: Expression },
    Data { name: String, cases: Vec<DataCase> },
    Debug { expression: Expression },
    DebugAst { expression: Expression },
}

#[derive(Debug, Clone)]
struct DataCase {
    name: String,
}

#[derive(Debug, Clone)]
enum Value {
    Integer(i64),
}

type Program = Vec<Statement>;

#[derive(Clone)]
struct ProgramContext {
    variables: HashMap<String, Expression>,
}

impl ProgramContext {
    fn new() -> ProgramContext {
        ProgramContext {
            variables: HashMap::new(),
        }
    }

    fn run_program(self, program: Program) -> ProgramContext {
        program
            .iter()
            .fold(self, |context, statement| self.evaluate_statement(&statement))
    }

    fn evaluate_statement(&self, statement: &Statement) -> ProgramContext {
        match statement {
            Assignment { name, value } => {
                println!(
                    "{} {} {} {} {}",
                    "[debug]:".cyan(),
                    "Assigning".cyan(),
                    name.blue(),
                    "=",
                    format!("{:?}", value).red()
                );
                ProgramContext {..self.clone()}
            }
            DebugAst { expression } => {
                println!(
                    "{} {}",
                    "[debug]:".cyan(),
                    format!("{:?}", expression).blue()
                );
                ProgramContext {..self.clone()}
            }
            Debug { expression } => {
                println!(
                    "{} {}",
                    "[debug]:".cyan(),
                    match expression {
                        Name(name) => {
                            let expression = self.variables[name];
                            let value = self.evaluate(&expression);
                            format!("{:?}", value).blue()
                        }
                        _ => todo!(),
                    }
                );
                ProgramContext {..self.clone()}
            }
            _ => todo!(),
        }
    }

    fn evaluate(&self, expression: &Expression) -> Value {
        match expression {
            IntegerLiteral(int) => Value::Integer(*int),
            _ => todo!()
        }
    }
}

fn main() {
    let program: Program = vec![
        Assignment {
            name: "identity".to_string(),
            value: Function {
                parameter: "x".to_string(),
                body: Box::new(Name("x".to_string())),
            },
        },
        DebugAst {
            expression: IntegerLiteral(5),
        },
        Assignment {
            name: "value".to_string(),
            value: Call {
                name: "identity".to_string(),
                argument: Box::new(IntegerLiteral(5)),
            },
        },
        Debug {
            expression: Name("value".to_string()),
        },
    ];

    let mut context = ProgramContext::new();
    context.run_program(program);
}
