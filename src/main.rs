use crate::Expression::*;
use crate::Statement::*;
use colored::Colorize;
use immutable_chunkmap::map::MapM;

#[derive(Debug, Clone)]
enum Type {
    String,
    Integer,
    Sum,
    Function(Box<Type>, Box<Type>)
}

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
    variables: MapM<String, Expression>,
}

impl ProgramContext {
    fn new() -> ProgramContext {
        ProgramContext {
            variables: MapM::new(),
        }
    }

    fn run_program(self, program: Program) -> ProgramContext {
        program.iter().fold(self, |context, statement| {
            context.evaluate_statement(&statement)
        })
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
                ProgramContext {
                    variables: self.variables.insert(name.clone(), value.clone()).0,
                    ..self.clone()
                }
            }
            DebugAst { expression } => {
                println!(
                    "{} {}",
                    "[debug]:".cyan(),
                    format!("{:?}", expression).blue()
                );
                ProgramContext { ..self.clone() }
            }
            Debug { expression } => {
                println!(
                    "{} {}",
                    "[debug]:".cyan(),
                    match expression {
                        Name(name) => {
                            let expression = self.variables[name].clone();
                            let value = self.evaluate(&expression);
                            format!("{:?}", value).blue()
                        }
                        _ => todo!(),
                    }
                );
                ProgramContext { ..self.clone() }
            }
            _ => todo!(),
        }
    }
    
    fn resolve(&self, expression: Expression) -> Expression{
        match expression {
            Name(name) => self.variables[&name].clone(),
            _ => expression
        }
    }

    // TODO: Nice error messages for variable lookup
    // TODO: Nice error messages for application to a non-function type
    // TODO: Type check before function application
    fn evaluate(&self, expression: &Expression) -> Value {
        match expression {
            IntegerLiteral(int) => Value::Integer(*int),
            Name(name) => self.evaluate(&self.variables[name]),
            Call { name, argument } => match &self.variables[name] {
                Function { parameter, body } => ProgramContext {
                    variables: self
                        .variables
                        .insert(parameter.clone(), self.resolve(*argument.clone()))
                        .0,
                    ..self.clone()
                }
                .evaluate(&body),
                _ => panic!("Attempted to apply to non-function type {:?}", expression),
            },
            _ => todo!("{:?}", expression),
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
        Assignment {
            name: "value'".to_string(),
            value: Function {
                parameter: "x".to_string(),
                body: Box::new(Call {
                    name: "x".to_string(),
                    argument: Box::new(Name("x".to_string()))
                })
            }
        },
        Assignment {
                name: "value''".to_string(),
                value:Call {
                    name: "value'".to_string(),
                    argument: Box::new(Name("value'".to_string())),
                }
        },
        Debug {
            expression: Name("value''".to_string())
        }
    ];

    ProgramContext::new().run_program(program);
}
