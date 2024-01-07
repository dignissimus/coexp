use crate::Expression::*;
use crate::Statement::*;
use crate::Type::Arrow;
use colored::Colorize;
use immutable_chunkmap::map::MapM;

#[derive(Debug, Clone, PartialEq)]
enum Type {
    String,
    Integer,
    Sum(Box<Type>, Box<Type>),
    SpeculativeSum(Box<Type>, Box<Type>),
    Unit,
    Arrow(Box<Type>, Box<Type>),
    Co(Box<Type>),
    Free,
}

#[derive(Debug, Clone)]
enum Expression {
    Function {
        parameter: String,
        body: Box<Expression>,
        from: Type,
        to: Type,
    },
    Cofunction,
    IntegerLiteral(i64),
    Application {
        name: String,
        argument: Box<Expression>,
    },
    Coapplication {
        name: String,
        argument: Box<Expression>,
    },
    Name(String),
    Inl(Box<Expression>),
    Inr(Box<Expression>),
    Unit,
    Case {
        value: Box<Expression>,
        inl: CaseBind,
        inr: CaseBind,
    },
}

#[derive(Debug, Clone)]
struct CaseBind {
    name: String,
    expression: Box<Expression>,
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
    Function(Expression),
    SpeculativeSum,
    Inl(Box<Value>),
    Inr(Box<Value>),
}

type Program = Vec<Statement>;

#[derive(Clone)]
struct ProgramContext {
    variables: MapM<String, Expression>,
}

fn debug(message: String) {
    println!("{} {}", "[debug]:".cyan(), message);
}

macro_rules! debug {
    ($($y:expr),+) => (debug(format!($($y),+)))
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
                debug!(
                    "{} {} {} {}",
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
                debug!("{}", format!("{:?}", expression).blue());
                ProgramContext { ..self.clone() }
            }
            Debug { expression } => {
                debug!("{}", format!("{:?}", self.evaluate(expression)).blue());
                ProgramContext { ..self.clone() }
            }
            _ => todo!(),
        }
    }

    fn resolve(&self, expression: &Expression) -> Expression {
        match expression {
            Name(name) => self.variables[name].clone(),
            _ => expression.clone(),
        }
    }

    // TODO: Nice error messages for variable lookup
    // TODO: Nice error messages for application to a non-function type
    // TODO: Type check before function application
    // TODO: Return Result, don't panic
    fn evaluate(&self, expression: &Expression) -> Value {
        match expression {
            IntegerLiteral(int) => Value::Integer(*int),
            Name(name) => self.evaluate(&self.variables[name]),
            Application { name, argument } => match &self.variables[name] {
                Function {
                    parameter,
                    body,
                    from,
                    to,
                } => {
                    if !self.typecheck(argument, from) {
                        panic!(
                            "Attempted to apply {:?} to a function of type {:?} -> {:?}",
                            self.resolve(argument),
                            from,
                            to
                        )
                    }
                    let context = ProgramContext {
                        variables: self
                            .variables
                            .insert(parameter.clone(), self.resolve(argument))
                            .0,
                        ..self.clone()
                    };
                    context.evaluate(&body)
                }
                _ => panic!("Attempted to apply to non-function type {:?}", expression),
            },
            Function { .. } => Value::Function(expression.clone()),
            Case { value, inl, inr } => {
                let (context, expression): (ProgramContext, Expression) = match self.resolve(value)
                {
                    Inl(expression) => (
                        ProgramContext {
                            variables: self
                                .variables
                                .insert(inl.name.clone(), *expression.clone())
                                .0,
                            ..self.clone()
                        },
                        *inl.expression.clone(),
                    ),
                    Inr(expression) => (
                        ProgramContext {
                            variables: self
                                .variables
                                .insert(inr.name.clone(), *expression.clone())
                                .0,
                            ..self.clone()
                        },
                        *inr.expression.clone(),
                    ),
                    _ => todo!(),
                };
                context.evaluate(&expression)
            }
            Inl(expression) => Value::Inl(Box::new(self.evaluate(expression))),
            Inr(expression) => Value::Inr(Box::new(self.evaluate(expression)))
            _ => todo!("{:?}", expression),
        }
    }

    fn typecheck(&self, expression: &Expression, signature: &Type) -> bool {
        let expression = &self.resolve(expression);
        match (expression, signature) {
            (_, Type::Free) => true,
            (IntegerLiteral(_), Type::Integer) => true,
            (Function { from, to, .. }, Arrow(left, right)) => *from == **left && *to == **right,
            _ => todo!(
                "Failed to type check {:?} against {:?}",
                expression,
                signature
            ),
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
                from: Type::Integer,
                to: Type::Integer,
            },
        },
        Assignment {
            name: "identity'".to_string(),
            value: Function {
                parameter: "x".to_string(),
                body: Box::new(Name("x".to_string())),
                from: Arrow(Box::new(Type::Integer), Box::new(Type::Integer)),
                to: Arrow(Box::new(Type::Integer), Box::new(Type::Integer)),
            },
        },
        DebugAst {
            expression: IntegerLiteral(5),
        },
        Assignment {
            name: "value".to_string(),
            value: Application {
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
                body: Box::new(Application {
                    name: "identity'".to_string(),
                    argument: Box::new(Name("x".to_string())),
                }),
                from: Arrow(Box::new(Type::Integer), Box::new(Type::Integer)),
                to: Arrow(Box::new(Type::Integer), Box::new(Type::Integer)),
            },
        },
        Assignment {
            name: "value''".to_string(),
            value: Application {
                name: "value'".to_string(),
                argument: Box::new(Name("identity".to_string())),
            },
        },
        Debug {
            expression: Name("value''".to_string()),
        },
        Debug {
            expression: Case {
                value: Box::new(Inl(Box::new(IntegerLiteral(10)))),
                inl: CaseBind {
                    name: "n".to_string(),
                    expression: Box::new(Name("n".to_string())),
                },
                inr: CaseBind {
                    name: "k".to_string(),
                    expression: Box::new(Name("k".to_string())),
                },
            },
        },
    ];

    ProgramContext::new().run_program(program);
}
