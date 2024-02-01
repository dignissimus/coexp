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
    Cofunction {
        parameter: String,
        body: Box<Expression>,
        from: Type,
        to: Type,
    },
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

impl Expression {
    fn is_constant(&self, parameter: &String) -> bool {
        // Check for correctness and completeness (TODO)
        // Check all code for name shadowing (TODO)
        match self {
            Name(ref name) => *name != *parameter,
            Function { ref body, .. } => body.is_constant(parameter),
            Cofunction { ref body, .. } => body.is_constant(parameter),
            Application {
                ref name, argument, ..
            } => argument.is_constant(parameter) && *name != *parameter,
            Coapplication {
                ref name,
                ref argument,
                ..
            } => argument.is_constant(parameter) && *name != *parameter,
            Inl(expression) => expression.is_constant(parameter),
            Inr(expression) => expression.is_constant(parameter),
            Case { value, inl, inr } => {
                value.is_constant(parameter)
                    && inl.expression.is_constant(parameter)
                    && inr.expression.is_constant(parameter)
            }
            _ => true,
        }
    }

    fn find_coapplication(self, parameter: &String) -> Option<Expression> {
        // Clarify semantics when the body contains coapplication as well as
        //   other constructs (TODO)
        match self {
            Name(_) => None,
            Function { body, .. } => body.find_coapplication(parameter),
            Cofunction { body, .. } => body.find_coapplication(parameter),
            Application { argument, .. } => argument.find_coapplication(parameter),
            Coapplication { ref name, .. } => {
                if *name == *parameter {
                    Some(self)
                } else {
                    None
                }
            }
            Inl(expression) => expression.find_coapplication(parameter),
            Inr(expression) => expression.find_coapplication(parameter),
            Case { value, inl, inr } => value
                .find_coapplication(parameter)
                .or(inl.expression.find_coapplication(parameter))
                .or(inr.expression.find_coapplication(parameter)),
            _ => None,
        }
    }
}

impl ProgramContext {
    fn new() -> ProgramContext {
        ProgramContext {
            variables: MapM::new(),
        }
    }

    fn run_program(self, program: Program) -> ProgramContext {
        program.iter().fold(self, |context, statement| {
            context.evaluate_statement(statement)
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
    // * Line numbers for error messages
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
                    };
                    context.evaluate(body)
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
                        },
                        *inl.expression.clone(),
                    ),
                    Inr(expression) => (
                        ProgramContext {
                            variables: self
                                .variables
                                .insert(inr.name.clone(), *expression.clone())
                                .0,
                        },
                        *inr.expression.clone(),
                    ),
                    Cofunction {
                        // Perhaps consider using the same identifier for consistent messages (TODO)
                        parameter,
                        body,
                        from,
                        to,
                    } => {
                        if body.is_constant(&parameter) {
                            return self.evaluate(&Case {
                                value: Box::new(Inr(body)),
                                inl: inl.clone(),
                                inr: inr.clone(),
                            });
                        }

                        if let Some(Coapplication { argument, .. }) =
                            body.find_coapplication(&parameter)
                        {
                            return self.evaluate(&Case {
                                value: Box::new(Inl(argument)),
                                inl: inl.clone(),
                                inr: inr.clone(),
                            });
                        }

                        // Speculatively match with the right branch
                        let context = self.clone();
                        let expression = Case {
                            value: Box::new(Cofunction {
                                parameter: inr.name.clone(),
                                body: inr.expression.clone(),
                                from,
                                to,
                            }),
                            inl: inl.clone(),

                            // We want an arbitrary symbol to construct the identity function
                            // I want to ensure that there aren't any uses from the choice of identifier (TODO)
                            inr: CaseBind {
                                name: "%n".to_string(),
                                expression: Box::new(Name("%n".to_string())),
                            },
                        };
                        (context, expression)
                    }
                    _ => todo!(),
                };
                context.evaluate(&expression)
            }
            Inl(expression) => Value::Inl(Box::new(self.evaluate(expression))),
            Inr(expression) => Value::Inr(Box::new(self.evaluate(expression))),
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

#[derive(Debug)]
struct ParseError {}
type ParseResult<T> = Result<(T, usize), ParseError>;
type Parser<T> = fn(&String, usize) -> ParseResult<T>;

macro_rules! exact {
    ($character:expr) => {
        &|source: &String, index: usize| {
            if let Some($character) = source.chars().nth(index) {
                Ok(($character, index + 1))
            } else {
                Err(ParseError {})
            }
        }
    };
}

macro_rules! any {
    () => {
        |_source, _index| Err(ParseError {})
    };
    ($left:expr $(, $right:expr)*) => {
        |source: &String, index: usize| {
            let result = $left(source, index);
            match result {
                Ok(_) => result,
                Err(_) => any!($($right),*)(source, index),
            }
        }
    };
}

macro_rules! chars {
    () => {};
    ($e:expr) => {
        exact!($e)
    };
    ($left:expr $(, $right:expr)+) => {
        any!(exact!($left), $(exact!($right)),+)
    };
}

macro_rules! map {
    ($p:expr $(, $f:expr)*) => { |source, index| $p(source, index).map(|(x, index)| (apply!(x $(, $f)*), index)) };
}

macro_rules! apply {
    ($x:expr) => { $x };
    ($x:expr, $f:expr $(, $g:expr)*) => { apply!($f($x) $(, $g)*) };
}

macro_rules! maybe {
    ($p:expr) => {
        |source, index| {
            $p(source, index)
                .map(|(item, index)| (Some(item), index))
                .unwrap_or((None, index))
        }
    };
}

macro_rules! many {
    ($p:expr) => {
        |source, index| {
            let mut result = Vec::new();
            let mut last = $p(source, index);
            let mut last_index = index;
            while let Ok((item, index)) = last {
                last_index = index;
                result.push(item);
                last = $p(source, index);
            }
            Ok((result, last_index))
        }
    };
}

macro_rules! many1 {
    ($p:expr) => {
        |source, index| {
            let result = many!($p)(source, index)?;
            if result.0.len() == 0 {
                Err(ParseError {})
            } else {
                Ok(result)
            }
        }
    };
}

macro_rules! sequence {
    ($l:expr, $r:expr) => {
        |source, index| {
            let (a, index) = $l(source, index)?;
            let (b, index) = $r(source, index)?;
            Ok(((a, b), index)) as ParseResult<_>
        }
    };
    ($l:expr, $r:expr $(, $rest:expr)+) => {
        sequence!($l, sequence!($r $(, $rest)+))
    };
}

const alpha: Parser<char> = chars!(
    'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's',
    't', 'u', 'v', 'w', 'x', 'y', 'z'
);
const numeral: Parser<char> = chars!('0', '1', '2', '3', '4', '5', '6', '7', '8', '9');

const WHITESPACE: Parser<Vec<char>> = many!(chars!(' ', '\n'));
const parse_name: Parser<String> = map!(many1!(alpha), |result: Vec<char>| result.iter().collect());
// TODO: unwrap
const parse_integer: Parser<Expression> = map!(
    many1!(numeral),
    |result: Vec<char>| IntegerLiteral(result.iter().collect::<String>().parse().unwrap())
);

const parse_unit: Parser<Expression> = map!(sequence!(exact!('('), exact!(')')), |_| Unit);

fn parse_case(source: &String, index: usize) -> ParseResult<Expression> {
    let (_, index) = sequence!(
        exact!('c'),
        exact!('a'),
        exact!('s'),
        exact!('e'),
        WHITESPACE
    )(source, index)?;
    let (value, index) = map!(parse_expression, Box::new)(source, index)?;
    let (_, index) = sequence!(WHITESPACE, exact!('{'), WHITESPACE)(source, index)?;
    let (_, index) = sequence!(
        WHITESPACE,
        exact!('|'),
        WHITESPACE,
        exact!('i'),
        exact!('n'),
        exact!('l'),
        WHITESPACE
    )(source, index)?;
    let (name, index) = parse_name(source, index)?;
    let (_, index) = sequence!(WHITESPACE, exact!('='), exact!('>'), WHITESPACE)(source, index)?;
    let (expression, index) = map!(parse_expression, Box::new)(source, index)?;
    let inl = CaseBind { name, expression };
    let (_, index) = sequence!(
        WHITESPACE,
        exact!('|'),
        WHITESPACE,
        exact!('i'),
        exact!('n'),
        exact!('r'),
        WHITESPACE
    )(source, index)?;
    let (name, index) = parse_name(source, index)?;
    let (_, index) = sequence!(WHITESPACE, exact!('='), exact!('>'), WHITESPACE)(source, index)?;
    let (expression, index) = map!(parse_expression, Box::new)(source, index)?;
    let (_, index) = sequence!(WHITESPACE, exact!('}'))(source, index)?;
    let inr = CaseBind { name, expression };
    Ok((Case { value, inl, inr }, index))
}

fn parse_inl(source: &String, index: usize) -> ParseResult<Expression> {
    let (_, index) = sequence!(exact!('i'), exact!('n'), exact!('l'), WHITESPACE)(source, index)?;
    let (inl, index) = map!(parse_expression, Box::new, Inl)(source, index)?;
    Ok((inl, index))
}

fn parse_inr(source: &String, index: usize) -> ParseResult<Expression> {
    let (_, index) = sequence!(exact!('i'), exact!('n'), exact!('r'), WHITESPACE)(source, index)?;
    let (inr, index) = map!(parse_expression, Box::new, Inr)(source, index)?;
    Ok((inr, index))
}

fn parse_function(source: &String, index: usize) -> ParseResult<Expression> {
    let (_, index) = sequence!(exact!('f'), exact!('n'), WHITESPACE)(source, index)?;
    let (parameter, index) = parse_name(source, index)?;
    let (_, index) = sequence!(WHITESPACE, exact!('='), exact!('>'), WHITESPACE)(source, index)?;
    let (body, index) = parse_expression(source, index)?;
    let body = Box::new(body);
    Ok((
        Function {
            parameter,
            body,
            from: Type::Free,
            to: Type::Free,
        },
        index,
    ))
}

fn parse_cofunction(source: &String, index: usize) -> ParseResult<Expression> {
    let (_, index) = sequence!(
        exact!('c'),
        exact!('o'),
        exact!('f'),
        exact!('n'),
        WHITESPACE
    )(source, index)?;
    let (parameter, index) = parse_name(source, index)?;
    let (_, index) = sequence!(WHITESPACE, exact!('='), exact!('>'), WHITESPACE)(source, index)?;
    let (body, index) = parse_expression(source, index)?;
    let body = Box::new(body);
    Ok((
        Cofunction {
            parameter,
            body,
            from: Type::Free,
            to: Type::Free,
        },
        index,
    ))
}

fn parse_application(source: &String, index: usize) -> ParseResult<Expression> {
    let (name, index) = parse_name(source, index)?;
    let (_, index) = WHITESPACE(source, index)?;
    let (value, index) = parse_expression(source, index)?;
    let argument = Box::new(value);
    Ok((Application { name, argument }, index))
}

fn parse_coapplication(source: &String, index: usize) -> ParseResult<Expression> {
    let (name, index) = parse_name(source, index)?;
    let (_, index) = WHITESPACE(source, index)?;
    let (_, index) = exact!('@')(source, index)?;
    let (_, index) = WHITESPACE(source, index)?;
    let (value, index) = parse_expression(source, index)?;
    let argument = Box::new(value);
    Ok((Coapplication { name, argument }, index))
}
const parse_expression: Parser<Expression> = any!(
    parse_inl,
    parse_inr,
    parse_case,
    parse_cofunction,
    parse_function,
    parse_integer,
    parse_unit,
    parse_coapplication,
    parse_application,
    map!(parse_name, Name)
);

fn parse_assignment(source: &String, index: usize) -> ParseResult<Statement> {
    let (name, index) = parse_name(source, index)?;
    let (_, index) = WHITESPACE(source, index)?;
    let (_, index) = exact!('=')(source, index)?;
    let (_, index) = WHITESPACE(source, index)?;
    let (value, index) = parse_expression(source, index)?;
    Ok((Assignment { name, value }, index))
}
fn parse_debug(_source: &String, _index: usize) -> ParseResult<Statement> {
    Err(ParseError {})
}

fn parse_program(source: &String) -> ParseResult<Program> {
    let mut start = 0;
    let mut program: Program = Vec::new();

    let _parsers: [Parser<Statement>; 2] = [parse_assignment, parse_debug];
    let parse_statement = any!(parse_assignment, parse_debug);

    while start < source.len() {
        let (_, index) = WHITESPACE(source, start)?;
        if let Ok((statement, index)) = parse_statement(source, index) {
            program.push(statement);
            start = index;
        } else {
            break;
        }
    }
    let (_, start) = WHITESPACE(source, start)?;
    if start == source.len() {
        Ok((program, start))
    } else {
        Err(ParseError {})
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
        Debug {
            expression: Case {
                value: Box::new(Cofunction {
                    parameter: "x".to_string(),
                    body: Box::new(Name("x".to_string())),
                    from: Type::Free,
                    to: Type::Free,
                }),

                inl: CaseBind {
                    name: "a".to_string(),
                    expression: Box::new(IntegerLiteral(0)),
                },
                inr: CaseBind {
                    name: "y".to_string(),
                    expression: Box::new(IntegerLiteral(1)),
                },
            },
        },
        Debug {
            expression: Case {
                value: Box::new(Cofunction {
                    parameter: "x".to_string(),
                    body: Box::new(Name("x".to_string())),
                    from: Type::Free,
                    to: Type::Free,
                }),

                inl: CaseBind {
                    name: "a".to_string(),
                    expression: Box::new(IntegerLiteral(0)),
                },
                inr: CaseBind {
                    name: "k".to_string(),
                    expression: Box::new(Coapplication {
                        name: "k".to_string(),
                        argument: Box::new(Inl(Box::new(IntegerLiteral(1)))),
                    }),
                },
            },
        },
        Debug {
            expression: Case {
                value: Box::new(Cofunction {
                    parameter: "x".to_string(),
                    body: Box::new(Name("x".to_string())),
                    from: Type::Free,
                    to: Type::Free,
                }),

                inl: CaseBind {
                    name: "a".to_string(),
                    expression: Box::new(IntegerLiteral(5)),
                },
                inr: CaseBind {
                    name: "k".to_string(),
                    expression: Box::new(Coapplication {
                        name: "k".to_string(),
                        argument: Box::new(Inl(Box::new(Name("k".to_string())))),
                    }),
                },
            },
        },
    ];

    ProgramContext::new().run_program(program);
    debug!("{:?}", parse_name(&"hello".to_string(), 0));
    debug!("{:?}", parse_program(&"hello = 1".to_string()));
    debug!("{:?}", parse_program(&"variable = other".to_string()));
    debug!("{:?}", parse_program(&"value = f 1".to_string()));
    debug!("{:?}", parse_program(&"value = f @ 1".to_string()));
    debug!("{:?}", parse_program(&"unit = ()".to_string()));
    debug!(
        "{:?}",
        parse_program(&"function = fn parameter => ()".to_string())
    );
    debug!(
        "{:?}",
        parse_program(&"cofunction = cofn parameter => ()".to_string())
    );
    debug!("{:?}", parse_program(&"left = inl 1".to_string()));
    debug!("{:?}", parse_program(&"right = inr 1".to_string()));
    debug!(
        "{:?}",
        parse_program(
            &"value = case inl 1 {
            | inl x => x
            | inr x => x
        }"
            .to_string()
        )
    );
    debug!(
        "{:?}",
        parse_program(&"cofunction = cofn parameter => ()".to_string())
    );
    debug!(
        "{:?}",
        parse_program(
            &"cofunction = case cofn x => x {
            | inl x => 0
            | inr y => 1
        }"
            .to_string()
        )
    );
}
