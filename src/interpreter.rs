use std::collections::HashMap;
use std::fmt;

use crate::lexer::Token;
use crate::parser::Expr;

#[derive(Debug, Clone)]
pub enum Value {
    Integer(i64),
    Float(f64),
    Bool(bool),
    String(String),
    Vector(Vec<Object>),
    Dict(HashMap<String, Object>),
    Func {
        params: Box<Expr>,
        body: Box<Expr>,
    },
    NativeFunc {
        params: Vec<String>,
        body: fn(Vec<Object>) -> Object,
    },
    Null,
    Object(Box<Object>),
}

impl Value {
    pub fn as_int(&self) -> i64 {
        match self {
            Self::Integer(n) => *n,
            _ => unreachable!(),
        }
    }

    pub fn as_float(&self) -> f64 {
        match self {
            Self::Float(n) => *n,
            _ => unreachable!(),
        }
    }

    pub fn as_bool(&self) -> bool {
        match self {
            Self::Bool(b) => *b,
            _ => unreachable!(),
        }
    }

    pub fn as_string(&self) -> String {
        match self {
            Self::String(s) => s.clone(),
            _ => unreachable!(),
        }
    }

    pub fn as_vector(&self) -> Vec<Object> {
        match self {
            Self::Vector(v) => v.clone(),
            _ => unreachable!(),
        }
    }

    pub fn as_dict(&self) -> HashMap<String, Object> {
        match self {
            Self::Dict(d) => d.clone(),
            _ => unreachable!(),
        }
    }

    pub fn as_func(&self) -> (Box<Expr>, Box<Expr>) {
        match self {
            Self::Func { params, body } => (params.clone(), body.clone()),
            _ => unreachable!(),
        }
    }

    pub fn as_native_func(&self) -> (Vec<String>, fn(Vec<Object>) -> Object) {
        match self {
            Self::NativeFunc { params, body } => (params.clone(), *body),
            _ => unreachable!(),
        }
    }

    pub fn as_object(&self) -> Box<Object> {
        match self {
            Self::Object(obj) => obj.clone(),
            _ => unreachable!(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Object {
    pub typeid: usize,
    pub value: Value,
    pub fields: HashMap<String, Object>,
}

impl Object {
    const ID_NULL: usize = 0;
    const ID_INT: usize = 1;
    const ID_FLOAT: usize = 2;
    const ID_BOOL: usize = 3;
    const ID_STRING: usize = 4;
    const ID_VECTOR: usize = 5;
    const ID_DICT: usize = 6;
    const ID_FUNCTION: usize = 7;
    const ID_NATIVE_FUNCTION: usize = 8;

    pub fn new(typeid: usize) -> Self {
        Self {
            typeid,
            value: Value::Null,
            fields: HashMap::new(),
        }
    }

    pub fn new_null() -> Self {
        Self::new(Self::ID_NULL)
    }

    pub fn new_int(value: i64) -> Self {
        let fields = HashMap::from([
            (
                "__add__".to_string(),
                Self::new_native_function(vec!["lhs".to_string(), "rhs".to_string()], |args| {
                    if args.len() != 2 {
                        panic!("Int.__add__(): Invalid number of arguments");
                    }
                    let lhs = match &args[0].value {
                        Value::Integer(n) => *n,
                        _ => panic!("Int.__add__(): Invalid argument type"),
                    };
                    let rhs = match &args[1].value {
                        Value::Integer(n) => *n,
                        _ => panic!("Int.__add__(): Invalid argument type"),
                    };
                    let result = lhs + rhs;
                    Object::new_int(result)
                }),
            ),
            (
                "__str__".to_string(),
                Self::new_native_function(vec!["this".to_string()], |args| {
                    if args.len() != 1 {
                        panic!("Int.__str__(): Invalid number of arguments");
                    }
                    let val = match &args[0].value {
                        Value::Integer(n) => *n,
                        _ => panic!("Int.__str__(): Invalid argument type"),
                    };
                    Object::new_string(val.to_string())
                }),
            ),
        ]);
        Self {
            typeid: Self::ID_INT,
            value: Value::Integer(value),
            fields,
        }
    }

    pub fn new_float(value: f64) -> Self {
        Self {
            typeid: Self::ID_FLOAT,
            value: Value::Float(value),
            fields: HashMap::new(),
        }
    }

    pub fn new_bool(value: bool) -> Self {
        Self {
            typeid: Self::ID_BOOL,
            value: Value::Bool(value),
            fields: HashMap::new(),
        }
    }

    pub fn new_string(value: String) -> Self {
        Self {
            typeid: Self::ID_STRING,
            value: Value::String(value),
            fields: HashMap::new(),
        }
    }

    pub fn new_vector(value: Vec<Object>) -> Self {
        Self {
            typeid: Self::ID_VECTOR,
            value: Value::Vector(value),
            fields: HashMap::new(),
        }
    }

    pub fn new_dict(value: HashMap<String, Object>) -> Self {
        Self {
            typeid: Self::ID_DICT,
            value: Value::Dict(value),
            fields: HashMap::new(),
        }
    }

    pub fn new_native_function(params: Vec<String>, body: fn(Vec<Object>) -> Object) -> Self {
        Self {
            typeid: Self::ID_NATIVE_FUNCTION,
            value: Value::NativeFunc { params, body },
            fields: HashMap::new(),
        }
    }
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(str_method) = self.fields.get("__str__") {
            let result = str_method.value.as_native_func().1(vec![self.clone()]);
            write!(f, "{}", result.value.as_string())
        } else {
            write!(f, "{:?}", self.value)
        }
    }
}

#[derive(Debug, Clone)]
pub struct Env {
    pub store: HashMap<String, Object>,
}

impl Env {
    pub fn new() -> Self {
        Self {
            store: HashMap::new(),
        }
    }

    pub fn get(&self, name: &str) -> Option<&Object> {
        self.store.get(name)
    }

    pub fn get_mut(&mut self, name: &str) -> Option<&mut Object> {
        self.store.get_mut(name)
    }

    pub fn set(&mut self, name: String, value: Object) -> &mut Object {
        self.store.insert(name.clone(), value);
        self.store.get_mut(&name).unwrap()
    }
}

#[derive(Debug, Clone)]
pub enum ControlFlow {
    Return(Object),
    Failure(String),
}

#[derive(Debug, Clone)]
pub struct Interpreter {
    pub env: Env,
}

impl Interpreter {
    pub fn new() -> Self {
        let env = Env::new();
        Self { env }
    }

    pub fn eval(&mut self, expr: &Expr) -> Result<&mut Object, ControlFlow> {
        match expr {
            Expr::Integer(n) => Ok(self.temp(Object::new_int(*n))),
            Expr::Float(n) => Ok(self.temp(Object::new_float(*n))),
            Expr::Bool(b) => Ok(self.temp(Object::new_bool(*b))),
            Expr::String(s) => Ok(self.temp(Object::new_string(s.clone()))),
            Expr::Identifier(id) => Ok(self.env.get_mut(id).unwrap()),
            Expr::Return(val) => Err(ControlFlow::Return(self.eval(val)?.clone())),
            // Expr::Block(exprs) => {
            //     let mut result;
            //     for expr in exprs {
            //         let temp = self.eval(expr);
            //         if let Ok(obj) = temp {
            //             result = obj;
            //         } else if let Err(ControlFlow::Return(val)) = temp {
            //             return Ok(val);
            //         } else {
            //             return temp;
            //         }
            //     }
            //     Ok(result)
            // }
            Expr::Infix { left, op, right } => self.eval_infix(left, op, right),
            _ => Err(ControlFlow::Failure("Not Implemented".to_string())),
        }
    }

    fn temp(&mut self, obj: Object) -> &mut Object {
        self.env.set("_tmp".to_string(), obj)
    }

    fn eval_func(&mut self, func: Value, args: Value) -> Result<&mut Object, ControlFlow> {
        let (params, body) = match func {
            Value::Func { params, body } => (params, body),
            _ => return Err(ControlFlow::Failure("Invalid function".to_string())),
        };
        let params = match *params {
            Expr::Block(names) => names,
            _ => {
                return Err(ControlFlow::Failure(
                    "Invalid function parameters".to_string(),
                ))
            }
        };
        let args = match args {
            Value::Vector(values) => values,
            _ => {
                return Err(ControlFlow::Failure(
                    "Invalid function parameters".to_string(),
                ))
            }
        };
        if params.len() != args.len() {
            return Err(ControlFlow::Failure(
                "Invalid number of parameters".to_string(),
            ));
        }

        let mut env = self.env.clone();
        for (param, arg) in params.iter().zip(args.iter()) {
            if let Expr::Identifier(param) = param {
                env.set(param.clone(), arg.clone());
            } else {
                return Err(ControlFlow::Failure("Invalid parameter".to_string()));
            }
        }
        let mut interpreter = Interpreter { env };
        Ok(self.temp(interpreter.eval(&body)?.clone()))
    }

    fn eval_native_func(&mut self, func: Value, args: Value) -> Result<&mut Object, ControlFlow> {
        let (params, body) = match func {
            Value::NativeFunc { params, body } => (params, body),
            _ => return Err(ControlFlow::Failure("Invalid function".to_string())),
        };

        let args = match args {
            Value::Vector(values) => values,
            _ => {
                return Err(ControlFlow::Failure(
                    "Invalid function parameters".to_string(),
                ))
            }
        };

        if params.len() != args.len() {
            return Err(ControlFlow::Failure(
                "Invalid number of parameters".to_string(),
            ));
        }

        Ok(self.temp(body(args)))
    }

    fn eval_infix(
        &mut self,
        lhs: &Expr,
        op: &Token,
        rhs: &Expr,
    ) -> Result<&mut Object, ControlFlow> {
        if let Token::Assign = op {
            let rhs = self.eval(rhs)?.clone();
            match lhs {
                Expr::Identifier(id) => {
                    return Ok(self.env.set(id.clone(), rhs));
                }
                _ => {
                    let mut lhs = self.eval(lhs)?;
                    lhs = self.temp(rhs);
                    return Err(ControlFlow::Failure("Invalid assignment".to_string()));
                }
            }
        }
        if let Some(opcode) = op.op_code() {
            let left = self.eval(lhs)?.clone();
            let func = left.fields.get(opcode).ok_or_else(|| {
                ControlFlow::Failure(format!(
                    "Object of type {} does not support operator {}",
                    left.typeid, opcode
                ))
            })?;
            let right = self.eval(rhs)?;
            let params = Value::Vector(vec![left.clone(), right.clone()]);
            match func.value {
                Value::Func { .. } => self.eval_func(func.clone().value, params),
                Value::NativeFunc { .. } => self.eval_native_func(func.clone().value, params),
                _ => Err(ControlFlow::Failure(format!("Invalid operator {:?}", op))),
            }
        } else {
            Err(ControlFlow::Failure(format!("Invalid operator {:?}", op)))
        }
    }
}
