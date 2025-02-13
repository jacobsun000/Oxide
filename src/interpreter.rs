use std::cmp::Ordering;
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
    None,
    Object(Box<Object>),
}

// Implement as_int, ... methods for Value
macro_rules! impl_as_methods {
    ($($method_name:ident: $variant:ident => $ty:ty),+ $(,)?) => {
        impl Value {
            $(
                pub fn $method_name(&self) -> Option<$ty> {
                    if let Value::$variant(v) = self {
                        Some(v.clone())
                    } else {
                        None
                    }
                }
            )+
        }
    };
}

impl_as_methods! {
    as_i64: Integer => i64,
    as_f64: Float => f64,
    as_bool: Bool => bool,
    as_string: String => String,
    as_vector: Vector => Vec<Object>,
    as_dict: Dict => HashMap<String, Object>,
}

impl Value {
    pub fn as_native_func(&self) -> (&Vec<String>, fn(Vec<Object>) -> Object) {
        if let Value::NativeFunc { params, body } = self {
            (params, *body)
        } else {
            panic!("Value is not a native function");
        }
    }

    pub fn as_func(&self) -> (&Box<Expr>, &Box<Expr>) {
        if let Value::Func { params, body } = self {
            (params, body)
        } else {
            panic!("Value is not a function");
        }
    }
}

#[derive(Debug, Clone)]
pub struct Object {
    pub typeid: usize,
    pub value: Value,
    pub fields: HashMap<String, Object>,
}

macro_rules! define_native_infix_ops {
    ($type_name:expr, $(($name:expr, $lhs_ty:ident, $rhs_ty:ident, $operation:expr, $constructor:ident)),+ $(,)?) => {
        [
            $(
                (
                    $name.to_string(),
                    Object::new_native_function(vec!["lhs".to_string(), "rhs".to_string()], |args| {
                        expect_args_length(&args, 2, concat!($type_name, ".", $name));
                        let lhs = args[0]
                            .value
                            .$lhs_ty()
                            .expect(concat!($type_name, ".", $name, "(): Invalid argument type"));
                        let rhs = args[1]
                            .value
                            .$rhs_ty()
                            .expect(concat!($type_name, ".", $name, "(): Invalid argument type"));
                        let result = $operation(lhs, rhs);
                        Object::$constructor(result)
                    }),
                )
            ),+
        ]
    };
}

macro_rules! define_native_unary_ops {
    ($type_name:expr, $(($name:expr, $input_ty:ident, $operation:expr, $constructor:ident)),+ $(,)?) => {
        [
            $(
                (
                    $name.to_string(),
                    Object::new_native_function(vec!["value".to_string()], |args| {
                        expect_args_length(&args, 1, concat!($type_name, ".", $name));
                        let val = args[0]
                            .value
                            .$input_ty()
                            .expect(concat!($type_name, ".", $name, "(): Invalid argument type"));
                        let result = $operation(val);
                        Object::$constructor(result)
                    }),
                )
            ),+
        ]
    };
}

impl Object {
    const ID_NONE: usize = 0;
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
            value: Value::None,
            fields: HashMap::new(),
        }
    }

    pub fn new_null() -> Self {
        Self::new(Self::ID_NONE)
    }

    pub fn new_int(value: i64) -> Self {
        let infix_ops = define_native_infix_ops!(
            "Int",
            ("__add__", as_i64, as_i64, |l, r| l + r, new_int),
            ("__sub__", as_i64, as_i64, |l, r| l - r, new_int),
            ("__mul__", as_i64, as_i64, |l, r| l * r, new_int),
            ("__div__", as_i64, as_i64, |l, r| l / r, new_int),
            ("__mod__", as_i64, as_i64, |l, r| l % r, new_int),
            ("__mod__", as_i64, as_i64, |l, r| l % r, new_int),
            ("__bitand__", as_i64, as_i64, |l, r| l & r, new_int),
            ("__bitor__", as_i64, as_i64, |l, r| l | r, new_int),
            ("__bitxor__", as_i64, as_i64, |l, r| l ^ r, new_int),
            ("__lshift__", as_i64, as_i64, |l, r| l << r, new_int),
            ("__rshift__", as_i64, as_i64, |l, r| l >> r, new_int),
            (
                "__cmp__",
                as_i64,
                as_i64,
                |l: i64, r| cmp_to_i64(l.cmp(&r)),
                new_int
            ),
            (
                "__pow__",
                as_i64,
                as_i64,
                |l: i64, r| l.pow(r as u32),
                new_int
            ),
        );
        let unary_ops = define_native_unary_ops!(
            "Int",
            ("__not__", as_i64, |v: i64| !v, new_int),
            ("__neg__", as_i64, |v: i64| -v, new_int),
            ("__pos__", as_i64, |v: i64| v, new_int),
            ("__str__", as_i64, |v: i64| v.to_string(), new_string),
            ("__bitnot__", as_i64, |v: i64| !v, new_int),
        );

        let mut fields = HashMap::new();
        fields.extend(infix_ops.iter().cloned());
        fields.extend(unary_ops.iter().cloned());

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
            write!(f, "{}", result.value.as_string().unwrap_or("".to_string()))
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

fn expect_args_length(args: &[Object], expected: usize, func_name: &str) {
    if args.len() != expected {
        panic!(
            "{}(): Expected {} arguments, got {}",
            func_name,
            expected,
            args.len()
        );
    }
}

fn cmp_to_i64(v: Ordering) -> i64 {
    match v {
        Ordering::Less => -1,
        Ordering::Equal => 0,
        Ordering::Greater => 1,
    }
}
