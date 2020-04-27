use crate::ast;
use crate::evaluator::Environment;
use std::rc::Rc;
use std::cell::RefCell;

#[derive(Clone, Debug, PartialEq)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    Return(Box<Object>),
    Null,
    Error(String),
    Function(Vec<ast::Expression>, ast::Program, Rc<RefCell<Environment>>),
}

impl std::fmt::Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Object::Integer(x) => write!(f, "{}", x),
            Object::Boolean(x) => write!(f, "{}", x),
            Object::Return(x) => write!(f, "{}", *x),
            Object::Null => write!(f, "null"),
            Object::Error(x) => write!(f, "{}", x),
            Object::Function(params, body, ..) => write!(f, "fn({}) {{ {} }}", 
                    params.iter().enumerate().fold(
                        String::new(), |s, (i, p)| {
                            println!("{:?} {:?}", i, p);
                            if i != params.len()-1 {
                                format!("{}{}, ", s, p)
                            } else {
                                format!("{}{}", s, p)
                            }}
                        )
                    , body)
        }
    }
}

impl Object {
    pub fn get_type(&self) -> String {
        match self {
            Object::Integer(_) => "INTEGER",
            Object::Boolean(_) => "BOOLEAN",
            Object::Return(_) => "RETURN",
            Object::Null => "NULL",
            Object::Error(_) => "ERROR",
            Object::Function(..) => "FUNCTION",
        }.to_string()
    }
}
