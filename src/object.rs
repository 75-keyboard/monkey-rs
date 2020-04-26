#[derive(Clone, Debug, PartialEq)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    Return(Box<Object>),
    Null,
}

impl std::fmt::Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Object::Integer(x) => write!(f, "{}", x),
            Object::Boolean(x) => write!(f, "{}", x),
            Object::Return(x) => write!(f, "{}", *x),
            Object::Null => write!(f, "null"),
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
        }.to_string()
    }
}
