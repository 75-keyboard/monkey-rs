use crate::token::Token;
use crate::object::Object;
use crate::ast;

pub enum Node {
    Program(ast::Program),
    Statement(ast::Statement),
    Expression(ast::Expression),
}

pub fn eval(node: Node) -> Option<Object> {
    match node {
        Node::Program(x) => eval_statements(x),
        Node::Statement(x) => match x {
            ast::Statement::ExpressionStatement{ expr } => eval(Node::Expression(expr)),
            _ => None
        },
        Node::Expression(x) => match x {
            ast::Expression::IntegerLiteral(Token::Int(x)) => Some(Object::Integer(x)),
            ast::Expression::Boolean(Token::True) => Some(Object::Boolean(true)),
            ast::Expression::Boolean(Token::False) => Some(Object::Boolean(false)),
            ast::Expression::InfixExpression{ left, opr, right } => {
                let left = eval(Node::Expression(*left));
                let right = eval(Node::Expression(*right));
                Some(eval_infix_expression(left, opr, right))
            }
            ast::Expression::PrefixExpression{ opr, right } => {
                let right = eval(Node::Expression(*right));
                eval_prefix_expression(opr, right)
            },
            _ => None
        }
    }
}

fn eval_statements(stmts: ast::Program) -> Option<Object> {
    let mut result = None;
    for stmt in &*stmts {
        result = eval(Node::Statement(stmt.clone()));
    }
    result
}

fn eval_infix_expression(left: Option<Object>, opr: Token, right: Option<Object>) -> Object {
    match left {
        Some(Object::Integer(x)) => match right {
            Some(Object::Integer(y)) => return eval_integer_infix_expression(x, opr, y),
            _ => {}
        }, _ => {}
    }

    match opr {
        Token::Equal => native_bool_to_boolean_object(left == right),
        Token::NotEqual => native_bool_to_boolean_object(left != right),
        _ => Object::Null
    }
}

fn eval_integer_infix_expression(left: i64, opr: Token, right: i64) -> Object {
    match opr {
        Token::Plus => Object::Integer(left + right),
        Token::Minus => Object::Integer(left - right),
        Token::Asterisk => Object::Integer(left * right),
        Token::Slash => Object::Integer(left / right),
        Token::Lt => native_bool_to_boolean_object(left < right),
        Token::Gt => native_bool_to_boolean_object(left > right),
        Token::Equal => native_bool_to_boolean_object(left == right),
        Token::NotEqual => native_bool_to_boolean_object(left != right),
        _ => Object::Null
    }
}

fn native_bool_to_boolean_object(b: bool) -> Object {
    match b {
        true => Object::Boolean(true),
        false => Object::Boolean(false),
    }
}

fn eval_prefix_expression(opr: Token, right: Option<Object>) -> Option<Object> {
    match opr {
        Token::Minus => Some(eval_minus_prefix_iperator_expression(right.unwrap())),
        Token::Bang => Some(eval_bang_operator_expression(right.unwrap())),
        _ => Some(Object::Null)
    }
}

fn eval_minus_prefix_iperator_expression(right: Object) -> Object {
    match right {
        Object::Integer(x) => Object::Integer(-x),
        _ => Object::Null
    }
}

fn eval_bang_operator_expression(right: Object) -> Object {
    match right {
        Object::Boolean(true) => Object::Boolean(false),
        Object::Boolean(false) => Object::Boolean(true),
        Object::Null => Object::Boolean(true),
        _ => Object::Boolean(false)
    }
}

#[cfg(test)]
mod tests {
    use crate::lexer::Lexer;
    use crate::parser::Parser;
    use crate::object::Object;
    use crate::evaluator;
    
    #[test]
    fn test_eval_integer_expression() {
        let tests = vec![
            ("5", 5),
            ("10", 10),
            ("-5", -5),
            ("-10", -10),
            ("5 + 5 + 5 + 5 - 10", 10),
            ("2 * 2 * 2 * 2 * 2", 32),
            ("-50 + 100 + -50", 0),
            ("5 * 2 + 10", 20),
            ("5 + 2 * 10", 25),
            ("20 + 2 * -10", 0),
            ("50 / 2 * 2 + 10", 60),
            ("2 * (5 + 10)", 30),
            ("3 * 3 * 3 + 10", 37),
            ("(5 + 10 * 2 + 15 / 3) * 2 + -10", 50),
        ];

        for tt in tests {
            if let Some(evaluated) = test_eval(tt.0) {
                test_integer_object(evaluated, tt.1);
            } else { assert!(false) }
        }
    }

    fn test_eval(input: &str) -> Option<Object> {
        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();

        return evaluator::eval(evaluator::Node::Program(program));
    }

    fn test_integer_object(obj: Object, expected: i64) {
        if let Object::Integer(x) = obj {
            assert_eq!(x, expected);
        } else { assert!(false); }
    }

    #[test]
    fn test_eval_boolean_expression() {
        let tests = vec![
            ("true", true),
            ("false", false),
            ("1 < 2", true),
            ("1 > 2", false),
            ("1 < 1", false),
            ("1 > 1", false),
            ("1 == 1", true),
            ("1 != 1", false),
            ("1 == 2", false),
            ("1 != 2", true),
            ("true == true", true),
            ("false == false", true),
            ("true == false", false),
            ("true != false", true),
            ("false != true", true),
            ("(1 < 2) == true", true),
            ("(1 < 2) == false", false),
            ("(1 > 2) == true", false),
            ("(1 > 2) == false", true),
        ];

        for tt in tests {
            if let Some(evaluated) = test_eval(tt.0) {
                test_boolean_object(evaluated, tt.1);
            } else { assert!(false) }
        }
    }

    fn test_boolean_object(obj: Object, expected: bool) {
        if let Object::Boolean(x) = obj {
            assert_eq!(x, expected);
        } else { assert!(false); }
    }

    #[test]
    fn test_bang_operator() {
        let tests = vec![
            ("!false", true),
            ("!5", false),
            ("!!true", true),
            ("!!false", false),
            ("!!5", true),
        ];

        for tt in tests {
            if let Some(evaluated) = test_eval(tt.0) {
                test_boolean_object(evaluated, tt.1);
            } else { assert!(false) }
        }
    }
}
