
use std::str::Chars;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Token {
    Eof(u32),
    Newline(u32, u32, char, char),
    Indent(u32),
    Dedent(u32),

    DoubleDivAssign(u32, u32),
    DoubleDiv(u32, u32),
    DivAssign(u32, u32),
    Div(u32, u32),
    PowerAssign(u32, u32),
    Power(u32, u32),
    MulAssign(u32, u32),
    Mul(u32, u32),
    Less(u32, u32),
    LessEqual(u32, u32),
    Equal(u32, u32),
    GreaterEqual(u32, u32),
    Greater(u32, u32),
    NotEqual(u32, u32),
    ShiftLeftAssign(u32, u32),
    ShiftLeft(u32, u32),
    ShiftRightAssign(u32, u32),
    ShiftRight(u32, u32),
    PlusAssign(u32, u32),
    Plus(u32, u32),
    MinusAssign(u32, u32),
    Minus(u32, u32),
    Arrow(u32, u32),
    ModuloAssign(u32, u32),
    Modulo(u32, u32),
    DecoratorAssign(u32, u32),
    Decorator(u32, u32),
    BitAndAssign(u32, u32),
    BitAnd(u32, u32),
    BitOrAssign(u32, u32),
    BitOr(u32, u32),
    BitXorAssign(u32, u32),
    BitXor(u32, u32),
    BitInvert(u32, u32),
    ColonAssign(u32, u32),
    Colon(u32, u32),
    SemiColon(u32, u32),
    Comma(u32, u32),
    Dot(u32, u32),
    Elipsis(u32, u32),
    LeftParen(u32, u32),
    RightParen(u32, u32),
    LeftBracket(u32, u32),
    RightBracket(u32, u32),
    LeftCurly(u32, u32),
    RightCurly(u32, u32),
    Assign(u32, u32)
}



pub fn lexer(input: &String) -> Result<Vec<Token>, String> {
    let mut result = Vec::new();

    let mut it = input.chars().peekable();
    while let Some(&c) = it.peek() {
        if c.is_alphabetic() || c == '_' {
            it.next();
            
        }
        match c {
            'T' | 'e' | 's'| 't' => {
                println!(".");
                it.next(); 
            },
            _ => return Err("Syntax Error!".to_string())
        }
    }

    Ok(result)
}

fn is_reserved_keyword_or_name(text: &mut Chars) -> Result<Token, String> {

    Err("".to_string())
}

/// Analyzes source code for operators or delimiters.
pub fn is_operator_or_delimiter(c1: char, c2: char, c3: char, index: u32) -> Option<(Token, u8)> {
    match (c1, c2, c3) {
        ('/', '/', '=') => Some((Token::DoubleDivAssign(index, index + 3), 3)),
        ('/', '/', _ )  => Some((Token::DoubleDiv(index, index + 2), 2)),
        ('/', '=', _ )  => Some((Token::DivAssign(index, index + 2), 2)),
        ('/', _ , _ )   => Some((Token::Div(index, index + 1), 1)),
        ('*', '*', '=') => Some((Token::PowerAssign(index, index + 3), 3)),
        ('*', '*', _)   => Some((Token::Power(index, index + 2), 2)),
        ('*', '=', _)   => Some((Token::MulAssign(index, index + 2), 2)),
        ('*', _ , _)    => Some((Token::Mul(index, index + 1), 1)),
        ('<', '<', '=') => Some((Token::ShiftLeftAssign(index, index + 3), 3)),
        ('<', '<', _)   => Some((Token::ShiftLeft(index, index + 2), 2)),
        ('<', '=', _)   => Some((Token::LessEqual(index, index + 2), 2)),
        ('<', _ , _)    => Some((Token::Less(index, index + 1), 1)),
        ('>', '>', '=') => Some((Token::ShiftRightAssign(index, index + 3), 3)),
        ('>', '>', _)   => Some((Token::ShiftRight(index, index + 2), 2)),
        ('>', '=', _)   => Some((Token::GreaterEqual(index, index + 2), 2)),
        ('>', _ , _)    => Some((Token::ShiftRight(index, index + 1), 1)),
        ('+', '=', _)   => Some((Token::PlusAssign(index, index + 2), 2)),
        ('+', _ , _)    => Some((Token::Plus(index, index + 1), 1)),
        ('-', '=', _)   => Some((Token::MinusAssign(index, index + 2), 2)),
        ('-', '>', _)   => Some((Token::Arrow(index, index + 2), 2)),
        ('-', _ , _)    => Some((Token::Minus(index, index + 1), 1)),
        ('%','=' , _)   => Some((Token::ModuloAssign(index, index + 2), 2)),
        ('%', _ , _)    => Some((Token::Modulo(index, index + 1), 1)),
        ('@','=' , _)   => Some((Token::DecoratorAssign(index, index + 2), 2)),
        ('@', _ , _)    => Some((Token::Decorator(index, index + 1), 1)),
        ('&','=' , _)   => Some((Token::BitAndAssign(index, index + 2), 2)),
        ('&', _ , _)    => Some((Token::BitAnd(index, index + 1), 1)),
        ('|','=' , _)   => Some((Token::BitOrAssign(index, index + 2), 2)),
        ('|', _ , _)    => Some((Token::BitOr(index, index + 1), 1)),
        ('^','=' , _)   => Some((Token::BitXorAssign(index, index + 2), 2)),
        ('^', _ , _)    => Some((Token::BitXor(index, index + 1), 1)),
        ('!','=' , _)   => Some((Token::NotEqual(index, index + 2), 2)),
        ('=','=' , _)   => Some((Token::Equal(index, index + 2), 2)),
        ('=', _ , _)    => Some((Token::Assign(index, index + 1), 1)),
        (':','=' , _)   => Some((Token::ColonAssign(index, index + 2), 2)),
        (':', _ , _)    => Some((Token::Colon(index, index + 1), 1)),
        ('.','.', '.')  => Some((Token::Elipsis(index, index + 3), 3)),
        ('.', '.', _)   => None,
        ('.', _ , _)    => Some((Token::Dot(index, index + 1), 1)),
        ('~', _ , _)    => Some((Token::BitInvert(index, index + 1), 1)),
        ('(', _ , _)    => Some((Token::LeftParen(index, index + 1), 1)),
        (')', _ , _)    => Some((Token::RightParen(index, index + 1), 1)),
        ('[', _ , _)    => Some((Token::LeftBracket(index, index + 1), 1)),
        (']', _ , _)    => Some((Token::RightParen(index, index + 1), 1)),
        ('{', _ , _)    => Some((Token::LeftCurly(index, index + 1), 1)),
        ('}', _ , _)    => Some((Token::RightParen(index, index + 1), 1)),
        _ => None
    }


}

/// Unittests for Lexical Analyzer module
#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn operator_or_delimiter_double_div_assign() {
        let res = is_operator_or_delimiter('/', '/', '=', 4);
        match res {
            Some((x, y)) => {
                assert_eq!(x, Token::DoubleDivAssign(4, 7));
                assert_eq!(y, 3);
            },
            None => assert!(false)
        }
    }

    #[test]
    fn operator_or_delimiter_double_div() {
        let res = is_operator_or_delimiter('/', '/', ' ', 4);
        match res {
            Some((x, y)) => {
                assert_eq!(x, Token::DoubleDiv(4, 6));
                assert_eq!(y, 2);
            },
            None => assert!(false)
        }
    }

    #[test]
    fn operator_or_delimiter_div_assign() {
        let res = is_operator_or_delimiter('/', '=', ' ', 4);
        match res {
            Some((x, y)) => {
                assert_eq!(x, Token::DivAssign(4, 6));
                assert_eq!(y, 2);
            },
            None => assert!(false)
        }
    }

    #[test]
    fn operator_or_delimiter_div() {
        let res = is_operator_or_delimiter('/', ' ', ' ', 4);
        match res {
            Some((x, y)) => {
                assert_eq!(x, Token::Div(4, 5));
                assert_eq!(y, 1);
            },
            None => assert!(false)
        }
    }

    #[test]
    fn operator_or_delimiter_power_assign() {
        let res = is_operator_or_delimiter('*', '*', '=', 4);
        match res {
            Some((x, y)) => {
                assert_eq!(x, Token::PowerAssign(4, 7));
                assert_eq!(y, 3);
            },
            None => assert!(false)
        }
    }

    #[test]
    fn operator_or_delimiter_power() {
        let res = is_operator_or_delimiter('*', '*', ' ', 4);
        match res {
            Some((x, y)) => {
                assert_eq!(x, Token::Power(4, 6));
                assert_eq!(y, 2);
            },
            None => assert!(false)
        }
    }

    #[test]
    fn operator_or_delimiter_mul_assign() {
        let res = is_operator_or_delimiter('*', '=', ' ', 4);
        match res {
            Some((x, y)) => {
                assert_eq!(x, Token::MulAssign(4, 6));
                assert_eq!(y, 2);
            },
            None => assert!(false)
        }
    }

    #[test]
    fn operator_or_delimiter_mul() {
        let res = is_operator_or_delimiter('*', ' ', ' ', 4);
        match res {
            Some((x, y)) => {
                assert_eq!(x, Token::Mul(4, 5));
                assert_eq!(y, 1);
            },
            None => assert!(false)
        }
    }

}