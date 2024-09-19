pub const PREDEFINED_INPUTNUM_ID: usize = 0;
pub const PREDEFINED_OUTPUTNUM_ID: usize = 1;
pub const PREDEFINED_OUTPUTNEWLINE_ID: usize = 2;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum RelOp {
    Equal,
    NotEqual,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Token {
    // Operators
    Plus,
    Minus,
    Times,
    Divide,

    // Delimiters
    OpenParen,
    CloseParen,
    OpenBrace,
    CloseBrace,

    // Keywords
    Main,
    Var,
    Let,
    Call,
    If,
    Fi,
    Then,
    Else,
    While,
    Do,
    Od,
    Void,
    Function,
    Return,

    // Symbols
    Assignment,
    Comma,
    Semicolon,
    Period,

    // Multilength
    Number(isize),
    Ident(usize),
    RelOp(RelOp),
}
