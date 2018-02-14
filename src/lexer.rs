use std::ops::Deref;
use std::fmt::{
    self,
    Display,
    Formatter,
};

use failure::Error;
use regex::Regex;

// Declare the Regexes used to capture each token type.
//
// These regex parsers are lazily loaded. They are only initialized once when
// they are first needed, after which they are cached in global "static" memory
// for fast reuse.
lazy_static! {
    static ref COLON:     Regex = Regex::new(r"^:").unwrap();
    static ref SEMICOLON: Regex = Regex::new(r"^;").unwrap();
    static ref DOT:       Regex = Regex::new(r"^\.").unwrap();
    static ref COMMA:     Regex = Regex::new(r"^,").unwrap();
    static ref EQUALS:    Regex = Regex::new(r"^==").unwrap();
    static ref EQSIGN:    Regex = Regex::new(r"^=").unwrap();
    static ref BANG:      Regex = Regex::new(r"^!").unwrap();
    static ref LPAREN:    Regex = Regex::new(r"^\(").unwrap();
    static ref RPAREN:    Regex = Regex::new(r"^\)").unwrap();
    static ref LBRACKET:  Regex = Regex::new(r"^\[").unwrap();
    static ref RBRACKET:  Regex = Regex::new(r"^\]").unwrap();
    static ref LBRACE:    Regex = Regex::new(r"^\{").unwrap();
    static ref RBRACE:    Regex = Regex::new(r"^\}").unwrap();
    static ref AND:       Regex = Regex::new(r"^&&").unwrap();
    static ref OR:        Regex = Regex::new(r"^\|\|").unwrap();
    static ref LESSTHAN:  Regex = Regex::new(r"^<").unwrap();
    static ref PLUS:      Regex = Regex::new(r"^\+").unwrap();
    static ref MINUS:     Regex = Regex::new(r"^-").unwrap();
    static ref TIMES:     Regex = Regex::new(r"^\*").unwrap();
    static ref DIV:       Regex = Regex::new(r"^\\/").unwrap();
    static ref CLASS:     Regex = Regex::new(r"^class").unwrap();
    static ref PUBLIC:    Regex = Regex::new(r"^public").unwrap();
    static ref STATIC:    Regex = Regex::new(r"^static").unwrap();
    static ref VOID:      Regex = Regex::new(r"^void").unwrap();
    static ref STRING:    Regex = Regex::new(r"^String").unwrap();
    static ref EXTENDS:   Regex = Regex::new(r"^extends").unwrap();
    static ref INT:       Regex = Regex::new(r"^int").unwrap();
    static ref BOOLEAN:   Regex = Regex::new(r"^boolean").unwrap();
    static ref WHILE:     Regex = Regex::new(r"^while").unwrap();
    static ref IF:        Regex = Regex::new(r"^if").unwrap();
    static ref ELSE:      Regex = Regex::new(r"^else").unwrap();
    static ref MAIN:      Regex = Regex::new(r"^main").unwrap();
    static ref RETURN:    Regex = Regex::new(r"^return").unwrap();
    static ref LENGTH:    Regex = Regex::new(r"^length").unwrap();
    static ref TRUE:      Regex = Regex::new(r"^true").unwrap();
    static ref FALSE:     Regex = Regex::new(r"^false").unwrap();
    static ref THIS:      Regex = Regex::new(r"^this").unwrap();
    static ref NEW:       Regex = Regex::new(r"^new").unwrap();
    static ref PRINTLN:   Regex = Regex::new(r"^System\.out\.println").unwrap();
    static ref SIDEF:     Regex = Regex::new(r"^sidef").unwrap();
    static ref ID:        Regex = Regex::new(r"^[a-zA-Z][a-zA-Z\d_]*").unwrap();
    static ref INTLIT:    Regex = Regex::new(r"^\d+").unwrap();
    static ref STRINGLIT: Regex = Regex::new(r#"^"([a-zA-Z\d\s\.]*)""#).unwrap();

    static ref RULES: Vec<&'static Regex> = vec![
        &COLON, &SEMICOLON, &DOT, &COMMA, &EQUALS, &EQSIGN, &BANG, &LPAREN, &RPAREN,
        &LBRACKET, &RBRACKET, &LBRACE, &RBRACE, &AND, &OR, &LESSTHAN, &PLUS, &MINUS, &TIMES, &DIV,
        &CLASS, &PUBLIC, &STATIC, &VOID, &STRING, &EXTENDS, &INT, &BOOLEAN, &WHILE, &IF, &ELSE,
        &MAIN, &RETURN, &LENGTH, &TRUE, &FALSE, &THIS, &NEW, &PRINTLN, &SIDEF, &ID, &INTLIT,
        &STRINGLIT,
    ];
}

/// Defines the unique types of tokens which may be parsed from the input.
#[derive(Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub enum TokenType<'a> {
    COLON,
    SEMICOLON,
    DOT,
    COMMA,
    EQUALS,
    EQSIGN,
    BANG,
    LPAREN,
    RPAREN,
    LBRACKET,
    RBRACKET,
    LBRACE,
    RBRACE,
    AND,
    OR,
    LESSTHAN,
    PLUS,
    MINUS,
    TIMES,
    DIV,
    CLASS,
    PUBLIC,
    STATIC,
    VOID,
    STRING,
    EXTENDS,
    INT,
    BOOLEAN,
    WHILE,
    IF,
    ELSE,
    MAIN,
    RETURN,
    LENGTH,
    TRUE,
    FALSE,
    THIS,
    NEW,
    PRINTLN,
    SIDEF,
    ID(&'a str),
    INTLIT(&'a str),
    STRINGLIT(&'a str),
    UNRECOGNIZED(&'a str),
}

impl<'a> TokenType<'a> {
    /// Constructs an instance of a TokenType. This returns a tuple describing
    /// the length of the Token (how many characters it consumes), and the
    /// TokenType variant itself.
    fn new(index: usize, text: &'a str) -> (usize, Self) {
        debug!("TokenType::new('{}'), length {}", text, text.len());
        let t = match index {
            0 => TokenType::COLON,
            1 => TokenType::SEMICOLON,
            2 => TokenType::DOT,
            3 => TokenType::COMMA,
            4 => TokenType::EQUALS,
            5 => TokenType::EQSIGN,
            6 => TokenType::BANG,
            7 => TokenType::LPAREN,
            8 => TokenType::RPAREN,
            9 => TokenType::LBRACKET,
            10 => TokenType::RBRACKET,
            11 => TokenType::LBRACE,
            12 => TokenType::RBRACE,
            13 => TokenType::AND,
            14 => TokenType::OR,
            15 => TokenType::LESSTHAN,
            16 => TokenType::PLUS,
            17 => TokenType::MINUS,
            18 => TokenType::TIMES,
            19 => TokenType::DIV,
            20 => TokenType::CLASS,
            21 => TokenType::PUBLIC,
            22 => TokenType::STATIC,
            23 => TokenType::VOID,
            24 => TokenType::STRING,
            25 => TokenType::EXTENDS,
            26 => TokenType::INT,
            27 => TokenType::BOOLEAN,
            28 => TokenType::WHILE,
            29 => TokenType::IF,
            30 => TokenType::ELSE,
            31 => TokenType::MAIN,
            32 => TokenType::RETURN,
            33 => TokenType::LENGTH,
            34 => TokenType::TRUE,
            35 => TokenType::FALSE,
            36 => TokenType::THIS,
            37 => TokenType::NEW,
            38 => TokenType::PRINTLN,
            39 => TokenType::SIDEF,
            40 => TokenType::ID(text),
            41 => TokenType::INTLIT(text),
            42 => TokenType::STRINGLIT(text),
            _ => TokenType::UNRECOGNIZED(text),
        };
        (text.len(), t)
    }
}

impl<'a> Display for TokenType<'a> {
    /// This is how Rust knows how to pretty-print a TokenType.
    /// It's equivalent to Java's toString() method.
    fn fmt(&self, f: &mut Formatter) -> Result<(), fmt::Error> {
        let print = match *self {
            TokenType::ID(s) => format!("ID({})", s),
            TokenType::INTLIT(s) => format!("INTLIT({})", s),
            TokenType::STRINGLIT(s) => format!("STRINGLIT({})", s),
            TokenType::UNRECOGNIZED(s) => format!("UNRECOGNIZED({})", s),
            ref t => format!("{:?}()", t),
        };
        f.write_str(&print)
    }
}

/// A Token is the combination of a TokenType, the length of the string
/// that this token represents, and the line and column on which the
/// token begins.
#[derive(Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct Token<'a> {
    pub ty: TokenType<'a>,
    pub len: usize,
    pub line: usize,
    pub column: usize,
}

impl<'a> Display for Token<'a> {
    /// This is how Rust knows how to pretty-print a Token.
    /// It's equivalent to Java's toString() method.
    fn fmt(&self, f: &mut Formatter) -> Result<(), fmt::Error> {
        write!(f, "{}:{} {}", self.line, self.column, self.ty)
    }
}

pub struct Tokens<'a> {
    pub successful: Vec<Token<'a>>,
    pub failed: Vec<Token<'a>>
}

impl<'a> Tokens<'a> {
    fn new() -> Self {
        Tokens { successful: Vec::new(), failed: Vec::new() }
    }
}

impl<'a> Deref for Tokens<'a> {
    type Target = Vec<Token<'a>>;

    fn deref(&self) -> &Self::Target {
        &self.successful
    }
}

/// Given an input string from an "emj" file, return a list of Tokens.
pub fn lex(input: &str) -> Result<Tokens, Error> {
    info!("Begin lexing input");

    let (mut line, mut column) = (1, 1);
    let mut tokens = Tokens::new();
    let mut i = input;

    // While the input string has more unconsumed characters
    'outer: while i.len() > 0 {

        // Skip any whitespaces, keeping track of line and column numbers
        let mut block_comment = false;
        let mut line_comment = false;
        loop {
            if i.len() <= 0 { break 'outer }
            let (skip, next_line, next_column) = match &i[0..1] {
                " "  => (1, line, column + 1),
                "\t" => (1, line, column + 4),
                "\r" if &i[1..2] == "\n" => {
                    line_comment = false;
                    (2, line + 1, 1)
                },
                "\n" => {
                    line_comment = false;
                    (1, line + 1, 1)
                },
                "/" if &i[1..2] == "/" => {
                    if !block_comment { line_comment = true }
                    (2, line, column + 2)
                },
                "/" if &i[1..2] == "*" => {
                    if !line_comment { block_comment = true }
                    (2, line, column + 2)
                },
                "*" if &i[1..2] == "/" => {
                    block_comment = false;
                    (2, line, column + 2)
                }
                _ => if !line_comment && !block_comment { break } else {(1, line, column + 1)},
            };
            i = &i[skip..];
            line = next_line;
            column = next_column;
        }

        // From top to bottom, check whether any rule matches this string.
        let mut pattern = None;
        for (p, rule) in RULES.iter().enumerate() {
            if rule.is_match(i) {
                pattern = Some(p);
                break;
            }
        }

        // If a rule matches this string, make a token corresponding to the match.
        let skip = if let Some(p) = pattern {
            let (len, ty) = TokenType::new(p, RULES[p].captures(i).unwrap().get(0).unwrap().as_str());
            let token = Token { ty, len, line, column };
            tokens.successful.push(token);
            len
        } else {
            // If no rule matches this string, capture it as a "Bad" token for debugging.
            let len = 1;
            let bad = Token { ty: TokenType::UNRECOGNIZED(&i[0..1]), len, line, column };
            error!("Unrecognized input: {}", bad);
            tokens.failed.push(bad);
            len
        };

        // Skip a number of characters equal to the length of the parsed Token.
        column += skip;
        i = &i[skip..];
    }

    // Return the list of tokens.
    Ok(tokens)
}
