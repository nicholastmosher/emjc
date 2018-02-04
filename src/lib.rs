#![deny(warnings)]

#[macro_use]
extern crate log;
extern crate failure;
#[macro_use]
extern crate lazy_static;
extern crate regex;

use failure::Error;
use regex::Regex;

lazy_static! {
    static ref KEYWORD: Regex = Regex::new(r"^(class|public|static|void|main|if|while|sidef|true|false|this|new|return|System\.out\.println)").unwrap();
    static ref TYPE:    Regex = Regex::new(r"^(int|boolean|String|int\[\])").unwrap();
    static ref IDENT:   Regex = Regex::new(r"^[a-zA-Z][a-zA-Z\d_]*").unwrap();
    static ref INT:     Regex = Regex::new(r"^\d+").unwrap();
    static ref LPAREN:  Regex = Regex::new(r"^\(").unwrap();
    static ref RPAREN:  Regex = Regex::new(r"^\)").unwrap();
    static ref LBRACE:  Regex = Regex::new(r"^\{").unwrap();
    static ref RBRACE:  Regex = Regex::new(r"^\}").unwrap();
    static ref LBRACK:  Regex = Regex::new(r"^\[").unwrap();
    static ref RBRACK:  Regex = Regex::new(r"^\]").unwrap();
    static ref AND:     Regex = Regex::new(r"^&&").unwrap();
    static ref NOT:     Regex = Regex::new(r"^!").unwrap();
    static ref OR:      Regex = Regex::new(r"^\|\|").unwrap();
    static ref LT:      Regex = Regex::new(r"^<").unwrap();
    static ref GT:      Regex = Regex::new(r"^>").unwrap();
    static ref EQUALS:  Regex = Regex::new(r"^==").unwrap();
    static ref ASSIGN:  Regex = Regex::new(r"^=").unwrap();
    static ref PLUS:    Regex = Regex::new(r"^\+").unwrap();
    static ref MINUS:   Regex = Regex::new(r"^-").unwrap();
    static ref TIMES:   Regex = Regex::new(r"^\*").unwrap();
    static ref DIVIDE:  Regex = Regex::new(r"^\\/").unwrap();
    static ref SEMI:    Regex = Regex::new(r"^;").unwrap();
    static ref STRING:  Regex = Regex::new(r#"^"([a-zA-Z\d\s\.]*)""#).unwrap();

    static ref RULES: Vec<&'static Regex> = vec![
        &KEYWORD,
        &TYPE,
        &IDENT,
        &INT,
        &LPAREN,
        &RPAREN,
        &LBRACE,
        &RBRACE,
        &LBRACK,
        &RBRACK,
        &AND,
        &NOT,
        &OR,
        &LT,
        &GT,
        &EQUALS,
        &ASSIGN,
        &PLUS,
        &MINUS,
        &TIMES,
        &DIVIDE,
        &SEMI,
        &STRING,
    ];
}

#[derive(Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub enum Token<'a> {
    Keyword(&'a str),
    Type(&'a str),
    Ident(&'a str),
    Int(&'a str),
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBrack,
    RBrack,
    And,
    Not,
    Or,
    Lt,
    Gt,
    Equals,
    Assign,
    Plus,
    Minus,
    Times,
    Divide,
    Semi,
    String(&'a str),
    Unknown(&'a str),
}

impl<'a> Token<'a> {
    fn new(index: usize, text: &'a str) -> (usize, Self) {
        let t = match index {
            0  => Token::Keyword(text),
            1  => Token::Type(text),
            2  => Token::Ident(text),
            3  => Token::Int(text),
            4  => Token::LParen,
            5  => Token::RParen,
            6  => Token::LBrace,
            7  => Token::RBrace,
            8  => Token::LBrack,
            9  => Token::RBrack,
            10 => Token::And,
            11 => Token::Not,
            12 => Token::Or,
            13 => Token::Lt,
            14 => Token::Gt,
            15 => Token::Equals,
            16 => Token::Assign,
            17 => Token::Plus,
            18 => Token::Minus,
            19 => Token::Times,
            20 => Token::Divide,
            21 => Token::Semi,
            22 => Token::String(text),
            _  => Token::Unknown(text),
        };
        debug!("Made token: {:?}", t);
        (text.len(), t)
    }
}

pub fn lex(input: &str) -> Result<Vec<Token>, Error> {
    const WHITESPACES: &str = " \t\n";
    let mut tokens: Vec<Token> = Vec::new();
    let mut i = input;

    'outer: while i.len() > 0 {

        // Skip any whitespaces
        for c in i.chars() {
            if WHITESPACES.contains(c) {
                i = &i[1..];
                if i.len() == 0 { break 'outer }
            } else {
                break
            }
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
        let (skip, token) = if let Some(p) = pattern {
            Token::new(p, RULES[p].captures(i).unwrap().get(0).unwrap().as_str())
        } else {
            // If no rule matches this string, capture it as an "Unknown" for debugging.
            (1, Token::Unknown(&i[0..1]))
        };

        tokens.push(token);
        i = &i[skip..];
    }

    Ok(tokens)
}
