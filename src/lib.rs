#![recursion_limit="128"]
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
    static ref COLON:     Regex = Regex::new(r"^:").unwrap();
    static ref SEMICOLON: Regex = Regex::new(r"^;").unwrap();
    static ref DOT:       Regex = Regex::new(r"^\.").unwrap();
    static ref COMMA:     Regex = Regex::new(r"^,").unwrap();
    static ref EQSIGN:    Regex = Regex::new(r"^=").unwrap();
    static ref EQUALS:    Regex = Regex::new(r"^==").unwrap();
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
        &COLON, &SEMICOLON, &DOT, &COMMA, &EQSIGN, &EQUALS, &BANG, &LPAREN, &RPAREN,
        &LBRACKET, &RBRACKET, &LBRACE, &RBRACE, &AND, &OR, &LESSTHAN, &PLUS, &MINUS, &TIMES, &DIV,
        &CLASS, &PUBLIC, &STATIC, &VOID, &STRING, &EXTENDS, &INT, &BOOLEAN, &WHILE, &IF, &ELSE,
        &MAIN, &RETURN, &LENGTH, &TRUE, &FALSE, &THIS, &NEW, &PRINTLN, &SIDEF, &ID, &INTLIT,
        &STRINGLIT,
    ];
}

#[derive(Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub enum Token<'a> {
    COLON, SEMICOLON, DOT, COMMA, EQSIGN, EQUALS, BANG, LPAREN, RPAREN, LBRACKET, RBRACKET,
    LBRACE, RBRACE, AND, OR, LESSTHAN, PLUS, MINUS, TIMES, DIV, CLASS, PUBLIC, STATIC, VOID,
    STRING, EXTENDS, INT, BOOLEAN, WHILE, IF, ELSE, MAIN, RETURN, LENGTH, TRUE, FALSE, THIS, NEW,
    PRINTLN, SIDEF,
    ID(&'a str),
    INTLIT(&'a str),
    STRINGLIT(&'a str),
    BAD(&'a str),
}

impl<'a> Token<'a> {
    fn new(index: usize, text: &'a str) -> (usize, Self) {
        let t = match index {
            0  => Token::COLON,
            1  => Token::SEMICOLON,
            2  => Token::DOT,
            3  => Token::COMMA,
            4  => Token::EQSIGN,
            5  => Token::EQUALS,
            6  => Token::BANG,
            7  => Token::LPAREN,
            8  => Token::RPAREN,
            9  => Token::LBRACKET,
            10 => Token::RBRACKET,
            11 => Token::LBRACE,
            12 => Token::RBRACE,
            13 => Token::AND,
            14 => Token::OR,
            15 => Token::LESSTHAN,
            16 => Token::PLUS,
            17 => Token::MINUS,
            18 => Token::TIMES,
            19 => Token::DIV,
            20 => Token::CLASS,
            21 => Token::PUBLIC,
            22 => Token::STATIC,
            23 => Token::VOID,
            24 => Token::STRING,
            25 => Token::EXTENDS,
            26 => Token::INT,
            27 => Token::BOOLEAN,
            28 => Token::WHILE,
            29 => Token::IF,
            30 => Token::ELSE,
            31 => Token::MAIN,
            32 => Token::RETURN,
            33 => Token::LENGTH,
            34 => Token::TRUE,
            35 => Token::FALSE,
            36 => Token::THIS,
            37 => Token::NEW,
            38 => Token::PRINTLN,
            39 => Token::SIDEF,
            40 => Token::ID(text),
            41 => Token::INTLIT(text),
            42 => Token::STRINGLIT(text),
            _  => Token::BAD(text),
        };
        (text.len(), t)
    }
}

pub fn lex(input: &str) -> Result<Vec<Token>, Error> {
    info!("Begin lexing input");

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
            (1, Token::BAD(&i[0..1]))
        };

        tokens.push(token);
        i = &i[skip..];
    }

    Ok(tokens)
}
