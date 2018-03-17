use std::result;
use std::io::BufRead;
use std::ops::Deref;
use std::fmt::{
    self,
    Display,
    Formatter,
};
use std::collections::VecDeque;

use Result;

use regex::Regex;
use tendril::StrTendril;
use tendril::SubtendrilError;

#[derive(Debug, Fail)]
enum LexerError {
    #[fail(display = "{}:{} Expected {}, got {}", _0, _1, _3, _2)]
    UnexpectedToken(usize, usize, TokenType, TokenType),
    #[fail(display = "error taking a substring for a token: {:?}", _0)]
    SubTendril(SubtendrilError),
}

// Declare the Regexes used to capture each token type.
//
// These regex parsers are lazily loaded. They are only initialized once when
// they are first needed, after which they are cached in global "static" memory
// for fast reuse.
lazy_static! {
    static ref ID:        Regex = Regex::new(r"^[_a-zA-Z][_a-zA-Z\d]*").unwrap();
    static ref INTLIT:    Regex = Regex::new(r"^\d+").unwrap();
    static ref STRINGLIT: Regex = Regex::new(r#"^"(?:[^"\\]|\\.)*""#).unwrap();
}

/// Defines the unique types of tokens which may be parsed from the input.
#[derive(Debug, Hash, Copy, Clone, Eq, PartialEq, Ord, PartialOrd)]
pub enum TokenType {
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
    EOF,
    ID,
    INTLIT,
    STRINGLIT,
    UNRECOGNIZED,
}

impl Display for TokenType {
    /// This is how Rust knows how to pretty-print a TokenType.
    /// It's equivalent to Java's toString() method.
    fn fmt(&self, f: &mut Formatter) -> result::Result<(), fmt::Error> {
        write!(f, "{:?}()", self)
    }
}

/// A Token is the combination of a TokenType, the length of the string
/// that this token represents, and the line and column on which the
/// token begins.
#[derive(Debug, Hash, Clone, Eq, PartialEq, Ord, PartialOrd)]
pub struct Token {
    pub ty: TokenType,
    pub text: StrTendril,
    pub line: usize,
    pub column: usize,
}

impl Display for Token {
    /// This is how Rust knows how to pretty-print a Token.
    /// It's equivalent to Java's toString() method.
    fn fmt(&self, f: &mut Formatter) -> result::Result<(), fmt::Error> {
        write!(f, "{}:{} {}", self.line, self.column, self.ty)
    }
}

/// A representation of a Token whose fields are all owned for use
/// as a field in Error structs.
#[derive(Debug, Hash, Clone, Eq, PartialEq, Ord, PartialOrd)]
pub struct OwnedToken {
    pub kind: TokenType,
    pub text: String,
    pub line: usize,
    pub column: usize,
}

impl<'a> From<&'a Token> for OwnedToken {
    fn from(token: &'a Token) -> Self {
        OwnedToken {
            kind: token.ty,
            text: String::from(&token.text),
            line: token.line,
            column: token.column,
        }
    }
}

pub struct Lexer {
    buffer: StrTendril,
    tokens: VecDeque<Token>,
    offset: usize,
}

impl Deref for Lexer {
    type Target = VecDeque<Token>;
    fn deref(&self) -> &Self::Target {
        &self.tokens
    }
}

impl Lexer {
    pub fn new<Reader: BufRead>(input: &mut Reader) -> Result<Self> {
        let mut string = String::new();
        input.read_to_string(&mut string)?;
        let buffer = StrTendril::from(string);
        let mut lexer = Lexer { buffer, tokens: VecDeque::new(), offset: 0 };
        lexer.lex()?;
        Ok(lexer)
    }

    pub fn peek(&self) -> TokenType {
        self.peek_num(0)
    }

    pub fn peek_num(&self, num: usize) -> TokenType {
        self.tokens.get(num).map(|t| t.ty)
            .expect(&format!("Cannot peek {}, token stream ends", num))
    }

    pub fn munch(&mut self, tt: TokenType) -> Result<Token> {
        if self.peek() != tt {
            let c = self.tokens.get(0).unwrap();
            Err(LexerError::UnexpectedToken(c.line, c.column, c.ty, tt))?;
        }
        self.tokens.pop_front().ok_or(format_err!("Token stream ended unexpectedly"))
    }

    pub fn munch_by(&mut self, tt: TokenType, by: &str) -> Result<Token> {
        let ret = self.munch(tt)?;
        debug!("{:25} munched {}", by, tt);
        Ok(ret)
    }

    /// Given an input string from an "emj" file, return a list of Tokens.
    fn lex(&mut self) -> Result<()> {
        info!("Begin lexing input");

        let mut line = 1;
        let mut column = 1;
        let mut failed = Vec::new();
        let mut i = &self.buffer[..];

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
                    },
                    _ => if !line_comment && !block_comment { break } else {(1, line, column + 1)},
                };
//                i = &i[skip..];
                self.offset += skip;
                i = &self.buffer[self.offset..];
                line = next_line;
                column = next_column;
            }

            // Check if there are any trivial matches, such as static strings.
            let tm = match &i[0..1] {
                ":"                         => Some((1, TokenType::COLON)),
                ";"                         => Some((1, TokenType::SEMICOLON)),
                "."                         => Some((1, TokenType::DOT)),
                ","                         => Some((1, TokenType::COMMA)),
                "=" if &i[1..2] == "="      => Some((2, TokenType::EQUALS)),
                "="                         => Some((1, TokenType::EQSIGN)),
                "!"                         => Some((1, TokenType::BANG)),
                "("                         => Some((1, TokenType::LPAREN)),
                ")"                         => Some((1, TokenType::RPAREN)),
                "["                         => Some((1, TokenType::LBRACKET)),
                "]"                         => Some((1, TokenType::RBRACKET)),
                "{"                         => Some((1, TokenType::LBRACE)),
                "}"                         => Some((1, TokenType::RBRACE)),
                "&" if &i[1..2] == "&"      => Some((2, TokenType::AND)),
                "|" if &i[1..2] == "|"      => Some((2, TokenType::OR)),
                "<"                         => Some((1, TokenType::LESSTHAN)),
                "+"                         => Some((1, TokenType::PLUS)),
                "-"                         => Some((1, TokenType::MINUS)),
                "*"                         => Some((1, TokenType::TIMES)),
                "/"                         => Some((1, TokenType::DIV)),
                "c" if &i[1..5] == "lass"   => Some((6, TokenType::CLASS)),
                "p" if &i[1..6] == "ublic"  => Some((6, TokenType::PUBLIC)),
                "s" if &i[1..6] == "tatic"  => Some((6, TokenType::STATIC)),
                "s" if &i[1..5] == "idef"   => Some((5, TokenType::SIDEF)),
                "v" if &i[1..4] == "oid"    => Some((4, TokenType::VOID)),
                "e" if &i[1..7] == "xtends" => Some((7, TokenType::EXTENDS)),
                "e" if &i[1..4] == "lse"    => Some((4, TokenType::ELSE)),
                "i" if &i[1..2] == "f"      => Some((2, TokenType::IF)),
                "i" if &i[1..3] == "nt"     => Some((3, TokenType::INT)),
                "b" if &i[1..7] == "oolean" => Some((7, TokenType::BOOLEAN)),
                "w" if &i[1..5] == "hile"   => Some((5, TokenType::WHILE)),
                "m" if &i[1..4] == "ain"    => Some((4, TokenType::MAIN)),
                "r" if &i[1..6] == "eturn"  => Some((6, TokenType::RETURN)),
                "l" if &i[1..6] == "ength"  => Some((6, TokenType::LENGTH)),
                "t" if &i[1..4] == "rue"    => Some((4, TokenType::TRUE)),
                "f" if &i[1..5] == "alse"   => Some((5, TokenType::FALSE)),
                "t" if &i[1..4] == "his"    => Some((4, TokenType::THIS)),
                "n" if &i[1..3] == "ew"     => Some((3, TokenType::NEW)),
                "S" if &i[1..6] == "tring"  => Some((6, TokenType::STRING)),
                "S" if &i[1..18] == "ystem.out.println" => Some((18, TokenType::PRINTLN)),
                _ => None,
            };

            // If one of the static matches returned, add the token, apply skip, and continue.
            if let Some((len, ty)) = tm {
//                let text = &i[0..len];
                let text = self.buffer.try_subtendril(self.offset as u32, len as u32)
                    .map_err(|e| LexerError::SubTendril(e))?;
                let token = Token { ty, text, line, column, };
                debug!("Found static match of length {}: {}", len, token);
                self.tokens.push_back(token);
                column += len;
//                i = &i[len..];
                self.offset += len;
                i = &self.buffer[self.offset..];
                continue;
            }

            // Otherwise, check the string against each regex, capturing necessary matches.
            let skip = if ID.is_match(i) {
                let capture = ID.captures(i).unwrap().get(0).unwrap().as_str();
                let text = self.buffer.try_subtendril(self.offset as u32, capture.len() as u32)
                    .map_err(|e| LexerError::SubTendril(e))?;
                let token = Token { ty: TokenType::ID, text, line, column };
                debug!("Found identifier of length {}: {}", capture.len(), token);
                self.tokens.push_back(token);
                capture.len()
            } else if INTLIT.is_match(i) {
                let capture = INTLIT.captures(i).unwrap().get(0).unwrap().as_str();
                let text = self.buffer.try_subtendril(self.offset as u32, capture.len() as u32)
                    .map_err(|e| LexerError::SubTendril(e))?;
                let token = Token { ty: TokenType::INTLIT, text, line, column };
                debug!("Found int literal of length {}: {}", capture.len(), token);
                self.tokens.push_back(token);
                capture.len()
            } else if STRINGLIT.is_match(i) {
                let (_, len) = STRINGLIT.captures(i)
                    .and_then(|t| t.get(0))
                    .map(|t| t.as_str())
                    .map(|t| (t, t.len()))
                    .unwrap_or(("", 2));

                let text = self.buffer.try_subtendril(self.offset as u32, len as u32)
                    .map_err(|e| LexerError::SubTendril(e))?;
                let token = Token { ty: TokenType::STRINGLIT, text, line, column };
                debug!("Found string literal of length {}: {}", len, token);
                self.tokens.push_back(token);
                len
            } else {
//                let text = &i[0..1];
                let text = self.buffer.try_subtendril(self.offset as u32, 1)
                    .map_err(|e| LexerError::SubTendril(e))?;
                warn!("Failed to match: '{}' at ({}, {})", text, line, column);
                let token = Token { ty: TokenType::UNRECOGNIZED, text, line, column };
                failed.push(token);
                1
            };

            // Skip a number of characters equal to the length of the parsed Token.
            column += skip;
//            i = &i[skip..];
            self.offset += skip;
            i = &self.buffer[self.offset..];
        }

        self.tokens.push_back(Token { ty: TokenType::EOF, text: StrTendril::new(), line, column });
        Ok(())
    }
}

