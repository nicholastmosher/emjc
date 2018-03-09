use std::collections::HashMap;
use std::result;
use std::ops::{
    Deref,
    DerefMut,
};
use std::fmt::{
    self,
    Display,
    Formatter,
};
use Result;
use lexer::{
    Token,
    TokenType,
};
use super::ParseError;

#[derive(Debug, Hash, Copy, Clone, Eq, PartialEq, Ord, PartialOrd)]
pub struct Terminal(TokenType);

impl From<TokenType> for Terminal {
    fn from(tt: TokenType) -> Self {
        Terminal(tt)
    }
}

impl Display for Terminal {
    fn fmt(&self, f: &mut Formatter) -> result::Result<(), fmt::Error> {
        write!(f, "{:?}", self.0)
    }
}

#[derive(Debug, Hash, Copy, Clone, Eq, PartialEq, Ord, PartialOrd)]
pub enum NonTerminal {
    Program,
    MainClass,
    Class,
    ClassRepeat,
    Extends,
    Type,
    Variable,
    VariableRepeat,
    Function,
    FunctionRepeat,
    Statement,
    StatementRepeat,
    Argument,
    ArgumentRepeat,
    Expression,
    ExpressionExtension,
    ExpressionList,
    ExpressionListRepeat,
}

impl Display for NonTerminal {
    fn fmt(&self, f: &mut Formatter) -> result::Result<(), fmt::Error> {
        write!(f, "{:?}", self)
    }
}

#[derive(Debug, Hash, Copy, Clone, Eq, PartialEq, Ord, PartialOrd)]
pub enum Symbol {
    T(Terminal),
    N(NonTerminal),
}

impl Display for Symbol {
    fn fmt(&self, f: &mut Formatter) -> result::Result<(), fmt::Error> {
        match *self {
            Symbol::T(terminal) => write!(f, "T({})", terminal),
            Symbol::N(non_terminal) => write!(f, "N({})", non_terminal),
        }
    }
}

impl From<Terminal> for Symbol {
    fn from(terminal: Terminal) -> Self {
        Symbol::T(terminal)
    }
}

impl From<TokenType> for Symbol {
    fn from(tt: TokenType) -> Self {
        Symbol::T(Terminal(tt))
    }
}

impl From<NonTerminal> for Symbol {
    fn from(nt: NonTerminal) -> Self {
        Symbol::N(nt)
    }
}

/// A Production represents a possible transition from a NonTerminal
/// grammar item to a sequence of other grammar items (a possible mix
/// or Terminals and NonTerminals).
#[derive(Debug, Hash, Clone, Eq, PartialEq, Ord, PartialOrd)]
pub struct Production {
    from: NonTerminal,
    to: Vec<Symbol>,
}

impl Production {
    pub fn new(from: NonTerminal, to: Vec<Symbol>) -> Self {
        Production { from, to }
    }
}

impl Display for Production {
    fn fmt(&self, f: &mut Formatter) -> result::Result<(), fmt::Error> {
        write!(f, "{} -> {}", self.from, self.to[0])?;
        for item in self.to.iter().skip(1) {
            write!(f, ", {}", item)?;
        }
        Ok(())
    }
}

#[derive(Debug)]
pub struct ProductionTable {
    table: HashMap<(NonTerminal, Terminal), Production>,
}

impl ProductionTable {
    pub fn new() -> Self {
        ProductionTable { table: HashMap::new() }
    }
}

/// Gives the ProductionTable type access to all immutable HashMap functions.
impl Deref for ProductionTable {
    type Target = HashMap<(NonTerminal, Terminal), Production>;
    fn deref(&self) -> &Self::Target { &self.table }
}

/// Gives the ProductionTable type access to all mutable HashMap functions.
impl DerefMut for ProductionTable {
    fn deref_mut(&mut self) -> &mut Self::Target { &mut self.table }
}

#[derive(Debug)]
struct ParseStack {
    stack: Vec<Symbol>,
}

impl ParseStack {
    /// Create a new ParseStack containing the start symbol.
    fn new() -> Self {
        ParseStack { stack: vec![NonTerminal::Program.into()] }
    }

    /// Given a production rule, push the grammar items of the rule onto this
    /// ParseStack in reverse order.
    fn add(&mut self, rule: &Production) {
        for grammar_item in rule.to.iter().rev() {
            self.stack.push(grammar_item.clone())
        }
    }

    /// Peeks at the GrammarItem at the top of the stack without popping it.
    ///
    /// Returns `None` if the stack is empty.
    fn peek(&self) -> Option<&Symbol> {
        self.stack.last()
    }

    /// Pops the top GrammarItem off of the stack and returns it.
    ///
    /// Returns `None` if the stack is empty.
    fn pop(&mut self) -> Option<Symbol> {
        self.stack.pop()
    }
}

pub struct Parser<'a, T: Iterator<Item=Token<'a>>> {
    token_stream: T,
}

/// Allow Parser to be treated as if it were an iterator of tokens
impl<'a, T: Iterator<Item=Token<'a>>> Deref for Parser<'a, T> {
    type Target = T;
    fn deref(&self) -> &Self::Target { &self.token_stream }
}

impl<'a, T: Iterator<Item=Token<'a>>> DerefMut for Parser<'a, T> {
    fn deref_mut(&mut self) -> &mut Self::Target { &mut self.token_stream }
}

impl<'a, T: 'a + Iterator<Item=Token<'a>>> Parser<'a, T> {
    pub fn parse_nonrecursive(&mut self) -> Result<()> {
        let mut stack = ParseStack::new();

        let prod_one = Production::new(NonTerminal::Program, vec![NonTerminal::Class.into()]);
        let prod_two = Production::new(NonTerminal::Program, vec![
            TokenType::LPAREN.into(),
            NonTerminal::Program.into(),
            TokenType::PLUS.into(),
            NonTerminal::Class.into(),
            TokenType::RPAREN.into(),
        ]);
        let prod_three = Production::new(NonTerminal::Class, vec![
            TokenType::BANG.into(),
        ]);

        // Construct the production table
        let mut table = ProductionTable::new();
        table.insert((NonTerminal::Program, TokenType::BANG.into()), prod_one);
        table.insert((NonTerminal::Program, TokenType::LPAREN.into()), prod_two);
        table.insert((NonTerminal::Class, TokenType::BANG.into()), prod_three);

        let mut current = self.next();
        loop {
            match current {
                Some(Token { ty: TokenType::EOF, .. }) => break,
                None => Err(ParseError::PrematureConclusion)?,
                Some(Token { ty, .. }) => {
                    let top = stack.peek();
                    let current_terminal: Terminal = ty.into();
                    println!("Top is {:?}, token is {:?}", top, current_terminal);

                    match top {
                        // If the stack is empty, we have too much input. Error.
                        None => Err(ParseError::PrematureConclusion)?,
                        Some(&Symbol::T(top_terminal)) => {
                            if top_terminal == current_terminal {
                                // If the top of the stack is a Terminal that matches the current
                                // token, pop that terminal and continue parsing.
                                stack.pop();
                                current = self.next();
                            } else {
                                // If the top of the stack is a Terminal that does NOT match the
                                // current token, the input does not conform to the grammar.
//                                Err(ParseError::TerminalMismatch(top_terminal, current_terminal))?;
                                // FIXME not permanent
                                Err(ParseError::PrematureConclusion)?;
                            }
                        },
                        // If the top of the stack is a NonTerminal, check if there are any
                        // production rules for that NonTerminal and the current Terminal.
                        Some(&Symbol::N(non_terminal)) => {
                            if let Some(production) = table.get(&(non_terminal, current_terminal)) {
                                stack.pop();
                                stack.add(production);
                            } else {
//                                Err(ParseError::MissingProduction(non_terminal, current_terminal))?;
                                // FIXME not permanent
                                Err(ParseError::PrematureConclusion)?;
                            }
                        }
                    }
                }
            }
        }
        println!("Successfully parsed input");
        Ok(())
    }
}
