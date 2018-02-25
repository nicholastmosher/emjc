use lexer::TokenType;
use parser::{
    Terminal,
    NonTerminal,
    Symbol,
    Production,
    ProductionTable,
};

pub const PRODUCTIONS: Vec<Production> = vec![
    // FIRST(P) = FIRST(M) = { class }
    // P -> M C' $
    // Index 0
    Production::new(NonTerminal::Program, vec![
        NonTerminal::MainClass.into(),
        NonTerminal::ClassRepeat.into()
    ]),

    // FIRST(M) = { class }
    // FOLLOW(M) = FIRST(C') - { epsilon } = { class }
    // M -> class I { public static void main ( String [ ] I ) { S } }
    // Index 1
    Production::new(NonTerminal::MainClass, vec![
        TokenType::CLASS.into(),
        TokenType::ID.into(),
        TokenType::LBRACE.into(),
        TokenType::PUBLIC.into(),
        TokenType::STATIC.into(),
        TokenType::VOID.into(),
        TokenType::MAIN.into(),
        TokenType::LPAREN.into(),
        TokenType::STRING.into(),
        TokenType::LBRACKET.into(),
        TokenType::RBRACKET.into(),
        TokenType::ID.into(),
        TokenType::RPAREN.into(),
        TokenType::LBRACE.into(),
        NonTerminal::Statement.into(),
        TokenType::RBRACE.into(),
        TokenType::RBRACE.into(),
    ]),

    // FIRST(C) = { class }
    // FOLLOW(C) = FIRST(C') - { epsilon } U FOLLOW(C') = { class, $ }
    // C -> class I X { V' F' }
    // Index 2
    Production::new(NonTerminal::Class, vec![
        TokenType::CLASS.into(),
        TokenType::ID.into(),
        NonTerminal::Extends.into(),
        TokenType::LBRACE.into(),
        NonTerminal::VariableRepeat.into(),
        NonTerminal::FunctionRepeat.into(),
        TokenType::RBRACE.into(),
    ]),

    // FIRST(C') = FIRST(C) U { epsilon } = { class, epsilon }
    // FOLLOW(C') = { $ }
    // C' -> C C'
    // Index 3
    Production::new(NonTerminal::ClassRepeat, vec![
        NonTerminal::Class.into(),
        NonTerminal::ClassRepeat.into(),
    ]),
    // C' -> epsilon
    // Index 4
    Production::new(NonTerminal::ClassRepeat, vec![]),

    // FIRST(X) = { extends, epsilon }
    // FOLLOW(X) = { LBRACE }
    // X -> extends I
    // Index 5
    Production::new(NonTerminal::Extends, vec![
        TokenType::EXTENDS.into(),
        TokenType::ID.into(),
    ]),
    // X -> epsilon
    // Index 6
    Production::new(NonTerminal::Extends, vec![]),

    // FIRST(T) = { I, boolean, String, int }
    // FOLLOW(T) = { I }
    // T -> I
    // Index 7
    Production::new(NonTerminal::Type, vec![TokenType::ID.into()]),
    // T -> boolean
    // Index 8
    Production::new(NonTerminal::Type, vec![TokenType::BOOLEAN.into()]),
    // T -> String
    // Index 9
    Production::new(NonTerminal::Type, vec![TokenType::STRING.into()]),
    // T -> int
    // Index 10
    Production::new(NonTerminal::Type, vec![TokenType::INT.into()]),
    // T -> int [ ]
    // Index 11
    Production::new(NonTerminal::Type, vec![
        TokenType::INT.into(),
        TokenType::LBRACKET.into(),
        TokenType::RBRACKET.into(),
    ]),

    // FIRST(V) = FIRST(T) = { I, boolean, String, int }
    // FOLLOW(V) = FIRST(V') - { epsilon } U FOLLOW(V') = { I, boolean, String, int, public, RBRACE }
    // V -> T I ;
    // Index 12
    Production::new(NonTerminal::Variable, vec![
        NonTerminal::Type.into(),
        TokenType::ID.into(),
        TokenType::SEMICOLON.into(),
    ]),

    // FIRST(V') = FIRST(V) U { epsilon } = { I, boolean, String, int, epsilon }
    // FOLLOW(V') = FIRST(F') - { epsilon } U FOLLOW(F') = { public, RBRACE }
    // V' -> V V'
    // Index 13
    Production::new(NonTerminal::VariableRepeat, vec![
        NonTerminal::Variable.into(),
        NonTerminal::VariableRepeat.into(),
    ]),
    // V' -> epsilon
    // Index 14
    Production::new(NonTerminal::VariableRepeat, vec![]),

    // FIRST(F) = { public }
    // FOLLOW(F) = FIRST(F') - { epsilon } U FOLLOW(F') = { public, RBRACE }
    // F -> public T I ( A ) { V' S' return E ; }
    // Index 15
    Production::new(NonTerminal::Function, vec![
        TokenType::PUBLIC.into(),
        NonTerminal::Type.into(),
        TokenType::ID.into(),
        TokenType::LPAREN.into(),
        NonTerminal::Argument.into(),
        TokenType::RPAREN.into(),
        TokenType::LBRACE.into(),
        NonTerminal::VariableRepeat.into(),
        NonTerminal::StatementRepeat.into(),
        TokenType::RETURN.into(),
        NonTerminal::Expression.into(),
        TokenType::SEMICOLON.into(),
        TokenType::RBRACE.into(),
    ]),

    // FIRST(F') = FIRST(F) U { epsilon } = { public, epsilon }
    // FOLLOW(F') = { RBRACE }
    // F' -> F F'
    // Index 16
    Production::new(NonTerminal::FunctionRepeat, vec![
        NonTerminal::Function.into(),
        NonTerminal::FunctionRepeat.into(),
    ]),
    // F' -> epsilon
    // Index 17
    Production::new(NonTerminal::FunctionRepeat, vec![]),

    // FIRST(A) = FIRST(T) U { epsilon } = { I, boolean, String, int, epsilon }
    // FOLLOW(A) = { RPAREN }
    // A -> T I A'
    // Index 18
    Production::new(NonTerminal::Argument, vec![
        NonTerminal::Type.into(),
        TokenType::ID.into(),
        NonTerminal::ArgumentRepeat.into(),
    ]),
    // A -> epsilon
    // Index 19
    Production::new(NonTerminal::Argument, vec![]),

    // FIRST(A') = { COMMA, epsilon }
    // FOLLOW(A') = FOLLOW(A) = { RPAREN }
    // A' -> , T I A'
    // Index 20
    Production::new(NonTerminal::ArgumentRepeat, vec![
        TokenType::COMMA.into(),
        NonTerminal::Type.into(),
        TokenType::ID.into(),
        NonTerminal::ArgumentRepeat.into(),
    ]),
    // A' -> epsilon
    // Index 21
    Production::new(NonTerminal::ArgumentRepeat, vec![]),

    // FIRST(S) = { LBRACE, while, PRINTLN, I, sidef }
    // FOLLOW(S) = { RBRACE }
    // S -> { S' }
    // Index 22
    Production::new(NonTerminal::Statement, vec![
        TokenType::LBRACE.into(),
        NonTerminal::StatementRepeat.into(),
        TokenType::RBRACE.into(),
    ]),
    // S -> while ( E ) S
    // Index 23
    Production::new(NonTerminal::Statement, vec![
        TokenType::WHILE.into(),
        TokenType::LPAREN.into(),
        NonTerminal::Expression.into(),
        TokenType::RPAREN.into(),
        NonTerminal::Statement.into(),
    ]),
    // S -> System.out.println ( E )
    // Index 24
    Production::new(NonTerminal::Statement, vec![
        TokenType::PRINTLN.into(),
        TokenType::LPAREN.into(),
        NonTerminal::Expression.into(),
        TokenType::RPAREN.into(),
    ]),
    // S -> I = E ;
    // Index 25
    Production::new(NonTerminal::Statement, vec![
        TokenType::ID.into(),
        TokenType::EQSIGN.into(),
        NonTerminal::Expression.into(),
        TokenType::SEMICOLON.into(),
    ]),
    // S -> I [ E ] = E ;
    // Index 26
    Production::new(NonTerminal::Statement, vec![
        TokenType::ID.into(),
        TokenType::LBRACKET.into(),
        NonTerminal::Expression.into(),
        TokenType::RBRACKET.into(),
        TokenType::EQSIGN.into(),
        NonTerminal::Expression.into(),
        TokenType::SEMICOLON.into(),
    ]),
    // S -> sidef ( E ) ;
    // Index 27
    Production::new(NonTerminal::Statement, vec![
        TokenType::SIDEF.into(),
        TokenType::LPAREN.into(),
        NonTerminal::Expression.into(),
        TokenType::RPAREN.into(),
        TokenType::SEMICOLON.into(),
    ]),

    // FIRST(S') = FIRST(S) U { epsilon } = { LBRACE, while, PRINTLN, I, sidef, epsilon }
    // FOLLOW(S') = { RBRACE, return }
    // S' -> S S'
    // Index 28
    Production::new(NonTerminal::StatementRepeat, vec![
        NonTerminal::Statement.into(),
        NonTerminal::StatementRepeat.into(),
    ]),
    // S' -> epsilon
    // Index 29
    Production::new(NonTerminal::StatementRepeat, vec![]),
];

pub const EMINIJAVA_TABLE: ProductionTable = {
    let mut table = ProductionTable::new();
};