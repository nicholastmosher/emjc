use lexer::TokenType;
use parser::{
    NonTerminal,
    Production,
    ProductionTable,
};

// FIRST(P) = FIRST(M) = { class }
// P -> M C' $

// FIRST(M) = { class }
// FOLLOW(M) = FIRST(C') - { epsilon } = { class }
// M -> class I { public static void main ( String [ ] I ) { S } }

// FIRST(C) = { class }
// FOLLOW(C) = FIRST(C') - { epsilon } U FOLLOW(C') = { class, $ }
// C -> class I X { V' F' }

// FIRST(C') = FIRST(C) U { epsilon } = { class, epsilon }
// FOLLOW(C') = { $ }
// C' -> C C'
// C' -> epsilon

// FIRST(X) = { extends, epsilon }
// FOLLOW(X) = { LBRACE }
// X -> extends I
// X -> epsilon

// FIRST(T) = { I, boolean, String, int }
// FOLLOW(T) = { I }
// T -> I
// T -> boolean
// T -> String
// T -> int
// T -> int[]

// FIRST(V) = FIRST(T) = { I, boolean, String, int }
// FOLLOW(V) = FIRST(V') - { epsilon } U FOLLOW(V') = { I, boolean, String, int, public, RBRACE }
// V -> T I ;

// FIRST(V') = FIRST(V) U { epsilon } = { I, boolean, String, int, epsilon }
// FOLLOW(V') = FIRST(F') - { epsilon } U FOLLOW(F') = { public, RBRACE }
// V' -> V V'
// V' -> epsilon

// FIRST(F) = { public }
// FOLLOW(F) = FIRST(F') - { epsilon } U FOLLOW(F') = { public, RBRACE }
// F -> public T I ( A ) { V' S' return E ; }

// FIRST(F') = FIRST(F) U { epsilon } = { public, epsilon }
// FOLLOW(F') = { RBRACE }
// F' -> F F'
// F' -> epsilon

// FIRST(A) = FIRST(T) U { epsilon } = { I, boolean, String, int, epsilon }
// FOLLOW(A) = { RPAREN }
// A -> T I A'
// A -> epsilon

// FIRST(A') = { COMMA, epsilon }
// FOLLOW(A') = FOLLOW(A) = { RPAREN }
// A' -> , T I A'
// A' -> epsilon

// FIRST(S) = { LBRACE, while, PRINTLN, I, sidef }
// FOLLOW(S) = { RBRACE }
// S -> { S' }
// S -> while ( E ) S
// S -> System.out.println ( E )
// S -> I = E ;
// S -> I [ E ] = E ;
// S -> sidef ( E ) ;

// FIRST(S') = FIRST(S) U { epsilon } = { LBRACE, while, PRINTLN, I, sidef, epsilon }
// FOLLOW(S') = { RBRACE, return }
// S' -> S S'
// S' -> epsilon

// FIRST(E) = { new, STRINGLIT, INTLIT, true, false, I, this, !, ( }
// E -> new N
// E -> STRINGLIT EE
// E -> INTLIT EE
// E -> true EE
// E -> false EE
// E -> I EE
// E -> this EE
// E -> ! E EE
// E -> ( E ) EE

// EE is for Expression Extension
// FIRST(EE) = FIRST(Or) U { epsilon }
// EE -> Or
// EE -> epsilon

// FIRST(Or) = FIRST(AndTerm) U { || }
// Or -> || AndTerm Or
// Or -> epsilon

// FIRST(AndTerm)
// AndTerm -> LessEqualTerm And

// And -> && LessEqualTerm And
// And -> epsilon


// LessEqualTerm -> PlusTerm LessEqual
// PlusTerm -> MulTerm Plus
// MulTerm -> BracketTerm Mul
// BracketTerm -> DotTerm Bracket
// DotTerm -> E Dot








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

    // Index 30
    Production::new(NonTerminal::Expression, vec![
        TokenType::INT.into(),
        NonTerminal::ExpressionExtension.into(),
    ]),

    // Index 31
    Production::new(NonTerminal::Expression, vec![
        TokenType::STRINGLIT.into(),
        NonTerminal::ExpressionExtension.into(),
    ]),

    // Index 32
    Production::new(NonTerminal::Expression, vec![
        TokenType::TRUE.into(),
        NonTerminal::ExpressionExtension.into(),
    ]),

    // Index 33
    Production::new(NonTerminal::Expression, vec![
        TokenType::FALSE.into(),
        NonTerminal::ExpressionExtension.into(),
    ]),

    // Index 34
    Production::new(NonTerminal::Expression, vec![
        TokenType::ID.into(),
        NonTerminal::ExpressionExtension.into(),
    ]),

    // Index 35
    Production::new(NonTerminal::Expression, vec![
        TokenType::THIS.into(),
        NonTerminal::ExpressionExtension.into(),
    ]),

    // Index 36
    Production::new(NonTerminal::Expression, vec![
        TokenType::NEW.into(),
        TokenType::INT.into(),
        TokenType::LBRACKET.into(),
        NonTerminal::Expression.into(),
        TokenType::RBRACKET.into(),
        NonTerminal::ExpressionExtension.into(),
    ]),

    // Index 37
    Production::new(NonTerminal::Expression, vec![
        TokenType::BANG.into(),
        NonTerminal::ExpressionExtension.into(),
    ]),

    // Index 38
    Production::new(NonTerminal::Expression, vec![
        TokenType::BANG.into(),
        NonTerminal::Expression.into(),
        NonTerminal::ExpressionExtension.into(),
    ]),

    // Index 39
    Production::new(NonTerminal::Expression, vec![
        TokenType::LPAREN.into(),
        NonTerminal::Expression.into(),
        TokenType::RPAREN.into(),
        NonTerminal::ExpressionExtension.into(),
    ]),

    // Index 40
    Production::new(NonTerminal::ExpressionExtension, vec![
        TokenType::AND.into(),
        NonTerminal::Expression.into(),
    ]),

    // Index 41
    Production::new(NonTerminal::ExpressionExtension, vec![
        TokenType::OR.into(),
        NonTerminal::Expression.into(),
    ]),

    // Index 42
    Production::new(NonTerminal::ExpressionExtension, vec![
        TokenType::EQUALS.into(),
        NonTerminal::Expression.into(),
    ]),

    // Index 43
    Production::new(NonTerminal::ExpressionExtension, vec![
        TokenType::LESSTHAN.into(),
        NonTerminal::Expression.into(),
    ]),

    // Index 44
    Production::new(NonTerminal::ExpressionExtension, vec![
        TokenType::PLUS.into(),
        NonTerminal::Expression.into(),
    ]),

    // Index 45
    Production::new(NonTerminal::ExpressionExtension, vec![
        TokenType::MINUS.into(),
        NonTerminal::Expression.into(),
    ]),

    // Index 46
    Production::new(NonTerminal::ExpressionExtension, vec![
        TokenType::TIMES.into(),
        NonTerminal::Expression.into(),
    ]),

    // Index 47
    Production::new(NonTerminal::ExpressionExtension, vec![
        TokenType::DIV.into(),
        NonTerminal::Expression.into(),
    ]),

    // Index 48
    Production::new(NonTerminal::ExpressionExtension, vec![
        TokenType::LBRACKET.into(),
        NonTerminal::Expression.into(),
        TokenType::RBRACKET.into(),
    ]),

    // Index 49
    Production::new(NonTerminal::ExpressionExtension, vec![
        TokenType::DOT.into(),
        TokenType::LENGTH.into(),
    ]),

    // Index 50
    Production::new(NonTerminal::ExpressionExtension, vec![
        TokenType::DOT.into(),
        TokenType::ID.into(),
        TokenType::LPAREN.into(),
        NonTerminal::ExpressionList.into(),
        TokenType::RPAREN.into(),
    ]),

    // Index 51
    Production::new(NonTerminal::ExpressionExtension, vec![]),

    // Index 52
    Production::new(NonTerminal::ExpressionList, vec![
        NonTerminal::Expression.into(),
        NonTerminal::ExpressionListRepeat.into(),
    ]),

    // Index 53
    Production::new(NonTerminal::ExpressionList, vec![]),

    // Index 54
    Production::new(NonTerminal::ExpressionListRepeat, vec![
        TokenType::COMMA.into(),
        NonTerminal::Expression.into(),
        NonTerminal::ExpressionListRepeat.into(),
    ]),

    // Index 55
    Production::new(NonTerminal::ExpressionListRepeat, vec![]),
];

pub const EMINIJAVA_TABLE: ProductionTable = {
    let mut table = ProductionTable::new();
    // (P, class) => P -> M C' $
    table.insert((NonTerminal::Program, TokenType::CLASS.into()), PRODUCTIONS[0]);
    // (M, class) => M -> class I { public static void main ( String [ ] I ) { S } }
    table.insert((NonTerminal::MainClass, TokenType::CLASS.into()), PRODUCTIONS[1]);
    // (C, class) => C -> class I X { V' F' }
    table.insert((NonTerminal::Class, TokenType::CLASS.into()), PRODUCTIONS[2]);
    // (C', class) => C' -> C C'
    table.insert((NonTerminal::ClassRepeat, TokenType::CLASS.into()), PRODUCTIONS[3]);
    // (X, extends) => X -> extends I
    table.insert((NonTerminal::Extends, TokenType::EXTENDS.into()), PRODUCTIONS[5]);
    // (F, public) => F -> public T I ( A ) { V' S' return E ; }
    table.insert((NonTerminal::Function, TokenType::PUBLIC.into()), PRODUCTIONS[15]);
    // (F', public) => F' -> F F'
    table.insert((NonTerminal::FunctionRepeat, TokenType::PUBLIC.into()), PRODUCTIONS[16]);
    // (V, String) => V -> T I ;
    table.insert((NonTerminal::Variable, TokenType::STRING.into()), PRODUCTIONS[12]);
    // (V, int) => V -> T I ;
    table.insert((NonTerminal::Variable, TokenType::INT.into()), PRODUCTIONS[12]);
    // (V, boolean) => V -> T I ;
    table.insert((NonTerminal::Variable, TokenType::BOOLEAN.into()), PRODUCTIONS[12]);
    // (V', String) => V' -> V V'
    table.insert((NonTerminal::VariableRepeat, TokenType::STRING.into()), PRODUCTIONS[13]);
    // (V', int) => V' -> V V'
    table.insert((NonTerminal::VariableRepeat, TokenType::INT.into()), PRODUCTIONS[13]);
    // (V', boolean) => V' -> V V'
    table.insert((NonTerminal::VariableRepeat, TokenType::BOOLEAN.into()), PRODUCTIONS[13]);
    // (S, LBRACE) => S -> { S' }
    table.insert((NonTerminal::Statement, TokenType::LBRACE.into()), PRODUCTIONS[22]);
    // (S, while) => S -> while ( E ) S
    table.insert((NonTerminal::Statement, TokenType::WHILE.into()), PRODUCTIONS[23]);
    // (A, String) => A -> T I A'
    table.insert((NonTerminal::Argument, TokenType::STRING.into()), PRODUCTIONS[18]);
    // (A, int) => A -> T I A'
    table.insert((NonTerminal::Argument, TokenType::INT.into()), PRODUCTIONS[18]);
    // (A, boolean) => A -> T I A'
    table.insert((NonTerminal::Argument, TokenType::BOOLEAN.into()), PRODUCTIONS[18]);
    table
};