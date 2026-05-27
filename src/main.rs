use ariadne::{Color, Label, Report, ReportKind, Source};
use chumsky::{
    input::{Stream, ValueInput},
    pratt::*,
    prelude::*,
};
use logos::{Filter, Lexer, Logos};
use std::fs;

#[derive(Default, Clone, Copy)]
struct LexerExtra {
    line_start: bool,
}

#[derive(Logos, Clone, PartialEq, logos_display::Display, Debug)]
#[logos(extras = LexerExtra)]
enum Token<'a> {
    #[display_override("<error>")]
    Error,

    #[regex(r"[A-Za-z_]\w*", |lex| { input_callback(lex); lex.slice() })]
    #[display_override("<identifier>")]
    Ident(&'a str),

    #[regex(r"\d+", |lex| { input_callback(lex); lex.slice().parse::<u32>().unwrap() })]
    #[display_override("<number>")]
    Number(u32),

    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Star,
    #[token("/")]
    Slash,

    #[token(":", input_callback)]
    Colon,
    #[token(".", input_callback)]
    Dot,
    #[token(",", input_callback)]
    Comma,
    #[token("(", input_callback)]
    LParen,
    #[token(")", input_callback)]
    RParen,

    #[token("\n", |lex| lex.extras.line_start = true)]
    #[display_override("<newline>")]
    Newline,
    #[regex(r"[ \t]", space_callback)]
    Indent,

    #[regex(r";[^\n]*", logos::skip, allow_greedy = true)]
    Comment,
    // #[regex(r"[ \t\f]+", logos::skip)]
    // #[display_override("<whitespace>")]
    // Whitespace,
}

fn input_callback<'a>(lex: &mut Lexer<'a, Token<'a>>) -> bool {
    lex.extras.line_start = false;
    true
}

fn space_callback<'a>(lex: &mut Lexer<'a, Token<'a>>) -> Filter<Token<'a>> {
    if lex.span().start == 0 || lex.extras.line_start {
        input_callback(lex);
        Filter::Emit(Token::Indent)
    } else {
        Filter::Skip
    }
}

#[derive(Debug)]
enum Command<'a> {
    Instruction(&'a str, Vec<Operand<'a>>),
    Label(&'a str),
    Directive(&'a str, Vec<Expression<'a>>),
}

#[derive(Debug)]
enum Operand<'a> {
    Indirection(Expression<'a>),
    Value(Expression<'a>),
}

#[derive(Debug)]
enum Expression<'a> {
    Integer(u32),
    Identifier(&'a str),
    Add(Box<Expression<'a>>, Box<Expression<'a>>),
    Sub(Box<Expression<'a>>, Box<Expression<'a>>),
    Mul(Box<Expression<'a>>, Box<Expression<'a>>),
    Div(Box<Expression<'a>>, Box<Expression<'a>>),
}

// This function signature looks complicated, but don't fear! We're just saying that this function is generic over
// inputs that:
//     - Can have tokens pulled out of them by-value, by cloning (`ValueInput`)
//     - Produces tokens of type `Token`, the type we defined above (`Token = Token<'a>`)
//     - Produces spans of type `SimpleSpan`, a built-in span type provided by chumsky (`Span = SimpleSpan`)
// The function then returns a parser that:
//     - Has an input type of type `I`, the one we declared as a type parameter
//     - Produces an `SExpr` as its output
//     - Uses `Rich`, a built-in error type provided by chumsky, for error generation
fn parser<'tok, 'src: 'tok, I>()
-> impl Parser<'tok, I, Vec<Command<'tok>>, extra::Err<Rich<'tok, Token<'src>>>>
where
    I: ValueInput<'tok, Token = Token<'src>, Span = SimpleSpan>,
{
    let newline = just(Token::Newline);
    let indent = just(Token::Indent);

    let atom = select! {
        Token::Number(num) => Expression::Integer(num),
        Token::Ident(ident) => Expression::Identifier(ident),
    };

    let expression = atom.pratt((
        infix(left(2), just(Token::Star), |l, _, r, _| {
            Expression::Mul(Box::new(l), Box::new(r))
        }),
        infix(left(2), just(Token::Slash), |l, _, r, _| {
            Expression::Div(Box::new(l), Box::new(r))
        }),
        infix(left(1), just(Token::Plus), |l, _, r, _| {
            Expression::Add(Box::new(l), Box::new(r))
        }),
        infix(left(1), just(Token::Minus), |l, _, r, _| {
            Expression::Sub(Box::new(l), Box::new(r))
        }),
    ));

    let operand = choice((
        expression.clone().map(Operand::Value),
        expression
            .clone()
            .delimited_by(just(Token::LParen), just(Token::RParen))
            .map(Operand::Indirection),
    ));

    let directive = just(Token::Dot)
        .ignore_then(select! {
            Token::Ident(ident) => ident
        })
        .then(expression.separated_by(just(Token::Comma)).collect())
        .map(|(ident, expressions)| Command::Directive(ident, expressions));

    let instruction = select! {
        Token::Ident(ident) => ident
    }
    .then(operand.separated_by(just(Token::Comma)).collect())
    .map(|(ident, operands)| Command::Instruction(ident, operands));

    let label = select! {
        Token::Ident(ident) => Command::Label(ident)
    }
    .then_ignore(just(Token::Colon).or_not());

    let line = label
        .map(Some)
        .or(indent.map(|_| None))
        .then(instruction.or(directive).or_not())
        .map(|(a, b)| [a, b]);

    // let directory =
    //     just(Token::Dot).ignore_then(select! { Token::Ident(ident) => Command::Directory(ident) });

    // choice((label, instruction, indent.map(|_| Command::Label("yo"))))
    // line.separated_by(newline)
    //     .allow_leading()
    //     .allow_trailing()
    //     .collect::<Vec<(Option<Command>, Option<Command>)>>()
    //     .map(|v| {
    //         v.into_iter()
    //             .flat_map(|(a, b)| a.into_iter().chain(b))
    //             .collect()
    //     })
    line.or_not()
        .separated_by(newline)
        // .allow_leading()
        // .allow_trailing()
        .flatten()
        .flatten()
        .flatten()
        .collect()
}

fn assembler(commands: &[Command]) -> Vec<u8> {
    commands
        .iter()
        .filter_map(|command| {
            if let Command::Instruction(name, operands) = command {
                Some((name, operands))
            } else {
                None
            }
        })
        .map::<&'static [u8], _>(|(name, operands)| match (*name, operands.as_slice()) {
            (
                "adc",
                [
                    Operand::Value(Expression::Identifier("A")),
                    Operand::Indirection(Expression::Identifier("HL")),
                ],
            ) => &[0x25, 0x50],
            _ => &[],
        })
        .flatten()
        .map(|a| *a)
        .collect()
}

fn main() {
    let filename = "src/foo.z80";
    let src = &fs::read_to_string(filename).unwrap();

    // Create a logos lexer over the source code
    let token_iter = Token::lexer(src)
        .spanned()
        // Convert logos errors into tokens. We want parsing to be recoverable and not fail at the lexing stage, so
        // we have a dedicated `Token::Error` variant that represents a token error that was previously encountered
        .map(|(tok, span)| match tok {
            // Turn the `Range<usize>` spans logos gives us into chumsky's `SimpleSpan` via `Into`, because it's easier
            // to work with
            Ok(tok) => (tok, span.into()),
            Err(()) => (Token::Error, span.into()),
        });

    for (token, _) in token_iter.clone() {
        println!("{:?}", token);
    }

    // Turn the token iterator into a stream that chumsky can use for things like backtracking
    let token_stream = Stream::from_iter(token_iter)
        // Tell chumsky to split the (Token, SimpleSpan) stream into its parts so that it can handle the spans for us
        // This involves giving chumsky an 'end of input' span: we just use a zero-width span at the end of the string
        .map((0..src.len()).into(), |(t, s)| (t, s));

    // Parse the token stream with our chumsky parser
    let result = parser().parse(token_stream);
    match result.output() {
        Some(sexpr) => {
            println!("Result = {sexpr:#?}");
            println!("{:?}", assembler(sexpr));
        }
        None => println!("Parsing error"),
    }

    for err in result.errors() {
        Report::build(ReportKind::Error, (filename, err.span().into_range()))
            // .with_config(ariadne::Config::new())
            // .with_code(3)
            .with_message(err.to_string())
            .with_label(
                Label::new((filename, err.span().into_range()))
                    .with_message(err.reason().to_string())
                    .with_color(Color::Red),
            )
            .finish()
            .eprint((filename, Source::from(src)))
            .unwrap();
    }
}
