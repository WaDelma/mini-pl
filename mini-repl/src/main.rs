extern crate mini_pl;
#[macro_use]
extern crate clap;
extern crate termion;
extern crate failure;
#[macro_use]
extern crate failure_derive;
extern crate unicode_width;

use unicode_width::UnicodeWidthStr;

use clap::{Arg, App, ArgMatches};

use termion::{cursor, clear};
use termion::event::Key;
use termion::input::TermRead;
use termion::raw::{IntoRawMode, RawTerminal};

use mini_pl::lexer::tokenize;
use mini_pl::lexer::tokens::Token;
use mini_pl::parser::parse;
use mini_pl::parser::ast::{Stmt, Expr, Opnd};
use mini_pl::interpreter::interpret;
use mini_pl::interpreter::context::{Context, Io};
use mini_pl::interpreter::repr::TypedValue;
use mini_pl::util::Positioned;

use std::io::{Write, stdout, Stdout, stdin, Stdin};
use std::iter::repeat;
use std::str;

use history::History;
use style::{WithColor, clear_style, error_style, note_style, highlight_style, info_style, logo_theme, welcome_theme};

type Result<T> = ::std::result::Result<T, CliError>;

mod history;
mod style;

#[derive(Debug, Fail)]
pub enum CliError {
    #[fail(display = "IO error occured: {}", _0)]
    IoError(#[cause] ::std::io::Error)
}

impl From<::std::io::Error> for CliError {
    fn from(e: ::std::io::Error) -> Self {
        CliError::IoError(e)
    }
}

arg_enum! {
    #[derive(PartialEq, Debug, Clone, Copy)]
    enum ColorChoice {
        Always,
        AlwaysAnsi,
        Auto,
        Never
    }
}

const REPL_COMMANDS: [(&str, &str); 3] = [
    (":h", "In-repl help"),
    (":q", "Exit the repl"),
    (":c", "Clears the code from the repl session"),
    // (":t <variable>", "Tells the type of the variable"),
];

fn main() {
    let mut about = String::from(" ⌘ The repl for Mini-pl.\n\nCOMMANDS:\n");
    let repl_cmd_width = REPL_COMMANDS.iter().map(|&(ref c, _)| c.width()).max().unwrap_or(0);
    for &(c, h) in REPL_COMMANDS.iter() {
        about.push_str(&format!("    {}", c));
        about.push_str(&repeat(" ").take(repl_cmd_width - c.width()).collect::<String>());
        about.push_str(&format!("    {}\n", h));
    }
    let matches = App::new("mini-repl")
        .version(crate_version!())
        .author("Delma")
        .about(&*about)
        .arg(Arg::with_name("INPUT")
            .value_name("FILE")
            .help("Executes mini-pl file in the repl.")
            .index(1))
        .arg(Arg::with_name("plain")
            .short("p")
            .long("plain")
            .help("Determines if the repl uses plain formatting without fancy unicode symbols."))
        .arg(Arg::with_name("symbol")
            .short("s")
            .long("symbol")
            .takes_value(true)
            .help("Sets the symbol preceding the repl."))
        .arg(Arg::with_name("color")
                .short("c")
                .long("color")
                .takes_value(true)
                .possible_values(&ColorChoice::variants())
                .case_insensitive(true)
                .help("Sets the mode of coloring of the repl."))
        .get_matches();
    run(matches).unwrap();
}

fn fancy_plain<'a>(args: &ArgMatches, fancy: &'a str, plain: &'a str) -> &'a str {
    if args.is_present("plain") {
        plain
    } else {
        fancy
    }
}

fn repl_symbol<'a>(args: &'a ArgMatches) -> &'a str {
    args.value_of("symbol").unwrap_or_else(|| fancy_plain(args, "ᵯ℘⧽", "mp>"))
}

fn print_repl_symbol<W: Write>(s: &mut W, args: &ArgMatches) -> Result<()> {
    s.cwrite(highlight_style(), repl_symbol(args))?;
    Ok(())
}

fn print_help<W: Write>(s: &mut W, args: &ArgMatches) -> Result<()> {
    let hl = highlight_style();
    s.cwrite(hl, fancy_plain(args, " ❘〃", " |//"))?;
    s.cwriteln(info_style(), "HELP")?;
    let repl_cmd_width = REPL_COMMANDS.iter().map(|&(ref c, _)| c.width()).max().unwrap_or(0);
    for &(c, h) in REPL_COMMANDS.iter() {
        s.cwrite(hl, fancy_plain(args, " ❘", " |"))?;
        s.cwrite(clear_style(), &format!("  {}", c))?;
        s.cwrite(clear_style(), &repeat(" ").take(repl_cmd_width - c.width()).collect::<String>())?;
        s.cwriteln(clear_style(), &format!("  {}", h))?;
    }
    Ok(())
}

fn interpret_line(line: &str, memory: &mut Context<TypedValue>, stdio: &mut ReplStdio) -> Result<bool> {
    let tokens = match tokenize(line) {
        Ok((tokens, _, _)) => tokens,
        Err((e, _)) => {
            stdio.stdout.cwriteln(error_style(), "Lexing failed:")?;
            stdio.stdout.cwriteln(clear_style(), &format!("{:#?}", e))?;
            return Ok(false);
        }
    };

    let mut errors = false;
    for err in tokens.iter().filter_map(|t| if let Token::Error(_) = t.data { Some(t) } else { None }) {
        if !errors {
            stdio.stdout.cwriteln(error_style(), "Lexing failed:")?;
            errors = true;
        }
        stdio.stdout.cwriteln(clear_style(), &format!("{:#?}", err))?;
    }
    if errors {
        return Ok(false);
    }

    let ast = match parse(&tokens[..]) {
        Ok((ast, _, _)) => ast,
        Err((e, _)) => {
            stdio.stdout.cwriteln(error_style(), "Parsing failed:")?;
            stdio.stdout.cwriteln(clear_style(), &format!("{:#?}", e))?;
            return Ok(false);
        }
    };

    // TODO: Make this work.
    // let mut errors = false;
    // for err in find_parse_errors(ast) {
    //     if !errors {
    //         stdio.stdout.cwriteln(error_style(), "Parsing failed:")?;
    //         errors = true;
    //     }
    //     stdio.stdout.cwriteln(clear_style(), &format!("{:#?}", err))?;
    // }
    // if errors {
    //     return Ok(false);
    // }

    fn parse_error_opnd(opnd: &Positioned<Opnd>, stdio: &mut ReplStdio) -> Result<bool> {
        use self::Opnd::*;
        match opnd.data {
            OpndErr(_) => {
                stdio.stdout.cwriteln(clear_style(), &format!("{:#?}", opnd))?;
                Ok(true)
            }
            Int(_) | StrLit(_) | Ident(_) => Ok(false),
            Expr(ref expr) => parse_error_expr(&*expr, stdio),
        }
    }

    fn parse_error_expr(expr: &Positioned<Expr>, stdio: &mut ReplStdio) -> Result<bool> {
        use self::Expr::*;
        match expr.data {
            ErrExpr(_) => {
                stdio.stdout.cwriteln(clear_style(), &format!("{:#?}", expr))?;
                Ok(true)
            },
            BinOper {
                ref lhs,
                ref rhs,
                ..
            } => Ok(
                parse_error_opnd(lhs, stdio)? |
                parse_error_opnd(rhs, stdio)?
            ),
            UnaOper {
                ref rhs,
                ..
            } => parse_error_opnd(rhs, stdio),
            Opnd(ref opnd) => parse_error_opnd(opnd, stdio),
        }
    }

    fn parse_error_stmts(stmts: &[Positioned<Stmt>], stdio: &mut ReplStdio) -> Result<bool> {
        use self::Stmt::*;
        let mut error = false;
        for stmt in stmts {
            match stmt.data {
                ErrStmt(_) => {
                    stdio.stdout.cwriteln(clear_style(), &format!("{:#?}", stmt))?;
                    error = true;
                },
                Declaration {
                    value: ref e,
                    ..
                } => if let Some(ref e) = *e {
                    error |= parse_error_expr(e, stdio)?;
                },
                Assignment {
                    ref value,
                    ..
                } => {
                    error |= parse_error_expr(value, stdio)?;
                },
                Loop {
                    ref from,
                    ref to,
                    ref stmts,
                    ..
                } => {
                    error |= parse_error_expr(from, stdio)?;
                    error |= parse_error_expr(to, stdio)?;
                    error |= parse_error_stmts(&stmts[..], stdio)?;
                },
                Read { .. } => {},
                Print {
                    ref expr
                } => {
                    error |= parse_error_expr(expr, stdio)?;
                },
                Assert {
                    ref expr
                } => {
                    error |= parse_error_expr(expr, stdio)?;
                },
            }
        }
        Ok(error)
    }
    
    if parse_error_stmts(&ast[..], stdio)? {
        return Ok(false);
    }

    interpret(&ast[..], memory, stdio);
    Ok(true)
}

struct ReplStdio<'a> {
    stdout: &'a mut RawTerminal<Stdout>,
}

impl<'a> Io for ReplStdio<'a> {
    fn write<S: AsRef<[u8]>>(&mut self, s: &S) {
        self.stdout.cwrite(clear_style(), str::from_utf8(s.as_ref()).unwrap()).unwrap();
    }
    fn read_to_whitespace(&mut self) -> String {
        let mut result = String::new();
        for c in stdin().keys() {
            use self::Key::*;
            match c.unwrap() {
                Char('\n') => break,
                Char(c) => result.push(c),
                _ => {}
            }
        }
        result
    }
}

fn run(args: ArgMatches) -> Result<()> {
    let mut out = stdout().into_raw_mode()?;

    let mut history = History::new();
    let mut memory = Context::<TypedValue>::new();

    out.cwrite(logo_theme(), &format!("{}", fancy_plain(&args, "⌘", "~")))?;
    out.cwriteln(welcome_theme(), &format!(" Welcome to the repl for Mini-pl {}!", crate_version!()))?;
    print_repl_symbol(&mut out, &args)?;
    for c in stdin().keys() {
        use self::Key::*;
        match c? {
            Char('\n') => {
                out.cwriteln(clear_style(), "")?;
                if !history.current().trim().is_empty() {
                    if history.current().starts_with(":") {
                        match &history.current().trim()[1..] {
                            "q" => return Ok(()),
                            "h" => print_help(&mut out, &args)?,
                            "c" => {
                                memory.clear();
                                out.cwrite(note_style(), fancy_plain(&args, "⚠", "!"))?;
                                out.cwriteln(note_style(), " Cleared all code from the session")?
                            },
                            _ => out.cwriteln(error_style(), "Unknown repl command")?,
                        }
                    } else {
                        interpret_line(history.current(), &mut memory, &mut ReplStdio {
                            stdout: &mut out,
                        })?;
                    }
                    history.proceed();
                }
                print_repl_symbol(&mut out, &args)?;
            },
            Char(c) => {
                history.write(c);
                write!(out, "{}", c)?;
                out.flush()?;
            },
            Down => if history.go_forwards() {
                update_pos(&mut out, 0, &args)?;
                out.cwrite(clear_style(), &format!("{}", history.current()))?;
                out.flush()?;
            },
            Up => if history.go_backwards() {
                update_pos(&mut out, 0, &args)?;
                out.cwrite(clear_style(), &format!("{}", history.current()))?;
                out.flush()?;
            },
            Backspace => if history.erase().is_some() {
                update_pos(&mut out, history.current().width(), &args)?;
                out.flush()?;
            },
            PageUp => {
                history.to_history();
                update_pos(&mut out, 0, &args)?;
                out.cwrite(clear_style(), &format!("{}", history.current()))?;
                out.flush()?;
            },
            PageDown => {
                history.to_future();
                update_pos(&mut out, 0, &args)?;
                out.cwrite(clear_style(), &format!("{}", history.current()))?;
                out.flush()?;
            },
            Esc => break,
            Ctrl('c') => break,
            // TODO: Implement pasting: Ctrl('v') => ,
            _ => {},
        }
    }
    Ok(())
}

fn update_pos<W: Write>(out: &mut W, offset: usize, args: &ArgMatches) -> Result<()> {
    let place = repl_symbol(&args).width() + offset;
    write!(out, "{}{}{}", cursor::Left(!0), cursor::Right(place as u16), clear::AfterCursor)?;
    Ok(())
}