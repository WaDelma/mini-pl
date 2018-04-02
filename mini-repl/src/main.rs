//! # Repl for mini-pl
//! 
//! CLI application 
//! 
//! Supported flags:
//! 
//! | Long                 | Short | Description                |
//! | -------------------- |:-----:|:--------------------------:|
//! | --version            | -V    | Prints version information |
//! | --plain              | -p    | Don't use fancy utf-8      |
//! | --symbol <symbol>    | -s    | Change symbol before input |
//! | --color Always/Never | -c    | Determine if color is used |
//! 
//! Supported commands inside repl:
//! 
//! | Command | Description   |
//! | ------- |:-------------:|
//! | :h      | In-repl help  |
//! | :q      | Exit the repl |
//! | :c      | Clear session |

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
use termion::raw::IntoRawMode;

use mini_pl::lexer::tokenize;
use mini_pl::lexer::tokens::Token;
use mini_pl::parser::parse;
use mini_pl::analyzer::{Type, Mutability, analyze};
use mini_pl::interpreter::interpret;
use mini_pl::util::context::Context;
use mini_pl::util::Io;
use mini_pl::interpreter::repr::TypedValue;

use std::mem::replace;
use std::panic::{self, UnwindSafe, catch_unwind};
use std::io::{Write, Read, stdout, stdin};
use std::iter::repeat;
use std::str;
use std::fs::File;

use history::History;
use style::{WithColor, clear_style, error_style, note_style, highlight_style, info_style, logo_theme, welcome_theme};

type Result<T> = ::std::result::Result<T, CliError>;

mod history;
mod style;

#[derive(Debug, Fail)]
enum CliError {
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
        // AlwaysAnsi,
        // Auto,
        Never
    }
}

const REPL_COMMANDS: [(&str, &str); 3] = [
    (":h", "In-repl help"),
    (":q", "Exit the repl"),
    (":c", "Clears the code from the repl session"),
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

fn color_choice(args: &ArgMatches) -> ColorChoice {
    value_t!(args, "color", ColorChoice).unwrap_or(ColorChoice::Always)
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
    s.cwrite(highlight_style(), repl_symbol(args), color_choice(args))?;
    Ok(())
}

fn print_help<W: Write>(s: &mut W, args: &ArgMatches) -> Result<()> {
    let cc = color_choice(args);
    let hl = highlight_style();
    s.cwrite(hl, fancy_plain(args, " ❘〃", " |//"), cc)?;
    s.cwriteln(info_style(), "HELP", cc)?;
    let repl_cmd_width = REPL_COMMANDS.iter().map(|&(ref c, _)| c.width()).max().unwrap_or(0);
    for &(c, h) in REPL_COMMANDS.iter() {
        s.cwrite(hl, fancy_plain(args, " ❘", " |"), cc)?;
        s.cwrite(clear_style(), &format!("  {}", c), cc)?;
        s.cwrite(clear_style(), &repeat(" ").take(repl_cmd_width - c.width()).collect::<String>(), cc)?;
        s.cwriteln(clear_style(), &format!("  {}", h), cc)?;
    }
    Ok(())
}

fn interpret_line<'a, R: Write>(line: &str, memory: &mut Context<TypedValue>, check_ctx: &mut Context<(Type, Mutability)>, stdout: &mut R, cc: ColorChoice) -> Result<bool> {
    let new_memory = memory.clone();
    let new_check_ctx = check_ctx.clone();
    let prev_hook = panic::take_hook();
    panic::set_hook(Box::new(move |panic_info| {
        let mut out = ::stdout();
        out.cwriteln(error_style(), "Internal interpreter error:", cc).unwrap();
        if let Some(info) = panic_info.payload().downcast_ref::<String>() {
            out.cwriteln(clear_style(), &format!("{}", info), cc).unwrap();
        } else if let Some(info) = panic_info.payload().downcast_ref::<&str>() {
            out.cwriteln(clear_style(), &format!("{}", info), cc).unwrap();
        } else {
            out.cwriteln(clear_style(), "Unknown error", cc).unwrap();
        }
        // TODO: Seems useless
        // if let Some(location) = panic_info.location() {
        //     out.cwriteln(clear_style(), &format!("in {} at line {}.", location.file(), location.line())).unwrap();
        // }
    }));
    let result = {
        let mut stdio = ReplStdio {
            stdout,
            cc
        };
        match catch_unwind(move || {
            let mut memory = new_memory;
            let mut check_ctx = new_check_ctx;

            (|| -> Result<_> {
                let tokens = match tokenize(line) {
                    Ok((tokens, _, _)) => tokens,
                    Err((e, _)) => {
                        stdio.stdout.cwriteln(error_style(), "Lexing failed:", cc)?;
                        stdio.stdout.cwriteln(clear_style(), &format!("{:#?}", e), cc)?;
                        return Ok(false);
                    }
                };

                let mut errors = false;
                for err in tokens.iter().filter_map(|t| if let Token::Error(_) = t.data { Some(t) } else { None }) {
                    if !errors {
                        stdio.stdout.cwriteln(error_style(), "Lexing failed:", cc)?;
                        errors = true;
                    }
                    stdio.stdout.cwriteln(clear_style(), &format!("{:#?}", err), cc)?;
                }
                if errors {
                    return Ok(false);
                }

                let ast = match parse(&tokens[..]) {
                    Ok((ast, _, _)) => ast,
                    Err((e, _)) => {
                        stdio.stdout.cwriteln(error_style(), "Parsing failed:", cc)?;
                        stdio.stdout.cwriteln(clear_style(), &format!("{:#?}", e), cc)?;
                        return Ok(false);
                    }
                };

                let mut no_error = true;
                for e in analyze(&ast[..], &mut check_ctx) {
                    if no_error {
                        stdio.stdout.cwriteln(error_style(), "Analysis failed:", cc)?;
                        no_error = false;
                    }
                    stdio.stdout.cwriteln(clear_style(), &format!("{:#?}", e), cc)?;
                }

                if no_error {
                    interpret(&ast[..], &mut memory, &mut stdio);
                }
                Ok(no_error)
            })().map(|b| (b, memory, check_ctx))
        }) {
            Ok(r) => match r {
                Ok((b, m, c)) => {
                    if b {
                        replace(memory, m);
                        replace(check_ctx, c);
                    }
                    Ok(b)
                },
                Err(e) => Err(e),
            },
            Err(_) => Ok(false),
        }
    };
    panic::set_hook(prev_hook);
    result
}

struct ReplStdio<'a, W: Write + 'a> {
    stdout: &'a mut W,
    cc: ColorChoice,
}

impl<'a, W: Write + 'a> UnwindSafe for ReplStdio<'a, W> {}

impl<'a, W: Write + 'a> Io for ReplStdio<'a, W> {
    fn write<S: AsRef<[u8]>>(&mut self, s: &S) {
        self.stdout.cwrite(clear_style(), str::from_utf8(s.as_ref()).unwrap(), self.cc).unwrap();
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

    let args = &args;
    let cc = color_choice(args);
    if let Some(file) = args.value_of("INPUT") {
        let mut file = File::open(file)?;
        let mut code = String::new();
        file.read_to_string(&mut code)?;
        let mut memory = Context::<TypedValue>::new();
        let mut check_ctx = Context::<(Type, Mutability)>::new();
        interpret_line(&code, &mut memory, &mut check_ctx, &mut stdout(), cc)?;
        return Ok(());
    }

    let mut out = stdout().into_raw_mode()?;

    let mut history = History::new();
    let mut memory = Context::<TypedValue>::new();
    let mut check_ctx = Context::<(Type, Mutability)>::new();
    out.cwrite(logo_theme(), &format!("{}", fancy_plain(&args, "⌘", "~")), cc)?;
    out.cwriteln(welcome_theme(), &format!(" Welcome to the repl for Mini-pl {}!", crate_version!()), cc)?;
    print_repl_symbol(&mut out, args)?;
    for c in stdin().keys() {
        use self::Key::*;
        match c? {
            Char('\n') => {
                out.cwriteln(clear_style(), "", cc)?;
                if !history.current().trim().is_empty() {
                    if history.current().starts_with(":") {
                        match &history.current().trim()[1..] {
                            "q" => return Ok(()),
                            "h" => print_help(&mut out, args)?,
                            "c" => {
                                memory.clear();
                                check_ctx.clear();
                                out.cwrite(note_style(), fancy_plain(&args, "⚠", "!"), cc)?;
                                out.cwriteln(note_style(), " Cleared all code from the session", cc)?
                            },
                            _ => out.cwriteln(error_style(), "Unknown repl command", cc)?,
                        }
                    } else {
                        interpret_line(history.current(), &mut memory, &mut check_ctx, &mut out, cc)?;
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
                out.cwrite(clear_style(), &format!("{}", history.current()), cc)?;
                out.flush()?;
            },
            Up => if history.go_backwards() {
                update_pos(&mut out, 0, &args)?;
                out.cwrite(clear_style(), &format!("{}", history.current()), cc)?;
                out.flush()?;
            },
            Backspace => if history.erase().is_some() {
                update_pos(&mut out, history.current().width(), args)?;
                out.flush()?;
            },
            PageUp => {
                history.to_history();
                update_pos(&mut out, 0, &args)?;
                out.cwrite(clear_style(), &format!("{}", history.current()), cc)?;
                out.flush()?;
            },
            PageDown => {
                history.to_future();
                update_pos(&mut out, 0, &args)?;
                out.cwrite(clear_style(), &format!("{}", history.current()), cc)?;
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