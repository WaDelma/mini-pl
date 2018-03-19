use termion::color as col;
use termion::style as sty;
use termion::cursor as cur;

use std::io::Write;
use std::fmt;

use super::Result;

#[derive(Clone, Copy)]
pub struct Style<C: col::Color, B: col::Color, S> {
    fg: col::Fg<C>,
    bg: col::Bg<B>,
    style: S,
}

impl<C: col::Color, B: col::Color, S> Style<C, B, S> {
    pub fn new(c: C, b: B, s: S) -> Self {
        Style {
            fg: col::Fg(c),
            bg: col::Bg(b),
            style: s,
        }
    }
}

impl<C: col::Color, B: col::Color, S: fmt::Display> fmt::Display for Style<C, B, S> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}{}{}", self.style, self.fg, self.bg)
    }
}

pub trait WithColor {
    fn cwrite<C: col::Color, B: col::Color, S: fmt::Display>(&mut self, c: Style<C, B, S>, s: &str) -> Result<()>;
    fn cwriteln<C: col::Color, B: col::Color, S: fmt::Display>(&mut self, c: Style<C, B, S>, s: &str) -> Result<()>;
}

impl<W: Write> WithColor for W {
    fn cwrite<C: col::Color, B: col::Color, S: fmt::Display>(&mut self, c: Style<C, B, S>, s: &str) -> Result<()> {
        write!(self, "{}", c)?;
        let result = self.write(s.replace("\n", &format!("\n{}", cur::Left(!0))).as_ref());
        write!(self, "{}", clear_style())?;
        self.flush()?;
        result?;
        Ok(())
    }

    fn cwriteln<C: col::Color, B: col::Color, S: fmt::Display>(&mut self, c: Style<C, B, S>, s: &str) -> Result<()> {
        write!(self, "{}", c)?;
        let result = self.write(s.replace("\n", &format!("\n{}", cur::Left(!0))).as_ref());
        write!(self, "{}", clear_style())?;
        write!(self, "\n{}", cur::Left(!0))?;
        self.flush()?;
        result?;
        Ok(())
    }
}

pub fn clear_style() -> Style<col::Reset, col::Reset, sty::Reset> {
    Style::new(col::Reset, col::Reset, sty::Reset)
}

pub fn error_style() -> Style<col::Red, col::Reset, sty::Bold> {
    Style::new(col::Red, col::Reset, sty::Bold)
}

pub fn note_style() -> Style<col::Yellow, col::Reset, sty::Reset> {
    Style::new(col::Yellow, col::Reset, sty::Reset)
}

pub fn highlight_style() -> Style<col::Green, col::Reset, sty::Reset> {
    Style::new(col::Green, col::Reset, sty::Reset)
}

pub fn info_style() -> Style<col::LightBlue, col::Reset, sty::Reset> {
    Style::new(col::LightBlue, col::Reset, sty::Reset)
}

pub fn logo_theme() -> Style<col::LightBlue, col::Reset, sty::Bold> {
    Style::new(col::LightBlue, col::Reset, sty::Bold)
}

pub fn welcome_theme() -> Style<col::LightGreen, col::Reset, sty::Bold> {
    Style::new(col::LightGreen, col::Reset, sty::Bold)
}