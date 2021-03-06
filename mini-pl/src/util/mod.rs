//! Utilities used by rest of the interpreter

use std::cell::Cell;
use std::fmt;
use std::io::{stdout, Write};

use char_stream::CharStream;

pub mod context;

/// Line and column position
#[derive(Clone, PartialEq)]
pub struct Position {
    /// Line number
    pub line: usize,
    /// Column number
    pub column: usize,
}

impl fmt::Debug for Position {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        fmt.write_str(&format!("{}:{}", self.line, self.column))
    }
}

impl Position {
    /// Creates new position from line and column
    pub fn new(line: usize, column: usize) -> Self {
        Position {
            line,
            column
        }
    }
}

/// Wraps data that is resides between two line-column pairs
#[derive(Clone, PartialEq)]
pub struct Positioned<T> {
    /// Data that is positioned
    pub data: T,
    /// Line-column pair that after which data resides
    pub from: Position,
    /// Line-column pair that before which data resides
    pub to: Position,
}

impl<T: fmt::Debug> fmt::Debug for Positioned<T> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        // TODO: self.data should use formating args...
        write!(fmt, "{:#?} at {:?}..{:?}", self.data, self.from, self.to)
    }
}

impl<T> Positioned<T> {
    /// Creates new positioned piece of data
    pub fn new(data: T, from: Position, to: Position) -> Self {
        Positioned {
            data,
            from,
            to
        }
    }

    /// Creates different type of data with the same position
    pub fn clone_with_data<P>(&self, data: P) -> Positioned<P> {
        Positioned {
            data,
            from: self.from.clone(),
            to: self.to.clone()
        }
    }
}

/// Trait for adding `update` function for cell types
pub trait UpdateCell<T> {
    /// Updates contents of a cell by applying given function to them
    fn update<F>(&self, f: F) -> T
        where F: FnOnce(T) -> T;
}

impl<T: Copy> UpdateCell<T> for Cell<T> {
    fn update<F>(&self, f: F) -> T
        where F: FnOnce(T) -> T
    {
        let cur = self.get();
        self.set(f(cur));
        cur
    }
}

#[test]
fn cell_update() {
    let cell = Cell::new(1);
    let old = cell.update(|v| v + 1);
    assert_eq!(2, cell.get());
    assert_eq!(1, old);
}

/// Trait for abstracting console IO
pub trait Io {
    /// Writes bytes to output
    fn write<S: AsRef<[u8]>>(&mut self, s: &S);
    /// Reads string until a whitespace from input
    fn read_to_whitespace(&mut self) -> String;
}

/// Stdio type that implements `Io` trait.
pub struct Stdio;

impl Io for Stdio {
    fn write<S: AsRef<[u8]>>(&mut self, s: &S) {
        stdout().write(s.as_ref()).unwrap();
    }
    fn read_to_whitespace(&mut self) -> String {
        CharStream::from_stdin()
            .take_while(|c| !c.is_whitespace())
            .collect::<String>()
    }
}

impl<'a> Io for (&'a str, Vec<u8>, Vec<u8>) {
    fn write<S: AsRef<[u8]>>(&mut self, s: &S) {
        self.1.extend(s.as_ref());
    }

    fn read_to_whitespace(&mut self) -> String {
        self.0.chars()
            .take_while(|c| !c.is_whitespace())
            .collect::<String>()
    }
}