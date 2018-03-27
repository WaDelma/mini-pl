use std::cell::Cell;
use std::fmt;

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
    pub fn new(data: T, from: Position, to: Position) -> Self {
        Positioned {
            data,
            from,
            to
        }
    }

    pub fn clone_with_data<P>(&self, data: P) -> Positioned<P> {
        Positioned {
            data,
            from: self.from.clone(),
            to: self.to.clone()
        }
    }
}

pub trait UpdateCell<T> {
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