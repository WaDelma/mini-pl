use std::cell::Cell;
use std::fmt;

#[derive(Clone, PartialEq)]
pub struct Position {
    pub line: usize,
    pub column: usize,
}

impl fmt::Debug for Position {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        fmt.write_str(&format!("{}:{}", self.line, self.column))
    }
}

impl Position {
    pub fn new(line: usize, column: usize) -> Self {
        Position {
            line,
            column
        }
    }
}

#[derive(Clone, PartialEq)]
pub struct Positioned<T> {
    pub data: T,
    pub from: Position,
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