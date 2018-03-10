use std::cell::Cell;

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