use std::mem::replace;

pub struct History {
    history: Vec<String>,
    cursor: usize,
    next: String,
}

impl History {
    pub fn new() -> Self {
        History {
            history: vec![],
            cursor: 0,
            next: String::new(),
        }
    }

    pub fn current(&self) -> &str {
        if self.cursor < self.history.len() {
            &self.history[self.cursor]
        } else {
            &*self.next
        }
    }

    pub fn go_backwards(&mut self) -> bool {
        if self.cursor > 0 {
            self.cursor -= 1;
            true
        } else {
            false
        }
    }

    pub fn go_forwards(&mut self) -> bool {
        if self.cursor < self.history.len() {
            self.cursor += 1;
            true
        } else {
            false
        }
    }

    pub fn to_history(&mut self) {
        self.cursor = 0;
    }

    pub fn to_future(&mut self) {
        self.cursor = self.history.len();
    }

    pub fn proceed(&mut self) {
        if self.cursor < self.history.len() {
            self.next = self.history[self.cursor].clone();
        }
        if !self.history.is_empty() {
            if self.history[self.history.len() - 1] == self.next {
                self.next = String::new();
                self.to_future();
                return;
            }
        }
        if !self.next.is_empty() {
            self.history.push(replace(&mut self.next, String::new()));
        }
        self.to_future();
    }

    pub fn write(&mut self, c: char) {
        if self.cursor < self.history.len() {
            self.next = self.history[self.cursor].clone();
            self.to_future();
            self.next.push(c);
        } else {
            self.next.push(c);
        }
    }

    pub fn erase(&mut self) -> Option<char> {
        if self.cursor < self.history.len() {
            self.next = self.history[self.cursor].clone();
            self.to_future();
            self.next.pop()
        } else {
            self.next.pop()
        }
    }

}