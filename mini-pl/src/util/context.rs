//! Context that works as symbol table for the interpreter.

use std::collections::HashMap;

use Ident;

/// Context that contains variables and their corresponding data
#[derive(Clone)]
pub struct Context<T> {
    scopes: Vec<HashMap<Ident, T>>,
    #[cfg(test)]
    used_scopes: Vec<HashMap<Ident, T>>,
}

impl<T> Context<T> {
    /// Creats new empty context
    pub fn new() -> Self {
        Context {
            scopes: vec![HashMap::new()],
            #[cfg(test)]
            used_scopes: vec![],
        }
    }

    /// Creates new identifier with specified value in context
    /// 
    /// Returns overwritten value if any.
    pub fn create(&mut self, ident: Ident, value: T) -> Option<T> {
        self.scopes.last_mut()
            .unwrap()
            .insert(ident, value)
    }

    /// Gets value associated with identifier
    pub fn get(&self, ident: &Ident) -> Option<&T> {
        self.scopes.iter()
            .rev()
            .filter_map(|s| s.get(ident))
            .next()
    }

    /// Mutably gets value associated with identifier
    pub fn get_mut(&mut self, ident: &Ident) -> Option<&mut T> {
        self.scopes.iter_mut()
            .rev()
            .filter_map(|s| s.get_mut(ident))
            .next()
    }

    /// Executes code in inner scope
    pub fn scope<F: FnOnce(&mut Context<T>) -> R, R>(&mut self, f: F) -> R {
        self.scopes.push(HashMap::new());
        let result = f(self);
        let _scope = self.scopes.pop();
        #[cfg(test)]
        self.used_scopes.push(_scope.expect("There should be at least one scope."));
        result
    }
    
    /// Clears context
    pub fn clear(&mut self) {
        self.scopes.clear();
    }
}