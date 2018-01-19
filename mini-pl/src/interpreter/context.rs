use std::collections::HashMap;

use Ident;

pub struct Context<T> {
    scopes: Vec<HashMap<Ident, T>>,
    #[cfg(test)]
    used_scopes: Vec<HashMap<Ident, T>>,
}

impl<T> Context<T> {
    fn new() -> Self {
        Context {
            scopes: vec![],
        }
    }

    fn create(&self, ident: Ident, value: T) -> Option<T>{
        self.scopes.last().expect("Scopeless context").insert(ident, value)
    }

    fn get(&self, ident: Ident) -> Option<&T> {
        self.scopes.iter()
            .rev()
            .filter_map(|s| s.get(&ident))
            .next()
    }

    fn get_mut(&self, ident: Ident) -> Option<&mut T> {
        self.scopes.iter()
            .rev()
            .filter_map(|s| s.get_mut(&ident))
            .next()
    }

    fn scope<F: FnOnce(&mut Context<T>) -> R, R>(&mut self, f: F) -> R {
        self.scopes.push(HashMap::new());
        let result = f(self);
        let _scope = self.scopes.pop();
        #[cfg(test)]
        self.used_scopes.push(_scope);
        result
    }
}