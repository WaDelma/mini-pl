use std::collections::HashMap;
use std::convert::AsRef;


use Ident;

#[derive(Clone)]
pub struct Context<T> {
    scopes: Vec<HashMap<Ident, (T, bool)>>,
    #[cfg(test)]
    used_scopes: Vec<HashMap<Ident, (T, bool)>>,
}

impl<T> Context<T> {
    pub fn new() -> Self {
        Context {
            scopes: vec![HashMap::new()],
            #[cfg(test)]
            used_scopes: vec![],
        }
    }

    pub fn create(&mut self, ident: Ident, value: T) -> Option<T> {
        self.scopes.last_mut()
            .unwrap()
            .insert(ident, (value, false))
            .map(|(v, _)| v)
    }

    pub fn get(&self, ident: &Ident) -> Option<&T> {
        self.scopes.iter()
            .rev()
            .filter_map(|s| s.get(ident))
            .next()
            .map(|&(ref v, _)| v)
    }

    pub fn get_mut(&mut self, ident: &Ident) -> Option<&mut T> {
        self.scopes.iter_mut()
            .rev()
            .filter_map(|s| s.get_mut(ident))
            .map(|&mut (ref mut s, f)| if f {
                panic!("Cannot mutate value of frozen identifier.");
            } else {
                s
            })
            .next()
    }

    pub fn freeze(&mut self, ident: &Ident) -> Option<bool> {
        self.scopes.iter_mut()
            .rev()
            .filter_map(|s| s.get_mut(ident))
            .next()
            .map(|&mut (_, ref mut t)| {
                let orig = *t;
                *t = true;
                orig == true
            })
    }

    pub fn thaw(&mut self, ident: &Ident) -> Option<bool> {
        self.scopes.iter_mut()
            .rev()
            .filter_map(|s| s.get_mut(ident))
            .next()
            .map(|&mut (_, ref mut t)| {
                let orig = *t;
                *t = false;
                orig == false
            })
    }

    pub fn scope<F: FnOnce(&mut Context<T>) -> R, R>(&mut self, f: F) -> R {
        self.scopes.push(HashMap::new());
        let result = f(self);
        let _scope = self.scopes.pop();
        #[cfg(test)]
        self.used_scopes.push(_scope.expect("There should be at least one scope."));
        result
    }
    
    pub fn clear(&mut self) {
        self.scopes.clear();
    }
}