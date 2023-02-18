use crate::{eval::Val, token::Token};
use fxhash::FxHashMap;
use std::cell::RefCell;
use std::rc::Rc;

#[derive(Debug)]
pub struct Environment {
    enclosing: Option<Rc<RefCell<Environment>>>,
    values: FxHashMap<String, Val>,
}

impl Environment {
    pub fn global() -> Environment {
        Environment {
            enclosing: None,
            values: FxHashMap::default(),
        }
    }

    pub fn new(enclosing: &Rc<RefCell<Environment>>) -> Environment {
        Environment {
            enclosing: Some(Rc::clone(enclosing)),
            values: FxHashMap::default(),
        }
    }

    pub fn define(&mut self, name: String, v: Val) {
        self.values.insert(name, v);
    }

    pub fn get(&self, name: &Token) -> Val {
        match self.values.get(&name.lexeme) {
            Some(v) => v.clone(),
            None => match &self.enclosing {
                Some(env) => env.borrow().get(name),
                None => panic!("Runtime Error: Undefined variable {name}"),
            },
        }
    }

    pub fn get_at(&self, distance: usize, name: &str) -> Val {
        if distance == 0 {
            self.values.get(name).unwrap().clone()
        } else {
            self.ancestor(distance)
                .borrow()
                .values
                .get(name)
                .unwrap()
                .clone()
        }
    }

    pub fn assign(&mut self, name: &Token, v: &Val) {
        if self.values.contains_key(&name.lexeme) {
            self.values.insert(name.lexeme.clone(), v.clone());
        } else {
            match &self.enclosing {
                Some(env) => env.borrow_mut().assign(name, v),
                None => panic!("Undefined variable {}", name.lexeme),
            }
        }
    }

    pub fn assign_at(&mut self, distance: usize, name: &Token, value: &Val) {
        if distance == 0 {
            self.values.insert(name.lexeme.clone(), value.clone());
        } else {
            self.ancestor(distance)
                .borrow_mut()
                .values
                .insert(name.lexeme.clone(), value.clone());
        }
    }

    fn ancestor(&self, distance: usize) -> Rc<RefCell<Environment>> {
        if distance == 1 {
            Rc::clone(self.enclosing.as_ref().unwrap())
        } else {
            self.enclosing
                .as_ref()
                .unwrap()
                .borrow()
                .ancestor(distance - 1)
        }
    }
}
