use std::collections::HashMap;
use std::hash::Hash;


pub struct Map<K, V> {
    committed: HashMap<K, V>,
    new: HashMap<K, V>,
}

impl<K: Eq + Hash + Clone, V: Clone> Map<K, V> {
    pub fn new() -> Self {
        Map {
            committed: HashMap::new(),
            new: HashMap::new(),
        }
    }

    pub fn insert(&mut self, key: K, value: V) {
        self.new.insert(key, value);
    }

    pub fn get(&self, key: &K) -> Option<&V> {
        self.new.get(key).or_else(|| self.committed.get(key))
    }

    pub fn commit(&mut self) {
        self.committed.extend(self.new.drain())
    }

    pub fn rollback(&mut self) {
        self.new.clear()
    }
}
