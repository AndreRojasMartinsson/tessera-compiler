use std::{
    fmt::{Debug, Display},
    sync::{LazyLock, Mutex},
};

use gxhash::HashMap;

#[derive(Clone, PartialEq, Eq, Copy, Hash, Default)]
pub struct Atom(pub(super) u32);

impl Display for Atom {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let interner = INTERNER
            .lock()
            .expect("Should have valid global interner instance");
        write!(f, "{}", interner.lookup(self))
    }
}

impl Debug for Atom {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let interner = INTERNER
            .lock()
            .expect("Should have valid global interner instance");
        write!(f, "ATOM(\"{}\")", interner.lookup(self))
    }
}

pub struct Interner {
    map: HashMap<&'static str, Atom>,
    vec: Vec<&'static str>,
    buf: String,
    full: Vec<String>,
}

impl Interner {
    pub fn with_capacity(cap: usize) -> Interner {
        let cap = cap.next_power_of_two();

        Interner {
            map: HashMap::default(),
            vec: Vec::new(),
            buf: String::with_capacity(cap),
            full: Vec::new(),
        }
    }

    pub fn intern(&mut self, name: &str) -> Atom {
        if let Some(id) = self.map.get(name) {
            return *id;
        }

        let name = unsafe { self.alloc(name) };
        let id = Atom(self.map.len() as u32);

        self.map.insert(name, id);
        self.vec.push(name);

        debug_assert!(self.lookup(&id) == name);
        debug_assert!(self.intern(name) == id);

        id
    }

    pub fn lookup(&self, id: &Atom) -> &str {
        self.vec[id.0 as usize]
    }

    unsafe fn alloc(&mut self, name: &str) -> &'static str {
        let cap = self.buf.capacity();
        if cap < self.buf.len() + name.len() {
            let new_cap = (cap.max(name.len()) + 1).next_power_of_two();
            let new_buf = String::with_capacity(new_cap);
            let old_buf = std::mem::replace(&mut self.buf, new_buf);
            self.full.push(old_buf);
        }

        let interned = {
            let start = self.buf.len();
            self.buf.push_str(name);
            &self.buf[start..]
        };

        unsafe { &*(interned as *const str) }
    }
}

pub static INTERNER: LazyLock<Mutex<Interner>> =
    LazyLock::new(|| Mutex::new(Interner::with_capacity(512)));

pub fn global_intern(name: &str) -> Atom {
    let mut interner = INTERNER.lock().expect("Should have a interner instance.");
    interner.intern(name)
}

pub fn global_lookup(id: &Atom) -> String {
    let interner = INTERNER.lock().expect("Should have a interner instance.");

    interner.lookup(id).to_string()
}

#[macro_export]
macro_rules! intern {
    ($name:expr) => {{
        let mut interner = $crate::INTERNER
            .lock()
            .expect("Should have a interner instance.");

        interner.intern($name)
    }};
}

#[macro_export]
macro_rules! lookup {
    ($id:expr) => {{
        let interner = $crate::INTERNER
            .lock()
            .expect("Should have a interner instance.");

        interner.lookup(&$id).to_string()
    }};
}

#[cfg(test)]
mod tests {

    use crate::Interner;

    #[test]
    fn test_macros() {
        let id1 = intern!("hello");
        let id2 = intern!("world");
        let id3 = intern!("hello");

        assert_eq!(id1, id3, "The same string should return the same Atom");
        assert_ne!(id1, id2, "Different strings should return different Atoms");

        assert_eq!(lookup!(id1), "hello");
        assert_eq!(lookup!(id2), "world");
        assert_eq!(lookup!(id3), "hello");
    }

    #[test]
    fn test_buffer_growth() {
        let mut interner = Interner::with_capacity(4);

        let initial_capacity = interner.buf.capacity();

        interner.intern("a".repeat(initial_capacity + 1).as_str());

        assert!(
            interner.buf.capacity() > initial_capacity,
            "Buffer capacity should grow"
        );
    }
}
