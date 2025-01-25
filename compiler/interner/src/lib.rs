pub use interner::{Atom, INTERNER, Interner, global_intern, global_lookup};

mod interner;

#[cfg(test)]
mod tests {
    use interner::Atom;

    use super::*;

    #[test]
    fn test_intern_basic() {
        let mut interner = Interner::with_capacity(16);

        let id1 = interner.intern("hello");
        let id2 = interner.intern("world");
        let id3 = interner.intern("hello"); // Same as id1

        assert_eq!(interner.lookup(&id1), "hello");
        assert_eq!(interner.lookup(&id2), "world");
        assert_eq!(
            id1, id3,
            "Re-interning the same string should return the same ID"
        );
    }

    #[test]
    fn test_intern_empty_string() {
        let mut interner = Interner::with_capacity(16);

        let id = interner.intern("");
        assert_eq!(interner.lookup(&id), "");
    }

    #[test]
    fn test_intern_large_string() {
        let mut interner = Interner::with_capacity(16);

        let large_string = "a".repeat(20_000);
        let id = interner.intern(&large_string);

        assert_eq!(interner.lookup(&id), large_string);
    }

    #[test]
    fn test_intern_duplicate_strings() {
        let mut interner = Interner::with_capacity(16);

        let id1 = interner.intern("duplicate");
        let id2 = interner.intern("duplicate");

        assert_eq!(id1, id2, "Duplicate strings should have the same ID");
        assert_eq!(interner.lookup(&id1), "duplicate");
    }

    #[test]
    fn test_intern_strings_with_capacity_expansion() {
        let mut interner = Interner::with_capacity(4);

        let strings = ["a", "b", "c", "d", "e"];
        let ids: Vec<Atom> = strings.iter().map(|&s| interner.intern(s)).collect();

        for (i, &id) in ids.iter().enumerate() {
            assert_eq!(interner.lookup(&id), strings[i]);
        }
    }

    #[test]
    fn test_lookup_invalid_id() {
        let interner = Interner::with_capacity(16);

        let invalid_id = Atom(9999);
        let result = std::panic::catch_unwind(|| {
            interner.lookup(&invalid_id);
        });

        assert!(result.is_err(), "Lookup with an invalid ID should panic");
    }

    #[test]
    fn test_intern_unicode_strings() {
        let mut interner = Interner::with_capacity(16);

        let id1 = interner.intern("こんにちは"); // Japanese
        let id2 = interner.intern("你好"); // Chinese
        let id3 = interner.intern("안녕하세요"); // Korean

        assert_eq!(interner.lookup(&id1), "こんにちは");
        assert_eq!(interner.lookup(&id2), "你好");
        assert_eq!(interner.lookup(&id3), "안녕하세요");
    }

    #[test]
    fn test_intern_long_repeated_strings() {
        let mut interner = Interner::with_capacity(16);

        let repeated_string = "x".repeat(1_000);
        let id1 = interner.intern(&repeated_string);
        let id2 = interner.intern(&repeated_string);

        assert_eq!(
            id1, id2,
            "Re-interning the same long string should return the same ID"
        );
        assert_eq!(interner.lookup(&id1), repeated_string);
    }

    #[test]
    fn test_edge_case_no_capacity() {
        let mut interner = Interner::with_capacity(0);

        let id = interner.intern("test");
        assert_eq!(interner.lookup(&id), "test");
    }
}
