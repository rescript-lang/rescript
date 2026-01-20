//! Marshal trait and implementations for basic types
//!
//! This module defines the `Marshal` trait that types implement to serialize
//! themselves to the OCaml Marshal format.

use super::marshal::MarshalWriter;

/// Trait for types that can be serialized to OCaml Marshal format
pub trait Marshal {
    /// Serialize this value to the Marshal writer
    fn marshal(&self, w: &mut MarshalWriter);
}

// ========== Primitive type implementations ==========

impl Marshal for bool {
    /// Booleans are encoded as integers: false = 0, true = 1
    fn marshal(&self, w: &mut MarshalWriter) {
        w.write_int(if *self { 1 } else { 0 });
    }
}

impl Marshal for i32 {
    fn marshal(&self, w: &mut MarshalWriter) {
        w.write_int(*self as i64);
    }
}

impl Marshal for i64 {
    fn marshal(&self, w: &mut MarshalWriter) {
        w.write_int(*self);
    }
}

impl Marshal for usize {
    fn marshal(&self, w: &mut MarshalWriter) {
        w.write_int(*self as i64);
    }
}

impl Marshal for f64 {
    fn marshal(&self, w: &mut MarshalWriter) {
        w.write_double(*self);
    }
}

impl Marshal for String {
    /// Strings are NOT shared by content because OCaml doesn't share
    /// most string objects - each parse creates a new string.
    /// Filenames in positions use separate sharing logic.
    fn marshal(&self, w: &mut MarshalWriter) {
        w.write_str(self);
    }
}

impl Marshal for str {
    /// Strings are NOT shared by content.
    fn marshal(&self, w: &mut MarshalWriter) {
        w.write_str(self);
    }
}

impl Marshal for [u8] {
    fn marshal(&self, w: &mut MarshalWriter) {
        w.write_string(self);
    }
}

// ========== Unit type ==========

impl Marshal for () {
    /// Unit () is encoded as integer 0
    fn marshal(&self, w: &mut MarshalWriter) {
        w.write_int(0);
    }
}

// ========== Option type ==========

impl<T: Marshal> Marshal for Option<T> {
    /// Options are encoded as:
    /// - None → integer 0 (constant constructor, index 0)
    /// - Some(v) → Block(tag=0, [v]) (non-constant constructor, tag 0)
    fn marshal(&self, w: &mut MarshalWriter) {
        match self {
            None => w.write_int(0),
            Some(v) => {
                w.write_block_header(0, 1);
                v.marshal(w);
            }
        }
    }
}

// ========== Vec/List type ==========

impl<T: Marshal> Marshal for Vec<T> {
    /// Lists are encoded as linked cons cells:
    /// - [] → integer 0
    /// - [a; b; c] → Block(tag=0, [a, Block(tag=0, [b, Block(tag=0, [c, 0])])])
    fn marshal(&self, w: &mut MarshalWriter) {
        if self.is_empty() {
            w.write_int(0); // Empty list = int 0
        } else {
            // Build cons cells from head to tail
            for (i, item) in self.iter().enumerate() {
                // Cons cell: tag 0, size 2
                w.write_block_header(0, 2);
                item.marshal(w);
                // If this is the last element, write nil; otherwise continue
                if i == self.len() - 1 {
                    w.write_int(0); // Nil
                }
            }
        }
    }
}

impl<T: Marshal> Marshal for [T] {
    fn marshal(&self, w: &mut MarshalWriter) {
        if self.is_empty() {
            w.write_int(0);
        } else {
            for (i, item) in self.iter().enumerate() {
                w.write_block_header(0, 2);
                item.marshal(w);
                if i == self.len() - 1 {
                    w.write_int(0);
                }
            }
        }
    }
}

// ========== Box type ==========

impl<T: Marshal> Marshal for Box<T> {
    fn marshal(&self, w: &mut MarshalWriter) {
        (**self).marshal(w);
    }
}

// ========== Tuple implementations ==========

impl<A: Marshal, B: Marshal> Marshal for (A, B) {
    /// Tuples are blocks with tag 0
    fn marshal(&self, w: &mut MarshalWriter) {
        w.write_block_header(0, 2);
        self.0.marshal(w);
        self.1.marshal(w);
    }
}

impl<A: Marshal, B: Marshal, C: Marshal> Marshal for (A, B, C) {
    fn marshal(&self, w: &mut MarshalWriter) {
        w.write_block_header(0, 3);
        self.0.marshal(w);
        self.1.marshal(w);
        self.2.marshal(w);
    }
}

impl<A: Marshal, B: Marshal, C: Marshal, D: Marshal> Marshal for (A, B, C, D) {
    fn marshal(&self, w: &mut MarshalWriter) {
        w.write_block_header(0, 4);
        self.0.marshal(w);
        self.1.marshal(w);
        self.2.marshal(w);
        self.3.marshal(w);
    }
}

impl<A: Marshal, B: Marshal, C: Marshal, D: Marshal, E: Marshal> Marshal for (A, B, C, D, E) {
    fn marshal(&self, w: &mut MarshalWriter) {
        w.write_block_header(0, 5);
        self.0.marshal(w);
        self.1.marshal(w);
        self.2.marshal(w);
        self.3.marshal(w);
        self.4.marshal(w);
    }
}

// ========== Character type ==========

/// Marshal a character (used for Pconst_char)
///
/// OCaml characters are integers 0-255.
/// In ReScript, we typically have Unicode code points.
impl Marshal for char {
    fn marshal(&self, w: &mut MarshalWriter) {
        w.write_int(*self as i64);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_bool_false() {
        let mut w = MarshalWriter::new();
        false.marshal(&mut w);
        assert_eq!(w.payload(), &[0x40]); // int 0
    }

    #[test]
    fn test_bool_true() {
        let mut w = MarshalWriter::new();
        true.marshal(&mut w);
        assert_eq!(w.payload(), &[0x41]); // int 1
    }

    #[test]
    fn test_option_none() {
        let mut w = MarshalWriter::new();
        let opt: Option<i32> = None;
        opt.marshal(&mut w);
        assert_eq!(w.payload(), &[0x40]); // int 0
    }

    #[test]
    fn test_option_some() {
        let mut w = MarshalWriter::new();
        let opt: Option<i32> = Some(42);
        opt.marshal(&mut w);
        // Block(tag=0, size=1), then int 42
        assert_eq!(w.payload(), &[0x90, 0x6A]); // small block + small int
    }

    #[test]
    fn test_empty_list() {
        let mut w = MarshalWriter::new();
        let v: Vec<i32> = vec![];
        v.marshal(&mut w);
        assert_eq!(w.payload(), &[0x40]); // int 0
    }

    #[test]
    fn test_single_element_list() {
        let mut w = MarshalWriter::new();
        let v: Vec<i32> = vec![42];
        v.marshal(&mut w);
        // Cons(42, nil): Block(tag=0, size=2), 42, 0
        assert_eq!(w.payload(), &[0xA0, 0x6A, 0x40]);
    }

    #[test]
    fn test_multi_element_list() {
        let mut w = MarshalWriter::new();
        let v: Vec<i32> = vec![1, 2, 3];
        v.marshal(&mut w);
        // Cons(1, Cons(2, Cons(3, nil)))
        // Block(0,2), 1, Block(0,2), 2, Block(0,2), 3, 0
        assert_eq!(
            w.payload(),
            &[0xA0, 0x41, 0xA0, 0x42, 0xA0, 0x43, 0x40]
        );
    }

    #[test]
    fn test_tuple_2() {
        let mut w = MarshalWriter::new();
        let t: (i32, i32) = (1, 2);
        t.marshal(&mut w);
        // Block(tag=0, size=2), 1, 2
        assert_eq!(w.payload(), &[0xA0, 0x41, 0x42]);
    }

    #[test]
    fn test_tuple_3() {
        let mut w = MarshalWriter::new();
        let t: (i32, i32, i32) = (1, 2, 3);
        t.marshal(&mut w);
        // Block(tag=0, size=3), 1, 2, 3
        assert_eq!(w.payload(), &[0xB0, 0x41, 0x42, 0x43]);
    }

    #[test]
    fn test_string() {
        let mut w = MarshalWriter::new();
        "hello".marshal(&mut w);
        assert_eq!(
            w.payload(),
            &[0x25, b'h', b'e', b'l', b'l', b'o']
        );
    }

    #[test]
    fn test_unit() {
        let mut w = MarshalWriter::new();
        ().marshal(&mut w);
        assert_eq!(w.payload(), &[0x40]); // int 0
    }

    #[test]
    fn test_nested_option() {
        let mut w = MarshalWriter::new();
        let opt: Option<Option<i32>> = Some(Some(42));
        opt.marshal(&mut w);
        // Some(Some(42)): Block(0,1) containing Block(0,1) containing 42
        assert_eq!(w.payload(), &[0x90, 0x90, 0x6A]);
    }

    #[test]
    fn test_list_of_strings() {
        let mut w = MarshalWriter::new();
        let v: Vec<String> = vec!["a".to_string(), "b".to_string()];
        v.marshal(&mut w);
        // Cons("a", Cons("b", nil))
        // Block(0,2), "a", Block(0,2), "b", nil
        assert_eq!(
            w.payload(),
            &[
                0xA0, 0x21, b'a', // Cons cell 1, "a"
                0xA0, 0x21, b'b', // Cons cell 2, "b"
                0x40              // nil
            ]
        );
    }
}
