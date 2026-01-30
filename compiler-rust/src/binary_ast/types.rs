//! Marshal implementations for core types
//!
//! This module provides Marshal implementations for the fundamental types used
//! in the AST: Position, Location, Located<T>, and Longident.

use super::marshal::MarshalWriter;
use super::serialize::Marshal;
use crate::location::{Located, Location, Position};
use crate::parse_arena::{self, LidentIdx, LocIdx, PosIdx};
use crate::parser::longident::Longident;

// ========== Position (Lexing.position) ==========

impl Marshal for Position {
    /// Serialize a Position to Marshal format.
    ///
    /// OCaml type:
    /// ```ocaml
    /// type position = {
    ///   pos_fname : string;
    ///   pos_lnum : int;
    ///   pos_bol : int;
    ///   pos_cnum : int;
    /// }
    /// ```
    ///
    /// Encoded as a block with tag 0 and 4 fields.
    ///
    /// Uses StrIdx-based sharing for file names: the same StrIdx will be shared
    /// in the marshal output via back-references.
    fn marshal(&self, w: &mut MarshalWriter) {
        w.write_block_header(0, 4);
        w.write_str_idx(self.file_name);
        w.write_int(self.line as i64);
        w.write_int(self.bol as i64);
        w.write_int(self.cnum as i64);
    }
}

// ========== Location (Location.t) ==========

impl Marshal for Location {
    /// Serialize a Location to Marshal format.
    ///
    /// OCaml type:
    /// ```ocaml
    /// type t = {
    ///   loc_start: Lexing.position;
    ///   loc_end: Lexing.position;
    ///   loc_ghost: bool;
    /// }
    /// ```
    ///
    /// Encoded as a block with tag 0 and 3 fields.
    ///
    /// NOTE: This impl is for the standalone Location struct which has no identity.
    /// For proper sharing, use arena-based LocIdx instead.
    fn marshal(&self, w: &mut MarshalWriter) {
        w.write_block_header(0, 3);
        self.loc_start.marshal(w);
        self.loc_end.marshal(w);
        w.write_int(if self.loc_ghost { 1 } else { 0 });
    }
}

// ========== Located<T> ('a loc) from crate::location ==========

impl<T: Marshal> Marshal for Located<T> {
    /// Serialize a Located<T> to Marshal format.
    ///
    /// OCaml type:
    /// ```ocaml
    /// type 'a loc = { txt: 'a; loc: t }
    /// ```
    ///
    /// Encoded as a block with tag 0 and 2 fields.
    fn marshal(&self, w: &mut MarshalWriter) {
        w.write_block_header(0, 2);
        self.txt.marshal(w);
        self.loc.marshal(w);
    }
}

// ========== Arena-based types ==========

impl Marshal for PosIdx {
    /// Serialize a PosIdx by looking up the position in the arena.
    /// Requires that MarshalWriter::set_arena() was called first.
    fn marshal(&self, w: &mut MarshalWriter) {
        w.write_pos_idx(*self);
    }
}

impl Marshal for LocIdx {
    /// Serialize a LocIdx by looking up the location in the arena.
    /// Requires that MarshalWriter::set_arena() was called first.
    fn marshal(&self, w: &mut MarshalWriter) {
        w.write_loc_idx(*self);
    }
}

impl<T: Marshal> Marshal for parse_arena::Located<T> {
    /// Serialize a parse_arena::Located<T> to Marshal format.
    ///
    /// OCaml type:
    /// ```ocaml
    /// type 'a loc = { txt: 'a; loc: t }
    /// ```
    ///
    /// Encoded as a block with tag 0 and 2 fields.
    /// Uses arena-based LocIdx for the location.
    fn marshal(&self, w: &mut MarshalWriter) {
        w.write_block_header(0, 2);
        self.txt.marshal(w);
        self.loc.marshal(w);
    }
}

// ========== Longident ==========

impl Marshal for LidentIdx {
    /// Serialize a LidentIdx by looking up the longident in the arena.
    /// Requires that MarshalWriter::set_arena() was called first.
    fn marshal(&self, w: &mut MarshalWriter) {
        w.write_lident_idx(*self);
    }
}

impl Marshal for Longident {
    /// Serialize a Longident to Marshal format.
    ///
    /// OCaml type:
    /// ```ocaml
    /// type t =
    ///   | Lident of string      (* tag 0 *)
    ///   | Ldot of t * string    (* tag 1 *)
    ///   | Lapply of t * t       (* tag 2 *)
    /// ```
    ///
    /// All constructors are non-constant (have data), so they all use block tags.
    /// Uses StrIdx-based sharing to match OCaml's behavior where the same
    /// string (e.g., "compare" in both a label and identifier) shares memory.
    fn marshal(&self, w: &mut MarshalWriter) {
        match self {
            Longident::Lident(s) => {
                w.write_block_header(0, 1);
                w.write_str_idx(*s);
            }
            Longident::Ldot(prefix, name) => {
                w.write_block_header(1, 2);
                prefix.marshal(w);
                w.write_str_idx(*name);
            }
            Longident::Lapply(func, arg) => {
                w.write_block_header(2, 2);
                func.marshal(w);
                arg.marshal(w);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parse_arena::ParseArena;

    #[test]
    fn test_position_marshal() {
        let mut w = MarshalWriter::new();
        let pos = Position::new("test.res", 1, 0, 0);
        pos.marshal(&mut w);

        // Block(tag=0, size=4): 0xC0
        // String "test.res" (len=8): 0x28 + bytes
        // Int 1: 0x41
        // Int 0: 0x40
        // Int 0: 0x40
        let payload = w.payload();
        assert_eq!(payload[0], 0xC0); // block tag=0, size=4
        assert_eq!(payload[1], 0x28); // string len=8
        assert_eq!(&payload[2..10], b"test.res");
        assert_eq!(payload[10], 0x41); // int 1
        assert_eq!(payload[11], 0x40); // int 0
        assert_eq!(payload[12], 0x40); // int 0
    }

    #[test]
    fn test_location_marshal() {
        let mut w = MarshalWriter::new();
        let loc = Location::from_positions(
            Position::new("test.res", 1, 0, 0),
            Position::new("test.res", 1, 0, 5),
        );
        loc.marshal(&mut w);

        let payload = w.payload();
        // Block(tag=0, size=3): 0xB0
        assert_eq!(payload[0], 0xB0);
    }

    #[test]
    fn test_located_marshal() {
        let mut w = MarshalWriter::new();
        let located = Located::new("hello".to_string(), Location::none());
        located.marshal(&mut w);

        let payload = w.payload();
        // Block(tag=0, size=2): 0xA0
        assert_eq!(payload[0], 0xA0);
        // String "hello" (len=5): 0x25 + bytes
        assert_eq!(payload[1], 0x25);
        assert_eq!(&payload[2..7], b"hello");
    }

    #[test]
    fn test_longident_lident() {
        let mut arena = ParseArena::default();
        let foo_idx = arena.intern_string("foo");
        let lid = Longident::Lident(foo_idx);

        let mut w = MarshalWriter::new();
        w.set_arena(&arena);
        lid.marshal(&mut w);

        let payload = w.payload();
        // Block(tag=0, size=1): 0x90
        assert_eq!(payload[0], 0x90);
        // String "foo" (len=3): 0x23 + bytes
        assert_eq!(payload[1], 0x23);
        assert_eq!(&payload[2..5], b"foo");
    }

    #[test]
    fn test_longident_ldot() {
        let mut arena = ParseArena::default();
        let foo_idx = arena.intern_string("Foo");
        let bar_idx = arena.intern_string("bar");
        let lid = Longident::Ldot(
            Box::new(Longident::Lident(foo_idx)),
            bar_idx,
        );

        let mut w = MarshalWriter::new();
        w.set_arena(&arena);
        lid.marshal(&mut w);

        let payload = w.payload();
        // Block(tag=1, size=2): 0xA1
        assert_eq!(payload[0], 0xA1);
        // Then nested Lident("Foo")
        assert_eq!(payload[1], 0x90); // Block(tag=0, size=1)
        assert_eq!(payload[2], 0x23); // String len=3
        assert_eq!(&payload[3..6], b"Foo");
        // Then "bar"
        assert_eq!(payload[6], 0x23); // String len=3
        assert_eq!(&payload[7..10], b"bar");
    }

    #[test]
    fn test_longident_lapply() {
        let mut arena = ParseArena::default();
        let f_idx = arena.intern_string("F");
        let x_idx = arena.intern_string("X");
        let lid = Longident::Lapply(
            Box::new(Longident::Lident(f_idx)),
            Box::new(Longident::Lident(x_idx)),
        );

        let mut w = MarshalWriter::new();
        w.set_arena(&arena);
        lid.marshal(&mut w);

        let payload = w.payload();
        // Block(tag=2, size=2): 0xA2
        assert_eq!(payload[0], 0xA2);
    }

    #[test]
    fn test_location_none_marshal() {
        let mut w = MarshalWriter::new();
        let loc = Location::none();
        loc.marshal(&mut w);

        // The location should serialize, including the "_none_" filename
        assert!(!w.payload().is_empty());
    }

    #[test]
    fn test_nested_longident() {
        let mut arena = ParseArena::default();
        let foo_idx = arena.intern_string("Foo");
        let bar_idx = arena.intern_string("Bar");
        let baz_idx = arena.intern_string("baz");
        // Foo.Bar.baz
        let lid = Longident::Ldot(
            Box::new(Longident::Ldot(
                Box::new(Longident::Lident(foo_idx)),
                bar_idx,
            )),
            baz_idx,
        );

        let mut w = MarshalWriter::new();
        w.set_arena(&arena);
        lid.marshal(&mut w);

        let payload = w.payload();
        // Should be: Block(1,2)[Block(1,2)[Block(0,1)["Foo"], "Bar"], "baz"]
        assert_eq!(payload[0], 0xA1); // outermost Ldot
    }
}
