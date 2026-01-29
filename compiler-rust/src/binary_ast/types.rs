//! Marshal implementations for core types
//!
//! This module provides Marshal implementations for the fundamental types used
//! in the AST: Position, Location, Located<T>, and Longident.

use super::marshal::MarshalWriter;
use super::serialize::Marshal;
use crate::location::{Located, Location, Position};
use crate::parse_arena::{self, LocIdx, PosIdx};
use crate::parser::longident::Longident;

// ========== Position (Lexing.position) ==========

impl Marshal for Position {
    /// Serialize a Position to Marshal format with identity-based sharing.
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
    /// Uses PositionId for identity-based sharing to match OCaml's pointer-based sharing.
    fn marshal(&self, w: &mut MarshalWriter) {
        w.write_position_shared(
            self.id,
            &self.file_name,
            self.line,
            self.bol,
            self.cnum,
        );
    }
}

// ========== Location (Location.t) ==========

impl Marshal for Location {
    /// Serialize a Location to Marshal format with LocationId-based sharing.
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
    /// Locations are shared when they have the same LocationId, which happens when:
    /// - The same Location object is used in multiple places (clone preserves ID)
    /// This mimics OCaml's pointer-based sharing.
    ///
    /// NOTE: Locations with ID 0 (default_id) are special "uninitialized" locations
    /// and are NOT shared - each occurrence is written as a new object.
    fn marshal(&self, w: &mut MarshalWriter) {
        // Check if we've seen this Location before (by ID)
        // Skip sharing for ID 0 (default_id) - these are uninitialized locations
        // that shouldn't be shared (like Location::none(), Location::merge(), etc.)
        if self.id.raw() != 0 {
            if let Some(obj_idx) = w.get_location_by_id(self.id) {
                // Write shared reference to previously written Location
                let d = w.obj_counter() - obj_idx;
                w.write_shared_ref(d);
                return;
            }
        }

        // Record this Location's index BEFORE writing (OCaml assigns then increments)
        let obj_idx = w.obj_counter();

        // Write a new Location block
        w.write_block_header(0, 3);
        // Positions are shared by identity (PositionId)
        self.loc_start.marshal(w);
        self.loc_end.marshal(w);
        w.write_int(if self.loc_ghost { 1 } else { 0 });

        // Record for future sharing (only if not default_id)
        if self.id.raw() != 0 {
            w.record_location_id(self.id, obj_idx);
        }
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
    /// Operator strings (like "-", "+") are shared by content to match OCaml's
    /// behavior (OCaml's parser interns operator tokens). Regular identifiers
    /// are NOT shared since OCaml creates new string objects for each occurrence.
    fn marshal(&self, w: &mut MarshalWriter) {
        match self {
            Longident::Lident(s) => {
                w.write_block_header(0, 1);
                w.write_identifier_string(s);
            }
            Longident::Ldot(prefix, name) => {
                w.write_block_header(1, 2);
                prefix.marshal(w);
                w.write_identifier_string(name);
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
        let mut w = MarshalWriter::new();
        let lid = Longident::Lident("foo".to_string());
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
        let mut w = MarshalWriter::new();
        let lid = Longident::Ldot(
            Box::new(Longident::Lident("Foo".to_string())),
            "bar".to_string(),
        );
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
        let mut w = MarshalWriter::new();
        let lid = Longident::Lapply(
            Box::new(Longident::Lident("F".to_string())),
            Box::new(Longident::Lident("X".to_string())),
        );
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
        let mut w = MarshalWriter::new();
        // Foo.Bar.baz
        let lid = Longident::Ldot(
            Box::new(Longident::Ldot(
                Box::new(Longident::Lident("Foo".to_string())),
                "Bar".to_string(),
            )),
            "baz".to_string(),
        );
        lid.marshal(&mut w);

        let payload = w.payload();
        // Should be: Block(1,2)[Block(1,2)[Block(0,1)["Foo"], "Bar"], "baz"]
        assert_eq!(payload[0], 0xA1); // outermost Ldot
    }
}
