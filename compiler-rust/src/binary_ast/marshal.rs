//! OCaml Marshal format writer
//!
//! This module implements a writer for OCaml's Marshal binary serialization format.
//! The Marshal format is used by ReScript for binary AST files (.ast, .iast).
//!
//! # Format Overview
//!
//! The Marshal format consists of:
//! 1. Magic number (4 bytes): 0x8495A6BE for small format
//! 2. Header (16 bytes): data_len, obj_count, size_32, size_64
//! 3. Payload: encoded OCaml values
//!
//! # Position Identity-Based Sharing
//!
//! OCaml's Marshal format uses pointer-based sharing - when two positions
//! are the SAME object in memory, they get a shared reference (CODE_SHARED8/16/32).
//! We mimic this using PositionId: positions with the same ID are considered
//! "the same object" and get shared.
//!
//! This is critical for byte-for-byte parity with OCaml's parser output.
//!
//! # References
//!
//! - OCaml runtime: runtime/extern.c
//! - OCaml runtime: runtime/caml/intext.h

use std::collections::HashMap;

use crate::location::{LocationId, PositionId};

/// Magic number for small Marshal format
const MAGIC_NUMBER_SMALL: u32 = 0x8495A6BE;

/// Prefix codes for compact encoding
const PREFIX_SMALL_BLOCK: u8 = 0x80;
const PREFIX_SMALL_INT: u8 = 0x40;
const PREFIX_SMALL_STRING: u8 = 0x20;

/// Explicit opcodes
const CODE_INT8: u8 = 0x00;
const CODE_INT16: u8 = 0x01;
const CODE_INT32: u8 = 0x02;
const CODE_INT64: u8 = 0x03;
const CODE_SHARED8: u8 = 0x04;
const CODE_SHARED16: u8 = 0x05;
const CODE_SHARED32: u8 = 0x06;
const CODE_BLOCK32: u8 = 0x08;
const CODE_STRING8: u8 = 0x09;
const CODE_STRING32: u8 = 0x0A;
const CODE_DOUBLE_BIG: u8 = 0x0B;
const CODE_DOUBLE_LITTLE: u8 = 0x0C;
#[allow(dead_code)]
const CODE_BLOCK64: u8 = 0x13;
#[allow(dead_code)]
const CODE_SHARED64: u8 = 0x14;
#[allow(dead_code)]
const CODE_STRING64: u8 = 0x15;

/// Writer for OCaml Marshal binary format
///
/// This struct maintains the serialization state and provides methods for
/// encoding OCaml values to the Marshal format.
///
/// # Usage
///
/// ```ignore
/// let mut writer = MarshalWriter::new();
/// writer.write_int(42);
/// writer.write_string(b"hello");
/// let bytes = writer.finish();
/// ```

/// Key for Location-based sharing (start_id, end_id, ghost)
/// Uses PositionId for identity-based sharing like OCaml
pub type LocationKey = (PositionId, PositionId, bool);

#[derive(Debug)]
pub struct MarshalWriter {
    /// The output buffer containing encoded data
    buffer: Vec<u8>,

    /// Maps pointer identity to object counter at time of serialization (for sharing)
    obj_table: HashMap<usize, u32>,

    /// Maps string content to object counter (for content-based string sharing)
    string_table: HashMap<String, u32>,

    /// Maps Position ID to object counter (for identity-based Position sharing)
    /// Uses PositionId instead of content to mimic OCaml's pointer-based sharing
    position_table: HashMap<PositionId, u32>,

    /// Maps Position content to object counter (for content-based Position sharing)
    /// Key is (line, bol, cnum) - filename is always shared separately
    position_content_table: HashMap<(i32, i32, i32), u32>,

    /// Maps Location content to object counter (for content-based Location sharing)
    /// Key is (start_line, start_bol, start_cnum, end_line, end_bol, end_cnum, ghost)
    location_content_table: HashMap<(i32, i32, i32, i32, i32, i32, bool), u32>,

    /// Maps Location identity to object counter (for identity-based Location sharing)
    /// Uses PositionIds of start/end positions to determine identity
    location_table: HashMap<LocationKey, u32>,

    /// Maps LocationId to object counter (for identity-based Location sharing)
    /// Uses LocationId to determine when locations should be shared (like OCaml's pointer sharing)
    pub location_id_table: HashMap<LocationId, u32>,

    /// Current object counter (incremented each time we record a sharable object)
    obj_counter: u32,

    /// Size in 32-bit words (for header)
    size_32: u32,

    /// Size in 64-bit words (for header)
    size_64: u32,
}

impl Default for MarshalWriter {
    fn default() -> Self {
        Self::new()
    }
}

impl MarshalWriter {
    /// Create a new MarshalWriter
    pub fn new() -> Self {
        Self {
            buffer: Vec::with_capacity(4096),
            obj_table: HashMap::new(),
            string_table: HashMap::new(),
            position_table: HashMap::new(),
            position_content_table: HashMap::new(),
            location_content_table: HashMap::new(),
            location_table: HashMap::new(),
            location_id_table: HashMap::new(),
            obj_counter: 0,
            size_32: 0,
            size_64: 0,
        }
    }

    /// Create a new MarshalWriter with expected capacity
    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            buffer: Vec::with_capacity(capacity),
            obj_table: HashMap::new(),
            string_table: HashMap::new(),
            position_table: HashMap::new(),
            position_content_table: HashMap::new(),
            location_content_table: HashMap::new(),
            location_table: HashMap::new(),
            location_id_table: HashMap::new(),
            obj_counter: 0,
            size_32: 0,
            size_64: 0,
        }
    }

    /// Get the current object counter value
    pub fn obj_counter(&self) -> u32 {
        self.obj_counter
    }

    /// Reset the writer state for reuse
    pub fn reset(&mut self) {
        self.buffer.clear();
        self.obj_table.clear();
        self.string_table.clear();
        self.position_table.clear();
        self.location_table.clear();
        self.obj_counter = 0;
        self.size_32 = 0;
        self.size_64 = 0;
    }

    // ========== Low-level byte writing ==========

    /// Write a single byte
    #[inline]
    pub fn write_u8(&mut self, b: u8) {
        self.buffer.push(b);
    }

    /// Write an i8 (as one byte, two's complement)
    #[inline]
    pub fn write_i8(&mut self, n: i8) {
        self.buffer.push(n as u8);
    }

    /// Write a big-endian i16
    #[inline]
    pub fn write_i16_be(&mut self, n: i16) {
        self.buffer.push((n >> 8) as u8);
        self.buffer.push(n as u8);
    }

    /// Write a big-endian u16
    #[inline]
    pub fn write_u16_be(&mut self, n: u16) {
        self.buffer.push((n >> 8) as u8);
        self.buffer.push(n as u8);
    }

    /// Write a big-endian i32
    #[inline]
    pub fn write_i32_be(&mut self, n: i32) {
        self.buffer.push((n >> 24) as u8);
        self.buffer.push((n >> 16) as u8);
        self.buffer.push((n >> 8) as u8);
        self.buffer.push(n as u8);
    }

    /// Write a big-endian u32
    #[inline]
    pub fn write_u32_be(&mut self, n: u32) {
        self.buffer.push((n >> 24) as u8);
        self.buffer.push((n >> 16) as u8);
        self.buffer.push((n >> 8) as u8);
        self.buffer.push(n as u8);
    }

    /// Write a big-endian i64
    #[inline]
    pub fn write_i64_be(&mut self, n: i64) {
        self.buffer.push((n >> 56) as u8);
        self.buffer.push((n >> 48) as u8);
        self.buffer.push((n >> 40) as u8);
        self.buffer.push((n >> 32) as u8);
        self.buffer.push((n >> 24) as u8);
        self.buffer.push((n >> 16) as u8);
        self.buffer.push((n >> 8) as u8);
        self.buffer.push(n as u8);
    }

    /// Write a big-endian u64
    #[inline]
    #[allow(dead_code)]
    pub fn write_u64_be(&mut self, n: u64) {
        self.buffer.push((n >> 56) as u8);
        self.buffer.push((n >> 48) as u8);
        self.buffer.push((n >> 40) as u8);
        self.buffer.push((n >> 32) as u8);
        self.buffer.push((n >> 24) as u8);
        self.buffer.push((n >> 16) as u8);
        self.buffer.push((n >> 8) as u8);
        self.buffer.push(n as u8);
    }

    /// Write raw bytes
    #[inline]
    pub fn write_bytes(&mut self, bytes: &[u8]) {
        self.buffer.extend_from_slice(bytes);
    }

    // ========== Marshal value encoding ==========

    /// Write an integer in Marshal format
    ///
    /// Uses the most compact encoding:
    /// - 0-63: single byte (PREFIX_SMALL_INT | n)
    /// - -128 to 127: CODE_INT8 + 1 byte
    /// - -32768 to 32767: CODE_INT16 + 2 bytes
    /// - -2^30 to 2^30-1: CODE_INT32 + 4 bytes
    /// - larger: CODE_INT64 + 8 bytes
    pub fn write_int(&mut self, n: i64) {
        if n >= 0 && n < 0x40 {
            // Small int: single byte 0x40 | n
            self.write_u8(PREFIX_SMALL_INT + (n as u8));
        } else if n >= -128 && n < 128 {
            self.write_u8(CODE_INT8);
            self.write_i8(n as i8);
        } else if n >= -32768 && n < 32768 {
            self.write_u8(CODE_INT16);
            self.write_i16_be(n as i16);
        } else if n >= -(1 << 30) && n < (1 << 30) {
            self.write_u8(CODE_INT32);
            self.write_i32_be(n as i32);
        } else {
            self.write_u8(CODE_INT64);
            self.write_i64_be(n);
        }
    }

    /// Write a string in Marshal format
    ///
    /// Uses the most compact encoding:
    /// - length 0-31: PREFIX_SMALL_STRING | length, then string bytes
    /// - length 32-255: CODE_STRING8 + 1 byte length, then string bytes
    /// - length 256+: CODE_STRING32 + 4 byte length, then string bytes
    ///
    /// Note: This increments the object counter AFTER writing, as every string is a heap object.
    /// The object is assigned the index that obj_counter had before this call.
    pub fn write_string(&mut self, s: &[u8]) {
        let len = s.len();
        if len < 0x20 {
            // Small string: PREFIX_SMALL_STRING | len
            self.write_u8(PREFIX_SMALL_STRING + (len as u8));
        } else if len < 0x100 {
            self.write_u8(CODE_STRING8);
            self.write_u8(len as u8);
        } else if len < 0x100000000 {
            self.write_u8(CODE_STRING32);
            self.write_u32_be(len as u32);
        } else {
            // Strings > 4GB are not supported
            panic!("String too large for Marshal format: {} bytes", len);
        }
        self.write_bytes(s);

        // Update size counters for string allocation
        // OCaml strings: 1 header word + ceil((len + 1) / word_size) data words
        // The +1 is for the null terminator padding
        self.size_32 += 1 + ((len as u32 + 4) / 4);
        self.size_64 += 1 + ((len as u32 + 8) / 8);

        // Increment object counter AFTER writing (OCaml assigns index then increments)
        self.obj_counter += 1;
    }

    /// Write a string from a Rust &str (convenience method)
    pub fn write_str(&mut self, s: &str) {
        self.write_string(s.as_bytes());
    }

    /// Write a string with content-based sharing
    ///
    /// If this string content has been written before, writes a shared reference.
    /// Otherwise writes the string and records it for future sharing.
    ///
    /// This is useful for strings that appear multiple times in the AST,
    /// such as file names in locations.
    pub fn write_string_shared(&mut self, s: &str) {
        // Check if we've seen this string content before
        if let Some(&obj_idx) = self.string_table.get(s) {
            // Write a shared reference
            let d = self.obj_counter - obj_idx;
            self.write_shared_ref(d);
        } else {
            // Record the object index BEFORE incrementing (OCaml assigns before increment)
            let obj_idx = self.obj_counter;

            // Write the string (this increments obj_counter after assigning index)
            self.write_string(s.as_bytes());

            // Record for future sharing
            self.string_table.insert(s.to_string(), obj_idx);
        }
    }

    /// Write a string for an identifier, with content-based sharing.
    ///
    /// OCaml's parser reuses string objects from its string pool, so identical
    /// identifier strings get shared when serialized. We mimic this by sharing
    /// ALL identifier strings by content.
    pub fn write_identifier_string(&mut self, s: &str) {
        // All identifier strings are shared by content to match OCaml's behavior
        self.write_string_shared(s);
    }

    /// Write a Position with identity-based sharing
    ///
    /// If a Position with this PositionId has been written before,
    /// writes a shared reference. Otherwise writes the full Position block.
    ///
    /// This mimics OCaml's pointer-based sharing - positions with the same
    /// PositionId are considered "the same object" and get shared.
    ///
    /// Returns true if the position was written (not shared), false if a shared ref was written.
    pub fn write_position_shared(
        &mut self,
        _id: PositionId,
        file_name: &str,
        line: i32,
        bol: i32,
        cnum: i32,
    ) -> bool {
        // Use content-based sharing: positions with the same (line, bol, cnum) are shared.
        // This approximates OCaml's pointer-based sharing - positions that have identical
        // content are likely to be the same object in OCaml's parser.
        let content_key = (line, bol, cnum);

        if let Some(&obj_idx) = self.position_content_table.get(&content_key) {
            // Write a shared reference
            let d = self.obj_counter - obj_idx;
            self.write_shared_ref(d);
            return false;
        }

        // Record the object index BEFORE writing
        let obj_idx = self.obj_counter;

        // Write the Position block
        self.write_block_header(0, 4);
        self.write_string_shared(file_name);
        self.write_int(line as i64);
        self.write_int(bol as i64);
        self.write_int(cnum as i64);

        // Record for future content-based sharing
        self.position_content_table.insert(content_key, obj_idx);

        true
    }

    /// Write a Location with identity-based sharing
    ///
    /// If a Location with the same start/end PositionIds has been written before,
    /// writes a shared reference. Otherwise writes the full Location block.
    ///
    /// This mimics OCaml's pointer-based sharing - locations are considered
    /// "the same object" if their start and end positions have the same identities.
    ///
    /// Returns true if the location was written (not shared), false if a shared ref was written.
    #[allow(clippy::too_many_arguments)]
    pub fn write_location_shared(
        &mut self,
        start_id: PositionId,
        start_file: &str,
        start_line: i32,
        start_bol: i32,
        start_cnum: i32,
        end_id: PositionId,
        end_file: &str,
        end_line: i32,
        end_bol: i32,
        end_cnum: i32,
        ghost: bool,
    ) -> bool {
        let loc_key: LocationKey = (start_id, end_id, ghost);

        if let Some(&obj_idx) = self.location_table.get(&loc_key) {
            // Write a shared reference
            let d = self.obj_counter - obj_idx;
            self.write_shared_ref(d);
            false
        } else {
            // Record the object index BEFORE writing (OCaml assigns before increment)
            let obj_idx = self.obj_counter;

            // Write the Location block (this increments obj_counter after writing)
            self.write_block_header(0, 3);
            self.write_position_shared(start_id, start_file, start_line, start_bol, start_cnum);
            self.write_position_shared(end_id, end_file, end_line, end_bol, end_cnum);
            self.write_int(if ghost { 1 } else { 0 });

            // Record for future sharing
            self.location_table.insert(loc_key, obj_idx);
            true
        }
    }

    /// Check if a Location with this LocationId has been written before.
    /// Returns the object index if found, None otherwise.
    pub fn get_location_by_id(&self, id: LocationId) -> Option<u32> {
        self.location_id_table.get(&id).copied()
    }

    /// Record a LocationId with its object index for future sharing.
    pub fn record_location_id(&mut self, id: LocationId, obj_idx: u32) {
        self.location_id_table.insert(id, obj_idx);
    }

    /// Write a Location with content-based sharing
    ///
    /// If a Location with the same content (start/end positions, ghost) has been written before,
    /// writes a shared reference. Otherwise writes the full Location block with its Positions.
    ///
    /// This approximates OCaml's pointer-based sharing - locations with identical content
    /// are likely to be the same object in OCaml's parser.
    ///
    /// Returns true if the location was written (not shared), false if a shared ref was written.
    #[allow(clippy::too_many_arguments)]
    pub fn write_location_content_shared(
        &mut self,
        _start_id: PositionId,
        start_file: &str,
        start_line: i32,
        start_bol: i32,
        start_cnum: i32,
        _end_id: PositionId,
        end_file: &str,
        end_line: i32,
        end_bol: i32,
        end_cnum: i32,
        ghost: bool,
    ) -> bool {
        // Content-based key for the entire location
        let content_key = (
            start_line, start_bol, start_cnum,
            end_line, end_bol, end_cnum,
            ghost,
        );

        if let Some(&obj_idx) = self.location_content_table.get(&content_key) {
            // Write a shared reference to the previously written Location
            let d = self.obj_counter - obj_idx;
            self.write_shared_ref(d);
            return false;
        }

        // Record the object index BEFORE writing the Location block
        let obj_idx = self.obj_counter;

        // Write the Location block
        self.write_block_header(0, 3);
        // Write start position (with content-based sharing)
        self.write_position_shared(_start_id, start_file, start_line, start_bol, start_cnum);
        // Write end position (with content-based sharing)
        self.write_position_shared(_end_id, end_file, end_line, end_bol, end_cnum);
        // Write ghost flag
        self.write_int(if ghost { 1 } else { 0 });

        // Record for future content-based sharing
        self.location_content_table.insert(content_key, obj_idx);

        true
    }

    /// Write a block header in Marshal format
    ///
    /// Blocks represent OCaml heap objects: records, tuples, and variant constructors
    /// with data.
    ///
    /// - tag: the block tag (0-255, typically < 16 for user types)
    /// - size: number of fields in the block
    ///
    /// Uses compact encoding for small blocks (tag < 16, size < 8).
    ///
    /// Note: This increments the object counter AFTER writing, as every block is a heap object
    /// that can potentially be referenced by shared pointers. The object is assigned the
    /// index that obj_counter had before this call.
    pub fn write_block_header(&mut self, tag: u8, size: usize) {
        if tag < 16 && size < 8 {
            // Small block: PREFIX_SMALL_BLOCK + tag + (size << 4)
            // Bits 0-3: tag (0-15)
            // Bits 4-6: size (0-7)
            // Bit 7: always 1 (from PREFIX_SMALL_BLOCK = 0x80)
            self.write_u8(PREFIX_SMALL_BLOCK + tag + ((size as u8) << 4));
        } else {
            // Large block: header = (size << 10) | (color << 8) | tag
            // OCaml's Make_header(sz, tag, color) = (sz << 10) | (color << 8) | tag
            // OCaml's marshal uses color=3 (Caml_black) for marshaled blocks
            let header = ((size as u64) << 10) | (3u64 << 8) | (tag as u64);
            if header < (1u64 << 32) {
                self.write_u8(CODE_BLOCK32);
                self.write_u32_be(header as u32);
            } else {
                self.write_u8(CODE_BLOCK64);
                self.write_u64_be(header);
            }
        }

        // Update size counters for block allocation
        // Block: 1 header word + size data words
        self.size_32 += 1 + (size as u32);
        self.size_64 += 1 + (size as u32);

        // Increment object counter AFTER writing (OCaml assigns index then increments)
        self.obj_counter += 1;
    }

    /// Write a double (64-bit float) in Marshal format
    ///
    /// OCaml uses big-endian for floats on big-endian systems and little-endian
    /// on little-endian systems. Most modern systems are little-endian.
    pub fn write_double(&mut self, f: f64) {
        // Detect endianness at compile time
        #[cfg(target_endian = "little")]
        {
            self.write_u8(CODE_DOUBLE_LITTLE);
            self.write_bytes(&f.to_le_bytes());
        }
        #[cfg(target_endian = "big")]
        {
            self.write_u8(CODE_DOUBLE_BIG);
            self.write_bytes(&f.to_be_bytes());
        }

        // Update size counters for float allocation
        // Float: 1 header word + 1 double word (8 bytes) on 64-bit
        //        1 header word + 2 words on 32-bit
        self.size_32 += 1 + 2; // 32-bit: double is 2 words
        self.size_64 += 1 + 1; // 64-bit: double is 1 word
    }

    // ========== Sharing support ==========

    /// Check if an object with the given identity has been seen before
    ///
    /// If found, writes a shared reference and returns true.
    /// If not found, returns false and the caller should serialize the object.
    ///
    /// The `ptr` parameter should be a unique identifier for the object,
    /// typically derived from a pointer address.
    pub fn check_shared(&mut self, ptr: usize) -> bool {
        if let Some(&pos) = self.obj_table.get(&ptr) {
            // Calculate distance from current position
            let d = self.obj_counter - pos;
            self.write_shared_ref(d);
            true
        } else {
            false
        }
    }

    /// Record that an object has been serialized at the current position
    ///
    /// Call this after serializing a sharable object (strings, blocks).
    /// The `ptr` parameter should match what was passed to `check_shared`.
    /// Record a pointer location for sharing.
    ///
    /// Note: This does NOT increment obj_counter because the object should
    /// have already been written (and counted) by write_block_header or write_string.
    /// The caller should pass the object index that was used when writing.
    pub fn record_location_at(&mut self, ptr: usize, obj_idx: u32) {
        self.obj_table.insert(ptr, obj_idx);
    }

    /// Record a pointer location for sharing using the current counter value.
    ///
    /// DEPRECATED: Use record_location_at with the correct object index instead.
    /// This is kept for backwards compatibility with tests.
    pub fn record_location(&mut self, ptr: usize) {
        // Record at the previous object index (obj_counter - 1) since the object
        // was already written and the counter was incremented
        self.obj_table.insert(ptr, self.obj_counter - 1);
    }

    /// Write a shared reference
    pub fn write_shared_ref(&mut self, d: u32) {
        if d < 0x100 {
            self.write_u8(CODE_SHARED8);
            self.write_u8(d as u8);
        } else if d < 0x10000 {
            self.write_u8(CODE_SHARED16);
            self.write_u16_be(d as u16);
        } else {
            self.write_u8(CODE_SHARED32);
            self.write_u32_be(d);
        }
    }

    // ========== Finalization ==========

    /// Get the serialized data with Marshal header prepended
    ///
    /// Returns the complete Marshal output including:
    /// - Magic number (4 bytes)
    /// - Header (16 bytes)
    /// - Payload (encoded data)
    pub fn finish(&self) -> Vec<u8> {
        let data_len = self.buffer.len() as u32;

        let mut result = Vec::with_capacity(20 + self.buffer.len());

        // Magic number
        result.push((MAGIC_NUMBER_SMALL >> 24) as u8);
        result.push((MAGIC_NUMBER_SMALL >> 16) as u8);
        result.push((MAGIC_NUMBER_SMALL >> 8) as u8);
        result.push(MAGIC_NUMBER_SMALL as u8);

        // Data length
        result.push((data_len >> 24) as u8);
        result.push((data_len >> 16) as u8);
        result.push((data_len >> 8) as u8);
        result.push(data_len as u8);

        // Object count (number of objects that can be referenced by CODE_SHARED)
        let obj_count = self.obj_counter;
        result.push((obj_count >> 24) as u8);
        result.push((obj_count >> 16) as u8);
        result.push((obj_count >> 8) as u8);
        result.push(obj_count as u8);

        // Size on 32-bit platform (words)
        result.push((self.size_32 >> 24) as u8);
        result.push((self.size_32 >> 16) as u8);
        result.push((self.size_32 >> 8) as u8);
        result.push(self.size_32 as u8);

        // Size on 64-bit platform (words)
        result.push((self.size_64 >> 24) as u8);
        result.push((self.size_64 >> 16) as u8);
        result.push((self.size_64 >> 8) as u8);
        result.push(self.size_64 as u8);

        // Payload
        result.extend_from_slice(&self.buffer);

        result
    }

    /// Get just the payload without the header (for testing)
    pub fn payload(&self) -> &[u8] {
        &self.buffer
    }

    /// Get the current payload length
    pub fn payload_len(&self) -> usize {
        self.buffer.len()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_small_int_0() {
        let mut w = MarshalWriter::new();
        w.write_int(0);
        assert_eq!(w.payload(), &[0x40]); // PREFIX_SMALL_INT | 0
    }

    #[test]
    fn test_small_int_42() {
        let mut w = MarshalWriter::new();
        w.write_int(42);
        assert_eq!(w.payload(), &[0x6A]); // PREFIX_SMALL_INT | 42 = 0x40 | 0x2A
    }

    #[test]
    fn test_small_int_63() {
        let mut w = MarshalWriter::new();
        w.write_int(63);
        assert_eq!(w.payload(), &[0x7F]); // PREFIX_SMALL_INT | 63 = 0x40 | 0x3F
    }

    #[test]
    fn test_int8_positive() {
        let mut w = MarshalWriter::new();
        w.write_int(64);
        assert_eq!(w.payload(), &[CODE_INT8, 64]);
    }

    #[test]
    fn test_int8_negative() {
        let mut w = MarshalWriter::new();
        w.write_int(-1);
        assert_eq!(w.payload(), &[CODE_INT8, 0xFF]); // -1 as i8
    }

    #[test]
    fn test_int8_min() {
        let mut w = MarshalWriter::new();
        w.write_int(-128);
        assert_eq!(w.payload(), &[CODE_INT8, 0x80]); // -128 as i8
    }

    #[test]
    fn test_int16_positive() {
        let mut w = MarshalWriter::new();
        w.write_int(128);
        assert_eq!(w.payload(), &[CODE_INT16, 0x00, 0x80]);
    }

    #[test]
    fn test_int16_negative() {
        let mut w = MarshalWriter::new();
        w.write_int(-129);
        // -129 as i16 in big-endian: 0xFF7F
        assert_eq!(w.payload(), &[CODE_INT16, 0xFF, 0x7F]);
    }

    #[test]
    fn test_int32() {
        let mut w = MarshalWriter::new();
        w.write_int(32768);
        // 32768 as i32 in big-endian: 0x00008000
        assert_eq!(w.payload(), &[CODE_INT32, 0x00, 0x00, 0x80, 0x00]);
    }

    #[test]
    fn test_int32_large() {
        let mut w = MarshalWriter::new();
        w.write_int(1_000_000);
        // 1_000_000 = 0x000F4240
        assert_eq!(w.payload(), &[CODE_INT32, 0x00, 0x0F, 0x42, 0x40]);
    }

    #[test]
    fn test_int64() {
        let mut w = MarshalWriter::new();
        let n: i64 = (1i64 << 30) + 1; // Just over CODE_INT32 range
        w.write_int(n);
        assert_eq!(w.payload()[0], CODE_INT64);
        assert_eq!(w.payload().len(), 9); // 1 opcode + 8 bytes
    }

    #[test]
    fn test_small_string() {
        let mut w = MarshalWriter::new();
        w.write_string(b"hello");
        // PREFIX_SMALL_STRING | 5, then "hello"
        assert_eq!(
            w.payload(),
            &[0x25, b'h', b'e', b'l', b'l', b'o']
        );
    }

    #[test]
    fn test_small_string_empty() {
        let mut w = MarshalWriter::new();
        w.write_string(b"");
        assert_eq!(w.payload(), &[0x20]); // PREFIX_SMALL_STRING | 0
    }

    #[test]
    fn test_small_string_max() {
        let mut w = MarshalWriter::new();
        let s = vec![b'x'; 31]; // Max small string size
        w.write_string(&s);
        assert_eq!(w.payload()[0], 0x3F); // PREFIX_SMALL_STRING | 31
        assert_eq!(w.payload().len(), 32); // 1 + 31
    }

    #[test]
    fn test_string8() {
        let mut w = MarshalWriter::new();
        let s = vec![b'x'; 32]; // Just over small string limit
        w.write_string(&s);
        assert_eq!(w.payload()[0], CODE_STRING8);
        assert_eq!(w.payload()[1], 32);
        assert_eq!(w.payload().len(), 34); // 1 opcode + 1 len + 32 data
    }

    #[test]
    fn test_string32() {
        let mut w = MarshalWriter::new();
        let s = vec![b'x'; 256]; // Over STRING8 limit
        w.write_string(&s);
        assert_eq!(w.payload()[0], CODE_STRING32);
        // Length 256 = 0x00000100
        assert_eq!(&w.payload()[1..5], &[0x00, 0x00, 0x01, 0x00]);
        assert_eq!(w.payload().len(), 261); // 1 opcode + 4 len + 256 data
    }

    #[test]
    fn test_small_block() {
        let mut w = MarshalWriter::new();
        w.write_block_header(0, 2);
        // Small block: PREFIX_SMALL_BLOCK + tag + (size << 4)
        // 0x80 + 0 + (2 << 4) = 0x80 + 0x20 = 0xA0
        assert_eq!(w.payload(), &[0xA0]);
    }

    #[test]
    fn test_small_block_tag_1_size_1() {
        let mut w = MarshalWriter::new();
        w.write_block_header(1, 1);
        // 0x80 + 1 + (1 << 4) = 0x80 + 1 + 0x10 = 0x91
        assert_eq!(w.payload(), &[0x91]);
    }

    #[test]
    fn test_small_block_max() {
        let mut w = MarshalWriter::new();
        w.write_block_header(15, 7);
        // 0x80 + 15 + (7 << 4) = 0x80 + 0x0F + 0x70 = 0xFF
        assert_eq!(w.payload(), &[0xFF]);
    }

    #[test]
    fn test_large_block_tag() {
        let mut w = MarshalWriter::new();
        w.write_block_header(16, 1); // tag too large for small block
        assert_eq!(w.payload()[0], CODE_BLOCK32);
        // header = (1 << 10) | (3 << 8) | 16 = 0x710 (size=1, color=3, tag=16)
        assert_eq!(&w.payload()[1..5], &[0x00, 0x00, 0x07, 0x10]);
    }

    #[test]
    fn test_large_block_size() {
        let mut w = MarshalWriter::new();
        w.write_block_header(0, 8); // size too large for small block
        assert_eq!(w.payload()[0], CODE_BLOCK32);
        // header = (8 << 10) | (3 << 8) | 0 = 0x2300 (size=8, color=3, tag=0)
        assert_eq!(&w.payload()[1..5], &[0x00, 0x00, 0x23, 0x00]);
    }

    #[test]
    fn test_double_little_endian() {
        let mut w = MarshalWriter::new();
        w.write_double(1.5);

        #[cfg(target_endian = "little")]
        {
            assert_eq!(w.payload()[0], CODE_DOUBLE_LITTLE);
            // 1.5 in IEEE 754: 0x3FF8000000000000
            let expected_bytes = 1.5f64.to_le_bytes();
            assert_eq!(&w.payload()[1..9], &expected_bytes);
        }
        #[cfg(target_endian = "big")]
        {
            assert_eq!(w.payload()[0], CODE_DOUBLE_BIG);
        }
    }

    #[test]
    fn test_finish_header() {
        let mut w = MarshalWriter::new();
        w.write_int(42);
        let output = w.finish();

        // Magic number: 0x8495A6BE
        assert_eq!(&output[0..4], &[0x84, 0x95, 0xA6, 0xBE]);

        // Data length: 1 byte
        assert_eq!(&output[4..8], &[0x00, 0x00, 0x00, 0x01]);

        // Object count: 0
        assert_eq!(&output[8..12], &[0x00, 0x00, 0x00, 0x00]);

        // Payload
        assert_eq!(output[20], 0x6A);
    }

    #[test]
    fn test_sharing() {
        let mut w = MarshalWriter::new();

        // Simulate serializing two objects, second references first
        let ptr1 = 0x1000;
        let ptr2 = 0x2000;

        // First object: not shared
        assert!(!w.check_shared(ptr1));
        w.write_block_header(0, 1);
        w.write_int(42);
        w.record_location(ptr1);

        // Second object: also not shared
        assert!(!w.check_shared(ptr2));
        w.write_block_header(0, 1);
        w.write_int(99);
        w.record_location(ptr2);

        // First object again: should be shared
        assert!(w.check_shared(ptr1));
        // Distance = 2 - 0 = 2
        // CODE_SHARED8, 2
        let payload = w.payload();
        let len = payload.len();
        assert_eq!(&payload[len - 2..], &[CODE_SHARED8, 2]);
    }

    #[test]
    fn test_size_counters() {
        let mut w = MarshalWriter::new();

        // A block with 2 fields
        w.write_block_header(0, 2);
        assert_eq!(w.size_32, 3); // 1 header + 2 fields
        assert_eq!(w.size_64, 3);

        // A string of length 5 ("hello")
        w.write_string(b"hello");
        // From OCaml extern.c: size_32 += 1 + (len + 4) / 4
        //                      size_64 += 1 + (len + 8) / 8
        // For len=5: size_32 = 1 + (5 + 4) / 4 = 1 + 2 = 3
        //           size_64 = 1 + (5 + 8) / 8 = 1 + 1 = 2
        assert_eq!(w.size_32, 3 + 3); // 3 (block) + 3 (string)
        assert_eq!(w.size_64, 3 + 2); // 3 (block) + 2 (string)
    }

    #[test]
    fn test_string_sharing() {
        let mut w = MarshalWriter::new();

        // First occurrence: write the string
        w.write_string_shared("test.res");
        let after_first = w.payload().len();
        assert_eq!(w.obj_counter(), 1); // String recorded

        // Second occurrence: should write shared reference
        w.write_string_shared("test.res");
        let after_second = w.payload().len();
        assert_eq!(w.obj_counter(), 1); // No new object recorded

        // The second write should be much smaller (shared reference = 2 bytes)
        // First write: 1 byte prefix + 8 bytes string = 9 bytes
        // Second write: CODE_SHARED8 (1 byte) + distance (1 byte) = 2 bytes
        assert_eq!(after_first, 9); // PREFIX_SMALL_STRING | 8, then 8 chars
        assert_eq!(after_second, 11); // 9 + 2 (shared ref)

        // Verify the shared reference
        let payload = w.payload();
        assert_eq!(payload[9], CODE_SHARED8);
        assert_eq!(payload[10], 1); // Distance from current obj_counter (1) to string index (0) = 1
    }

    #[test]
    fn test_string_sharing_different_strings() {
        let mut w = MarshalWriter::new();

        // Write two different strings
        w.write_string_shared("a.res");
        w.write_string_shared("b.res");

        // Both should be recorded as objects
        assert_eq!(w.obj_counter(), 2);

        // Now reference the first one again
        w.write_string_shared("a.res");
        // Should still be 2 objects (first one is shared)
        assert_eq!(w.obj_counter(), 2);

        // Payload should have:
        // - "a.res" (6 bytes: 0x25 + 5 chars)
        // - "b.res" (6 bytes: 0x25 + 5 chars)
        // - shared ref to "a.res" (2 bytes: CODE_SHARED8 + distance)
        assert_eq!(w.payload().len(), 14);
    }
}
