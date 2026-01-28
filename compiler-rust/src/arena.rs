//! Generic arena allocation.
//!
//! This module provides a simple arena for storing values and referencing them
//! by lightweight indices. Unlike traditional interning, there is no deduplication -
//! each push creates a new entry.
//!
//! # Example
//!
//! ```ignore
//! use rescript_compiler::arena::{Arena, ArenaIdx};
//!
//! // Define a custom index type
//! #[derive(Clone, Copy, PartialEq, Eq)]
//! struct MyIdx(u32);
//!
//! impl ArenaIdx for MyIdx {
//!     fn from_u32(idx: u32) -> Self { MyIdx(idx) }
//!     fn to_u32(self) -> u32 { self.0 }
//! }
//!
//! let mut arena: Arena<String, MyIdx> = Arena::new();
//! let idx = arena.push("hello".to_string());
//! assert_eq!(arena.get(idx), "hello");
//! ```

use std::marker::PhantomData;

// ============================================================================
// Arena Index Trait
// ============================================================================

/// Trait for arena index types.
///
/// This allows Arena to be generic over the index type, providing type safety.
/// Instead of using raw `u32` indices everywhere, you can define wrapper types
/// like `PosIdx` or `LocIdx` that implement this trait.
pub trait ArenaIdx: Copy {
    /// Create an index from a raw u32 value.
    fn from_u32(idx: u32) -> Self;
    /// Convert the index to a raw u32 value.
    fn to_u32(self) -> u32;
}

impl ArenaIdx for u32 {
    fn from_u32(idx: u32) -> Self {
        idx
    }
    fn to_u32(self) -> u32 {
        self
    }
}

// ============================================================================
// Generic Arena
// ============================================================================

/// Simple arena for storing values and referencing them by index.
///
/// Values are pushed and indices are returned. No automatic deduplication -
/// each push creates a new entry.
///
/// The `Idx` type parameter allows using typed indices like `PosIdx` or `LocIdx`
/// instead of plain `u32`, which provides type safety and prevents mixing up
/// indices from different arenas.
#[derive(Debug)]
pub struct Arena<T, Idx: ArenaIdx = u32> {
    items: Vec<T>,
    _phantom: PhantomData<Idx>,
}

impl<T, Idx: ArenaIdx> Default for Arena<T, Idx> {
    fn default() -> Self {
        Self {
            items: Vec::new(),
            _phantom: PhantomData,
        }
    }
}

impl<T, Idx: ArenaIdx> Arena<T, Idx> {
    /// Create a new empty arena.
    pub fn new() -> Self {
        Self::default()
    }

    /// Create a new arena with pre-allocated capacity.
    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            items: Vec::with_capacity(capacity),
            _phantom: PhantomData,
        }
    }

    /// Push a value into the arena and return its typed index.
    pub fn push(&mut self, item: T) -> Idx {
        let idx = self.items.len() as u32;
        self.items.push(item);
        Idx::from_u32(idx)
    }

    /// Get a reference to a value by typed index.
    pub fn get(&self, idx: Idx) -> &T {
        &self.items[idx.to_u32() as usize]
    }

    /// Get a mutable reference to a value by typed index.
    pub fn get_mut(&mut self, idx: Idx) -> &mut T {
        &mut self.items[idx.to_u32() as usize]
    }

    /// Get the number of items in the arena.
    pub fn len(&self) -> usize {
        self.items.len()
    }

    /// Check if the arena is empty.
    pub fn is_empty(&self) -> bool {
        self.items.is_empty()
    }

    /// Get all items as a slice.
    pub fn as_slice(&self) -> &[T] {
        &self.items
    }

    /// Iterate over all items with their indices.
    pub fn iter(&self) -> impl Iterator<Item = (Idx, &T)> {
        self.items
            .iter()
            .enumerate()
            .map(|(i, item)| (Idx::from_u32(i as u32), item))
    }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_arena_basic() {
        let mut arena: Arena<i32> = Arena::new();
        assert!(arena.is_empty());

        let idx0 = arena.push(10);
        let idx1 = arena.push(20);
        let idx2 = arena.push(30);

        assert_eq!(idx0, 0);
        assert_eq!(idx1, 1);
        assert_eq!(idx2, 2);

        assert_eq!(*arena.get(0), 10);
        assert_eq!(*arena.get(1), 20);
        assert_eq!(*arena.get(2), 30);

        assert_eq!(arena.len(), 3);
    }

    #[derive(Clone, Copy, PartialEq, Eq, Debug)]
    struct TestIdx(u32);

    impl ArenaIdx for TestIdx {
        fn from_u32(idx: u32) -> Self {
            TestIdx(idx)
        }
        fn to_u32(self) -> u32 {
            self.0
        }
    }

    #[test]
    fn test_typed_arena() {
        let mut arena: Arena<i32, TestIdx> = Arena::new();

        let idx0 = arena.push(10);
        let idx1 = arena.push(20);

        assert_eq!(idx0, TestIdx(0));
        assert_eq!(idx1, TestIdx(1));

        assert_eq!(*arena.get(idx0), 10);
        assert_eq!(*arena.get(idx1), 20);
    }

    #[test]
    fn test_arena_iter() {
        let mut arena: Arena<&str, TestIdx> = Arena::new();
        arena.push("a");
        arena.push("b");
        arena.push("c");

        let collected: Vec<_> = arena.iter().collect();
        assert_eq!(collected.len(), 3);
        assert_eq!(collected[0], (TestIdx(0), &"a"));
        assert_eq!(collected[1], (TestIdx(1), &"b"));
        assert_eq!(collected[2], (TestIdx(2), &"c"));
    }

    #[test]
    fn test_arena_get_mut() {
        let mut arena: Arena<i32> = Arena::new();
        let idx = arena.push(10);

        *arena.get_mut(idx) = 20;
        assert_eq!(*arena.get(idx), 20);
    }
}
