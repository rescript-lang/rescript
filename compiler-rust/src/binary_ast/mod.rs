//! Binary AST file generation
//!
//! This module implements generation of binary AST files (.ast, .iast) that are
//! byte-identical to those produced by the OCaml compiler. These files are used for:
//!
//! 1. **Incremental compilation**: Cached parsed ASTs avoid re-parsing unchanged files
//! 2. **PPX support**: Preprocessor extensions read and write binary ASTs
//! 3. **Build system integration**: `rewatch` uses binary ASTs for dependency tracking
//!
//! # Binary AST File Format
//!
//! The binary AST file has three sections:
//!
//! ```text
//! ┌─────────────────────────────────────────────────────────────────────┐
//! │ SECTION 1: Header                                                   │
//! ├─────────────────────────────────────────────────────────────────────┤
//! │ Offset 0-3: Dependency section size (4 bytes, big-endian u32)       │
//! ├─────────────────────────────────────────────────────────────────────┤
//! │ SECTION 2: Dependencies + Source Path                               │
//! ├─────────────────────────────────────────────────────────────────────┤
//! │ Byte 4: '\n' (0x0A) - separator                                     │
//! │ "ModuleName1\n"                                                     │
//! │ "ModuleName2\n"                                                     │
//! │ ...                                                                 │
//! │ "/" - terminates deps AND starts absolute path                      │
//! │ "Users/path/to/source.res\n" - rest of path + newline               │
//! ├─────────────────────────────────────────────────────────────────────┤
//! │ SECTION 3: OCaml Marshal Data                                       │
//! ├─────────────────────────────────────────────────────────────────────┤
//! │ Magic: 0x84 0x95 0xA6 0xBE (4 bytes)                                │
//! │ Header: data_len(4) + obj_count(4) + size_32(4) + size_64(4)        │
//! │ Payload: Encoded parsetree0 AST                                     │
//! └─────────────────────────────────────────────────────────────────────┘
//! ```
//!
//! # Module Structure
//!
//! - [`marshal`] - OCaml Marshal format writer
//! - [`serialize`] - Marshal trait and implementations for basic types
//! - [`parsetree0`] - Frozen PPX-compatible AST type definitions (TODO)
//! - [`parsetree0_marshal`] - Marshal implementations for parsetree0
//! - [`mapper_to0`] - Current parsetree → parsetree0 conversion
//! - [`deps`] - Module dependency extraction
//! - [`writer`] - Binary AST file writer

pub mod current_marshal;
pub mod deps;
pub mod mapper_from0;
pub mod mapper_to0;
pub mod marshal;
pub mod parsetree0;
pub mod parsetree0_marshal;
pub mod serialize;
pub mod types;
pub mod writer;

// Re-exports
pub use deps::{extract_signature_deps, extract_structure_deps, Dependencies};
pub use mapper_to0::{map_signature, map_structure};
pub use marshal::MarshalWriter;
pub use parsetree0::*;
pub use serialize::Marshal;
pub use writer::{
    write_signature_ast, write_signature_ast_to_vec, write_structure_ast,
    write_structure_ast_to_vec,
};
