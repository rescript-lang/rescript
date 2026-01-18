//! FFI boundary for OCaml/Rust interoperability.
//!
//! This module provides the infrastructure for communication between OCaml
//! and Rust components during the incremental migration. It uses serialization
//! (bincode) for data exchange across the FFI boundary.
//!
//! # Design
//!
//! During the transition period, the compiler will have both OCaml and Rust
//! components. They communicate by:
//!
//! 1. Serializing data structures to bytes on one side
//! 2. Passing bytes across the FFI boundary
//! 3. Deserializing on the other side
//!
//! This approach is simpler and safer than direct memory sharing, though
//! slightly less performant. For the parser (which runs once per file),
//! this overhead is negligible.
//!
//! # Example
//!
//! ```rust,ignore
//! use rescript_compiler::ffi::{FfiResult, to_ffi_bytes, from_ffi_bytes};
//!
//! // Rust side: serialize a result
//! let result = ParseResult { ast: some_ast, errors: vec![] };
//! let bytes = to_ffi_bytes(&result)?;
//!
//! // Pass bytes to OCaml...
//!
//! // OCaml side would deserialize using bincode
//! ```

use serde::{Deserialize, Serialize, de::DeserializeOwned};
use std::ffi::{CStr, CString, c_char};
use std::ptr;

use crate::diagnostics::Diagnostic;
use crate::location::Location;

/// Result type for FFI operations.
pub type FfiResult<T> = Result<T, FfiError>;

/// Errors that can occur during FFI operations.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[allow(missing_docs)]
pub enum FfiError {
    /// Serialization failed.
    SerializationError(String),
    /// Deserialization failed.
    DeserializationError(String),
    /// Invalid UTF-8 string.
    Utf8Error(String),
    /// Null pointer encountered.
    NullPointer,
    /// Buffer too small.
    BufferTooSmall { required: usize, provided: usize },
    /// Generic error.
    Other(String),
}

impl std::fmt::Display for FfiError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FfiError::SerializationError(msg) => write!(f, "serialization error: {}", msg),
            FfiError::DeserializationError(msg) => write!(f, "deserialization error: {}", msg),
            FfiError::Utf8Error(msg) => write!(f, "UTF-8 error: {}", msg),
            FfiError::NullPointer => write!(f, "null pointer"),
            FfiError::BufferTooSmall { required, provided } => {
                write!(
                    f,
                    "buffer too small: need {} bytes, got {}",
                    required, provided
                )
            }
            FfiError::Other(msg) => write!(f, "{}", msg),
        }
    }
}

impl std::error::Error for FfiError {}

/// Serialize a value to bytes for FFI transfer.
pub fn to_ffi_bytes<T: Serialize>(value: &T) -> FfiResult<Vec<u8>> {
    bincode::serialize(value).map_err(|e| FfiError::SerializationError(e.to_string()))
}

/// Deserialize a value from bytes received via FFI.
pub fn from_ffi_bytes<T: DeserializeOwned>(bytes: &[u8]) -> FfiResult<T> {
    bincode::deserialize(bytes).map_err(|e| FfiError::DeserializationError(e.to_string()))
}

/// A buffer for FFI data transfer.
///
/// This is a simple wrapper around a byte vector that can be passed
/// across the FFI boundary.
#[derive(Debug, Default)]
pub struct FfiBuffer {
    data: Vec<u8>,
}

impl FfiBuffer {
    /// Create a new empty buffer.
    pub fn new() -> Self {
        Self::default()
    }

    /// Create a buffer with pre-allocated capacity.
    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            data: Vec::with_capacity(capacity),
        }
    }

    /// Get the buffer data as a slice.
    pub fn as_slice(&self) -> &[u8] {
        &self.data
    }

    /// Get the buffer length.
    pub fn len(&self) -> usize {
        self.data.len()
    }

    /// Check if buffer is empty.
    pub fn is_empty(&self) -> bool {
        self.data.is_empty()
    }

    /// Write serialized data to the buffer.
    pub fn write<T: Serialize>(&mut self, value: &T) -> FfiResult<()> {
        self.data = to_ffi_bytes(value)?;
        Ok(())
    }

    /// Read and deserialize data from the buffer.
    pub fn read<T: DeserializeOwned>(&self) -> FfiResult<T> {
        from_ffi_bytes(&self.data)
    }

    /// Clear the buffer.
    pub fn clear(&mut self) {
        self.data.clear();
    }

    /// Get raw pointer to data (for C FFI).
    pub fn as_ptr(&self) -> *const u8 {
        self.data.as_ptr()
    }

    /// Set buffer contents from raw bytes.
    ///
    /// # Safety
    ///
    /// The caller must ensure the bytes are valid.
    pub fn set_from_slice(&mut self, bytes: &[u8]) {
        self.data.clear();
        self.data.extend_from_slice(bytes);
    }
}

/// Status codes for FFI function returns.
#[repr(i32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FfiStatus {
    /// Operation succeeded.
    Ok = 0,
    /// Generic error.
    Error = -1,
    /// Serialization error.
    SerializationError = -2,
    /// Deserialization error.
    DeserializationError = -3,
    /// Buffer too small.
    BufferTooSmall = -4,
    /// Invalid input.
    InvalidInput = -5,
    /// Null pointer.
    NullPointer = -6,
}

impl From<&FfiError> for FfiStatus {
    fn from(err: &FfiError) -> Self {
        match err {
            FfiError::SerializationError(_) => FfiStatus::SerializationError,
            FfiError::DeserializationError(_) => FfiStatus::DeserializationError,
            FfiError::BufferTooSmall { .. } => FfiStatus::BufferTooSmall,
            FfiError::NullPointer => FfiStatus::NullPointer,
            FfiError::Utf8Error(_) | FfiError::Other(_) => FfiStatus::Error,
        }
    }
}

/// Convert a Rust string to a C string.
///
/// Returns null on allocation failure or if the string contains null bytes.
pub fn string_to_c(s: &str) -> *mut c_char {
    match CString::new(s) {
        Ok(cstr) => cstr.into_raw(),
        Err(_) => ptr::null_mut(),
    }
}

/// Convert a C string to a Rust string.
///
/// # Safety
///
/// The pointer must be valid and null-terminated.
pub unsafe fn c_to_string(ptr: *const c_char) -> FfiResult<String> {
    if ptr.is_null() {
        return Err(FfiError::NullPointer);
    }
    // SAFETY: Caller guarantees ptr is valid and null-terminated
    unsafe { CStr::from_ptr(ptr) }
        .to_str()
        .map(|s| s.to_string())
        .map_err(|e| FfiError::Utf8Error(e.to_string()))
}

/// Free a C string allocated by Rust.
///
/// # Safety
///
/// The pointer must have been allocated by `string_to_c`.
pub unsafe fn free_c_string(ptr: *mut c_char) {
    if !ptr.is_null() {
        // SAFETY: Caller guarantees ptr was allocated by string_to_c
        drop(unsafe { CString::from_raw(ptr) });
    }
}

// ============================================================================
// FFI Data Types for Cross-Boundary Communication
// ============================================================================

/// Result of parsing a ReScript file.
///
/// This is the primary data structure passed from Rust parser to OCaml.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ParseResult {
    /// The parsed AST (serialized Parsetree).
    pub ast: Vec<u8>,
    /// Any diagnostics generated during parsing.
    pub diagnostics: Vec<Diagnostic>,
    /// Whether parsing was successful.
    pub success: bool,
}

impl ParseResult {
    /// Create a successful parse result.
    pub fn success(ast: Vec<u8>) -> Self {
        Self {
            ast,
            diagnostics: Vec::new(),
            success: true,
        }
    }

    /// Create a failed parse result.
    pub fn failure(diagnostics: Vec<Diagnostic>) -> Self {
        Self {
            ast: Vec::new(),
            diagnostics,
            success: false,
        }
    }

    /// Add a diagnostic.
    pub fn with_diagnostic(mut self, diagnostic: Diagnostic) -> Self {
        self.diagnostics.push(diagnostic);
        self
    }
}

/// Request to compile a module.
///
/// Sent from OCaml to Rust for compilation.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CompileRequest {
    /// Source file path.
    pub source_path: String,
    /// Source code content.
    pub source_code: String,
    /// Output path for JavaScript.
    pub output_path: Option<String>,
    /// Whether this is an interface file.
    pub is_interface: bool,
}

/// Result of compiling a module.
///
/// Sent from Rust to OCaml after compilation.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CompileResult {
    /// Generated JavaScript code (if successful).
    pub js_code: Option<String>,
    /// Generated source map (if requested).
    pub source_map: Option<String>,
    /// Module interface data (for .cmi).
    pub interface: Option<Vec<u8>>,
    /// Diagnostics generated during compilation.
    pub diagnostics: Vec<Diagnostic>,
    /// Whether compilation was successful.
    pub success: bool,
}

impl CompileResult {
    /// Create a successful compile result.
    pub fn success(js_code: String) -> Self {
        Self {
            js_code: Some(js_code),
            source_map: None,
            interface: None,
            diagnostics: Vec::new(),
            success: true,
        }
    }

    /// Create a failed compile result.
    pub fn failure(diagnostics: Vec<Diagnostic>) -> Self {
        Self {
            js_code: None,
            source_map: None,
            interface: None,
            diagnostics,
            success: false,
        }
    }
}

/// A simple error report for FFI.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ErrorReport {
    /// Error message.
    pub message: String,
    /// Location in source.
    pub location: Option<Location>,
    /// Sub-errors.
    pub sub_errors: Vec<ErrorReport>,
}

impl ErrorReport {
    /// Create a new error report.
    pub fn new(message: impl Into<String>) -> Self {
        Self {
            message: message.into(),
            location: None,
            sub_errors: Vec::new(),
        }
    }

    /// Create an error report with location.
    pub fn with_location(message: impl Into<String>, location: Location) -> Self {
        Self {
            message: message.into(),
            location: Some(location),
            sub_errors: Vec::new(),
        }
    }
}

// ============================================================================
// FFI Handle Management
// ============================================================================

/// An opaque handle to a Rust object for FFI.
///
/// This allows OCaml to hold references to Rust objects without
/// knowing their internal structure.
#[repr(C)]
pub struct FfiHandle {
    /// Type identifier for runtime type checking.
    type_id: u32,
    /// Pointer to the actual data.
    data: *mut (),
}

impl FfiHandle {
    /// Create a new handle for a boxed value.
    ///
    /// # Safety
    ///
    /// The type_id must uniquely identify the type T.
    pub fn new<T>(value: T, type_id: u32) -> Self {
        let boxed = Box::new(value);
        Self {
            type_id,
            data: Box::into_raw(boxed) as *mut (),
        }
    }

    /// Check if this handle has the expected type.
    pub fn is_type(&self, expected_type_id: u32) -> bool {
        self.type_id == expected_type_id
    }

    /// Get a reference to the underlying value.
    ///
    /// # Safety
    ///
    /// The type_id must match the actual type, and the handle must be valid.
    pub unsafe fn get<T>(&self, expected_type_id: u32) -> Option<&T> {
        if self.type_id != expected_type_id || self.data.is_null() {
            return None;
        }
        // SAFETY: Caller guarantees type_id matches and handle is valid
        Some(unsafe { &*(self.data as *const T) })
    }

    /// Get a mutable reference to the underlying value.
    ///
    /// # Safety
    ///
    /// The type_id must match the actual type, and the handle must be valid.
    pub unsafe fn get_mut<T>(&mut self, expected_type_id: u32) -> Option<&mut T> {
        if self.type_id != expected_type_id || self.data.is_null() {
            return None;
        }
        // SAFETY: Caller guarantees type_id matches and handle is valid
        Some(unsafe { &mut *(self.data as *mut T) })
    }

    /// Free the underlying value.
    ///
    /// # Safety
    ///
    /// The type_id must match the actual type, and this must only be called once.
    pub unsafe fn free<T>(&mut self, expected_type_id: u32) {
        if self.type_id == expected_type_id && !self.data.is_null() {
            // SAFETY: Caller guarantees type_id matches and this is called only once
            drop(unsafe { Box::from_raw(self.data as *mut T) });
            self.data = ptr::null_mut();
        }
    }
}

/// Type IDs for FFI handles.
pub mod type_ids {
    /// CompilationContext handle.
    pub const COMPILATION_CONTEXT: u32 = 1;
    /// ModuleCache handle.
    pub const MODULE_CACHE: u32 = 2;
    /// CompilerConfig handle.
    pub const COMPILER_CONFIG: u32 = 3;
    /// ParseResult handle.
    pub const PARSE_RESULT: u32 = 4;
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_serialization_roundtrip() {
        let result = ParseResult::success(vec![1, 2, 3, 4]);
        let bytes = to_ffi_bytes(&result).unwrap();
        let decoded: ParseResult = from_ffi_bytes(&bytes).unwrap();

        assert_eq!(decoded.ast, vec![1, 2, 3, 4]);
        assert!(decoded.success);
    }

    #[test]
    fn test_ffi_buffer() {
        let mut buffer = FfiBuffer::new();

        let data = CompileRequest {
            source_path: "test.res".to_string(),
            source_code: "let x = 1".to_string(),
            output_path: None,
            is_interface: false,
        };

        buffer.write(&data).unwrap();
        let decoded: CompileRequest = buffer.read().unwrap();

        assert_eq!(decoded.source_path, "test.res");
        assert_eq!(decoded.source_code, "let x = 1");
    }

    #[test]
    fn test_ffi_status() {
        assert_eq!(FfiStatus::Ok as i32, 0);
        assert_eq!(FfiStatus::Error as i32, -1);
    }

    #[test]
    fn test_c_string_roundtrip() {
        let s = "hello world";
        let c_ptr = string_to_c(s);
        assert!(!c_ptr.is_null());

        let recovered = unsafe { c_to_string(c_ptr) }.unwrap();
        assert_eq!(recovered, s);

        unsafe { free_c_string(c_ptr) };
    }

    #[test]
    fn test_ffi_handle() {
        let value = "test string".to_string();
        let mut handle = FfiHandle::new(value, 100);

        assert!(handle.is_type(100));
        assert!(!handle.is_type(101));

        unsafe {
            let val: &String = handle.get(100).unwrap();
            assert_eq!(val, "test string");

            handle.free::<String>(100);
        }
    }

    #[test]
    fn test_error_report() {
        let error = ErrorReport::with_location("type mismatch", Location::in_file("test.res"));
        assert_eq!(error.message, "type mismatch");
        assert!(error.location.is_some());
    }
}
