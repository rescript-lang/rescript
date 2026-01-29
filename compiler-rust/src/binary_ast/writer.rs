//! Binary AST file writer
//!
//! This module provides functionality to write binary AST files (.ast, .iast)
//! that are byte-identical to those produced by the OCaml compiler.
//!
//! # Binary AST File Format
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
//! ├─────────────────────────────────────────────────────────────────────┤
//! │ Source path + '\n'                                                  │
//! ├─────────────────────────────────────────────────────────────────────┤
//! │ SECTION 3: OCaml Marshal Data                                       │
//! ├─────────────────────────────────────────────────────────────────────┤
//! │ Magic: 0x84 0x95 0xA6 0xBE (4 bytes)                                │
//! │ Header: data_len(4) + obj_count(4) + size_32(4) + size_64(4)        │
//! │ Payload: Encoded parsetree0 AST                                     │
//! └─────────────────────────────────────────────────────────────────────┘
//! ```

use std::fs::File;
use std::io::{self, BufWriter, Write};
use std::path::Path;

use super::deps::{extract_signature_deps, extract_structure_deps};
use super::mapper_to0::{map_signature, map_structure};
use super::marshal::MarshalWriter;
use super::serialize::Marshal;
use crate::parse_arena::ParseArena;
use crate::parser::ast::{Signature, Structure};

/// The separator character used in the binary AST format.
const MAGIC_SEP_CHAR: u8 = b'\n';

/// Write a binary AST file for a structure (implementation, .ast file).
///
/// # Arguments
///
/// * `output_path` - Path to the output .ast file
/// * `source_path` - Absolute path to the source file
/// * `arena` - The parse arena containing location data
/// * `ast` - The parsed structure AST
///
/// # Returns
///
/// Returns `Ok(())` on success, or an `io::Error` if writing fails.
pub fn write_structure_ast(
    output_path: &Path,
    source_path: &str,
    arena: &ParseArena,
    ast: &Structure,
) -> io::Result<()> {
    // 1. Extract dependencies
    let deps = extract_structure_deps(ast);

    // 2. Convert AST to parsetree0 format
    let ast0 = map_structure(arena, ast);

    // 3. Write the binary AST file
    write_ast_file(output_path, source_path, &deps, &ast0)
}

/// Write a binary AST file for a signature (interface, .iast file).
///
/// # Arguments
///
/// * `output_path` - Path to the output .iast file
/// * `source_path` - Absolute path to the source file
/// * `arena` - The parse arena containing location data
/// * `ast` - The parsed signature AST
///
/// # Returns
///
/// Returns `Ok(())` on success, or an `io::Error` if writing fails.
pub fn write_signature_ast(
    output_path: &Path,
    source_path: &str,
    arena: &ParseArena,
    ast: &Signature,
) -> io::Result<()> {
    // 1. Extract dependencies
    let deps = extract_signature_deps(ast);

    // 2. Convert AST to parsetree0 format
    let ast0 = map_signature(arena, ast);

    // 3. Write the binary AST file
    write_ast_file(output_path, source_path, &deps, &ast0)
}

/// Write a binary AST file for a structure in the CURRENT parsetree format.
///
/// This is different from `write_structure_ast` which converts to parsetree0.
/// The current format is what OCaml's `-bs-ast` flag outputs.
pub fn write_structure_ast_current(
    output_path: &Path,
    source_path: &str,
    ast: &Structure,
) -> io::Result<()> {
    // 1. Extract dependencies
    let deps = extract_structure_deps(ast);

    // 2. Write the binary AST file directly (no conversion to parsetree0)
    write_ast_file(output_path, source_path, &deps, ast)
}

/// Write a binary AST file for a signature in the CURRENT parsetree format.
pub fn write_signature_ast_current(
    output_path: &Path,
    source_path: &str,
    ast: &Signature,
) -> io::Result<()> {
    // 1. Extract dependencies
    let deps = extract_signature_deps(ast);

    // 2. Write the binary AST file directly (no conversion to parsetree0)
    write_ast_file(output_path, source_path, &deps, ast)
}

/// Write a binary AST file with the given dependencies and AST.
fn write_ast_file<A: Marshal>(
    output_path: &Path,
    source_path: &str,
    deps: &std::collections::BTreeSet<String>,
    ast: &A,
) -> io::Result<()> {
    // Build the dependency section
    let mut dep_buf = Vec::with_capacity(256);
    dep_buf.push(MAGIC_SEP_CHAR); // Leading '\n'
    for dep in deps {
        dep_buf.extend_from_slice(dep.as_bytes());
        dep_buf.push(MAGIC_SEP_CHAR);
    }

    // Marshal the AST
    let mut marshal_writer = MarshalWriter::new();
    ast.marshal(&mut marshal_writer);
    let marshal_data = marshal_writer.finish();

    // Open output file
    let file = File::create(output_path)?;
    let mut writer = BufWriter::new(file);

    // Write Section 1: Dependency section size (big-endian u32)
    let dep_size = dep_buf.len() as u32;
    writer.write_all(&dep_size.to_be_bytes())?;

    // Write Section 2: Dependencies
    writer.write_all(&dep_buf)?;

    // Write source path + newline
    writer.write_all(source_path.as_bytes())?;
    writer.write_all(&[MAGIC_SEP_CHAR])?;

    // Write Section 3: Marshal data
    writer.write_all(&marshal_data)?;

    writer.flush()?;
    Ok(())
}

/// Write a structure AST to a Vec<u8> (for testing).
pub fn write_structure_ast_to_vec(source_path: &str, arena: &ParseArena, ast: &Structure) -> Vec<u8> {
    let deps = extract_structure_deps(ast);
    let ast0 = map_structure(arena, ast);
    write_ast_to_vec(source_path, &deps, &ast0)
}

/// Write a signature AST to a Vec<u8> (for testing).
pub fn write_signature_ast_to_vec(source_path: &str, arena: &ParseArena, ast: &Signature) -> Vec<u8> {
    let deps = extract_signature_deps(ast);
    let ast0 = map_signature(arena, ast);
    write_ast_to_vec(source_path, &deps, &ast0)
}

/// Write an AST to a Vec<u8>.
fn write_ast_to_vec<A: Marshal>(
    source_path: &str,
    deps: &std::collections::BTreeSet<String>,
    ast: &A,
) -> Vec<u8> {
    let mut result = Vec::new();

    // Build the dependency section
    let mut dep_buf = Vec::with_capacity(256);
    dep_buf.push(MAGIC_SEP_CHAR);
    for dep in deps {
        dep_buf.extend_from_slice(dep.as_bytes());
        dep_buf.push(MAGIC_SEP_CHAR);
    }

    // Marshal the AST
    let mut marshal_writer = MarshalWriter::new();
    ast.marshal(&mut marshal_writer);
    let marshal_data = marshal_writer.finish();

    // Write Section 1: Dependency section size
    let dep_size = dep_buf.len() as u32;
    result.extend_from_slice(&dep_size.to_be_bytes());

    // Write Section 2: Dependencies
    result.extend_from_slice(&dep_buf);

    // Write source path + newline
    result.extend_from_slice(source_path.as_bytes());
    result.push(MAGIC_SEP_CHAR);

    // Write Section 3: Marshal data
    result.extend_from_slice(&marshal_data);

    result
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parse_arena::{LocIdx, Located};
    use crate::parser::ast::{
        Constant, Expression, ExpressionDesc, Pattern, PatternDesc, RecFlag, StructureItem,
        StructureItemDesc, ValueBinding,
    };
    use crate::parser::longident::Longident;

    fn make_simple_let_binding(name: &str, value: i32) -> StructureItem {
        StructureItem {
            pstr_desc: StructureItemDesc::Pstr_value(
                RecFlag::Nonrecursive,
                vec![ValueBinding {
                    pvb_pat: Pattern {
                        ppat_desc: PatternDesc::Ppat_var(Located {
                            txt: name.to_string(),
                            loc: LocIdx::none(),
                        }),
                        ppat_loc: LocIdx::none(),
                        ppat_attributes: vec![],
                    },
                    pvb_expr: Expression {
                        pexp_desc: ExpressionDesc::Pexp_constant(Constant::Integer(
                            value.to_string(),
                            None,
                        )),
                        pexp_loc: LocIdx::none(),
                        pexp_attributes: vec![],
                    },
                    pvb_attributes: vec![],
                    pvb_loc: LocIdx::none(),
                }],
            ),
            pstr_loc: LocIdx::none(),
        }
    }

    fn make_module_ref_expr(module: &str, name: &str) -> Expression {
        Expression {
            pexp_desc: ExpressionDesc::Pexp_ident(Located {
                txt: Longident::ldot(Longident::lident(module), name),
                loc: LocIdx::none(),
            }),
            pexp_loc: LocIdx::none(),
            pexp_attributes: vec![],
        }
    }

    fn make_test_arena() -> crate::parse_arena::ParseArena {
        crate::parse_arena::ParseArena::new()
    }

    #[test]
    fn test_empty_structure() {
        let arena = make_test_arena();
        let ast: Structure = vec![];
        let result = write_structure_ast_to_vec("/test/empty.res", &arena, &ast);

        // Check header
        let dep_size = u32::from_be_bytes([result[0], result[1], result[2], result[3]]);
        assert_eq!(dep_size, 1); // Just the leading '\n'

        // Check leading '\n'
        assert_eq!(result[4], b'\n');

        // Check source path
        let path_start = 5;
        let expected_path = "/test/empty.res\n";
        assert_eq!(
            &result[path_start..path_start + expected_path.len()],
            expected_path.as_bytes()
        );

        // Check marshal magic number
        let marshal_start = path_start + expected_path.len();
        assert_eq!(result[marshal_start], 0x84);
        assert_eq!(result[marshal_start + 1], 0x95);
        assert_eq!(result[marshal_start + 2], 0xA6);
        assert_eq!(result[marshal_start + 3], 0xBE);
    }

    #[test]
    fn test_simple_binding() {
        let arena = make_test_arena();
        let ast = vec![make_simple_let_binding("x", 42)];
        let result = write_structure_ast_to_vec("/test/simple.res", &arena, &ast);

        // Should have marshal magic number
        let dep_size = u32::from_be_bytes([result[0], result[1], result[2], result[3]]) as usize;
        let source_path = "/test/simple.res";
        let marshal_start = 4 + dep_size + source_path.len() + 1;

        assert_eq!(result[marshal_start], 0x84);
        assert_eq!(result[marshal_start + 1], 0x95);
        assert_eq!(result[marshal_start + 2], 0xA6);
        assert_eq!(result[marshal_start + 3], 0xBE);
    }

    #[test]
    fn test_with_dependencies() {
        let arena = make_test_arena();
        let ast = vec![StructureItem {
            pstr_desc: StructureItemDesc::Pstr_eval(make_module_ref_expr("Array", "map"), vec![]),
            pstr_loc: LocIdx::none(),
        }];

        let result = write_structure_ast_to_vec("/test/deps.res", &arena, &ast);

        // Check dependency section
        let dep_size = u32::from_be_bytes([result[0], result[1], result[2], result[3]]) as usize;

        // dep_size should include '\n' + "Array\n"
        assert_eq!(dep_size, 1 + "Array\n".len());

        // Check the dependency content
        assert_eq!(result[4], b'\n'); // Leading separator
        assert_eq!(&result[5..5 + 5], b"Array");
        assert_eq!(result[10], b'\n'); // Trailing separator
    }

    #[test]
    fn test_multiple_dependencies() {
        let arena = make_test_arena();
        let ast = vec![
            StructureItem {
                pstr_desc: StructureItemDesc::Pstr_eval(
                    make_module_ref_expr("Array", "map"),
                    vec![],
                ),
                pstr_loc: LocIdx::none(),
            },
            StructureItem {
                pstr_desc: StructureItemDesc::Pstr_eval(make_module_ref_expr("Js", "log"), vec![]),
                pstr_loc: LocIdx::none(),
            },
        ];

        let result = write_structure_ast_to_vec("/test/multi.res", &arena, &ast);

        let dep_size = u32::from_be_bytes([result[0], result[1], result[2], result[3]]) as usize;

        // Dependencies are sorted, so: '\n' + "Array\n" + "Js\n"
        assert_eq!(dep_size, 1 + "Array\n".len() + "Js\n".len());
    }

    #[test]
    fn test_source_path_preserved() {
        let arena = make_test_arena();
        let ast: Structure = vec![];
        let source_path = "/Users/dev/project/src/Main.res";
        let result = write_structure_ast_to_vec(source_path, &arena, &ast);

        let dep_size = u32::from_be_bytes([result[0], result[1], result[2], result[3]]) as usize;
        let path_start = 4 + dep_size;

        // Find the path in the output
        let expected_with_newline = format!("{}\n", source_path);
        assert_eq!(
            &result[path_start..path_start + expected_with_newline.len()],
            expected_with_newline.as_bytes()
        );
    }

    #[test]
    fn test_write_to_file() {
        use std::fs;

        // Use a temp file in the target directory
        let output_path = std::path::Path::new("target/test_binary_ast_writer.ast");

        let arena = make_test_arena();
        let ast = vec![make_simple_let_binding("x", 1)];
        write_structure_ast(output_path, "/test/test.res", &arena, &ast).unwrap();

        // Verify file exists and has content
        let content = fs::read(output_path).unwrap();
        assert!(!content.is_empty());

        // Verify marshal magic number is present
        let dep_size = u32::from_be_bytes([content[0], content[1], content[2], content[3]]) as usize;
        let source_path = "/test/test.res";
        let marshal_start = 4 + dep_size + source_path.len() + 1;

        assert_eq!(content[marshal_start], 0x84);
        assert_eq!(content[marshal_start + 1], 0x95);

        // Clean up
        let _ = fs::remove_file(output_path);
    }

    #[test]
    fn test_empty_signature() {
        let arena = make_test_arena();
        let ast: Signature = vec![];
        let result = write_signature_ast_to_vec("/test/empty.resi", &arena, &ast);

        // Should have valid structure
        let dep_size = u32::from_be_bytes([result[0], result[1], result[2], result[3]]);
        assert_eq!(dep_size, 1);

        // Check marshal magic
        let marshal_start = 4 + 1 + "/test/empty.resi\n".len();
        assert_eq!(result[marshal_start], 0x84);
    }
}
