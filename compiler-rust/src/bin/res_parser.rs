//! ReScript Parser CLI
//!
//! This CLI provides a drop-in replacement for the OCaml `res_parser` CLI,
//! enabling the Rust parser to run the existing syntax tests.
//!
//! Usage:
//!   res_parser_rust [options] <file>
//!
//! Options:
//!   -print <format>    Output format: ml, res, sexp, ast, comments, tokens
//!   -recover           Enable error recovery mode
//!   -interface         Parse as interface file
//!   -width <n>         Line width for printer (default: 100)
//!   -jsx-version <n>   JSX version (3 or 4)
//!   -jsx-module <mod>  JSX module (default: react)

use clap::Parser as ClapParser;
use rescript_compiler::parser::{Parser, Scanner, module, print_structure};
use std::fs;
use std::io::{self, Write};
use std::path::PathBuf;
use std::process;

/// ReScript Parser CLI - for testing purposes only
#[derive(ClapParser, Debug)]
#[command(name = "res_parser_rust")]
#[command(about = "ReScript parser CLI for testing - matches OCaml res_parser interface")]
struct Args {
    /// Input file to parse
    file: PathBuf,

    /// Output format: ml, res, sexp, ast, comments, tokens (default: res)
    #[arg(short = 'p', long = "print", default_value = "res")]
    print: String,

    /// Enable error recovery mode
    #[arg(short = 'r', long = "recover")]
    recover: bool,

    /// Parse as interface file (.resi)
    #[arg(short = 'i', long = "interface")]
    interface: bool,

    /// Line width for printer
    #[arg(short = 'w', long = "width", default_value = "100")]
    width: usize,

    /// JSX version (3 or 4)
    #[arg(long = "jsx-version", default_value = "-1")]
    jsx_version: i32,

    /// JSX module
    #[arg(long = "jsx-module", default_value = "react")]
    jsx_module: String,

    /// Parse for typechecker (not printer)
    #[arg(long = "typechecker")]
    typechecker: bool,

    /// Test AST conversion
    #[arg(long = "test-ast-conversion")]
    test_ast_conversion: bool,
}

fn main() {
    let args = Args::parse();

    // Read the input file
    let source = match fs::read_to_string(&args.file) {
        Ok(content) => content,
        Err(e) => {
            eprintln!("Error reading file {:?}: {}", args.file, e);
            process::exit(1);
        }
    };

    let filename = args.file.to_string_lossy().to_string();

    // Determine if we're parsing an interface
    let is_interface = args.interface || filename.ends_with(".resi") || filename.ends_with(".mli");

    // Parse the file
    let mut parser = Parser::new(&filename, &source);

    // Parse structure or signature
    if is_interface {
        let signature = module::parse_signature(&mut parser);

        // Check for errors
        if parser.has_errors() && !args.recover {
            print_diagnostics(&parser, &source, &filename);
            process::exit(1);
        }

        // Print in requested format
        match args.print.as_str() {
            "ml" => print_signature_ml(&signature, &mut io::stdout()),
            "res" => print_signature_res(&signature, &mut io::stdout()),
            "sexp" => print_signature_sexp(&signature, &mut io::stdout()),
            "ast" => print_signature_ast(&signature, &mut io::stdout()),
            "comments" => print_comments(&parser, &mut io::stdout()),
            "tokens" => print_tokens(&source, &filename, &mut io::stdout()),
            _ => {
                eprintln!("Unknown print format: {}", args.print);
                process::exit(1);
            }
        }
    } else {
        let structure = module::parse_structure(&mut parser);

        // Check for errors
        if parser.has_errors() && !args.recover {
            print_diagnostics(&parser, &source, &filename);
            process::exit(1);
        }

        // Print in requested format
        match args.print.as_str() {
            "ml" => print_structure_ml(&structure, &mut io::stdout()),
            "res" => {
                let output = print_structure(&structure);
                let _ = io::stdout().write_all(output.as_bytes());
                let _ = io::stdout().write_all(b"\n");
            }
            "sexp" => print_structure_sexp(&structure, &mut io::stdout()),
            "ast" => print_structure_ast(&structure, &mut io::stdout()),
            "comments" => print_comments(&parser, &mut io::stdout()),
            "tokens" => print_tokens(&source, &filename, &mut io::stdout()),
            _ => {
                eprintln!("Unknown print format: {}", args.print);
                process::exit(1);
            }
        }
    }
}

/// Print parser diagnostics
fn print_diagnostics(parser: &Parser, source: &str, filename: &str) {
    for diag in parser.diagnostics() {
        let col = diag.start_pos.cnum - diag.start_pos.bol;
        eprintln!(
            "{}:{}:{}: {:?}",
            filename,
            diag.start_pos.line,
            col + 1,
            diag.category
        );
        // Show the source line if available (line is 1-indexed)
        if let Some(line) = source.lines().nth((diag.start_pos.line - 1) as usize) {
            eprintln!("  {}", line);
        }
    }
}

/// Print comments from parser
fn print_comments(parser: &Parser, out: &mut impl Write) {
    for comment in parser.comments() {
        let _ = writeln!(out, "{:?}", comment);
    }
}

/// Print tokens from source
fn print_tokens(source: &str, filename: &str, out: &mut impl Write) {
    let mut scanner = Scanner::new(filename, source);
    loop {
        let result = scanner.scan();
        let start_col = result.start_pos.cnum - result.start_pos.bol;
        let end_col = result.end_pos.cnum - result.end_pos.bol;
        let _ = writeln!(
            out,
            "{:?} @ {}:{}-{}:{}",
            result.token,
            result.start_pos.line,
            start_col + 1,
            result.end_pos.line,
            end_col + 1
        );
        if matches!(result.token, rescript_compiler::parser::Token::Eof) {
            break;
        }
    }
}

// ============================================================================
// ML Printer (OCaml-compatible output)
// ============================================================================

use rescript_compiler::parser::ast::*;
use rescript_compiler::parser::longident::Longident;

/// Print structure in ML format (like Pprintast in OCaml)
fn print_structure_ml(structure: &Structure, out: &mut impl Write) {
    for (i, item) in structure.iter().enumerate() {
        if i > 0 {
            let _ = out.write_all(b" ");
        }
        print_structure_item_ml(item, out);
    }
    let _ = out.write_all(b"\n");
}

fn print_structure_item_ml(item: &StructureItem, out: &mut impl Write) {
    match &item.pstr_desc {
        StructureItemDesc::Pstr_eval(expr, _attrs) => {
            let _ = write!(out, ";;");
            print_expression_ml(expr, out);
        }
        StructureItemDesc::Pstr_value(rec_flag, bindings) => {
            let rec_str = match rec_flag {
                RecFlag::Recursive => " rec",
                RecFlag::Nonrecursive => "",
            };
            let _ = write!(out, "let{}", rec_str);
            for (i, binding) in bindings.iter().enumerate() {
                if i > 0 {
                    let _ = write!(out, " and");
                }
                let _ = write!(out, " ");
                print_pattern_ml(&binding.pvb_pat, out);
                let _ = write!(out, " = ");
                print_expression_ml(&binding.pvb_expr, out);
            }
        }
        StructureItemDesc::Pstr_primitive(vd) => {
            let _ = write!(out, "external {} : ", vd.pval_name.txt);
            print_core_type_ml(&vd.pval_type, out);
            for prim in &vd.pval_prim {
                let _ = write!(out, " = \"{}\"", prim);
            }
        }
        StructureItemDesc::Pstr_type(rec_flag, decls) => {
            let rec_str = match rec_flag {
                RecFlag::Recursive => "",
                RecFlag::Nonrecursive => " nonrec",
            };
            for (i, decl) in decls.iter().enumerate() {
                if i == 0 {
                    let _ = write!(out, "type{} ", rec_str);
                } else {
                    let _ = write!(out, " and ");
                }
                print_type_declaration_ml(decl, out);
            }
        }
        StructureItemDesc::Pstr_typext(ext) => {
            let _ = write!(out, "type ");
            print_longident(&ext.ptyext_path.txt, out);
            let _ = write!(out, " += ");
            for (i, ctor) in ext.ptyext_constructors.iter().enumerate() {
                if i > 0 {
                    let _ = write!(out, " | ");
                }
                print_extension_constructor_ml(ctor, out);
            }
        }
        StructureItemDesc::Pstr_exception(ext) => {
            let _ = write!(out, "exception ");
            print_extension_constructor_ml(ext, out);
        }
        StructureItemDesc::Pstr_module(mb) => {
            let _ = write!(out, "module {} = ", mb.pmb_name.txt);
            print_module_expr_ml(&mb.pmb_expr, out);
        }
        StructureItemDesc::Pstr_recmodule(mbs) => {
            for (i, mb) in mbs.iter().enumerate() {
                if i == 0 {
                    let _ = write!(out, "module rec {} = ", mb.pmb_name.txt);
                } else {
                    let _ = write!(out, " and {} = ", mb.pmb_name.txt);
                }
                print_module_expr_ml(&mb.pmb_expr, out);
            }
        }
        StructureItemDesc::Pstr_modtype(mtd) => {
            let _ = write!(out, "module type {}", mtd.pmtd_name.txt);
            if let Some(mt) = &mtd.pmtd_type {
                let _ = write!(out, " = ");
                print_module_type_ml(mt, out);
            }
        }
        StructureItemDesc::Pstr_open(od) => {
            let _ = write!(out, "open ");
            print_longident(&od.popen_lid.txt, out);
        }
        StructureItemDesc::Pstr_include(incl) => {
            let _ = write!(out, "include ");
            print_module_expr_ml(&incl.pincl_mod, out);
        }
        StructureItemDesc::Pstr_attribute((name, _payload)) => {
            let _ = write!(out, "[@@{}]", name.txt);
        }
        StructureItemDesc::Pstr_extension((name, _payload), _attrs) => {
            let _ = write!(out, "[%%{}]", name.txt);
        }
    }
}

fn print_expression_ml(expr: &Expression, out: &mut impl Write) {
    match &expr.pexp_desc {
        ExpressionDesc::Pexp_ident(lid) => {
            print_longident(&lid.txt, out);
        }
        ExpressionDesc::Pexp_constant(c) => {
            print_constant_ml(c, out);
        }
        ExpressionDesc::Pexp_let(rec_flag, bindings, body) => {
            let rec_str = match rec_flag {
                RecFlag::Recursive => " rec",
                RecFlag::Nonrecursive => "",
            };
            let _ = write!(out, "(let{}", rec_str);
            for (i, binding) in bindings.iter().enumerate() {
                if i > 0 {
                    let _ = write!(out, " and");
                }
                let _ = write!(out, " ");
                print_pattern_ml(&binding.pvb_pat, out);
                let _ = write!(out, " = ");
                print_expression_ml(&binding.pvb_expr, out);
            }
            let _ = write!(out, " in ");
            print_expression_ml(body, out);
            let _ = write!(out, ")");
        }
        ExpressionDesc::Pexp_fun {
            arg_label,
            default,
            lhs,
            rhs,
            ..
        } => {
            let _ = write!(out, "(fun ");
            print_arg_label_ml(arg_label, out);
            if let Some(def) = default {
                let _ = write!(out, "?(");
                print_pattern_ml(lhs, out);
                let _ = write!(out, " = ");
                print_expression_ml(def, out);
                let _ = write!(out, ")");
            } else {
                print_pattern_ml(lhs, out);
            }
            let _ = write!(out, " -> ");
            print_expression_ml(rhs, out);
            let _ = write!(out, ")");
        }
        ExpressionDesc::Pexp_apply { funct, args, .. } => {
            let _ = write!(out, "(");
            print_expression_ml(funct, out);
            for (label, arg) in args {
                let _ = write!(out, " ");
                print_arg_label_ml(label, out);
                print_expression_ml(arg, out);
            }
            let _ = write!(out, ")");
        }
        ExpressionDesc::Pexp_match(scrutinee, cases) => {
            let _ = write!(out, "(match ");
            print_expression_ml(scrutinee, out);
            let _ = write!(out, " with");
            for case in cases {
                let _ = write!(out, " | ");
                print_pattern_ml(&case.pc_lhs, out);
                if let Some(guard) = &case.pc_guard {
                    let _ = write!(out, " when ");
                    print_expression_ml(guard, out);
                }
                let _ = write!(out, " -> ");
                print_expression_ml(&case.pc_rhs, out);
            }
            let _ = write!(out, ")");
        }
        ExpressionDesc::Pexp_try(body, cases) => {
            let _ = write!(out, "(try ");
            print_expression_ml(body, out);
            let _ = write!(out, " with");
            for case in cases {
                let _ = write!(out, " | ");
                print_pattern_ml(&case.pc_lhs, out);
                if let Some(guard) = &case.pc_guard {
                    let _ = write!(out, " when ");
                    print_expression_ml(guard, out);
                }
                let _ = write!(out, " -> ");
                print_expression_ml(&case.pc_rhs, out);
            }
            let _ = write!(out, ")");
        }
        ExpressionDesc::Pexp_tuple(exprs) => {
            let _ = write!(out, "(");
            for (i, e) in exprs.iter().enumerate() {
                if i > 0 {
                    let _ = write!(out, ", ");
                }
                print_expression_ml(e, out);
            }
            let _ = write!(out, ")");
        }
        ExpressionDesc::Pexp_construct(lid, arg) => {
            print_longident(&lid.txt, out);
            if let Some(a) = arg {
                let _ = write!(out, " ");
                print_expression_ml(a, out);
            }
        }
        ExpressionDesc::Pexp_variant(label, arg) => {
            let _ = write!(out, "`{}", label);
            if let Some(a) = arg {
                let _ = write!(out, " ");
                print_expression_ml(a, out);
            }
        }
        ExpressionDesc::Pexp_record(fields, base) => {
            let _ = write!(out, "{{");
            if let Some(b) = base {
                print_expression_ml(b, out);
                let _ = write!(out, " with ");
            }
            for (i, field) in fields.iter().enumerate() {
                if i > 0 {
                    let _ = write!(out, "; ");
                }
                print_longident(&field.lid.txt, out);
                let _ = write!(out, " = ");
                print_expression_ml(&field.expr, out);
            }
            let _ = write!(out, "}}");
        }
        ExpressionDesc::Pexp_field(obj, field) => {
            print_expression_ml(obj, out);
            let _ = write!(out, ".");
            print_longident(&field.txt, out);
        }
        ExpressionDesc::Pexp_setfield(obj, field, value) => {
            print_expression_ml(obj, out);
            let _ = write!(out, ".");
            print_longident(&field.txt, out);
            let _ = write!(out, " <- ");
            print_expression_ml(value, out);
        }
        ExpressionDesc::Pexp_array(elems) => {
            let _ = write!(out, "[|");
            for (i, e) in elems.iter().enumerate() {
                if i > 0 {
                    let _ = write!(out, "; ");
                }
                print_expression_ml(e, out);
            }
            let _ = write!(out, "|]");
        }
        ExpressionDesc::Pexp_ifthenelse(cond, then_expr, else_expr) => {
            let _ = write!(out, "(if ");
            print_expression_ml(cond, out);
            let _ = write!(out, " then ");
            print_expression_ml(then_expr, out);
            if let Some(e) = else_expr {
                let _ = write!(out, " else ");
                print_expression_ml(e, out);
            }
            let _ = write!(out, ")");
        }
        ExpressionDesc::Pexp_sequence(e1, e2) => {
            let _ = write!(out, "(");
            print_expression_ml(e1, out);
            let _ = write!(out, "; ");
            print_expression_ml(e2, out);
            let _ = write!(out, ")");
        }
        ExpressionDesc::Pexp_while(cond, body) => {
            let _ = write!(out, "(while ");
            print_expression_ml(cond, out);
            let _ = write!(out, " do ");
            print_expression_ml(body, out);
            let _ = write!(out, " done)");
        }
        ExpressionDesc::Pexp_for(pat, start, end, dir, body) => {
            let _ = write!(out, "(for ");
            print_pattern_ml(pat, out);
            let _ = write!(out, " = ");
            print_expression_ml(start, out);
            let dir_str = match dir {
                DirectionFlag::Upto => " to ",
                DirectionFlag::Downto => " downto ",
            };
            let _ = write!(out, "{}", dir_str);
            print_expression_ml(end, out);
            let _ = write!(out, " do ");
            print_expression_ml(body, out);
            let _ = write!(out, " done)");
        }
        ExpressionDesc::Pexp_constraint(e, t) => {
            let _ = write!(out, "(");
            print_expression_ml(e, out);
            let _ = write!(out, " : ");
            print_core_type_ml(t, out);
            let _ = write!(out, ")");
        }
        ExpressionDesc::Pexp_coerce(e, t1, t2) => {
            let _ = write!(out, "(");
            print_expression_ml(e, out);
            if let Some(t) = t1 {
                let _ = write!(out, " : ");
                print_core_type_ml(t, out);
            }
            let _ = write!(out, " :> ");
            print_core_type_ml(t2, out);
            let _ = write!(out, ")");
        }
        ExpressionDesc::Pexp_send(e, meth) => {
            print_expression_ml(e, out);
            let _ = write!(out, "#{}", meth.txt);
        }
        ExpressionDesc::Pexp_letmodule(name, mexpr, body) => {
            let _ = write!(out, "(let module {} = ", name.txt);
            print_module_expr_ml(mexpr, out);
            let _ = write!(out, " in ");
            print_expression_ml(body, out);
            let _ = write!(out, ")");
        }
        ExpressionDesc::Pexp_letexception(ext, body) => {
            let _ = write!(out, "(let exception ");
            print_extension_constructor_ml(ext, out);
            let _ = write!(out, " in ");
            print_expression_ml(body, out);
            let _ = write!(out, ")");
        }
        ExpressionDesc::Pexp_assert(e) => {
            let _ = write!(out, "(assert ");
            print_expression_ml(e, out);
            let _ = write!(out, ")");
        }
        ExpressionDesc::Pexp_newtype(name, body) => {
            let _ = write!(out, "(fun (type {}) -> ", name.txt);
            print_expression_ml(body, out);
            let _ = write!(out, ")");
        }
        ExpressionDesc::Pexp_pack(mexpr) => {
            let _ = write!(out, "(module ");
            print_module_expr_ml(mexpr, out);
            let _ = write!(out, ")");
        }
        ExpressionDesc::Pexp_open(_override_flag, lid, body) => {
            let _ = write!(out, "(let open ");
            print_longident(&lid.txt, out);
            let _ = write!(out, " in ");
            print_expression_ml(body, out);
            let _ = write!(out, ")");
        }
        ExpressionDesc::Pexp_extension((name, _payload)) => {
            let _ = write!(out, "[%%{}]", name.txt);
        }
        ExpressionDesc::Pexp_await(e) => {
            let _ = write!(out, "(await ");
            print_expression_ml(e, out);
            let _ = write!(out, ")");
        }
        ExpressionDesc::Pexp_jsx_element(_jsx) => {
            let _ = write!(out, "<jsx>");
        }
    }
}

fn print_pattern_ml(pat: &Pattern, out: &mut impl Write) {
    match &pat.ppat_desc {
        PatternDesc::Ppat_any => {
            let _ = write!(out, "_");
        }
        PatternDesc::Ppat_var(name) => {
            let _ = write!(out, "{}", name.txt);
        }
        PatternDesc::Ppat_alias(p, name) => {
            let _ = write!(out, "(");
            print_pattern_ml(p, out);
            let _ = write!(out, " as {})", name.txt);
        }
        PatternDesc::Ppat_constant(c) => {
            print_constant_ml(c, out);
        }
        PatternDesc::Ppat_interval(c1, c2) => {
            print_constant_ml(c1, out);
            let _ = write!(out, " .. ");
            print_constant_ml(c2, out);
        }
        PatternDesc::Ppat_tuple(pats) => {
            let _ = write!(out, "(");
            for (i, p) in pats.iter().enumerate() {
                if i > 0 {
                    let _ = write!(out, ", ");
                }
                print_pattern_ml(p, out);
            }
            let _ = write!(out, ")");
        }
        PatternDesc::Ppat_construct(lid, arg) => {
            print_longident(&lid.txt, out);
            if let Some(a) = arg {
                let _ = write!(out, " ");
                print_pattern_ml(a, out);
            }
        }
        PatternDesc::Ppat_variant(label, arg) => {
            let _ = write!(out, "`{}", label);
            if let Some(a) = arg {
                let _ = write!(out, " ");
                print_pattern_ml(a, out);
            }
        }
        PatternDesc::Ppat_record(fields, closed) => {
            let _ = write!(out, "{{");
            for (i, field) in fields.iter().enumerate() {
                if i > 0 {
                    let _ = write!(out, "; ");
                }
                print_longident(&field.lid.txt, out);
                let _ = write!(out, " = ");
                print_pattern_ml(&field.pat, out);
            }
            if matches!(closed, ClosedFlag::Open) {
                let _ = write!(out, "; _");
            }
            let _ = write!(out, "}}");
        }
        PatternDesc::Ppat_array(pats) => {
            let _ = write!(out, "[|");
            for (i, p) in pats.iter().enumerate() {
                if i > 0 {
                    let _ = write!(out, "; ");
                }
                print_pattern_ml(p, out);
            }
            let _ = write!(out, "|]");
        }
        PatternDesc::Ppat_or(p1, p2) => {
            let _ = write!(out, "(");
            print_pattern_ml(p1, out);
            let _ = write!(out, " | ");
            print_pattern_ml(p2, out);
            let _ = write!(out, ")");
        }
        PatternDesc::Ppat_constraint(p, t) => {
            let _ = write!(out, "(");
            print_pattern_ml(p, out);
            let _ = write!(out, " : ");
            print_core_type_ml(t, out);
            let _ = write!(out, ")");
        }
        PatternDesc::Ppat_type(lid) => {
            let _ = write!(out, "#");
            print_longident(&lid.txt, out);
        }
        PatternDesc::Ppat_unpack(name) => {
            let _ = write!(out, "(module {})", name.txt);
        }
        PatternDesc::Ppat_exception(p) => {
            let _ = write!(out, "(exception ");
            print_pattern_ml(p, out);
            let _ = write!(out, ")");
        }
        PatternDesc::Ppat_extension((name, _)) => {
            let _ = write!(out, "[%%{}]", name.txt);
        }
        PatternDesc::Ppat_open(lid, p) => {
            print_longident(&lid.txt, out);
            let _ = write!(out, ".(");
            print_pattern_ml(p, out);
            let _ = write!(out, ")");
        }
    }
}

fn print_core_type_ml(typ: &CoreType, out: &mut impl Write) {
    match &typ.ptyp_desc {
        CoreTypeDesc::Ptyp_any => {
            let _ = write!(out, "_");
        }
        CoreTypeDesc::Ptyp_var(name) => {
            let _ = write!(out, "'{}", name);
        }
        CoreTypeDesc::Ptyp_arrow { arg, ret, .. } => {
            let _ = write!(out, "(");
            print_arg_label_ml(&arg.lbl, out);
            print_core_type_ml(&arg.typ, out);
            let _ = write!(out, " -> ");
            print_core_type_ml(ret, out);
            let _ = write!(out, ")");
        }
        CoreTypeDesc::Ptyp_tuple(types) => {
            let _ = write!(out, "(");
            for (i, t) in types.iter().enumerate() {
                if i > 0 {
                    let _ = write!(out, " * ");
                }
                print_core_type_ml(t, out);
            }
            let _ = write!(out, ")");
        }
        CoreTypeDesc::Ptyp_constr(lid, args) => {
            if !args.is_empty() {
                if args.len() == 1 {
                    print_core_type_ml(&args[0], out);
                    let _ = write!(out, " ");
                } else {
                    let _ = write!(out, "(");
                    for (i, t) in args.iter().enumerate() {
                        if i > 0 {
                            let _ = write!(out, ", ");
                        }
                        print_core_type_ml(t, out);
                    }
                    let _ = write!(out, ") ");
                }
            }
            print_longident(&lid.txt, out);
        }
        CoreTypeDesc::Ptyp_object(fields, closed) => {
            let _ = write!(out, "<");
            for (i, field) in fields.iter().enumerate() {
                if i > 0 {
                    let _ = write!(out, "; ");
                }
                match field {
                    ObjectField::Otag(name, _attrs, typ) => {
                        let _ = write!(out, "{}: ", name.txt);
                        print_core_type_ml(typ, out);
                    }
                    ObjectField::Oinherit(typ) => {
                        print_core_type_ml(typ, out);
                    }
                }
            }
            if matches!(closed, ClosedFlag::Open) {
                let _ = write!(out, "; ..");
            }
            let _ = write!(out, ">");
        }
        CoreTypeDesc::Ptyp_alias(t, name) => {
            let _ = write!(out, "(");
            print_core_type_ml(t, out);
            let _ = write!(out, " as '{})", name);
        }
        CoreTypeDesc::Ptyp_variant(rows, closed, labels) => {
            let _ = write!(out, "[");
            match closed {
                ClosedFlag::Open => {
                    let _ = write!(out, "> ");
                }
                ClosedFlag::Closed => {
                    if labels.is_some() {
                        let _ = write!(out, "< ");
                    }
                }
            }
            for (i, row) in rows.iter().enumerate() {
                if i > 0 {
                    let _ = write!(out, " | ");
                }
                match row {
                    RowField::Rtag(label, _attrs, empty, types) => {
                        let _ = write!(out, "`{}", label.txt);
                        if !*empty || !types.is_empty() {
                            let _ = write!(out, " of ");
                            for (j, t) in types.iter().enumerate() {
                                if j > 0 {
                                    let _ = write!(out, " & ");
                                }
                                print_core_type_ml(t, out);
                            }
                        }
                    }
                    RowField::Rinherit(typ) => {
                        print_core_type_ml(typ, out);
                    }
                }
            }
            if let Some(lbls) = labels {
                let _ = write!(out, " > ");
                for (i, lbl) in lbls.iter().enumerate() {
                    if i > 0 {
                        let _ = write!(out, " ");
                    }
                    let _ = write!(out, "`{}", lbl);
                }
            }
            let _ = write!(out, "]");
        }
        CoreTypeDesc::Ptyp_poly(vars, t) => {
            if !vars.is_empty() {
                for var in vars {
                    let _ = write!(out, "'{} ", var.txt);
                }
                let _ = write!(out, ". ");
            }
            print_core_type_ml(t, out);
        }
        CoreTypeDesc::Ptyp_package((lid, constraints)) => {
            let _ = write!(out, "(module ");
            print_longident(&lid.txt, out);
            for (path, typ) in constraints {
                let _ = write!(out, " with type ");
                print_longident(&path.txt, out);
                let _ = write!(out, " = ");
                print_core_type_ml(typ, out);
            }
            let _ = write!(out, ")");
        }
        CoreTypeDesc::Ptyp_extension((name, _)) => {
            let _ = write!(out, "[%%{}]", name.txt);
        }
    }
}

fn print_arg_label_ml(label: &ArgLabel, out: &mut impl Write) {
    match label {
        ArgLabel::Nolabel => {}
        ArgLabel::Labelled(s) => {
            let _ = write!(out, "~{}:", s);
        }
        ArgLabel::Optional(s) => {
            let _ = write!(out, "?{}:", s);
        }
    }
}

fn print_constant_ml(c: &Constant, out: &mut impl Write) {
    match c {
        Constant::Integer(s, suffix) => {
            let _ = write!(out, "{}", s);
            if let Some(c) = suffix {
                let _ = write!(out, "{}", c);
            }
        }
        Constant::Char(i) => {
            if let Some(c) = char::from_u32(*i as u32) {
                let _ = write!(out, "'{}'", c);
            } else {
                let _ = write!(out, "'\\{}'", i);
            }
        }
        Constant::String(s, _) => {
            let _ = write!(out, "\"{}\"", escape_string(s));
        }
        Constant::Float(s, suffix) => {
            let _ = write!(out, "{}", s);
            if let Some(c) = suffix {
                let _ = write!(out, "{}", c);
            }
        }
    }
}

fn escape_string(s: &str) -> String {
    let mut result = String::new();
    for c in s.chars() {
        match c {
            '\n' => result.push_str("\\n"),
            '\r' => result.push_str("\\r"),
            '\t' => result.push_str("\\t"),
            '\\' => result.push_str("\\\\"),
            '"' => result.push_str("\\\""),
            c => result.push(c),
        }
    }
    result
}

fn print_longident(lid: &Longident, out: &mut impl Write) {
    match lid {
        Longident::Lident(name) => {
            let _ = write!(out, "{}", name);
        }
        Longident::Ldot(prefix, name) => {
            print_longident(prefix, out);
            let _ = write!(out, ".{}", name);
        }
        Longident::Lapply(m1, m2) => {
            print_longident(m1, out);
            let _ = write!(out, "(");
            print_longident(m2, out);
            let _ = write!(out, ")");
        }
    }
}

fn print_type_declaration_ml(decl: &TypeDeclaration, out: &mut impl Write) {
    // Print type parameters
    if !decl.ptype_params.is_empty() {
        if decl.ptype_params.len() == 1 {
            print_core_type_ml(&decl.ptype_params[0].0, out);
            let _ = write!(out, " ");
        } else {
            let _ = write!(out, "(");
            for (i, (t, _)) in decl.ptype_params.iter().enumerate() {
                if i > 0 {
                    let _ = write!(out, ", ");
                }
                print_core_type_ml(t, out);
            }
            let _ = write!(out, ") ");
        }
    }
    let _ = write!(out, "{}", decl.ptype_name.txt);

    // Print manifest
    if let Some(manifest) = &decl.ptype_manifest {
        let _ = write!(out, " = ");
        if matches!(decl.ptype_private, PrivateFlag::Private) {
            let _ = write!(out, "private ");
        }
        print_core_type_ml(manifest, out);
    }

    // Print kind
    match &decl.ptype_kind {
        TypeKind::Ptype_abstract => {}
        TypeKind::Ptype_variant(ctors) => {
            let _ = write!(out, " = ");
            if matches!(decl.ptype_private, PrivateFlag::Private) {
                let _ = write!(out, "private ");
            }
            for (i, ctor) in ctors.iter().enumerate() {
                if i > 0 {
                    let _ = write!(out, " | ");
                }
                let _ = write!(out, "{}", ctor.pcd_name.txt);
                match &ctor.pcd_args {
                    ConstructorArguments::Pcstr_tuple(args) if !args.is_empty() => {
                        let _ = write!(out, " of ");
                        for (j, arg) in args.iter().enumerate() {
                            if j > 0 {
                                let _ = write!(out, " * ");
                            }
                            print_core_type_ml(arg, out);
                        }
                    }
                    ConstructorArguments::Pcstr_record(fields) => {
                        let _ = write!(out, " of {{");
                        for (j, field) in fields.iter().enumerate() {
                            if j > 0 {
                                let _ = write!(out, "; ");
                            }
                            if matches!(field.pld_mutable, MutableFlag::Mutable) {
                                let _ = write!(out, "mutable ");
                            }
                            let _ = write!(out, "{}: ", field.pld_name.txt);
                            print_core_type_ml(&field.pld_type, out);
                        }
                        let _ = write!(out, "}}");
                    }
                    _ => {}
                }
                if let Some(res) = &ctor.pcd_res {
                    let _ = write!(out, " : ");
                    print_core_type_ml(res, out);
                }
            }
        }
        TypeKind::Ptype_record(fields) => {
            let _ = write!(out, " = ");
            if matches!(decl.ptype_private, PrivateFlag::Private) {
                let _ = write!(out, "private ");
            }
            let _ = write!(out, "{{");
            for (i, field) in fields.iter().enumerate() {
                if i > 0 {
                    let _ = write!(out, "; ");
                }
                if matches!(field.pld_mutable, MutableFlag::Mutable) {
                    let _ = write!(out, "mutable ");
                }
                let _ = write!(out, "{}: ", field.pld_name.txt);
                print_core_type_ml(&field.pld_type, out);
            }
            let _ = write!(out, "}}");
        }
        TypeKind::Ptype_open => {
            let _ = write!(out, " = ..");
        }
    }
}

fn print_extension_constructor_ml(ext: &ExtensionConstructor, out: &mut impl Write) {
    let _ = write!(out, "{}", ext.pext_name.txt);
    match &ext.pext_kind {
        ExtensionConstructorKind::Pext_decl(args, res) => {
            match args {
                ConstructorArguments::Pcstr_tuple(args) if !args.is_empty() => {
                    let _ = write!(out, " of ");
                    for (i, arg) in args.iter().enumerate() {
                        if i > 0 {
                            let _ = write!(out, " * ");
                        }
                        print_core_type_ml(arg, out);
                    }
                }
                ConstructorArguments::Pcstr_record(fields) => {
                    let _ = write!(out, " of {{");
                    for (i, field) in fields.iter().enumerate() {
                        if i > 0 {
                            let _ = write!(out, "; ");
                        }
                        if matches!(field.pld_mutable, MutableFlag::Mutable) {
                            let _ = write!(out, "mutable ");
                        }
                        let _ = write!(out, "{}: ", field.pld_name.txt);
                        print_core_type_ml(&field.pld_type, out);
                    }
                    let _ = write!(out, "}}");
                }
                _ => {}
            }
            if let Some(r) = res {
                let _ = write!(out, " : ");
                print_core_type_ml(r, out);
            }
        }
        ExtensionConstructorKind::Pext_rebind(lid) => {
            let _ = write!(out, " = ");
            print_longident(&lid.txt, out);
        }
    }
}

fn print_module_expr_ml(mexpr: &ModuleExpr, out: &mut impl Write) {
    match &mexpr.pmod_desc {
        ModuleExprDesc::Pmod_ident(lid) => {
            print_longident(&lid.txt, out);
        }
        ModuleExprDesc::Pmod_structure(items) => {
            let _ = write!(out, "struct ");
            for item in items {
                print_structure_item_ml(item, out);
                let _ = write!(out, " ");
            }
            let _ = write!(out, "end");
        }
        ModuleExprDesc::Pmod_functor(name, mtype, body) => {
            let _ = write!(out, "functor (");
            let _ = write!(out, "{}", name.txt);
            if let Some(mt) = mtype {
                let _ = write!(out, " : ");
                print_module_type_ml(mt, out);
            }
            let _ = write!(out, ") -> ");
            print_module_expr_ml(body, out);
        }
        ModuleExprDesc::Pmod_apply(m1, m2) => {
            print_module_expr_ml(m1, out);
            let _ = write!(out, "(");
            print_module_expr_ml(m2, out);
            let _ = write!(out, ")");
        }
        ModuleExprDesc::Pmod_constraint(m, mt) => {
            let _ = write!(out, "(");
            print_module_expr_ml(m, out);
            let _ = write!(out, " : ");
            print_module_type_ml(mt, out);
            let _ = write!(out, ")");
        }
        ModuleExprDesc::Pmod_unpack(e) => {
            let _ = write!(out, "(val ");
            print_expression_ml(e, out);
            let _ = write!(out, ")");
        }
        ModuleExprDesc::Pmod_extension((name, _)) => {
            let _ = write!(out, "[%%{}]", name.txt);
        }
    }
}

fn print_module_type_ml(mtype: &ModuleType, out: &mut impl Write) {
    match &mtype.pmty_desc {
        ModuleTypeDesc::Pmty_ident(lid) => {
            print_longident(&lid.txt, out);
        }
        ModuleTypeDesc::Pmty_signature(items) => {
            let _ = write!(out, "sig ");
            for item in items {
                print_signature_item_ml(item, out);
                let _ = write!(out, " ");
            }
            let _ = write!(out, "end");
        }
        ModuleTypeDesc::Pmty_functor(name, arg_type, ret_type) => {
            let _ = write!(out, "functor (");
            let _ = write!(out, "{}", name.txt);
            if let Some(mt) = arg_type {
                let _ = write!(out, " : ");
                print_module_type_ml(mt, out);
            }
            let _ = write!(out, ") -> ");
            print_module_type_ml(ret_type, out);
        }
        ModuleTypeDesc::Pmty_with(mt, constraints) => {
            print_module_type_ml(mt, out);
            for constraint in constraints {
                match constraint {
                    WithConstraint::Pwith_type(lid, decl) => {
                        let _ = write!(out, " with type ");
                        print_longident(&lid.txt, out);
                        let _ = write!(out, " = ");
                        if let Some(manifest) = &decl.ptype_manifest {
                            print_core_type_ml(manifest, out);
                        }
                    }
                    WithConstraint::Pwith_module(lid1, lid2) => {
                        let _ = write!(out, " with module ");
                        print_longident(&lid1.txt, out);
                        let _ = write!(out, " = ");
                        print_longident(&lid2.txt, out);
                    }
                    WithConstraint::Pwith_typesubst(lid, decl) => {
                        let _ = write!(out, " with type ");
                        print_longident(&lid.txt, out);
                        let _ = write!(out, " := ");
                        if let Some(manifest) = &decl.ptype_manifest {
                            print_core_type_ml(manifest, out);
                        }
                    }
                    WithConstraint::Pwith_modsubst(lid1, lid2) => {
                        let _ = write!(out, " with module ");
                        print_longident(&lid1.txt, out);
                        let _ = write!(out, " := ");
                        print_longident(&lid2.txt, out);
                    }
                }
            }
        }
        ModuleTypeDesc::Pmty_typeof(mexpr) => {
            let _ = write!(out, "module type of ");
            print_module_expr_ml(mexpr, out);
        }
        ModuleTypeDesc::Pmty_extension((name, _)) => {
            let _ = write!(out, "[%%{}]", name.txt);
        }
        ModuleTypeDesc::Pmty_alias(lid) => {
            let _ = write!(out, "(module ");
            print_longident(&lid.txt, out);
            let _ = write!(out, ")");
        }
    }
}

// ============================================================================
// Signature ML Printer
// ============================================================================

fn print_signature_ml(signature: &[SignatureItem], out: &mut impl Write) {
    for (i, item) in signature.iter().enumerate() {
        if i > 0 {
            let _ = out.write_all(b" ");
        }
        print_signature_item_ml(item, out);
    }
    let _ = out.write_all(b"\n");
}

fn print_signature_item_ml(item: &SignatureItem, out: &mut impl Write) {
    match &item.psig_desc {
        SignatureItemDesc::Psig_value(vd) => {
            let _ = write!(out, "val {} : ", vd.pval_name.txt);
            print_core_type_ml(&vd.pval_type, out);
        }
        SignatureItemDesc::Psig_type(rec_flag, decls) => {
            let rec_str = match rec_flag {
                RecFlag::Recursive => "",
                RecFlag::Nonrecursive => " nonrec",
            };
            for (i, decl) in decls.iter().enumerate() {
                if i == 0 {
                    let _ = write!(out, "type{} ", rec_str);
                } else {
                    let _ = write!(out, " and ");
                }
                print_type_declaration_ml(decl, out);
            }
        }
        SignatureItemDesc::Psig_typext(ext) => {
            let _ = write!(out, "type ");
            print_longident(&ext.ptyext_path.txt, out);
            let _ = write!(out, " += ");
            for (i, ctor) in ext.ptyext_constructors.iter().enumerate() {
                if i > 0 {
                    let _ = write!(out, " | ");
                }
                print_extension_constructor_ml(ctor, out);
            }
        }
        SignatureItemDesc::Psig_exception(ext) => {
            let _ = write!(out, "exception ");
            print_extension_constructor_ml(ext, out);
        }
        SignatureItemDesc::Psig_module(md) => {
            let _ = write!(out, "module {} : ", md.pmd_name.txt);
            print_module_type_ml(&md.pmd_type, out);
        }
        SignatureItemDesc::Psig_recmodule(mds) => {
            for (i, md) in mds.iter().enumerate() {
                if i == 0 {
                    let _ = write!(out, "module rec {} : ", md.pmd_name.txt);
                } else {
                    let _ = write!(out, " and {} : ", md.pmd_name.txt);
                }
                print_module_type_ml(&md.pmd_type, out);
            }
        }
        SignatureItemDesc::Psig_modtype(mtd) => {
            let _ = write!(out, "module type {}", mtd.pmtd_name.txt);
            if let Some(mt) = &mtd.pmtd_type {
                let _ = write!(out, " = ");
                print_module_type_ml(mt, out);
            }
        }
        SignatureItemDesc::Psig_open(od) => {
            let _ = write!(out, "open ");
            print_longident(&od.popen_lid.txt, out);
        }
        SignatureItemDesc::Psig_include(incl) => {
            let _ = write!(out, "include ");
            print_module_type_ml(&incl.pincl_mod, out);
        }
        SignatureItemDesc::Psig_attribute((name, _)) => {
            let _ = write!(out, "[@@{}]", name.txt);
        }
        SignatureItemDesc::Psig_extension((name, _), _) => {
            let _ = write!(out, "[%%{}]", name.txt);
        }
    }
}

// ============================================================================
// ReScript Printer
// ============================================================================

fn print_signature_res(signature: &[SignatureItem], out: &mut impl Write) {
    // Use the built-in printer from the parser module
    // For now, just print as ML - the actual res printer needs more work
    print_signature_ml(signature, out);
}

// ============================================================================
// S-expression Printer (for roundtrip tests)
// ============================================================================

fn print_structure_sexp(structure: &Structure, out: &mut impl Write) {
    let _ = write!(out, "(");
    for (i, item) in structure.iter().enumerate() {
        if i > 0 {
            let _ = write!(out, " ");
        }
        print_structure_item_sexp(item, out);
    }
    let _ = write!(out, ")");
    let _ = out.write_all(b"\n");
}

fn print_structure_item_sexp(item: &StructureItem, out: &mut impl Write) {
    match &item.pstr_desc {
        StructureItemDesc::Pstr_eval(expr, _) => {
            let _ = write!(out, "(Pstr_eval ");
            print_expression_sexp(expr, out);
            let _ = write!(out, ")");
        }
        StructureItemDesc::Pstr_value(rec_flag, bindings) => {
            let rec_str = match rec_flag {
                RecFlag::Recursive => "Recursive",
                RecFlag::Nonrecursive => "Nonrecursive",
            };
            let _ = write!(out, "(Pstr_value {} (", rec_str);
            for (i, binding) in bindings.iter().enumerate() {
                if i > 0 {
                    let _ = write!(out, " ");
                }
                let _ = write!(out, "(");
                print_pattern_sexp(&binding.pvb_pat, out);
                let _ = write!(out, " ");
                print_expression_sexp(&binding.pvb_expr, out);
                let _ = write!(out, ")");
            }
            let _ = write!(out, "))");
        }
        StructureItemDesc::Pstr_primitive(vd) => {
            let _ = write!(out, "(Pstr_primitive {} ", vd.pval_name.txt);
            print_core_type_sexp(&vd.pval_type, out);
            let _ = write!(out, ")");
        }
        StructureItemDesc::Pstr_type(rec_flag, decls) => {
            let rec_str = match rec_flag {
                RecFlag::Recursive => "Recursive",
                RecFlag::Nonrecursive => "Nonrecursive",
            };
            let _ = write!(out, "(Pstr_type {} (", rec_str);
            for (i, decl) in decls.iter().enumerate() {
                if i > 0 {
                    let _ = write!(out, " ");
                }
                print_type_declaration_sexp(decl, out);
            }
            let _ = write!(out, "))");
        }
        StructureItemDesc::Pstr_typext(ext) => {
            let _ = write!(out, "(Pstr_typext ");
            print_longident_sexp(&ext.ptyext_path.txt, out);
            let _ = write!(out, ")");
        }
        StructureItemDesc::Pstr_exception(ext) => {
            let _ = write!(out, "(Pstr_exception {})", ext.pext_name.txt);
        }
        StructureItemDesc::Pstr_module(mb) => {
            let _ = write!(out, "(Pstr_module {} ", mb.pmb_name.txt);
            print_module_expr_sexp(&mb.pmb_expr, out);
            let _ = write!(out, ")");
        }
        StructureItemDesc::Pstr_recmodule(mbs) => {
            let _ = write!(out, "(Pstr_recmodule (");
            for (i, mb) in mbs.iter().enumerate() {
                if i > 0 {
                    let _ = write!(out, " ");
                }
                let _ = write!(out, "({} ", mb.pmb_name.txt);
                print_module_expr_sexp(&mb.pmb_expr, out);
                let _ = write!(out, ")");
            }
            let _ = write!(out, "))");
        }
        StructureItemDesc::Pstr_modtype(mtd) => {
            let _ = write!(out, "(Pstr_modtype {})", mtd.pmtd_name.txt);
        }
        StructureItemDesc::Pstr_open(od) => {
            let _ = write!(out, "(Pstr_open ");
            print_longident_sexp(&od.popen_lid.txt, out);
            let _ = write!(out, ")");
        }
        StructureItemDesc::Pstr_include(_) => {
            let _ = write!(out, "(Pstr_include)");
        }
        StructureItemDesc::Pstr_attribute((name, _)) => {
            let _ = write!(out, "(Pstr_attribute {})", name.txt);
        }
        StructureItemDesc::Pstr_extension((name, _), _) => {
            let _ = write!(out, "(Pstr_extension {})", name.txt);
        }
    }
}

fn print_expression_sexp(expr: &Expression, out: &mut impl Write) {
    match &expr.pexp_desc {
        ExpressionDesc::Pexp_ident(lid) => {
            let _ = write!(out, "(Pexp_ident ");
            print_longident_sexp(&lid.txt, out);
            let _ = write!(out, ")");
        }
        ExpressionDesc::Pexp_constant(c) => {
            let _ = write!(out, "(Pexp_constant ");
            print_constant_sexp(c, out);
            let _ = write!(out, ")");
        }
        ExpressionDesc::Pexp_let(rec_flag, bindings, body) => {
            let rec_str = match rec_flag {
                RecFlag::Recursive => "Recursive",
                RecFlag::Nonrecursive => "Nonrecursive",
            };
            let _ = write!(out, "(Pexp_let {} (", rec_str);
            for (i, binding) in bindings.iter().enumerate() {
                if i > 0 {
                    let _ = write!(out, " ");
                }
                let _ = write!(out, "(");
                print_pattern_sexp(&binding.pvb_pat, out);
                let _ = write!(out, " ");
                print_expression_sexp(&binding.pvb_expr, out);
                let _ = write!(out, ")");
            }
            let _ = write!(out, ") ");
            print_expression_sexp(body, out);
            let _ = write!(out, ")");
        }
        ExpressionDesc::Pexp_fun { lhs, rhs, .. } => {
            let _ = write!(out, "(Pexp_fun ");
            print_pattern_sexp(lhs, out);
            let _ = write!(out, " ");
            print_expression_sexp(rhs, out);
            let _ = write!(out, ")");
        }
        ExpressionDesc::Pexp_apply { funct, args, .. } => {
            let _ = write!(out, "(Pexp_apply ");
            print_expression_sexp(funct, out);
            let _ = write!(out, " (");
            for (i, (_, arg)) in args.iter().enumerate() {
                if i > 0 {
                    let _ = write!(out, " ");
                }
                print_expression_sexp(arg, out);
            }
            let _ = write!(out, "))");
        }
        ExpressionDesc::Pexp_match(scrutinee, cases) => {
            let _ = write!(out, "(Pexp_match ");
            print_expression_sexp(scrutinee, out);
            let _ = write!(out, " (");
            for (i, case) in cases.iter().enumerate() {
                if i > 0 {
                    let _ = write!(out, " ");
                }
                let _ = write!(out, "(");
                print_pattern_sexp(&case.pc_lhs, out);
                let _ = write!(out, " ");
                print_expression_sexp(&case.pc_rhs, out);
                let _ = write!(out, ")");
            }
            let _ = write!(out, "))");
        }
        ExpressionDesc::Pexp_try(body, cases) => {
            let _ = write!(out, "(Pexp_try ");
            print_expression_sexp(body, out);
            let _ = write!(out, " (");
            for (i, case) in cases.iter().enumerate() {
                if i > 0 {
                    let _ = write!(out, " ");
                }
                let _ = write!(out, "(");
                print_pattern_sexp(&case.pc_lhs, out);
                let _ = write!(out, " ");
                print_expression_sexp(&case.pc_rhs, out);
                let _ = write!(out, ")");
            }
            let _ = write!(out, "))");
        }
        ExpressionDesc::Pexp_tuple(exprs) => {
            let _ = write!(out, "(Pexp_tuple (");
            for (i, e) in exprs.iter().enumerate() {
                if i > 0 {
                    let _ = write!(out, " ");
                }
                print_expression_sexp(e, out);
            }
            let _ = write!(out, "))");
        }
        ExpressionDesc::Pexp_construct(lid, arg) => {
            let _ = write!(out, "(Pexp_construct ");
            print_longident_sexp(&lid.txt, out);
            if let Some(a) = arg {
                let _ = write!(out, " ");
                print_expression_sexp(a, out);
            }
            let _ = write!(out, ")");
        }
        ExpressionDesc::Pexp_variant(label, arg) => {
            let _ = write!(out, "(Pexp_variant {}", label);
            if let Some(a) = arg {
                let _ = write!(out, " ");
                print_expression_sexp(a, out);
            }
            let _ = write!(out, ")");
        }
        ExpressionDesc::Pexp_record(fields, base) => {
            let _ = write!(out, "(Pexp_record (");
            for (i, field) in fields.iter().enumerate() {
                if i > 0 {
                    let _ = write!(out, " ");
                }
                let _ = write!(out, "(");
                print_longident_sexp(&field.lid.txt, out);
                let _ = write!(out, " ");
                print_expression_sexp(&field.expr, out);
                let _ = write!(out, ")");
            }
            let _ = write!(out, ")");
            if let Some(b) = base {
                let _ = write!(out, " ");
                print_expression_sexp(b, out);
            }
            let _ = write!(out, ")");
        }
        ExpressionDesc::Pexp_field(obj, field) => {
            let _ = write!(out, "(Pexp_field ");
            print_expression_sexp(obj, out);
            let _ = write!(out, " ");
            print_longident_sexp(&field.txt, out);
            let _ = write!(out, ")");
        }
        ExpressionDesc::Pexp_setfield(obj, field, value) => {
            let _ = write!(out, "(Pexp_setfield ");
            print_expression_sexp(obj, out);
            let _ = write!(out, " ");
            print_longident_sexp(&field.txt, out);
            let _ = write!(out, " ");
            print_expression_sexp(value, out);
            let _ = write!(out, ")");
        }
        ExpressionDesc::Pexp_array(elems) => {
            let _ = write!(out, "(Pexp_array (");
            for (i, e) in elems.iter().enumerate() {
                if i > 0 {
                    let _ = write!(out, " ");
                }
                print_expression_sexp(e, out);
            }
            let _ = write!(out, "))");
        }
        ExpressionDesc::Pexp_ifthenelse(cond, then_expr, else_expr) => {
            let _ = write!(out, "(Pexp_ifthenelse ");
            print_expression_sexp(cond, out);
            let _ = write!(out, " ");
            print_expression_sexp(then_expr, out);
            if let Some(e) = else_expr {
                let _ = write!(out, " ");
                print_expression_sexp(e, out);
            }
            let _ = write!(out, ")");
        }
        ExpressionDesc::Pexp_sequence(e1, e2) => {
            let _ = write!(out, "(Pexp_sequence ");
            print_expression_sexp(e1, out);
            let _ = write!(out, " ");
            print_expression_sexp(e2, out);
            let _ = write!(out, ")");
        }
        ExpressionDesc::Pexp_while(cond, body) => {
            let _ = write!(out, "(Pexp_while ");
            print_expression_sexp(cond, out);
            let _ = write!(out, " ");
            print_expression_sexp(body, out);
            let _ = write!(out, ")");
        }
        ExpressionDesc::Pexp_for(pat, start, end, _dir, body) => {
            let _ = write!(out, "(Pexp_for ");
            print_pattern_sexp(pat, out);
            let _ = write!(out, " ");
            print_expression_sexp(start, out);
            let _ = write!(out, " ");
            print_expression_sexp(end, out);
            let _ = write!(out, " ");
            print_expression_sexp(body, out);
            let _ = write!(out, ")");
        }
        ExpressionDesc::Pexp_constraint(e, t) => {
            let _ = write!(out, "(Pexp_constraint ");
            print_expression_sexp(e, out);
            let _ = write!(out, " ");
            print_core_type_sexp(t, out);
            let _ = write!(out, ")");
        }
        ExpressionDesc::Pexp_coerce(e, t1, t2) => {
            let _ = write!(out, "(Pexp_coerce ");
            print_expression_sexp(e, out);
            if let Some(t) = t1 {
                let _ = write!(out, " ");
                print_core_type_sexp(t, out);
            }
            let _ = write!(out, " ");
            print_core_type_sexp(t2, out);
            let _ = write!(out, ")");
        }
        ExpressionDesc::Pexp_send(e, meth) => {
            let _ = write!(out, "(Pexp_send ");
            print_expression_sexp(e, out);
            let _ = write!(out, " {})", meth.txt);
        }
        ExpressionDesc::Pexp_letmodule(name, mexpr, body) => {
            let _ = write!(out, "(Pexp_letmodule {} ", name.txt);
            print_module_expr_sexp(mexpr, out);
            let _ = write!(out, " ");
            print_expression_sexp(body, out);
            let _ = write!(out, ")");
        }
        ExpressionDesc::Pexp_letexception(ext, body) => {
            let _ = write!(out, "(Pexp_letexception {} ", ext.pext_name.txt);
            print_expression_sexp(body, out);
            let _ = write!(out, ")");
        }
        ExpressionDesc::Pexp_assert(e) => {
            let _ = write!(out, "(Pexp_assert ");
            print_expression_sexp(e, out);
            let _ = write!(out, ")");
        }
        ExpressionDesc::Pexp_newtype(name, body) => {
            let _ = write!(out, "(Pexp_newtype {} ", name.txt);
            print_expression_sexp(body, out);
            let _ = write!(out, ")");
        }
        ExpressionDesc::Pexp_pack(mexpr) => {
            let _ = write!(out, "(Pexp_pack ");
            print_module_expr_sexp(mexpr, out);
            let _ = write!(out, ")");
        }
        ExpressionDesc::Pexp_open(_, lid, body) => {
            let _ = write!(out, "(Pexp_open ");
            print_longident_sexp(&lid.txt, out);
            let _ = write!(out, " ");
            print_expression_sexp(body, out);
            let _ = write!(out, ")");
        }
        ExpressionDesc::Pexp_extension((name, _)) => {
            let _ = write!(out, "(Pexp_extension {})", name.txt);
        }
        ExpressionDesc::Pexp_await(e) => {
            let _ = write!(out, "(Pexp_await ");
            print_expression_sexp(e, out);
            let _ = write!(out, ")");
        }
        ExpressionDesc::Pexp_jsx_element(_) => {
            let _ = write!(out, "(Pexp_jsx_element)");
        }
    }
}

fn print_pattern_sexp(pat: &Pattern, out: &mut impl Write) {
    match &pat.ppat_desc {
        PatternDesc::Ppat_any => {
            let _ = write!(out, "(Ppat_any)");
        }
        PatternDesc::Ppat_var(name) => {
            let _ = write!(out, "(Ppat_var {})", name.txt);
        }
        PatternDesc::Ppat_alias(p, name) => {
            let _ = write!(out, "(Ppat_alias ");
            print_pattern_sexp(p, out);
            let _ = write!(out, " {})", name.txt);
        }
        PatternDesc::Ppat_constant(c) => {
            let _ = write!(out, "(Ppat_constant ");
            print_constant_sexp(c, out);
            let _ = write!(out, ")");
        }
        PatternDesc::Ppat_interval(c1, c2) => {
            let _ = write!(out, "(Ppat_interval ");
            print_constant_sexp(c1, out);
            let _ = write!(out, " ");
            print_constant_sexp(c2, out);
            let _ = write!(out, ")");
        }
        PatternDesc::Ppat_tuple(pats) => {
            let _ = write!(out, "(Ppat_tuple (");
            for (i, p) in pats.iter().enumerate() {
                if i > 0 {
                    let _ = write!(out, " ");
                }
                print_pattern_sexp(p, out);
            }
            let _ = write!(out, "))");
        }
        PatternDesc::Ppat_construct(lid, arg) => {
            let _ = write!(out, "(Ppat_construct ");
            print_longident_sexp(&lid.txt, out);
            if let Some(a) = arg {
                let _ = write!(out, " ");
                print_pattern_sexp(a, out);
            }
            let _ = write!(out, ")");
        }
        PatternDesc::Ppat_variant(label, arg) => {
            let _ = write!(out, "(Ppat_variant {}", label);
            if let Some(a) = arg {
                let _ = write!(out, " ");
                print_pattern_sexp(a, out);
            }
            let _ = write!(out, ")");
        }
        PatternDesc::Ppat_record(fields, closed) => {
            let closed_str = match closed {
                ClosedFlag::Closed => "Closed",
                ClosedFlag::Open => "Open",
            };
            let _ = write!(out, "(Ppat_record {} (", closed_str);
            for (i, field) in fields.iter().enumerate() {
                if i > 0 {
                    let _ = write!(out, " ");
                }
                let _ = write!(out, "(");
                print_longident_sexp(&field.lid.txt, out);
                let _ = write!(out, " ");
                print_pattern_sexp(&field.pat, out);
                let _ = write!(out, ")");
            }
            let _ = write!(out, "))");
        }
        PatternDesc::Ppat_array(pats) => {
            let _ = write!(out, "(Ppat_array (");
            for (i, p) in pats.iter().enumerate() {
                if i > 0 {
                    let _ = write!(out, " ");
                }
                print_pattern_sexp(p, out);
            }
            let _ = write!(out, "))");
        }
        PatternDesc::Ppat_or(p1, p2) => {
            let _ = write!(out, "(Ppat_or ");
            print_pattern_sexp(p1, out);
            let _ = write!(out, " ");
            print_pattern_sexp(p2, out);
            let _ = write!(out, ")");
        }
        PatternDesc::Ppat_constraint(p, t) => {
            let _ = write!(out, "(Ppat_constraint ");
            print_pattern_sexp(p, out);
            let _ = write!(out, " ");
            print_core_type_sexp(t, out);
            let _ = write!(out, ")");
        }
        PatternDesc::Ppat_type(lid) => {
            let _ = write!(out, "(Ppat_type ");
            print_longident_sexp(&lid.txt, out);
            let _ = write!(out, ")");
        }
        PatternDesc::Ppat_unpack(name) => {
            let _ = write!(out, "(Ppat_unpack {})", name.txt);
        }
        PatternDesc::Ppat_exception(p) => {
            let _ = write!(out, "(Ppat_exception ");
            print_pattern_sexp(p, out);
            let _ = write!(out, ")");
        }
        PatternDesc::Ppat_extension((name, _)) => {
            let _ = write!(out, "(Ppat_extension {})", name.txt);
        }
        PatternDesc::Ppat_open(lid, p) => {
            let _ = write!(out, "(Ppat_open ");
            print_longident_sexp(&lid.txt, out);
            let _ = write!(out, " ");
            print_pattern_sexp(p, out);
            let _ = write!(out, ")");
        }
    }
}

fn print_core_type_sexp(typ: &CoreType, out: &mut impl Write) {
    match &typ.ptyp_desc {
        CoreTypeDesc::Ptyp_any => {
            let _ = write!(out, "(Ptyp_any)");
        }
        CoreTypeDesc::Ptyp_var(name) => {
            let _ = write!(out, "(Ptyp_var {})", name);
        }
        CoreTypeDesc::Ptyp_arrow { arg, ret, .. } => {
            let _ = write!(out, "(Ptyp_arrow ");
            print_core_type_sexp(&arg.typ, out);
            let _ = write!(out, " ");
            print_core_type_sexp(ret, out);
            let _ = write!(out, ")");
        }
        CoreTypeDesc::Ptyp_tuple(types) => {
            let _ = write!(out, "(Ptyp_tuple (");
            for (i, t) in types.iter().enumerate() {
                if i > 0 {
                    let _ = write!(out, " ");
                }
                print_core_type_sexp(t, out);
            }
            let _ = write!(out, "))");
        }
        CoreTypeDesc::Ptyp_constr(lid, args) => {
            let _ = write!(out, "(Ptyp_constr ");
            print_longident_sexp(&lid.txt, out);
            if !args.is_empty() {
                let _ = write!(out, " (");
                for (i, t) in args.iter().enumerate() {
                    if i > 0 {
                        let _ = write!(out, " ");
                    }
                    print_core_type_sexp(t, out);
                }
                let _ = write!(out, ")");
            }
            let _ = write!(out, ")");
        }
        CoreTypeDesc::Ptyp_object(_, _) => {
            let _ = write!(out, "(Ptyp_object)");
        }
        CoreTypeDesc::Ptyp_alias(t, name) => {
            let _ = write!(out, "(Ptyp_alias ");
            print_core_type_sexp(t, out);
            let _ = write!(out, " {})", name);
        }
        CoreTypeDesc::Ptyp_variant(_, _, _) => {
            let _ = write!(out, "(Ptyp_variant)");
        }
        CoreTypeDesc::Ptyp_poly(vars, t) => {
            let _ = write!(out, "(Ptyp_poly (");
            for (i, v) in vars.iter().enumerate() {
                if i > 0 {
                    let _ = write!(out, " ");
                }
                let _ = write!(out, "{}", v.txt);
            }
            let _ = write!(out, ") ");
            print_core_type_sexp(t, out);
            let _ = write!(out, ")");
        }
        CoreTypeDesc::Ptyp_package((lid, _)) => {
            let _ = write!(out, "(Ptyp_package ");
            print_longident_sexp(&lid.txt, out);
            let _ = write!(out, ")");
        }
        CoreTypeDesc::Ptyp_extension((name, _)) => {
            let _ = write!(out, "(Ptyp_extension {})", name.txt);
        }
    }
}

fn print_constant_sexp(c: &Constant, out: &mut impl Write) {
    match c {
        Constant::Integer(s, suffix) => {
            let _ = write!(out, "(Pconst_integer {}", s);
            if let Some(c) = suffix {
                let _ = write!(out, " {}", c);
            }
            let _ = write!(out, ")");
        }
        Constant::Char(i) => {
            let _ = write!(out, "(Pconst_char {})", i);
        }
        Constant::String(s, _) => {
            let _ = write!(out, "(Pconst_string \"{}\")", escape_string(s));
        }
        Constant::Float(s, suffix) => {
            let _ = write!(out, "(Pconst_float {}", s);
            if let Some(c) = suffix {
                let _ = write!(out, " {}", c);
            }
            let _ = write!(out, ")");
        }
    }
}

fn print_longident_sexp(lid: &Longident, out: &mut impl Write) {
    match lid {
        Longident::Lident(name) => {
            let _ = write!(out, "(Lident {})", name);
        }
        Longident::Ldot(prefix, name) => {
            let _ = write!(out, "(Ldot ");
            print_longident_sexp(prefix, out);
            let _ = write!(out, " {})", name);
        }
        Longident::Lapply(m1, m2) => {
            let _ = write!(out, "(Lapply ");
            print_longident_sexp(m1, out);
            let _ = write!(out, " ");
            print_longident_sexp(m2, out);
            let _ = write!(out, ")");
        }
    }
}

fn print_type_declaration_sexp(decl: &TypeDeclaration, out: &mut impl Write) {
    let _ = write!(out, "(type_declaration {})", decl.ptype_name.txt);
}

fn print_module_expr_sexp(mexpr: &ModuleExpr, out: &mut impl Write) {
    match &mexpr.pmod_desc {
        ModuleExprDesc::Pmod_ident(lid) => {
            let _ = write!(out, "(Pmod_ident ");
            print_longident_sexp(&lid.txt, out);
            let _ = write!(out, ")");
        }
        ModuleExprDesc::Pmod_structure(items) => {
            let _ = write!(out, "(Pmod_structure (");
            for (i, item) in items.iter().enumerate() {
                if i > 0 {
                    let _ = write!(out, " ");
                }
                print_structure_item_sexp(item, out);
            }
            let _ = write!(out, "))");
        }
        ModuleExprDesc::Pmod_functor(name, _mtype, body) => {
            let _ = write!(out, "(Pmod_functor {} ", name.txt);
            print_module_expr_sexp(body, out);
            let _ = write!(out, ")");
        }
        ModuleExprDesc::Pmod_apply(m1, m2) => {
            let _ = write!(out, "(Pmod_apply ");
            print_module_expr_sexp(m1, out);
            let _ = write!(out, " ");
            print_module_expr_sexp(m2, out);
            let _ = write!(out, ")");
        }
        ModuleExprDesc::Pmod_constraint(m, _mt) => {
            let _ = write!(out, "(Pmod_constraint ");
            print_module_expr_sexp(m, out);
            let _ = write!(out, ")");
        }
        ModuleExprDesc::Pmod_unpack(e) => {
            let _ = write!(out, "(Pmod_unpack ");
            print_expression_sexp(e, out);
            let _ = write!(out, ")");
        }
        ModuleExprDesc::Pmod_extension((name, _)) => {
            let _ = write!(out, "(Pmod_extension {})", name.txt);
        }
    }
}

fn print_signature_sexp(signature: &[SignatureItem], out: &mut impl Write) {
    let _ = write!(out, "(");
    for (i, item) in signature.iter().enumerate() {
        if i > 0 {
            let _ = write!(out, " ");
        }
        print_signature_item_sexp(item, out);
    }
    let _ = write!(out, ")");
    let _ = out.write_all(b"\n");
}

fn print_signature_item_sexp(item: &SignatureItem, out: &mut impl Write) {
    match &item.psig_desc {
        SignatureItemDesc::Psig_value(vd) => {
            let _ = write!(out, "(Psig_value {} ", vd.pval_name.txt);
            print_core_type_sexp(&vd.pval_type, out);
            let _ = write!(out, ")");
        }
        SignatureItemDesc::Psig_type(rec_flag, decls) => {
            let rec_str = match rec_flag {
                RecFlag::Recursive => "Recursive",
                RecFlag::Nonrecursive => "Nonrecursive",
            };
            let _ = write!(out, "(Psig_type {} (", rec_str);
            for (i, decl) in decls.iter().enumerate() {
                if i > 0 {
                    let _ = write!(out, " ");
                }
                print_type_declaration_sexp(decl, out);
            }
            let _ = write!(out, "))");
        }
        SignatureItemDesc::Psig_typext(ext) => {
            let _ = write!(out, "(Psig_typext ");
            print_longident_sexp(&ext.ptyext_path.txt, out);
            let _ = write!(out, ")");
        }
        SignatureItemDesc::Psig_exception(ext) => {
            let _ = write!(out, "(Psig_exception {})", ext.pext_name.txt);
        }
        SignatureItemDesc::Psig_module(md) => {
            let _ = write!(out, "(Psig_module {})", md.pmd_name.txt);
        }
        SignatureItemDesc::Psig_recmodule(mds) => {
            let _ = write!(out, "(Psig_recmodule (");
            for (i, md) in mds.iter().enumerate() {
                if i > 0 {
                    let _ = write!(out, " ");
                }
                let _ = write!(out, "{}", md.pmd_name.txt);
            }
            let _ = write!(out, "))");
        }
        SignatureItemDesc::Psig_modtype(mtd) => {
            let _ = write!(out, "(Psig_modtype {})", mtd.pmtd_name.txt);
        }
        SignatureItemDesc::Psig_open(od) => {
            let _ = write!(out, "(Psig_open ");
            print_longident_sexp(&od.popen_lid.txt, out);
            let _ = write!(out, ")");
        }
        SignatureItemDesc::Psig_include(_) => {
            let _ = write!(out, "(Psig_include)");
        }
        SignatureItemDesc::Psig_attribute((name, _)) => {
            let _ = write!(out, "(Psig_attribute {})", name.txt);
        }
        SignatureItemDesc::Psig_extension((name, _), _) => {
            let _ = write!(out, "(Psig_extension {})", name.txt);
        }
    }
}

// ============================================================================
// AST Debug Printer
// ============================================================================

fn print_structure_ast(structure: &Structure, out: &mut impl Write) {
    let _ = writeln!(out, "{:#?}", structure);
}

fn print_signature_ast(signature: &[SignatureItem], out: &mut impl Write) {
    let _ = writeln!(out, "{:#?}", signature);
}
