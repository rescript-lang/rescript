//! ML Printer (OCaml-compatible output)
//!
//! This module provides printing functions that output AST in a format
//! compatible with OCaml's Pprintast module. Uses the OCaml-compatible
//! Format module for proper line breaking with box-based formatting.

use std::io::Write;

use crate::parse_arena::{LidentIdx, Located, ParseArena};
use super::ast::*;
use super::formatter::{BoxKind, Formatter};
use super::longident::Longident;
use super::parsetree_viewer;

// ============================================================================
// Print context (matching OCaml's pprintast ctxt)
// ============================================================================

/// Context for expression printing, matching OCaml's pprintast `ctxt` type.
/// Determines when extra parentheses are needed for disambiguation.
#[derive(Debug, Clone, Copy, Default)]
struct PrintCtx {
    pipe: bool,
    semi: bool,
    ifthenelse: bool,
}

impl PrintCtx {
    fn reset() -> Self {
        Self { pipe: false, semi: false, ifthenelse: false }
    }
    fn under_pipe(self) -> Self {
        Self { pipe: true, ..self }
    }
    fn under_semi(self) -> Self {
        Self { semi: true, ..self }
    }
    fn under_ifthenelse(self) -> Self {
        Self { ifthenelse: true, ..self }
    }
}

// ============================================================================
// Helper functions
// ============================================================================

/// Count the arity of nested Pexp_fun expressions
fn count_function_arity(expr: &Expression) -> usize {
    match &expr.pexp_desc {
        ExpressionDesc::Pexp_fun { rhs, .. } => 1 + count_function_arity(rhs),
        _ => 0,
    }
}

/// Check if pattern is a simple variable with given name (and no attributes)
fn pattern_is_simple_var(pat: &Pattern, name: &str) -> bool {
    if !pat.ppat_attributes.is_empty() {
        return false;
    }
    match &pat.ppat_desc {
        PatternDesc::Ppat_var(var) => var.txt == name,
        _ => false,
    }
}

/// Check if pattern needs parens when used as a function parameter
fn pattern_needs_parens_as_param(pat: &Pattern, arena: &ParseArena) -> bool {
    match &pat.ppat_desc {
        PatternDesc::Ppat_construct(lid, arg) => {
            match arena.get_longident(lid.txt) {
                // () and [] don't need parens, but :: with args does
                Longident::Lident(name_idx) => {
                    let name = arena.get_string(*name_idx);
                    if name == "()" || name == "[]" {
                        false
                    } else if name == "::" {
                        arg.is_some()
                    } else {
                        true
                    }
                }
                _ => true,
            }
        }
        PatternDesc::Ppat_variant(_, Some(_)) => true,
        PatternDesc::Ppat_alias(_, _) => true,
        PatternDesc::Ppat_or(_, _) => true,
        _ => false,
    }
}

/// Check if a pattern is "simple" according to OCaml's pprintast.
/// Simple patterns can appear as arguments to variants/constructors without parens.
/// Based on pprintast.ml's simple_pattern function.
fn pattern_is_simple(pat: &Pattern, arena: &ParseArena) -> bool {
    // In OCaml, simple_pattern delegates to pattern when attributes are present.
    // pattern adds ((...)attrs) wrapping, so no extra parens needed.
    if !pat.ppat_attributes.is_empty() {
        return true;
    }
    match &pat.ppat_desc {
        // These are all handled directly by simple_pattern:
        PatternDesc::Ppat_any => true,
        PatternDesc::Ppat_var(_) => true,
        PatternDesc::Ppat_array(_) => true,
        PatternDesc::Ppat_unpack(_) => true,
        PatternDesc::Ppat_type(_) => true,
        PatternDesc::Ppat_record(_, _) => true,
        PatternDesc::Ppat_tuple(_) => true,
        PatternDesc::Ppat_constant(_) => true,
        PatternDesc::Ppat_interval(_, _) => true,
        PatternDesc::Ppat_variant(_, None) => true,  // Only variant without arg
        PatternDesc::Ppat_constraint(_, _) => true,
        PatternDesc::Ppat_exception(_) => true,
        PatternDesc::Ppat_extension(_) => true,
        PatternDesc::Ppat_open(_, _) => true,
        PatternDesc::Ppat_construct(lid, _) => {
            // () and [] are simple, others are not
            match arena.get_longident(lid.txt) {
                Longident::Lident(name_idx) => {
                    let name = arena.get_string(*name_idx);
                    name == "()" || name == "[]"
                }
                _ => false,
            }
        }
        // These are NOT simple - need parens:
        PatternDesc::Ppat_alias(_, _) => false,
        PatternDesc::Ppat_or(_, _) => false,
        PatternDesc::Ppat_variant(_, Some(_)) => false,
    }
}

/// Check if a payload is empty (empty PStr)
fn payload_is_empty(payload: &Payload) -> bool {
    matches!(payload, Payload::PStr(items) if items.is_empty())
}

/// Check if a constant is negative (starts with -)
fn constant_is_negative(c: &Constant) -> bool {
    match c {
        Constant::Integer(s, _) | Constant::Float(s, _) => s.starts_with('-'),
        _ => false,
    }
}

/// Check if a pattern is a cons pattern (::)
fn is_cons_pattern(p: &Pattern, arena: &ParseArena) -> bool {
    if let PatternDesc::Ppat_construct(lid, _) = &p.ppat_desc {
        if let Longident::Lident(name_idx) = arena.get_longident(lid.txt) {
            let name = arena.get_string(*name_idx);
            return name == "::";
        }
    }
    false
}

/// Check if a string represents an infix operator
fn is_infix_operator(name: &str) -> bool {
    const INFIX_SPECIALS: &[&str] =
        &[":=", "!=", "::", "mod", "land", "lor", "lxor", "lsl", "lsr", "asr", "or"];
    if INFIX_SPECIALS.contains(&name) {
        return true;
    }
    const INFIX_SYMBOLS: &[char] = &[
        '=', '<', '>', '@', '^', '|', '&', '+', '-', '*', '/', '$', '%', '#',
    ];
    if let Some(c) = name.chars().next() {
        if INFIX_SYMBOLS.contains(&c) {
            return true;
        }
    }
    false
}

/// Check if a string represents a prefix operator
fn is_prefix_operator(name: &str) -> bool {
    const PREFIX_SYMBOLS: &[char] = &['!', '?', '~'];
    if let Some(c) = name.chars().next() {
        PREFIX_SYMBOLS.contains(&c)
    } else {
        false
    }
}

/// Check if an identifier needs parens when printed as a value
/// (OCaml wraps operators in parens when used as values)
fn ident_needs_parens(name: &str) -> bool {
    is_infix_operator(name) || is_prefix_operator(name)
}

/// Get operator name if expression is an identifier that is an operator
fn get_operator_name<'a>(expr: &Expression, arena: &'a ParseArena) -> Option<&'a str> {
    match &expr.pexp_desc {
        ExpressionDesc::Pexp_ident(lid) => match arena.get_longident(lid.txt) {
            Longident::Lident(name_idx) => Some(arena.get_string(*name_idx)),
            _ => None,
        },
        _ => None,
    }
}

/// Check if expression needs parens in binary context
fn needs_parens_in_binary_context(expr: &Expression) -> bool {
    match &expr.pexp_desc {
        ExpressionDesc::Pexp_ifthenelse(_, _, _)
        | ExpressionDesc::Pexp_let(_, _, _)
        | ExpressionDesc::Pexp_fun { .. }
        | ExpressionDesc::Pexp_match(_, _)
        | ExpressionDesc::Pexp_try(_, _)
        | ExpressionDesc::Pexp_sequence(_, _)
        | ExpressionDesc::Pexp_await(_) => true,
        // Note: for/while have do...done delimiters so don't need extra parens
        ExpressionDesc::Pexp_apply { args, .. } if !args.is_empty() => true,
        _ => false,
    }
}

/// Check if expression is an infix operator application
fn is_infix_application(expr: &Expression, arena: &ParseArena) -> bool {
    if let ExpressionDesc::Pexp_apply { funct, args, .. } = &expr.pexp_desc {
        if args.len() == 2 {
            if let Some(op_name) = get_operator_name(funct, arena) {
                return is_infix_operator(op_name);
            }
        }
    }
    false
}

/// Check if expression is a prefix operator application
fn is_prefix_application(expr: &Expression, arena: &ParseArena) -> bool {
    if let ExpressionDesc::Pexp_apply { funct, args, .. } = &expr.pexp_desc {
        if args.len() == 1 {
            if let Some(op_name) = get_operator_name(funct, arena) {
                return is_prefix_operator(op_name);
            }
        }
    }
    false
}

/// Check if a function application has labeled arguments
fn application_has_labeled_args(expr: &Expression) -> bool {
    if let ExpressionDesc::Pexp_apply { args, .. } = &expr.pexp_desc {
        args.iter().any(|(label, _)| !matches!(label, ArgLabel::Nolabel))
    } else {
        false
    }
}

/// Check if expression needs parens when used as a labeled argument value
fn needs_parens_as_labeled_arg(expr: &Expression, arena: &ParseArena) -> bool {
    if is_prefix_application(expr, arena) {
        return false;
    }
    needs_parens_in_binary_context(expr) || is_infix_application(expr, arena)
}

/// Check if expression needs parens when used as an unlabeled function argument
fn needs_parens_as_function_arg(expr: &Expression, arena: &ParseArena) -> bool {
    if is_prefix_application(expr, arena) {
        return false;
    }
    needs_parens_in_binary_context(expr) || application_has_labeled_args(expr)
}

/// Check if expression already prints its own parens
fn expression_has_own_parens(expr: &Expression) -> bool {
    matches!(&expr.pexp_desc, ExpressionDesc::Pexp_assert(_))
}

/// Check if a Pexp_construct is a "simple construct" (like OCaml's is_simple_construct)
/// This includes: [], (), complete lists [a; b; c], and simple constructors without args
fn is_simple_construct(expr: &Expression, arena: &ParseArena) -> bool {
    if !expr.pexp_attributes.is_empty() {
        return false;
    }
    match &expr.pexp_desc {
        ExpressionDesc::Pexp_construct(lid, None) => {
            // () and [] and simple constructors are simple
            true
        }
        ExpressionDesc::Pexp_construct(lid, Some(_)) => {
            // Check if it's a complete list (ends with [])
            if let Longident::Lident(name_idx) = arena.get_longident(lid.txt) {
                let name = arena.get_string(*name_idx);
                if name == "::" {
                    let (_, is_complete) = collect_list_elements(expr, arena);
                    return is_complete;
                }
            }
            false
        }
        _ => false,
    }
}

/// Check if expression is "simple" - doesn't need parens
/// Note: This version doesn't have arena access, so it can't check complete lists.
/// Use is_simple_expression_with_arena for full checking.
fn is_simple_expression(expr: &Expression) -> bool {
    if !expr.pexp_attributes.is_empty() {
        return false;
    }
    matches!(
        &expr.pexp_desc,
        ExpressionDesc::Pexp_ident(_)
            | ExpressionDesc::Pexp_constant(_)
            | ExpressionDesc::Pexp_tuple(_)
            | ExpressionDesc::Pexp_array(_)
            | ExpressionDesc::Pexp_record(_, _)
            | ExpressionDesc::Pexp_constraint(_, _)
            | ExpressionDesc::Pexp_coerce(_, _, _)
            | ExpressionDesc::Pexp_variant(_, None)
            | ExpressionDesc::Pexp_construct(_, None)
            | ExpressionDesc::Pexp_pack(_)
            // NOTE: Pexp_field and Pexp_send are NOT simple in OCaml's pprintast.
            // They are in expression2, which means they need parens when nested.
            // See expression2 vs simple_expr in pprintast.ml
            // OCaml's simple_expr handles these directly (no paren wrapping)
            | ExpressionDesc::Pexp_for(_, _, _, _, _)
            | ExpressionDesc::Pexp_while(_, _)
            | ExpressionDesc::Pexp_jsx_element(_)  // JSX elements are simple
    )
}

/// Check if expression is "simple" - doesn't need parens (with arena for complete list checking)
fn is_simple_expression_with_arena(expr: &Expression, arena: &ParseArena) -> bool {
    if !expr.pexp_attributes.is_empty() {
        return false;
    }
    // Check for simple constructs first (handles complete lists)
    if is_simple_construct(expr, arena) {
        return true;
    }
    matches!(
        &expr.pexp_desc,
        ExpressionDesc::Pexp_ident(_)
            | ExpressionDesc::Pexp_constant(_)
            | ExpressionDesc::Pexp_tuple(_)
            | ExpressionDesc::Pexp_array(_)
            | ExpressionDesc::Pexp_record(_, _)
            | ExpressionDesc::Pexp_constraint(_, _)
            | ExpressionDesc::Pexp_coerce(_, _, _)
            | ExpressionDesc::Pexp_variant(_, None)
            | ExpressionDesc::Pexp_pack(_)
            // NOTE: Pexp_field and Pexp_send are NOT simple in OCaml's pprintast.
            // They are in expression2, which means they need parens when nested.
            // OCaml's simple_expr handles these directly (no paren wrapping)
            | ExpressionDesc::Pexp_for(_, _, _, _, _)
            | ExpressionDesc::Pexp_while(_, _)
            | ExpressionDesc::Pexp_jsx_element(_)  // JSX elements are simple in OCaml
    )
}

/// Check if expression needs parens in "semi" context (array/list elements)
/// OCaml: when ctxt.semi is true and expr is Pexp_fun/match/try/sequence, wrap in parens
fn needs_parens_in_semi_context(expr: &Expression) -> bool {
    matches!(
        &expr.pexp_desc,
        ExpressionDesc::Pexp_fun { .. }
            | ExpressionDesc::Pexp_match(_, _)
            | ExpressionDesc::Pexp_try(_, _)
            | ExpressionDesc::Pexp_sequence(_, _)
    )
}

/// Check if expression needs parens in "pipe" context (match case RHS)
/// OCaml: when ctxt.pipe is true and expr is Pexp_fun/match/try/sequence, wrap in parens
/// This is the same set of expressions as semi context
fn needs_parens_in_pipe_context(expr: &Expression) -> bool {
    matches!(
        &expr.pexp_desc,
        ExpressionDesc::Pexp_fun { .. }
            | ExpressionDesc::Pexp_match(_, _)
            | ExpressionDesc::Pexp_try(_, _)
            | ExpressionDesc::Pexp_sequence(_, _)
    )
}

/// Check if expression needs parens in "ifthenelse" context
/// OCaml: when ctxt.ifthenelse is true and expr is Pexp_ifthenelse/Pexp_sequence, wrap in parens
fn needs_parens_in_ifthenelse_context(expr: &Expression) -> bool {
    matches!(
        &expr.pexp_desc,
        ExpressionDesc::Pexp_ifthenelse(_, _, _)
            | ExpressionDesc::Pexp_sequence(_, _)
    )
}

/// Check if expression is "expression2" level - includes Pexp_field and Pexp_send
/// These don't need parens when used as binary operands or function arguments.
/// OCaml's pprintast.ml: expression2 handles Pexp_field and Pexp_send directly,
/// then falls through to simple_expr for other cases.
fn is_expression2(expr: &Expression, arena: &ParseArena) -> bool {
    if !expr.pexp_attributes.is_empty() {
        // Attributed expressions need to go through full expression printing
        return false;
    }
    // Expression2 includes field and send expressions
    if matches!(&expr.pexp_desc, ExpressionDesc::Pexp_field(_, _) | ExpressionDesc::Pexp_send(_, _)) {
        return true;
    }
    // Plus everything that's simple
    is_simple_expression_with_arena(expr, arena)
}

/// Check if an attribute is internal and should not be printed
fn is_internal_attribute(name: &str) -> bool {
    // Note: "res.await" IS printed (it's the AST marker for await modules)
    // "res.array.access" is internal and should not be printed
    matches!(name, "res.array.access")
}

/// Filter attributes to only include ones that should be printed
fn printable_attributes(attrs: &[(Located<String>, Payload)]) -> Vec<&(Located<String>, Payload)> {
    attrs
        .iter()
        .filter(|(name, _)| !is_internal_attribute(&name.txt))
        .collect()
}

/// Check if attributes contain a specific attribute name
fn has_attribute(attrs: &[(Located<String>, Payload)], name: &str) -> bool {
    attrs.iter().any(|(n, _)| n.txt == name)
}

/// Collect list elements from a cons pattern
fn collect_list_elements<'a>(expr: &'a Expression, arena: &ParseArena) -> (Vec<&'a Expression>, bool) {
    let mut elements = Vec::new();
    let mut current = expr;

    loop {
        if !current.pexp_attributes.is_empty() {
            elements.push(current);
            return (elements, false);
        }

        match &current.pexp_desc {
            ExpressionDesc::Pexp_construct(lid, None) => {
                if let Longident::Lident(name_idx) = arena.get_longident(lid.txt) {
                    let name = arena.get_string(*name_idx);
                    if name == "[]" {
                        return (elements, true);
                    }
                }
                elements.push(current);
                return (elements, false);
            }
            ExpressionDesc::Pexp_construct(lid, Some(arg)) => {
                if let Longident::Lident(name_idx) = arena.get_longident(lid.txt) {
                    let name = arena.get_string(*name_idx);
                    if name == "::" {
                        if let ExpressionDesc::Pexp_tuple(items) = &arg.pexp_desc {
                            if items.len() == 2 && arg.pexp_attributes.is_empty() {
                                elements.push(&items[0]);
                                current = &items[1];
                                continue;
                            }
                        }
                    }
                }
                elements.push(current);
                return (elements, false);
            }
            _ => {
                elements.push(current);
                return (elements, false);
            }
        }
    }
}

fn escape_string(s: &str) -> String {
    // The source was read as Latin-1 (each byte became a char with code point 0-255).
    // We need to escape characters with code points > 127 using decimal escape sequences
    // like \226\156\133, matching OCaml's default string escaping behavior.
    // We iterate over CHARACTERS (not bytes) since each char IS the original byte value.
    let mut result = String::new();
    for ch in s.chars() {
        let code = ch as u32;
        match code {
            0x0A => result.push_str("\\n"),  // newline
            0x0D => result.push_str("\\r"),  // carriage return
            0x09 => result.push_str("\\t"),  // tab
            0x5C => result.push_str("\\\\"), // backslash
            0x22 => result.push_str("\\\""), // double quote
            0x20..=0x7E => result.push(ch),  // Printable ASCII
            _ => {
                // Non-ASCII or non-printable: use decimal escape
                // Code point IS the original byte value (0-255) due to Latin-1 reading
                result.push('\\');
                result.push_str(&code.to_string());
            }
        }
    }
    result
}

// ============================================================================
// Public API
// ============================================================================

/// Print structure in ML format (like Pprintast in OCaml)
pub fn print_structure_ml(structure: &Structure, arena: &ParseArena, out: &mut impl Write) {
    let mut f = Formatter::new(out);
    for (i, item) in structure.iter().enumerate() {
        if i > 0 {
            f.newline();
        }
        print_structure_item(&mut f, arena, item);
    }
    f.flush();
}

/// Print signature in ML format
pub fn print_signature_ml(signature: &[SignatureItem], arena: &ParseArena, out: &mut impl Write) {
    let mut f = Formatter::new(out);
    for (i, item) in signature.iter().enumerate() {
        if i > 0 {
            f.newline();  // Top-level items on separate lines
        }
        print_signature_item(&mut f, arena, item);
    }
    f.flush();
}

// ============================================================================
// Structure items
// ============================================================================

fn print_structure_item<W: Write>(f: &mut Formatter<W>, arena: &ParseArena, item: &StructureItem) {
    match &item.pstr_desc {
        StructureItemDesc::Pstr_eval(expr, attrs) => {
            // OCaml: pp f "@[<hov2>;;%a@]%a"
            f.open_box(BoxKind::HOV, 2);
            f.string(";;");
            print_expression_no_outer_parens(f, arena, expr);
            f.close_box();
            print_item_attributes(f, arena, attrs);
        }
        StructureItemDesc::Pstr_value(rec_flag, bindings) => {
            let rec_str = match rec_flag {
                RecFlag::Recursive => "let rec ",
                RecFlag::Nonrecursive => "let ",
            };
            for (i, binding) in bindings.iter().enumerate() {
                if i > 0 {
                    f.newline();
                }
                // OCaml: @[<2>%s %a%a@]%a
                f.open_box(BoxKind::Box, 2);
                if i > 0 {
                    f.string("and ");
                } else {
                    f.string(rec_str);
                }
                print_value_binding(f, arena, &binding.pvb_pat, &binding.pvb_expr);
                f.close_box();
                // Item attributes are OUTSIDE the box in OCaml
                print_item_attributes(f, arena, &binding.pvb_attributes);
            }
        }
        StructureItemDesc::Pstr_primitive(vd) => {
            // OCaml: pp f "@[<hov2>external@ %a@ :@ %a@]%a"
            f.open_box(BoxKind::HOV, 2);
            f.string("external");
            f.space();
            f.string(&vd.pval_name.txt);
            f.space();
            f.string(":");
            f.space();
            // OCaml value_description: pp f "@[<hov2>%a%a@]"
            // where %a%a = core_type followed by optional "@ =@ primitives"
            f.open_box(BoxKind::HOV, 2);
            print_core_type(f, arena, &vd.pval_type);
            if !vd.pval_prim.is_empty() {
                f.space();
                f.string("=");
                f.space();
                for (i, prim) in vd.pval_prim.iter().enumerate() {
                    if i > 0 {
                        f.space();
                    }
                    f.string("\"");
                    f.string(prim);
                    f.string("\"");
                }
            }
            f.close_box();
            f.close_box();
            print_item_attributes(f, arena, &vd.pval_attributes);
        }
        StructureItemDesc::Pstr_type(rec_flag, decls) => {
            // OCaml: @[<2>type %a%a%s%s%a@]%a for first, @[<2>and %a@]%a for others
            // Each declaration is wrapped in a box with indent 2
            let rec_str = match rec_flag {
                RecFlag::Recursive => "",
                RecFlag::Nonrecursive => " nonrec",
            };
            for (i, decl) in decls.iter().enumerate() {
                if i > 0 {
                    f.newline();
                }
                // OCaml: @[<2> - Open structural box with indent 2 for type declaration
                f.open_box(BoxKind::Box, 2);
                if i == 0 {
                    f.string("type");
                    f.string(rec_str);
                    f.string(" ");
                } else {
                    f.string("and ");
                }
                print_type_declaration(f, arena, decl);
                f.close_box();
                // OCaml: item attributes (%a) are printed OUTSIDE the @[<2>...@] box
                print_item_attributes(f, arena, &decl.ptype_attributes);
            }
        }
        StructureItemDesc::Pstr_typext(ext) => {
            // OCaml: pp f "@[<2>type %a%a += %a@ %a@]%a"
            // Format: "type t +=  \n  | Foo \n  | Bar"
            f.open_box(BoxKind::HV, 2);
            f.string("type ");
            // Print type params if present
            if !ext.ptyext_params.is_empty() {
                if ext.ptyext_params.len() == 1 {
                    // Single param: no parens, like OCaml's "type _ Tid.t +="
                    print_core_type(f, arena, &ext.ptyext_params[0].0);
                    f.string(" ");
                } else {
                    f.string("(");
                    for (i, (ty, _variance)) in ext.ptyext_params.iter().enumerate() {
                        if i > 0 {
                            f.string(",");
                        }
                        print_core_type(f, arena, ty);
                    }
                    f.string(") ");
                }
            }
            print_longident_idx(f, arena, ext.ptyext_path.txt);
            f.string(" += ");
            if matches!(ext.ptyext_private, PrivateFlag::Private) {
                f.string("private ");
            }
            // Extra space before line break to match OCaml's "+=  \n"
            f.string(" ");
            // Each constructor starts with @\n|@; (newline, pipe, break)
            for ctor in ext.ptyext_constructors.iter() {
                f.newline();
                f.string("| ");
                print_extension_constructor(f, arena, ctor);
            }
            f.close_box();
            print_item_attributes(f, arena, &ext.ptyext_attributes);
        }
        StructureItemDesc::Pstr_exception(ext) => {
            // OCaml: pp f "@[<hov2>exception@ %a@]"
            // OCaml: pp f "@[<hov2>exception@ %a@]"
            f.open_box(BoxKind::HOV, 2);
            f.string("exception");
            f.space(); // @ after "exception"
            print_extension_constructor(f, arena, ext);
            f.close_box();
        }
        StructureItemDesc::Pstr_module(mb) => {
            // OCaml: @[<hov2>module %s%a@]%a
            f.open_box(BoxKind::HOV, 2);
            f.string("module ");
            f.string(&mb.pmb_name.txt);
            // OCaml: module_helper unwraps Pmod_functor with no attributes
            // and prints functor parameters inline: `module F(A:X) = body`
            let mut me = &mb.pmb_expr;
            while let ModuleExprDesc::Pmod_functor(name, mt, body) = &me.pmod_desc {
                if me.pmod_attributes.is_empty() {
                    if let Some(mt) = mt {
                        f.string("(");
                        f.string(&name.txt);
                        f.string(":");
                        print_module_type(f, arena, mt);
                        f.string(")");
                    } else {
                        f.string("()");
                    }
                    me = body;
                } else {
                    break;
                }
            }
            // OCaml: special case for Pmod_constraint with ident or signature type
            // prints "module X : mt = me'" instead of "module X = (me' : mt)"
            if let ModuleExprDesc::Pmod_constraint(inner_me, mt) = &me.pmod_desc {
                let is_sugar_type = matches!(
                    mt.pmty_desc,
                    ModuleTypeDesc::Pmty_ident(..) | ModuleTypeDesc::Pmty_signature(..)
                );
                if is_sugar_type && me.pmod_attributes.is_empty() {
                    f.string(" :");
                    f.space();
                    print_module_type(f, arena, mt);
                    f.space();
                    f.string("=");
                    f.space();
                    print_module_expr(f, arena, inner_me);
                    f.space();  // OCaml: trailing @; produces space
                } else {
                    f.string(" =");
                    f.space();
                    print_module_expr(f, arena, me);
                }
            } else {
                f.string(" =");
                f.space();
                print_module_expr(f, arena, me);
            }
            f.close_box();
            print_item_attributes(f, arena, &mb.pmb_attributes);
        }
        StructureItemDesc::Pstr_recmodule(mbs) => {
            // OCaml: @[<hv>@[<hov2>module rec name:mt = me@]%a @ aux_list@]
            // Outer HV box ensures and-bindings break onto separate lines
            f.open_box(BoxKind::HV, 0);
            for (i, mb) in mbs.iter().enumerate() {
                if i == 0 {
                    f.open_box(BoxKind::HOV, 2);
                    f.string("module rec ");
                } else {
                    // Outer @ break: newline in HV box
                    f.space();
                    // OCaml: @[<hov2>@ and@ ...@] — HOV2 box starting with @ before "and"
                    f.open_box(BoxKind::HOV, 2);
                    f.space(); // @ before "and" — gives 1 space in packing mode
                    f.string("and ");
                }
                f.string(&mb.pmb_name.txt);
                // Check for constraint sugar: `A:Mt = Me` instead of `A = (Me : Mt)`
                if let ModuleExprDesc::Pmod_constraint(me, mt) = &mb.pmb_expr.pmod_desc {
                    f.string(":");
                    print_module_type(f, arena, mt);
                    f.space(); // @ before =
                    f.string("=");
                    f.space(); // @ after =
                    print_module_expr(f, arena, me);
                } else {
                    f.space(); // @ before =
                    f.string("=");
                    f.space(); // @ after =
                    print_module_expr(f, arena, &mb.pmb_expr);
                }
                f.close_box();
                // Print module binding attributes
                print_item_attributes(f, arena, &mb.pmb_attributes);
            }
            f.close_box();
        }
        StructureItemDesc::Pstr_modtype(mtd) => {
            // OCaml: pp f "@[<hov2>module@ type@ %s%a@]%a"
            // where inner is: pp_print_space f (); pp f "@ =@ %a" (module_type ctxt) mt
            // This produces two spaces before = when not breaking: "Sig  ="
            f.open_box(BoxKind::HOV, 2);
            f.string("module");
            f.space();
            f.string("type");
            f.space();
            f.string(&mtd.pmtd_name.txt);
            if let Some(mt) = &mtd.pmtd_type {
                // pp_print_space + "@ =" gives two spaces before =
                f.string(" ");  // literal space from pp_print_space
                f.space();      // break hint before =
                f.string("=");
                f.space();
                print_module_type(f, arena, mt);
            }
            f.close_box();
            print_item_attributes(f, arena, &mtd.pmtd_attributes);
        }
        StructureItemDesc::Pstr_open(od) => {
            f.string("open");
            if matches!(od.popen_override, OverrideFlag::Override) {
                f.string("!");
            }
            f.string(" ");
            print_longident_idx(f, arena, od.popen_lid.txt);
            print_item_attributes(f, arena, &od.popen_attributes);
        }
        StructureItemDesc::Pstr_include(incl) => {
            // OCaml: @[<hov2>include@ %a@]%a
            f.open_box(BoxKind::HOV, 2);
            f.string("include");
            f.space();
            print_module_expr(f, arena, &incl.pincl_mod);
            f.close_box();
            print_item_attributes(f, arena, &incl.pincl_attributes);
        }
        StructureItemDesc::Pstr_attribute((name, payload)) => {
            // Standalone/floating attributes use [@@@...] (3 @)
            f.string("[@@@");
            f.string(&name.txt);
            if !payload_is_empty(payload) {
                f.string(" ");
                print_payload(f, arena, payload);
            } else {
                f.string(" ");
            }
            f.string("]");
        }
        StructureItemDesc::Pstr_extension((name, payload), attrs) => {
            f.open_box(BoxKind::H, 2);
            f.string("[%%");
            f.string(&name.txt);
            f.space();
            print_payload(f, arena, payload);
            f.string("]");
            print_item_attributes(f, arena, attrs);
            f.close_box();
        }
    }
}

// ============================================================================
// Value bindings
// ============================================================================

fn print_value_binding<W: Write>(f: &mut Formatter<W>, arena: &ParseArena, pat: &Pattern, expr: &Expression) {
    if !expr.pexp_attributes.is_empty() {
        // OCaml: pp f "%a@;=@;%a" (pattern ctxt) p (expression ctxt) x
        print_pattern(f, arena, pat);
        f.space();
        f.string("=");
        f.space();
        print_expression(f, arena, expr);
        return;
    }

    // Check for desugared locally abstract types (GADT sugar):
    // let f : 'a 'b . type_expr = fun (type a) -> fun (type b) -> (body : type_expr')
    // This gets printed as: let f : type a b. type_expr' = body
    // OCaml pprintast.ml is_desugared_gadt function (lines 1054-1081)
    if let PatternDesc::Ppat_constraint(inner_pat, typ) = &pat.ppat_desc {
        if pat.ppat_attributes.is_empty() {
            if let CoreTypeDesc::Ptyp_poly(tyvars, _) = &typ.ptyp_desc {
                if !tyvars.is_empty() && typ.ptyp_attributes.is_empty() {
                    // Try to match the desugared GADT pattern
                    // Collect Pexp_newtype chain from expression
                    let mut e_tyvars = Vec::new();
                    let mut curr_expr = expr;
                    while let ExpressionDesc::Pexp_newtype(tyvar, inner) = &curr_expr.pexp_desc {
                        if !curr_expr.pexp_attributes.is_empty() {
                            break;
                        }
                        e_tyvars.push(&tyvar.txt);
                        curr_expr = inner;
                    }
                    // Check if we end with Pexp_constraint(body, ct) and tyvars match
                    if let ExpressionDesc::Pexp_constraint(body, ct) = &curr_expr.pexp_desc {
                        if curr_expr.pexp_attributes.is_empty() && e_tyvars.len() == tyvars.len() {
                            // Check that tyvar names match (pattern has 'a, expr has a -> same base name)
                            let tyvars_match = tyvars.iter().zip(e_tyvars.iter()).all(|(pt, et)| {
                                pt.txt == **et
                            });
                            if tyvars_match {
                                // Print sugared form: name : type t u v. ct = body
                                // OCaml: %a@;: type@;%a.@;%a@;=@;%a
                                print_simple_pattern(f, arena, inner_pat);
                                f.space();
                                f.string(": type");
                                f.space();
                                for (i, tv) in e_tyvars.iter().enumerate() {
                                    if i > 0 {
                                        f.space();
                                    }
                                    f.string(tv);
                                }
                                f.string(".");
                                f.space();
                                print_core_type(f, arena, ct);
                                f.space();
                                f.string("=");
                                f.space();
                                print_expression(f, arena, body);
                                return;
                            }
                        }
                    }
                }
            }
        }
    }

    // Special case: Ppat_constraint - different handling based on type
    // OCaml pprintast.ml lines 1096-1103
    if let PatternDesc::Ppat_constraint(inner_pat, typ) = &pat.ppat_desc {
        if pat.ppat_attributes.is_empty() {
            if matches!(typ.ptyp_desc, CoreTypeDesc::Ptyp_poly(..)) && typ.ptyp_attributes.is_empty() {
                // Ptyp_poly: print without outer parens
                // OCaml: pp f "%a@;:@;%a@;=@;%a"
                print_simple_pattern(f, arena, inner_pat);
                f.space();
                f.string(":");
                f.space();
                print_core_type(f, arena, typ);
                f.space();
                f.string("=");
                f.space();
                print_expression_no_outer_parens(f, arena, expr);
                return;
            } else {
                // Non-poly: print with outer parens using simple_pattern for inner
                // OCaml: pp f "(%a@;:@;%a)@;=@;%a"
                f.string("(");
                print_simple_pattern(f, arena, inner_pat);
                f.space();
                f.string(":");
                f.space();
                print_core_type(f, arena, typ);
                f.string(")");
                f.space();
                f.string("=");
                f.space();
                print_expression_no_outer_parens(f, arena, expr);
                return;
            }
        }
    }

    match &expr.pexp_desc {
        ExpressionDesc::Pexp_fun {
            arg_label,
            default,
            lhs,
            rhs,
            arity,
            is_async,
        } => {
            // OCaml: pp f "%a@ %a" (simple_pattern ctxt) p pp_print_pexp_function x
            // where pp_print_pexp_function does: pp f "%s%s%a@ %a" async_str arity_str param recurse
            print_pattern(f, arena, pat);
            f.space(); // @ between pattern and pp_print_pexp_function

            if *is_async {
                f.string("async ");
            }

            // Print arity annotation (only when explicitly set, matching OCaml's pprintast)
            if let Arity::Full(n) = arity {
                f.string(&format!("[arity:{}]", n));
            }

            print_inline_param(f, arena, arg_label, default, lhs);
            f.space(); // @ between param and next
            let body = print_remaining_inline_params(f, arena, rhs);

            // OCaml: pp f "=@;%a" (expression ctxt) x
            f.string("=");
            f.space();
            print_expression_no_outer_parens(f, arena, body);
        }
        ExpressionDesc::Pexp_newtype(name, inner) => {
            // Handle locally abstract types at the start: let f (type t) ...
            // OCaml: pp f "%a@ %a" where second %a is pp_print_pexp_function
            // pp_print_pexp_function for Pexp_newtype: pp f "(type@ %s)@ %a"
            print_pattern(f, arena, pat);
            f.space(); // @ between pattern and (type ...)
            f.string("(type");
            f.space(); // @ inside (type@ %s)
            f.string(&name.txt);
            f.string(")");
            f.space(); // @ between ) and next
            // Continue collecting newtypes and print arity when we hit a fun
            let body = print_remaining_inline_params_with_arity(f, arena, inner, true);
            // OCaml: pp f "=@;%a" (expression ctxt) x
            f.string("=");
            f.space();
            print_expression_no_outer_parens(f, arena, body);
        }
        _ => {
            // OCaml: pp f "%a@;=@;%a" (pattern ctxt) p (expression ctxt) x
            print_pattern(f, arena, pat);
            f.space();
            f.string("=");
            f.space();
            print_expression_no_outer_parens(f, arena, expr);
        }
    }
}

/// Print a function parameter.
/// For Nolabel: no trailing space (caller adds break)
/// For Labelled/Optional: includes trailing break (@;) to match OCaml's label_exp
fn print_inline_param<W: Write>(
    f: &mut Formatter<W>,
    arena: &ParseArena,
    label: &ArgLabel,
    default: &Option<Box<Expression>>,
    pat: &Pattern,
) {
    match label {
        ArgLabel::Nolabel => {
            // OCaml: pp f "%a@ " (simple_pattern ctxt) p
            // The "@ " break is emitted by the caller, not by label_exp for Nolabel
            // Wait - actually for Nolabel, pp_print_pexp_function uses (simple_pattern ctxt) directly,
            // not label_exp. So there's no trailing break from the param itself.
            if pattern_needs_parens_as_param(pat, arena) {
                f.string("(");
                print_pattern(f, arena, pat);
                f.string(")");
            } else {
                print_pattern(f, arena, pat);
            }
        }
        ArgLabel::Labelled(name) => {
            // OCaml label_exp: pp f "~%s@;" l  OR  pp f "~%s:%a@;" l (simple_pattern ctxt) p
            // The trailing @; is Break(1,0)
            let name_str = arena.get_string(name.txt);
            if pattern_is_simple_var(pat, name_str) {
                f.string("~");
                f.string(name_str);
                f.space(); // @; from label_exp
            } else {
                f.string("~");
                f.string(name_str);
                f.string(":");
                print_pattern(f, arena, pat);
                f.space(); // @; from label_exp
            }
        }
        ArgLabel::Optional(name) => {
            // OCaml label_exp: pp f "?(%s=@;%a)@;" OR pp f "?%s:(%a=@;%a)@;" etc.
            // All cases have trailing @; = Break(1,0)
            let name_str = arena.get_string(name.txt);
            if let Some(def) = default {
                if pattern_is_simple_var(pat, name_str) {
                    f.string("?(");
                    f.string(name_str);
                    f.string("=");
                    f.space(); // =@; inside
                    print_expression(f, arena, def);
                    f.string(")");
                    f.space(); // trailing @;
                } else {
                    f.string("?");
                    f.string(name_str);
                    f.string(":(");
                    print_pattern(f, arena, pat);
                    f.string("=");
                    f.space(); // =@; inside
                    print_expression(f, arena, def);
                    f.string(")");
                    f.space(); // trailing @;
                }
            } else if pattern_is_simple_var(pat, name_str) {
                f.string("?");
                f.string(name_str);
                f.space(); // @; trailing
            } else {
                f.string("?");
                f.string(name_str);
                f.string(":");
                print_pattern(f, arena, pat);
                f.space(); // @; trailing
            }
        }
    }
}

/// Print label_exp matching OCaml's label_exp function (without trailing break).
/// Used in print_fun_body where the caller adds " -> " after.
fn print_label_exp<W: Write>(
    f: &mut Formatter<W>,
    arena: &ParseArena,
    label: &ArgLabel,
    default: &Option<Box<Expression>>,
    pat: &Pattern,
) {
    match label {
        ArgLabel::Nolabel => {
            // OCaml: simple_pattern ctxt p followed by "@ "
            print_simple_pattern(f, arena, pat);
        }
        ArgLabel::Labelled(name) => {
            let name_str = arena.get_string(name.txt);
            if pattern_is_simple_var(pat, name_str) {
                // Punned: ~foo
                f.string("~");
                f.string(name_str);
            } else {
                // Explicit: ~foo:pattern
                f.string("~");
                f.string(name_str);
                f.string(":");
                print_simple_pattern(f, arena, pat);
            }
        }
        ArgLabel::Optional(name) => {
            let name_str = arena.get_string(name.txt);
            if pattern_is_simple_var(pat, name_str) {
                match default {
                    Some(def) => {
                        // ?(%s=@;%a)
                        f.string("?(");
                        f.string(name_str);
                        f.string("=");
                        f.space();
                        print_expression(f, arena, def);
                        f.string(")");
                    }
                    None => {
                        // ?%s
                        f.string("?");
                        f.string(name_str);
                    }
                }
            } else {
                match default {
                    Some(def) => {
                        // ?%s:(%a=@;%a)
                        f.string("?");
                        f.string(name_str);
                        f.string(":(");
                        print_pattern(f, arena, pat);
                        f.string("=");
                        f.space();
                        print_expression(f, arena, def);
                        f.string(")");
                    }
                    None => {
                        // ?%s:%a
                        f.string("?");
                        f.string(name_str);
                        f.string(":");
                        print_simple_pattern(f, arena, pat);
                    }
                }
            }
        }
    }
}

/// Print label_exp with trailing break/space, matching OCaml's label_exp.
/// Used in Pexp_fun expression printing where format breaks are used.
fn print_label_exp_with_break<W: Write>(
    f: &mut Formatter<W>,
    arena: &ParseArena,
    label: &ArgLabel,
    default: &Option<Box<Expression>>,
    pat: &Pattern,
) {
    match label {
        ArgLabel::Nolabel => {
            // OCaml: %a@ (simple_pattern + break hint with space)
            print_simple_pattern(f, arena, pat);
            f.space();
        }
        ArgLabel::Labelled(name) => {
            let name_str = arena.get_string(name.txt);
            if pattern_is_simple_var(pat, name_str) {
                // Punned: ~foo@;
                f.string("~");
                f.string(name_str);
                f.space();
            } else {
                // Explicit: ~foo:pattern@;
                f.string("~");
                f.string(name_str);
                f.string(":");
                print_simple_pattern(f, arena, pat);
                f.space();
            }
        }
        ArgLabel::Optional(name) => {
            let name_str = arena.get_string(name.txt);
            if pattern_is_simple_var(pat, name_str) {
                match default {
                    Some(def) => {
                        // ?(%s=@;%a)@;
                        f.string("?(");
                        f.string(name_str);
                        f.string("=");
                        f.space();
                        print_expression(f, arena, def);
                        f.string(")");
                        f.space();
                    }
                    None => {
                        // ?%s@ (note: OCaml uses @ not @; for punned optional)
                        f.string("?");
                        f.string(name_str);
                        f.space();
                    }
                }
            } else {
                match default {
                    Some(def) => {
                        // ?%s:(%a=@;%a)@;
                        f.string("?");
                        f.string(name_str);
                        f.string(":(");
                        print_pattern(f, arena, pat);
                        f.string("=");
                        f.space();
                        print_expression(f, arena, def);
                        f.string(")");
                        f.space();
                    }
                    None => {
                        // ?%s:%a@;
                        f.string("?");
                        f.string(name_str);
                        f.string(":");
                        print_simple_pattern(f, arena, pat);
                        f.space();
                    }
                }
            }
        }
    }
}

fn print_remaining_inline_params<'a, W: Write>(
    f: &mut Formatter<W>,
    arena: &ParseArena,
    expr: &'a Expression,
) -> &'a Expression {
    print_remaining_inline_params_with_arity(f, arena, expr, false)
}

/// Helper to print remaining inline params, optionally printing arity on first Pexp_fun
fn print_remaining_inline_params_with_arity<'a, W: Write>(
    f: &mut Formatter<W>,
    arena: &ParseArena,
    expr: &'a Expression,
    need_arity: bool,
) -> &'a Expression {
    if !expr.pexp_attributes.is_empty() {
        return expr;
    }

    match &expr.pexp_desc {
        ExpressionDesc::Pexp_fun {
            arg_label,
            default,
            lhs,
            rhs,
            arity,
            ..
        } => {
            // Print arity for each Pexp_fun that has an arity annotation
            // (matching OCaml's pprintast: only print when explicitly set)
            if let Arity::Full(n) = arity {
                f.string(&format!("[arity:{}]", n));
            }
            print_inline_param(f, arena, arg_label, default, lhs);
            f.space(); // @ between param and next recursive call
            print_remaining_inline_params_with_arity(f, arena, rhs, false)
        }
        ExpressionDesc::Pexp_newtype(name, inner) => {
            // OCaml: pp f "(type@ %s)@ %a" str.txt pp_print_pexp_function e
            f.string("(type");
            f.space(); // @ inside (type@ %s)
            f.string(&name.txt);
            f.string(")");
            f.space(); // @ between ) and next
            print_remaining_inline_params_with_arity(f, arena, inner, need_arity)
        }
        _ => expr,
    }
}

// ============================================================================
// Expressions
// ============================================================================

fn print_expression<W: Write>(f: &mut Formatter<W>, arena: &ParseArena, expr: &Expression) {
    let attrs = printable_attributes(&expr.pexp_attributes);
    let has_attrs = !attrs.is_empty();

    if has_attrs {
        // OCaml Format: pp f "((%a)@,%a)" (expression ctxt) {x with pexp_attributes = []} (attributes ctxt) x.pexp_attributes
        // One cut before ALL attributes (not per-attribute), then attributes are concatenated
        f.string("((");
        print_expression_inner(f, arena, expr, false, PrintCtx::reset());
        f.string(")");
        f.cut(); // Single cut before all attributes
        print_attributes(f, arena, &expr.pexp_attributes);
        f.string(")");
    } else {
        print_expression_inner(f, arena, expr, false, PrintCtx::reset());
    }
}

fn print_expression_no_outer_parens<W: Write>(f: &mut Formatter<W>, arena: &ParseArena, expr: &Expression) {
    let attrs = printable_attributes(&expr.pexp_attributes);
    let has_attrs = !attrs.is_empty();

    if has_attrs {
        // OCaml Format: pp f "((%a)@,%a)" - one cut before all attributes
        f.string("((");
        print_expression_inner(f, arena, expr, false, PrintCtx::reset());
        f.string(")");
        f.cut(); // Single cut before all attributes
        print_attributes(f, arena, &expr.pexp_attributes);
        f.string(")");
    } else {
        print_expression_inner(f, arena, expr, false, PrintCtx::reset());
    }
}

fn print_expression_simple<W: Write>(f: &mut Formatter<W>, arena: &ParseArena, expr: &Expression) {
    print_expression_simple_ctx(f, arena, expr, PrintCtx::reset());
}

/// Context-aware version of print_expression_simple.
/// In OCaml, `simple_expr ctxt` passes the context through, so when it falls through
/// to `expression ctxt`, context-dependent parenthesization (pipe, semi, ifthenelse) is applied.
fn print_expression_simple_ctx<W: Write>(f: &mut Formatter<W>, arena: &ParseArena, expr: &Expression, ctx: PrintCtx) {
    // OCaml's simple_expr wraps non-simple expressions in parens
    // BUT: attributed expressions already produce ((expr)[@attr]) structure
    // so we don't add extra parens for them
    let has_attrs = !printable_attributes(&expr.pexp_attributes).is_empty();

    if has_attrs {
        // OCaml: simple_expr for attributed exprs -> expression ctxt -> handles attrs
        print_expression_with_ctx(f, arena, expr, ctx);
    } else if is_simple_expression_with_arena(expr, arena) {
        // Simple expressions print directly (no context needed - they don't produce parens)
        print_expression_with_ctx(f, arena, expr, ctx);
    } else {
        // Non-simple: wrap in parens and call expression with context
        // OCaml's paren + expression boxes create a break opportunity
        f.string("(");
        print_expression_with_ctx(f, arena, expr, ctx);
        f.string(")");
    }
}

/// Print expression as simple_expr with under_semi context.
/// Used for array elements: OCaml uses `simple_expr (under_semi ctxt)`.
fn print_expression_simple_under_semi<W: Write>(f: &mut Formatter<W>, arena: &ParseArena, expr: &Expression) {
    print_expression_simple_under_semi_ctx(f, arena, expr, PrintCtx::reset());
}

/// Context-aware version of print_expression_simple_under_semi.
/// OCaml: `simple_expr (under_semi ctxt)` - applies under_semi to the inherited context.
fn print_expression_simple_under_semi_ctx<W: Write>(f: &mut Formatter<W>, arena: &ParseArena, expr: &Expression, ctx: PrintCtx) {
    let semi_ctx = ctx.under_semi();
    let has_attrs = !printable_attributes(&expr.pexp_attributes).is_empty();

    if has_attrs {
        // OCaml: expression (under_semi ctxt) for attributed non-simple expressions
        print_expression_with_ctx(f, arena, expr, semi_ctx);
    } else if is_simple_expression_with_arena(expr, arena) {
        print_expression_with_ctx(f, arena, expr, semi_ctx);
    } else {
        f.string("(");
        print_expression_with_ctx(f, arena, expr, semi_ctx);
        f.string(")");
    }
}

/// Print expression at "expression2" level - used for binary operands and function arguments.
/// OCaml's under_app: if x.pexp_attributes <> [] then expression ctxt f x else expression2 ctxt f x
/// Expression2 includes Pexp_field and Pexp_send without parens, other exprs use simple_expr.
fn print_expression2<W: Write>(f: &mut Formatter<W>, arena: &ParseArena, expr: &Expression) {
    print_expression2_ctx(f, arena, expr, PrintCtx::reset());
}

/// Context-aware version of print_expression2.
/// In OCaml, `expression2 ctxt` passes the context through.
fn print_expression2_ctx<W: Write>(f: &mut Formatter<W>, arena: &ParseArena, expr: &Expression, ctx: PrintCtx) {
    let has_attrs = !printable_attributes(&expr.pexp_attributes).is_empty();

    if has_attrs {
        // Attributed expressions go through full expression printing with context
        print_expression_with_ctx(f, arena, expr, ctx);
    } else if is_expression2(expr, arena) {
        // Expression2-level expressions don't need parens
        print_expression_with_ctx(f, arena, expr, ctx);
    } else {
        // Everything else needs parens (falls through from simple_expr)
        f.string("(");
        print_expression_with_ctx(f, arena, expr, ctx);
        f.string(")");
    }
}

fn print_expression_semi_context<W: Write>(f: &mut Formatter<W>, arena: &ParseArena, expr: &Expression) {
    let attrs = printable_attributes(&expr.pexp_attributes);
    let has_attrs = !attrs.is_empty();
    let needs_semi_parens = needs_parens_in_semi_context(expr);
    let is_simple = is_simple_expression(expr);

    if has_attrs {
        f.string("((");
        print_expression_inner(f, arena, expr, false, PrintCtx::reset());
        f.string(")");
        f.cut(); // Single cut before all attributes
        print_attributes(f, arena, &expr.pexp_attributes);
        f.string(")");
    } else if needs_semi_parens {
        f.string("((");
        print_expression_inner(f, arena, expr, false, PrintCtx::reset());
        f.string("))");
    } else if !is_simple {
        f.string("(");
        print_expression_inner(f, arena, expr, false, PrintCtx::reset());
        f.string(")");
    } else {
        print_expression_inner(f, arena, expr, false, PrintCtx::reset());
    }
}

/// Print expression under OCaml's `under_semi` context.
/// Only wraps specific types in parens (fun/match/try/sequence, let/letmodule/open/letexception).
/// Everything else goes through normal expression printing.
///
/// OCaml's `expression` function checks attributes FIRST, before context checks.
/// When an attributed expression is in semi context, the attribute handling wraps
/// the expression, and then the inner (unattributed) expression gets the semi parens.
/// This produces `(((seq)))[@attr]` instead of `(((seq))[@attr])`.
fn print_expression_under_semi<W: Write>(f: &mut Formatter<W>, arena: &ParseArena, expr: &Expression) {
    let attrs = printable_attributes(&expr.pexp_attributes);
    let has_attrs = !attrs.is_empty();

    if has_attrs {
        // OCaml: attributes are checked first, then the inner expression gets the context check.
        // pp f "((%a)@,%a)" (expression ctxt) {x with pexp_attributes=[]} (attributes ctxt) x.pexp_attributes
        // The inner call to `expression ctxt` will then hit the semi context check.
        f.string("((");
        // Print the inner expression with semi context (attrs stripped)
        print_expression_under_semi_inner(f, arena, expr);
        f.string(")");
        f.cut();
        print_attributes(f, arena, &expr.pexp_attributes);
        f.string(")");
    } else {
        let needs_semi_parens = needs_parens_in_semi_context(expr);
        let needs_let_parens = matches!(
            &expr.pexp_desc,
            ExpressionDesc::Pexp_let(_, _, _)
                | ExpressionDesc::Pexp_letmodule(_, _, _)
                | ExpressionDesc::Pexp_open(_, _, _)
                | ExpressionDesc::Pexp_letexception(_, _)
        );
        if needs_semi_parens || needs_let_parens {
            // OCaml: paren true (expression reset_ctxt) f x
            f.string("(");
            print_expression(f, arena, expr);
            f.string(")");
        } else {
            print_expression(f, arena, expr);
        }
    }
}

/// Helper: print the inner expression (ignoring its attributes) under semi context.
/// This replicates OCaml's behavior where `expression ctxt` is called with attrs stripped,
/// and the semi context check then applies to the unattributed expression.
fn print_expression_under_semi_inner<W: Write>(f: &mut Formatter<W>, arena: &ParseArena, expr: &Expression) {
    let needs_semi_parens = needs_parens_in_semi_context(expr);
    let needs_let_parens = matches!(
        &expr.pexp_desc,
        ExpressionDesc::Pexp_let(_, _, _)
            | ExpressionDesc::Pexp_letmodule(_, _, _)
            | ExpressionDesc::Pexp_open(_, _, _)
            | ExpressionDesc::Pexp_letexception(_, _)
    );
    if needs_semi_parens || needs_let_parens {
        // paren true (expression reset_ctxt) f x
        f.string("(");
        print_expression_inner(f, arena, expr, false, PrintCtx::reset());
        f.string(")");
    } else {
        print_expression_inner(f, arena, expr, false, PrintCtx::reset());
    }
}

fn print_expression_list_context<W: Write>(f: &mut Formatter<W>, arena: &ParseArena, expr: &Expression) {
    let attrs = printable_attributes(&expr.pexp_attributes);
    let has_attrs = !attrs.is_empty();
    let needs_semi_parens = needs_parens_in_semi_context(expr);

    if has_attrs {
        f.string("(");
        print_expression_inner(f, arena, expr, false, PrintCtx::reset());
        f.cut(); // Single cut before all attributes
        print_attributes(f, arena, &expr.pexp_attributes);
        f.string(")");
    } else if needs_semi_parens {
        f.string("(");
        print_expression_inner(f, arena, expr, false, PrintCtx::reset());
        f.string(")");
    } else {
        print_expression(f, arena, expr);
    }
}

/// Print expression in "pipe" context (match case RHS)
/// OCaml wraps Pexp_fun/match/try/sequence in parens when under_pipe
/// Like under_semi, OCaml checks attributes first, then context.
fn print_expression_pipe_context<W: Write>(f: &mut Formatter<W>, arena: &ParseArena, expr: &Expression) {
    let attrs = printable_attributes(&expr.pexp_attributes);
    let has_attrs = !attrs.is_empty();
    let needs_pipe_parens = needs_parens_in_pipe_context(expr);

    if has_attrs && needs_pipe_parens {
        // Attribute handling first, then pipe context for inner expression
        f.string("((");
        // Inner: apply pipe parens
        f.string("(");
        print_expression_inner(f, arena, expr, false, PrintCtx::reset());
        f.string(")");
        f.string(")");
        f.cut();
        print_attributes(f, arena, &expr.pexp_attributes);
        f.string(")");
    } else if needs_pipe_parens {
        f.string("(");
        print_expression(f, arena, expr);
        f.string(")");
    } else {
        print_expression(f, arena, expr);
    }
}

/// Print expression under OCaml's `under_ifthenelse` context.
/// When ifthenelse=true, Pexp_ifthenelse and Pexp_sequence get wrapped in parens.
/// This is used for the condition and then-body of if-then-else expressions.
///
/// Like under_semi, OCaml checks attributes first, then context.
fn print_expression_under_ifthenelse<W: Write>(f: &mut Formatter<W>, arena: &ParseArena, expr: &Expression) {
    let attrs = printable_attributes(&expr.pexp_attributes);
    let has_attrs = !attrs.is_empty();

    if has_attrs {
        // Attribute handling first, then ifthenelse context for inner expression
        f.string("((");
        print_expression_under_ifthenelse_inner(f, arena, expr);
        f.string(")");
        f.cut();
        print_attributes(f, arena, &expr.pexp_attributes);
        f.string(")");
    } else if needs_parens_in_ifthenelse_context(expr) {
        f.string("(");
        print_expression(f, arena, expr);
        f.string(")");
    } else {
        print_expression(f, arena, expr);
    }
}

/// Helper: print inner expression (ignoring attrs) under ifthenelse context
fn print_expression_under_ifthenelse_inner<W: Write>(f: &mut Formatter<W>, arena: &ParseArena, expr: &Expression) {
    if needs_parens_in_ifthenelse_context(expr) {
        f.string("(");
        print_expression_inner(f, arena, expr, false, PrintCtx::reset());
        f.string(")");
    } else {
        print_expression_inner(f, arena, expr, false, PrintCtx::reset());
    }
}

fn print_expression_parens_if_complex<W: Write>(f: &mut Formatter<W>, arena: &ParseArena, expr: &Expression) {
    let attrs = printable_attributes(&expr.pexp_attributes);
    let has_attrs = !attrs.is_empty();
    let needs_parens = needs_parens_as_function_arg(expr, arena);
    let has_own_parens = expression_has_own_parens(expr);

    if has_attrs {
        // OCaml Format: pp f "((%a)@,%a)" - one cut before all attributes
        if has_own_parens {
            f.string("(");
            print_expression_inner(f, arena, expr, false, PrintCtx::reset());
            f.cut(); // Single cut before all attributes
            print_attributes(f, arena, &expr.pexp_attributes);
            f.string(")");
        } else {
            f.string("((");
            print_expression_inner(f, arena, expr, false, PrintCtx::reset());
            f.string(")");
            f.cut(); // Single cut before all attributes
            print_attributes(f, arena, &expr.pexp_attributes);
            f.string(")");
        }
    } else if needs_parens && !has_own_parens {
        f.string("(");
        print_expression_inner(f, arena, expr, false, PrintCtx::reset());
        f.string(")");
    } else {
        print_expression_inner(f, arena, expr, false, PrintCtx::reset());
    }
}

fn print_fun_body<W: Write>(f: &mut Formatter<W>, arena: &ParseArena, expr: &Expression) {
    match &expr.pexp_desc {
        ExpressionDesc::Pexp_fun {
            arg_label,
            default,
            lhs,
            rhs,
            ..
        } if expr.pexp_attributes.is_empty() => {
            f.string("fun ");
            // OCaml label_exp: handles punning for labeled args
            print_label_exp(f, arena, arg_label, default, lhs);
            f.string(" -> ");
            print_fun_body(f, arena, rhs);
        }
        _ => {
            print_expression_no_outer_parens(f, arena, expr);
        }
    }
}

fn print_expression_with_ctx<W: Write>(f: &mut Formatter<W>, arena: &ParseArena, expr: &Expression, ctx: PrintCtx) {
    let attrs = printable_attributes(&expr.pexp_attributes);
    let has_attrs = !attrs.is_empty();

    if has_attrs {
        f.string("((");
        print_expression_inner(f, arena, expr, false, ctx);
        f.string(")");
        f.cut();
        print_attributes(f, arena, &expr.pexp_attributes);
        f.string(")");
    } else {
        print_expression_inner(f, arena, expr, false, ctx);
    }
}

fn print_expression_inner<W: Write>(f: &mut Formatter<W>, arena: &ParseArena, expr: &Expression, use_parens: bool, ctx: PrintCtx) {
    // Context-dependent parenthesization (matching OCaml's pprintast lines 606-615)
    if (ctx.pipe || ctx.semi) && matches!(&expr.pexp_desc,
        ExpressionDesc::Pexp_fun { .. }
        | ExpressionDesc::Pexp_match(_, _)
        | ExpressionDesc::Pexp_try(_, _)
        | ExpressionDesc::Pexp_sequence(_, _)) {
        if use_parens { f.string("("); }
        f.string("(");
        print_expression_inner(f, arena, expr, false, PrintCtx::reset());
        f.string(")");
        if use_parens { f.string(")"); }
        return;
    }
    if ctx.ifthenelse && matches!(&expr.pexp_desc,
        ExpressionDesc::Pexp_ifthenelse(_, _, _)
        | ExpressionDesc::Pexp_sequence(_, _)) {
        if use_parens { f.string("("); }
        f.string("(");
        print_expression_inner(f, arena, expr, false, PrintCtx::reset());
        f.string(")");
        if use_parens { f.string(")"); }
        return;
    }
    if ctx.semi && matches!(&expr.pexp_desc,
        ExpressionDesc::Pexp_let(_, _, _)
        | ExpressionDesc::Pexp_letmodule(_, _, _)
        | ExpressionDesc::Pexp_open(_, _, _)
        | ExpressionDesc::Pexp_letexception(_, _)) {
        if use_parens { f.string("("); }
        f.string("(");
        print_expression_inner(f, arena, expr, false, PrintCtx::reset());
        f.string(")");
        if use_parens { f.string(")"); }
        return;
    }

    match &expr.pexp_desc {
        ExpressionDesc::Pexp_ident(lid) => {
            // OCaml wraps operators in parens when used as values
            if let Longident::Lident(name_idx) = arena.get_longident(lid.txt) {
                let name = arena.get_string(*name_idx);
                if ident_needs_parens(name) {
                    f.string("(");
                    f.string(name);
                    f.string(")");
                    return;
                }
            }
            print_longident_idx(f, arena, lid.txt);
        }
        ExpressionDesc::Pexp_constant(c) => {
            let is_negative = match c {
                Constant::Integer(s, _) | Constant::Float(s, _) => s.starts_with('-'),
                _ => false,
            };
            if is_negative {
                f.string("(");
            }
            print_constant(f, c);
            if is_negative {
                f.string(")");
            }
        }
        ExpressionDesc::Pexp_let(rec_flag, bindings, body) => {
            // OCaml: @[<2>%a in@;<1 -2>%a@]
            // where %a is (bindings reset_ctxt) (rf, l) and (expression ctxt) e
            if use_parens {
                f.string("(");
            }
            f.open_box(BoxKind::Box, 2);
            // OCaml: @[<2>%a in@;<1 -2>%a@]
            // where %a is (bindings reset_ctxt) (rf, l)
            // bindings wraps each binding in @[<2>kwd binding@]
            let rec_str = match rec_flag {
                RecFlag::Recursive => "let rec ",
                RecFlag::Nonrecursive => "let ",
            };
            for (i, binding) in bindings.iter().enumerate() {
                // OCaml bindings: pp f "@[<2>%s %a%a@]%a"
                f.open_box(BoxKind::Box, 2);
                if i > 0 {
                    f.newline();
                    f.string("and ");
                } else {
                    f.string(rec_str);
                }
                print_value_binding(f, arena, &binding.pvb_pat, &binding.pvb_expr);
                f.close_box();
                // Item attributes are outside the binding box in OCaml
                print_item_attributes(f, arena, &binding.pvb_attributes);
            }
            f.string(" in");
            f.break_(1, -2); // @;<1 -2> - space or newline with -2 indent offset
            print_expression_with_ctx(f, arena, body, ctx);
            f.close_box();
            if use_parens {
                f.string(")");
            }
        }
        ExpressionDesc::Pexp_fun {
            arg_label,
            default,
            lhs,
            rhs,
            arity,
            is_async,
        } => {
            // OCaml: arity is only printed when explicitly set, not computed
            let arity_value = match arity {
                Arity::Full(n) => Some(*n),
                Arity::Unknown => None,
            };
            if use_parens {
                f.string("(");
            }
            // OCaml: @[<2>%sfun@;%s%a->@;%a@]
            f.open_box(BoxKind::Box, 2);
            if *is_async {
                f.string("async ");
            }
            f.string("fun");
            f.space();
            if let Some(n) = arity_value {
                f.string(&format!("[arity:{}]", n));
            }
            // OCaml's label_exp prints param + trailing break/space
            print_label_exp_with_break(f, arena, arg_label, default, lhs);
            f.string("->");
            f.space();
            print_expression(f, arena, rhs);
            f.close_box();
            if use_parens {
                f.string(")");
            }
        }
        ExpressionDesc::Pexp_apply { funct, args, partial, .. } => {
            // Array access: Array.get(arr, idx) -> arr.(idx)
            // Also handles @res.array.access attribute for backwards compatibility
            // OCaml uses simple_expr for the array expression and expression for the index
            if (parsetree_viewer::is_array_access(arena, expr) || has_attribute(&expr.pexp_attributes, "res.array.access")) && args.len() == 2 {
                let (_, arr) = &args[0];
                let (_, idx) = &args[1];
                if use_parens {
                    f.string("(");
                }
                // OCaml: (simple_expr ctxt) a for the array expression
                print_expression_simple_ctx(f, arena, arr, ctx);
                f.string(".(");
                print_expression(f, arena, idx);
                f.string(")");
                if use_parens {
                    f.string(")");
                }
                return;
            }
            // Array set: Array.set(arr, idx, val) -> arr.(idx) <- val
            // OCaml uses simple_expr for both array and value expressions
            if parsetree_viewer::is_array_set(arena, expr) && args.len() == 3 {
                let (_, arr) = &args[0];
                let (_, idx) = &args[1];
                let (_, val) = &args[2];
                if use_parens {
                    f.string("(");
                }
                // OCaml: (simple_expr ctxt) a for the array expression
                print_expression_simple_ctx(f, arena, arr, ctx);
                f.string(".(");
                print_expression(f, arena, idx);
                f.string(") <- ");
                // OCaml: (simple_expr ctxt) v for the value expression
                print_expression_simple_ctx(f, arena, val, ctx);
                if use_parens {
                    f.string(")");
                }
                return;
            }

            // Infix/prefix operators
            if let Some(op_name) = get_operator_name(funct, arena) {
                if is_infix_operator(op_name) && args.len() == 2 {
                    let (_, lhs) = &args[0];
                    let (_, rhs) = &args[1];
                    if use_parens {
                        f.string("(");
                    }
                    f.open_box(BoxKind::Box, 2);
                    // OCaml: arg1 uses (label_x_expression_param reset_ctxt), arg2 uses (label_x_expression_param ctxt)
                    print_expression2_ctx(f, arena, lhs, PrintCtx::reset());
                    f.space();
                    f.string(op_name);
                    f.space();
                    print_expression2_ctx(f, arena, rhs, ctx);
                    if *partial {
                        f.string(" ...");
                    }
                    f.close_box();
                    if use_parens {
                        f.string(")");
                    }
                    return;
                } else if is_prefix_operator(op_name) && args.len() == 1 {
                    let (_, arg) = &args[0];
                    // OCaml converts ~-, ~+, ~-., ~+., ~~ to shorter forms unless arg is a constant
                    // (to avoid (- 1) being parsed as negative literal)
                    let shortened_op = match op_name {
                        "~-" | "~+" | "~-." | "~+." | "~~" => {
                            if !matches!(&arg.pexp_desc, ExpressionDesc::Pexp_constant(_)) {
                                &op_name[1..] // Remove leading ~
                            } else {
                                op_name
                            }
                        }
                        _ => op_name,
                    };
                    // OCaml: @[<2>%s@;%a@] - no outer parens, uses (simple_expr ctxt)
                    f.open_box(BoxKind::Box, 2);
                    f.string(shortened_op);
                    f.space();
                    print_expression_simple_ctx(f, arena, arg, ctx);
                    if *partial {
                        f.string(" ...");
                    }
                    f.close_box();
                    return;
                }
            }

            // Default: prefix application
            // OCaml: pp f "%a@ %a" (expression2 ctxt) e (list (label_x_expression_param reset_ctxt)) l
            if use_parens {
                f.string("(");
            }
            f.open_box(BoxKind::HOV, 2);
            // OCaml uses (expression2 ctxt) for the function expression
            print_expression2_ctx(f, arena, funct, ctx);
            for (label, arg) in args {
                f.space(); // Break hint between func and args
                // OCaml uses (label_x_expression_param reset_ctxt) for all args in normal apply
                print_arg_with_label_ctx(f, arena, label, arg, PrintCtx::reset());
            }
            if *partial {
                f.string(" ...");
            }
            f.close_box();
            if use_parens {
                f.string(")");
            }
        }
        ExpressionDesc::Pexp_match(scrutinee, cases) => {
            if use_parens {
                f.string("(");
            }
            f.open_box(BoxKind::HV, 0);
            f.open_box(BoxKind::HV, 0);
            f.open_box(BoxKind::Box, 2);
            f.string("match ");
            print_expression(f, arena, scrutinee);
            f.close_box();
            f.space();
            f.string("with");
            f.close_box();
            for case in cases {
                f.space();
                f.string("| ");
                // OCaml: @[<2>%a%a@;->@;%a@]
                f.open_box(BoxKind::Box, 2);
                print_pattern(f, arena, &case.pc_lhs);
                if let Some(guard) = &case.pc_guard {
                    // OCaml: (option (expression ctxt) ~first:"@;when@;")
                    f.space();  // @; before "when"
                    f.string("when");
                    f.space();  // @; after "when"
                    print_expression(f, arena, guard);
                }
                f.space();
                f.string("->");
                f.space();
                // OCaml uses (expression (under_pipe ctxt)) for case RHS,
                // which wraps Pexp_fun/match/try/sequence in parens
                print_expression_with_ctx(f, arena, &case.pc_rhs, ctx.under_pipe());
                f.close_box();
            }
            f.close_box();
            if use_parens {
                f.string(")");
            }
        }
        ExpressionDesc::Pexp_try(body, cases) => {
            if use_parens {
                f.string("(");
            }
            // OCaml: @[<0>@[<hv2>try@ %a@]@ @[<0>with%a@]@]
            f.open_box(BoxKind::Box, 0);
            f.open_box(BoxKind::HV, 2);
            f.string("try");
            f.space();  // @ break between try and body
            print_expression(f, arena, body);
            f.close_box();
            f.space();  // @ break between try-box and with-box
            f.open_box(BoxKind::Box, 0);
            f.string("with");
            for case in cases {
                f.space();
                f.string("| ");
                f.open_box(BoxKind::Box, 2);
                print_pattern(f, arena, &case.pc_lhs);
                if let Some(guard) = &case.pc_guard {
                    f.space();  // @; before "when"
                    f.string("when");
                    f.space();  // @; after "when"
                    print_expression(f, arena, guard);
                }
                f.string(" -> ");
                // OCaml uses (expression (under_pipe ctxt)) for case RHS
                print_expression_with_ctx(f, arena, &case.pc_rhs, ctx.under_pipe());
                f.close_box();
            }
            f.close_box();
            f.close_box();
            if use_parens {
                f.string(")");
            }
        }
        ExpressionDesc::Pexp_tuple(exprs) => {
            // OCaml: @[<hov2>(%a)@] with (list (simple_expr ctxt) ~sep:",@;")
            f.open_box(BoxKind::HOV, 2);
            f.string("(");
            for (i, e) in exprs.iter().enumerate() {
                if i > 0 {
                    f.string(",");
                    f.space();
                }
                print_expression_simple_ctx(f, arena, e, ctx);
            }
            f.string(")");
            f.close_box();
        }
        ExpressionDesc::Pexp_construct(lid, arg) => {
            // Special handling for list syntax
            if let Longident::Lident(name_idx) = arena.get_longident(lid.txt) {
                let name = arena.get_string(*name_idx);
                if name == "::" {
                    let (elements, is_complete) = collect_list_elements(expr, arena);
                    if is_complete && !elements.is_empty() {
                        // OCaml: @[<hv0>[%a]@] with expression (under_semi ctxt) and sep ";@;"
                        f.open_box(BoxKind::HV, 0);
                        f.string("[");
                        for (i, elem) in elements.iter().enumerate() {
                            if i > 0 {
                                f.string(";");
                                f.space();
                            }
                            // OCaml uses expression (under_semi), NOT simple_expr
                            print_expression_under_semi(f, arena, elem);
                        }
                        f.string("]");
                        f.close_box();
                        return;
                    }
                    // Non-complete list: a :: b :: expr
                    // OCaml: list (simple_expr ctxt) ~sep:"@;::@;"
                    for (i, elem) in elements.iter().enumerate() {
                        if i > 0 {
                            f.space();
                            f.string("::");
                            f.space();
                        }
                        print_expression_simple_ctx(f, arena, elem, ctx);
                    }
                    return;
                } else if name == "[]" {
                    f.string("[]");
                    return;
                } else if name == "()" {
                    f.string("()");
                    return;
                }
            }
            if let Some(a) = arg {
                // OCaml: @[<2>%a@;%a@] with (simple_expr ctxt) for arg
                f.open_box(BoxKind::Box, 2);
                print_longident_idx(f, arena, lid.txt);
                f.space();
                print_expression_simple_ctx(f, arena, a, ctx);
                f.close_box();
            } else {
                print_longident_idx(f, arena, lid.txt);
            }
        }
        ExpressionDesc::Pexp_variant(label, arg) => {
            if let Some(a) = arg {
                // OCaml: @[<2>`%s@;%a@] with (simple_expr ctxt) for arg
                f.open_box(BoxKind::Box, 2);
                f.string("`");
                f.string(label);
                f.space();
                print_expression_simple_ctx(f, arena, a, ctx);
                f.close_box();
            } else {
                f.string("`");
                f.string(label);
            }
        }
        ExpressionDesc::Pexp_record(fields, base) => {
            // OCaml: pp f "@[<hv0>@[<hv2>{@;%a%a@]@;}@]"
            f.open_box(BoxKind::HV, 0);
            f.open_box(BoxKind::HV, 2);
            f.string("{");
            f.space(); // @; after {
            if let Some(b) = base {
                print_expression_simple_ctx(f, arena, b, ctx);
                f.string(" with");
                f.space(); // @; after "with"
            }
            for (i, field) in fields.iter().enumerate() {
                if i > 0 {
                    f.string(";");
                    f.space(); // ;@; separator
                }
                // Check for punning: { a } is shorthand for { a = a }
                // OCaml only uses punning when field is Lident (not qualified)
                let lid = arena.get_longident(field.lid.txt);
                let is_punned = if let Longident::Lident(name_idx) = lid {
                    let fname = arena.get_string(*name_idx);
                    // Check if expression is Pexp_ident(Lident(fname))
                    if let ExpressionDesc::Pexp_ident(expr_lid) = &field.expr.pexp_desc {
                        match arena.get_longident(expr_lid.txt) {
                            Longident::Lident(var_idx) => arena.get_string(*var_idx) == fname,
                            _ => false,
                        }
                    } else {
                        false
                    }
                } else {
                    false
                };

                if is_punned {
                    // OCaml: pp f "@[<hov2>%a%s@]"
                    f.open_box(BoxKind::HOV, 2);
                    print_longident_idx(f, arena, field.lid.txt);
                    if field.opt {
                        f.string("?");  // Optional punned field: name?
                    }
                    f.close_box();
                } else {
                    // OCaml: pp f "@[<hov2>%a@;=@;%s%a@]"
                    f.open_box(BoxKind::HOV, 2);
                    print_longident_idx(f, arena, field.lid.txt);
                    f.space(); // @; before =
                    f.string("=");
                    f.space(); // @; after =
                    if field.opt {
                        f.string("?");  // Optional field: name = ?value
                    }
                    // OCaml uses (simple_expr ctxt) for record field values
                    print_expression_simple_ctx(f, arena, &field.expr, ctx);
                    f.close_box();
                }
            }
            f.close_box();
            f.space(); // @; before }
            f.string("}");
            f.close_box();
        }
        ExpressionDesc::Pexp_field(obj, field) => {
            // OCaml: (simple_expr ctxt) for the object
            print_expression_simple_ctx(f, arena, obj, ctx);
            f.string(".");
            print_longident_idx(f, arena, field.txt);
        }
        ExpressionDesc::Pexp_setfield(obj, field, value) => {
            // OCaml: pp f "@[<2>%a.%a@ <-@ %a@]" (simple_expr ctxt) e1 longident_loc li (simple_expr ctxt) e2
            // Note: uses simple_expr ctxt for both object and value
            print_expression_simple_ctx(f, arena, obj, ctx);
            f.string(".");
            print_longident_idx(f, arena, field.txt);
            f.string(" <- ");
            print_expression_simple_ctx(f, arena, value, ctx);
        }
        ExpressionDesc::Pexp_array(elems) => {
            // OCaml: @[<0>@[<2>[|%a|]@]@] with (list (simple_expr (under_semi ctxt)) ~sep:";")
            f.open_box(BoxKind::Box, 0);
            f.open_box(BoxKind::Box, 2);
            f.string("[|");
            for (i, e) in elems.iter().enumerate() {
                if i > 0 {
                    f.string(";");
                }
                print_expression_simple_under_semi_ctx(f, arena, e, ctx);
            }
            f.string("|]");
            f.close_box();
            f.close_box();
        }
        ExpressionDesc::Pexp_ifthenelse(cond, then_expr, else_expr) => {
            if use_parens {
                f.string("(");
            }
            // OCaml: @[<hv0>@[<2>if@ %a@]@;@[<2>then@ %a@]%a@]
            // Both condition and then body use expression (under_ifthenelse ctxt)
            f.open_box(BoxKind::HV, 0);
            f.open_box(BoxKind::Box, 2);
            f.string("if");
            f.space();  // @ break between if and condition
            print_expression_with_ctx(f, arena, cond, ctx.under_ifthenelse());
            f.close_box();
            f.space();  // @; break between if-box and then-box
            f.open_box(BoxKind::Box, 2);
            f.string("then");
            f.space();  // @ break between then and body
            print_expression_with_ctx(f, arena, then_expr, ctx.under_ifthenelse());
            f.close_box();
            if let Some(e) = else_expr {
                f.space();  // @; break before else
                f.open_box(BoxKind::Box, 2);
                f.string("else");
                f.space();  // @; break between else and body
                // OCaml: expression (under_semi ctxt) for else body
                print_expression_with_ctx(f, arena, e, ctx.under_semi());
                f.close_box();
            }
            f.close_box();
            if use_parens {
                f.string(")");
            }
        }
        ExpressionDesc::Pexp_sequence(e1, e2) => {
            if use_parens {
                f.string("(");
            }
            // OCaml: flatten sequence chain and print with @[<hv>%a@] and ";@;" separator
            // Uses expression (under_semi ctxt) for each element
            let mut elements: Vec<&Expression> = Vec::new();
            // Collect flattened sequence elements (only unfold non-attributed sequences)
            elements.push(e1);
            let mut rest = e2.as_ref();
            loop {
                match &rest.pexp_desc {
                    ExpressionDesc::Pexp_sequence(next_e1, next_e2) if rest.pexp_attributes.is_empty() => {
                        elements.push(next_e1);
                        rest = next_e2.as_ref();
                    }
                    _ => {
                        elements.push(rest);
                        break;
                    }
                }
            }
            // Print in HV box: horizontal if fits, vertical otherwise
            f.open_box(BoxKind::HV, 0);
            for (i, elem) in elements.iter().enumerate() {
                // OCaml: each element uses expression (under_semi ctxt)
                // under_semi only wraps specific types in parens, NOT all non-simple exprs
                print_expression_with_ctx(f, arena, elem, ctx.under_semi());
                if i < elements.len() - 1 {
                    f.string(";");
                    f.space(); // ";@;" separator
                }
            }
            f.close_box();
            if use_parens {
                f.string(")");
            }
        }
        ExpressionDesc::Pexp_while(cond, body) => {
            // OCaml: @[<2>while@;%a@;do@;%a@;done@]
            if use_parens {
                f.string("(");
            }
            f.open_box(BoxKind::Box, 2);
            f.string("while");
            f.space();
            print_expression(f, arena, cond);
            f.space();
            f.string("do");
            f.space();
            print_expression(f, arena, body);
            f.space();
            f.string("done");
            f.close_box();
            if use_parens {
                f.string(")");
            }
        }
        ExpressionDesc::Pexp_for(pat, start, end, dir, body) => {
            // OCaml: @[<hv0>@[<hv2>@[<2>for %a =@;%a@;%a%a@;do@]@;%a@]@;done@]
            if use_parens {
                f.string("(");
            }
            f.open_box(BoxKind::HV, 0);
            f.open_box(BoxKind::HV, 2);
            f.open_box(BoxKind::Box, 2);
            f.string("for ");
            print_pattern(f, arena, pat);
            f.string(" =");
            f.space();
            print_expression(f, arena, start);
            f.space();
            let dir_str = match dir {
                DirectionFlag::Upto => "to",
                DirectionFlag::Downto => "downto",
            };
            f.string(dir_str);
            f.space();
            print_expression(f, arena, end);
            f.space();
            f.string("do");
            f.close_box();
            f.space();
            print_expression(f, arena, body);
            f.close_box();
            f.space();
            f.string("done");
            f.close_box();
            if use_parens {
                f.string(")");
            }
        }
        ExpressionDesc::Pexp_constraint(e, t) => {
            f.string("(");
            print_expression(f, arena, e);
            f.string(" : ");
            print_core_type(f, arena, t);
            f.string(")");
        }
        ExpressionDesc::Pexp_coerce(e, t1, t2) => {
            f.string("(");
            print_expression(f, arena, e);
            if let Some(t) = t1 {
                f.string(" : ");
                print_core_type(f, arena, t);
            }
            f.string(" :> ");
            print_core_type(f, arena, t2);
            f.string(")");
        }
        ExpressionDesc::Pexp_send(e, meth) => {
            // OCaml: pp f "@[<hov2>%a#%s@]" (simple_expr ctxt) e s.txt
            f.open_box(super::formatter::BoxKind::HOV, 2);
            print_expression_simple_ctx(f, arena, e, ctx);
            f.string("#");
            f.string(&meth.txt);
            f.close_box();
        }
        ExpressionDesc::Pexp_letmodule(name, mexpr, body) => {
            // OCaml: pp f "@[<hov2>let@ module@ %s@ =@ %a@ in@ %a@]"
            if use_parens {
                f.string("(");
            }
            f.open_box(BoxKind::HOV, 2);
            f.string("let");
            f.space();
            f.string("module");
            f.space();
            f.string(&name.txt);
            f.space();
            f.string("=");
            f.space();
            print_module_expr(f, arena, mexpr);
            f.space();
            f.string("in");
            f.space();
            print_expression_with_ctx(f, arena, body, ctx);
            f.close_box();
            if use_parens {
                f.string(")");
            }
        }
        ExpressionDesc::Pexp_letexception(ext, body) => {
            // OCaml: pp f "@[<hov2>let@ exception@ %a@ in@ %a@]"
            if use_parens {
                f.string("(");
            }
            f.open_box(BoxKind::HOV, 2);
            f.string("let");
            f.space();
            f.string("exception");
            f.space();
            print_extension_constructor(f, arena, ext);
            f.space();
            f.string("in");
            f.space();
            print_expression_with_ctx(f, arena, body, ctx);
            f.close_box();
            if use_parens {
                f.string(")");
            }
        }
        ExpressionDesc::Pexp_assert(e) => {
            // OCaml: @[<hov2>assert@ %a@] - no parens
            // Parens come from (simple_expr ctxt) when needed
            f.open_box(BoxKind::HOV, 2);
            f.string("assert ");
            print_expression_simple_ctx(f, arena, e, ctx);
            f.close_box();
        }
        ExpressionDesc::Pexp_newtype(name, body) => {
            // OCaml: pp f "fun@;(type@;%s)@;->@;%a"
            f.string("fun");
            f.space(); // @; after "fun"
            f.string("(type");
            f.space(); // @; after "(type"
            f.string(&name.txt);
            f.string(")");
            f.space(); // @; after ")"
            f.string("->");
            f.space(); // @; after "->"
            print_expression(f, arena, body);
        }
        ExpressionDesc::Pexp_pack(mexpr) => {
            // OCaml: pp f "(module@;%a)" (module_expr ctxt) me
            f.string("(module");
            f.space(); // @; after "(module"
            print_module_expr(f, arena, mexpr);
            f.string(")");
        }
        ExpressionDesc::Pexp_open(override_flag, lid, body) => {
            // OCaml: pp f "@[<2>let open%s %a in@;%a@]"
            if use_parens {
                f.string("(");
            }
            f.open_box(BoxKind::Box, 2);
            f.string("let open");
            if matches!(override_flag, OverrideFlag::Override) {
                f.string("!");
            }
            f.string(" ");
            print_longident_idx(f, arena, lid.txt);
            f.string(" in");
            f.space(); // @;
            print_expression_with_ctx(f, arena, body, ctx);
            f.close_box();
            if use_parens {
                f.string(")");
            }
        }
        ExpressionDesc::Pexp_extension((name, payload)) => {
            // OCaml: @[<2>[%%%s@ %a]@] — structural box with indent 2
            f.open_box(BoxKind::Box, 2);
            f.string("[%");
            f.string(&name.txt);
            f.space();
            print_payload(f, arena, payload);
            f.string("]");
            f.close_box();
        }
        ExpressionDesc::Pexp_await(e) => {
            // OCaml: @[<hov2>await@ %a@]
            f.open_box(BoxKind::HOV, 2);
            f.string("await");
            f.space();  // @ break between await and expression
            // Check if expression is "simple" enough to not need parens
            // Note: attributed expressions will get their own parens from print_expression
            let has_attrs = !printable_attributes(&e.pexp_attributes).is_empty();
            let is_simple = matches!(
                &e.pexp_desc,
                ExpressionDesc::Pexp_ident(_)
                    | ExpressionDesc::Pexp_constant(_)
                    | ExpressionDesc::Pexp_construct(_, None)
                    | ExpressionDesc::Pexp_tuple(_)
                    | ExpressionDesc::Pexp_array(_)
                    | ExpressionDesc::Pexp_record(_, _)
            );
            // If has attributes, print_expression already produces ((expr)[@attr ])
            // so we don't need to add extra parens
            let needs_parens = !is_simple && !has_attrs;
            if needs_parens {
                f.string("(");
            }
            print_expression(f, arena, e);
            if needs_parens {
                f.string(")");
            }
            f.close_box();
        }
        ExpressionDesc::Pexp_jsx_element(jsx) => {
            print_jsx_element(f, arena, jsx);
        }
    }
}

// ============================================================================
// JSX Printing
// ============================================================================

fn jsx_tag_name_to_string(tag_name: &JsxTagName, arena: &ParseArena) -> String {
    match tag_name {
        JsxTagName::Lower(name) => name.clone(),
        JsxTagName::QualifiedLower { path, name } => {
            let path_str = print_longident_to_string(arena.get_longident(*path), arena);
            format!("{}.{}", path_str, name)
        }
        JsxTagName::Upper(lident) => {
            print_longident_to_string(arena.get_longident(*lident), arena)
        }
        JsxTagName::Invalid(name) => name.clone(),
    }
}

fn print_longident_to_string(lid: &Longident, arena: &ParseArena) -> String {
    match lid {
        Longident::Lident(name_idx) => arena.get_string(*name_idx).to_string(),
        Longident::Ldot(prefix, name_idx) => {
            let prefix_str = print_longident_to_string(prefix.as_ref(), arena);
            let name = arena.get_string(*name_idx);
            format!("{}.{}", prefix_str, name)
        }
        Longident::Lapply(left, right) => {
            let left_str = print_longident_to_string(left.as_ref(), arena);
            let right_str = print_longident_to_string(right.as_ref(), arena);
            format!("{}({})", left_str, right_str)
        }
    }
}

fn print_jsx_element<W: Write>(f: &mut Formatter<W>, arena: &ParseArena, jsx: &JsxElement) {
    match jsx {
        JsxElement::Fragment(frag) => {
            f.string("<>");
            for (i, child) in frag.children.iter().enumerate() {
                if i > 0 {
                    f.space(); // OCaml uses "@ " (space with optional break) between children
                }
                print_expression_simple(f, arena, child);
            }
            f.string("</>");
        }
        JsxElement::Unary(elem) => {
            let name = jsx_tag_name_to_string(&elem.tag_name.txt, arena);
            if elem.props.is_empty() {
                f.string(&format!("<{} />", name));
            } else {
                f.string(&format!("<{} ", name));
                print_jsx_props(f, arena, &elem.props);
                f.string(" />");
            }
        }
        JsxElement::Container(elem) => {
            let name = jsx_tag_name_to_string(&elem.tag_name_start.txt, arena);
            let closing_name = match &elem.closing_tag {
                None => String::new(),
                Some(closing) => {
                    let close_name = jsx_tag_name_to_string(&closing.name.txt, arena);
                    format!("</{}>", close_name)
                }
            };
            if elem.props.is_empty() {
                f.string(&format!("<{}>", name));
                for (i, child) in elem.children.iter().enumerate() {
                    if i > 0 {
                        f.space(); // OCaml uses "@ " between children
                    }
                    print_expression_simple(f, arena, child);
                }
                f.string(&closing_name);
            } else {
                f.string(&format!("<{} ", name));
                print_jsx_props(f, arena, &elem.props);
                f.string(">");
                for (i, child) in elem.children.iter().enumerate() {
                    if i > 0 {
                        f.space(); // OCaml uses "@ " between children
                    }
                    print_expression_simple(f, arena, child);
                }
                f.string(&closing_name);
            }
        }
    }
}

fn print_jsx_props<W: Write>(f: &mut Formatter<W>, arena: &ParseArena, props: &[JsxProp]) {
    for (i, prop) in props.iter().enumerate() {
        if i > 0 {
            f.string(" ");
        }
        print_jsx_prop(f, arena, prop);
    }
}

fn print_jsx_prop<W: Write>(f: &mut Formatter<W>, arena: &ParseArena, prop: &JsxProp) {
    match prop {
        JsxProp::Punning { optional, name } => {
            if *optional {
                f.string("?");
            }
            f.string(&name.txt);
        }
        JsxProp::Value { name, optional, value } => {
            f.string(&name.txt);
            f.string("=");
            if *optional {
                f.string("?");
            }
            print_expression_simple(f, arena, value);
        }
        JsxProp::Spreading { expr, .. } => {
            f.string("{...");
            print_expression_simple(f, arena, expr);
            f.string("}");
        }
    }
}

// ============================================================================
// Patterns
// ============================================================================

fn print_pattern<W: Write>(f: &mut Formatter<W>, arena: &ParseArena, pat: &Pattern) {
    let has_attrs = !pat.ppat_attributes.is_empty();
    if has_attrs {
        f.string("((");
    }
    print_pattern_inner(f, arena, pat);
    if has_attrs {
        f.string(")");
        // OCaml: pp f "((%a)%a)" — NO cut/break between ) and attributes for patterns
        // (Unlike expressions which use @, = Cut between ) and attributes)
        print_attributes(f, arena, &pat.ppat_attributes);
        f.string(")");
    }
}

/// Print a pattern in "simple" context - non-simple patterns get wrapped in parens.
/// This matches OCaml's simple_pattern which wraps non-simple patterns via:
/// `| _ -> paren true (pattern ctxt) f x`
fn print_simple_pattern<W: Write>(f: &mut Formatter<W>, arena: &ParseArena, pat: &Pattern) {
    if pattern_is_simple(pat, arena) {
        print_pattern(f, arena, pat);
    } else {
        f.string("(");
        print_pattern(f, arena, pat);
        f.string(")");
    }
}

fn print_pattern_inner<W: Write>(f: &mut Formatter<W>, arena: &ParseArena, pat: &Pattern) {
    match &pat.ppat_desc {
        PatternDesc::Ppat_any => {
            f.string("_");
        }
        PatternDesc::Ppat_var(name) => {
            f.string(&name.txt);
        }
        PatternDesc::Ppat_alias(p, name) => {
            // OCaml: @[<2>%a@;as@;%a@]
            f.open_box(BoxKind::Box, 2);
            print_pattern(f, arena, p);
            f.space(); // @; before as
            f.string("as");
            f.space(); // @; after as
            f.string(&name.txt);
            f.close_box();
        }
        PatternDesc::Ppat_constant(c) => {
            // Negative numbers need parentheses in patterns
            if constant_is_negative(c) {
                f.string("(");
                print_constant(f, c);
                f.string(")");
            } else {
                print_constant(f, c);
            }
        }
        PatternDesc::Ppat_interval(c1, c2) => {
            // Negative numbers in intervals need parentheses
            let c1_needs_parens = constant_is_negative(c1);
            let c2_needs_parens = constant_is_negative(c2);
            if c1_needs_parens {
                f.string("(");
            }
            print_constant(f, c1);
            if c1_needs_parens {
                f.string(")");
            }
            f.string("..");
            if c2_needs_parens {
                f.string("(");
            }
            print_constant(f, c2);
            if c2_needs_parens {
                f.string(")");
            }
        }
        PatternDesc::Ppat_tuple(pats) => {
            // OCaml: @[<1>(%a)@] with sep ",@;"
            f.open_box(BoxKind::Box, 1);
            f.string("(");
            for (i, p) in pats.iter().enumerate() {
                if i > 0 {
                    f.string(",");
                    f.space(); // ,@; separator
                }
                // Alias patterns inside tuples need parentheses
                let needs_parens = matches!(p.ppat_desc, PatternDesc::Ppat_alias(..));
                if needs_parens {
                    f.string("(");
                }
                print_pattern(f, arena, p);
                if needs_parens {
                    f.string(")");
                }
            }
            f.string(")");
            f.close_box();
        }
        PatternDesc::Ppat_construct(lid, arg) => {
            // Special case for cons pattern - no outer parens by default
            if let Longident::Lident(name_idx) = arena.get_longident(lid.txt) {
                let name = arena.get_string(*name_idx);
                if name == "::" {
                    if let Some(a) = arg {
                        if let PatternDesc::Ppat_tuple(elements) = &a.ppat_desc {
                            if elements.len() == 2 {
                                // Left element needs parens if it's an alias or constraint
                                let left_needs_parens = pattern_needs_parens_as_param(&elements[0], arena);
                                if left_needs_parens {
                                    f.string("(");
                                }
                                print_pattern(f, arena, &elements[0]);
                                if left_needs_parens {
                                    f.string(")");
                                }
                                f.string("::");
                                // Right element needs parens if it's an alias (so it doesn't
                                // look like `(x::xs) as tail`)
                                let right_needs_parens = matches!(
                                    &elements[1].ppat_desc,
                                    PatternDesc::Ppat_alias(..)
                                );
                                if right_needs_parens {
                                    f.string("(");
                                }
                                print_pattern(f, arena, &elements[1]);
                                if right_needs_parens {
                                    f.string(")");
                                }
                                return;
                            }
                        }
                    }
                }
            }
            if let Some(a) = arg {
                // OCaml: @[<2>%a@;%a@] with simple_pattern for arg
                f.open_box(BoxKind::Box, 2);
                print_longident_idx(f, arena, lid.txt);
                f.space();
                print_simple_pattern(f, arena, a);
                f.close_box();
            } else {
                print_longident_idx(f, arena, lid.txt);
            }
        }
        PatternDesc::Ppat_variant(label, arg) => {
            if let Some(a) = arg {
                // OCaml: @[<2>`%s@;%a@] with simple_pattern
                f.open_box(BoxKind::Box, 2);
                f.string("`");
                f.string(label);
                f.space();
                print_simple_pattern(f, arena, a);
                f.close_box();
            } else {
                f.string("`");
                f.string(label);
            }
        }
        PatternDesc::Ppat_record(fields, closed) => {
            // OCaml closed: @[<2>{@;%a@;}@]  with sep ";@;"
            // OCaml open:   @[<2>{@;%a;_}@]  with sep ";@;"
            f.open_box(BoxKind::Box, 2);
            f.string("{");
            f.space(); // @; after {
            for (i, field) in fields.iter().enumerate() {
                if i > 0 {
                    f.string(";");
                    f.space(); // ;@; separator
                }
                // OCaml only punns for simple Lident fields, not qualified Ldot fields
                let is_punned = match arena.get_longident(field.lid.txt) {
                    Longident::Lident(name_idx) => {
                        let fname = arena.get_string(*name_idx);
                        field.pat.ppat_attributes.is_empty() &&
                            matches!(&field.pat.ppat_desc, PatternDesc::Ppat_var(var) if var.txt == fname)
                    }
                    _ => false,
                };

                if is_punned {
                    // OCaml: @[<2>%a%s@]
                    f.open_box(BoxKind::Box, 2);
                    print_longident_idx(f, arena, field.lid.txt);
                    if field.opt {
                        f.string("?");
                    }
                    f.close_box();
                } else {
                    // OCaml: @[<2>%a%s@;=@;%a@]
                    f.open_box(BoxKind::Box, 2);
                    print_longident_idx(f, arena, field.lid.txt);
                    if field.opt {
                        f.string("?");
                    }
                    f.space(); // @; before =
                    f.string("=");
                    f.space(); // @; after =
                    // Print with parens for alias patterns (pattern1 context)
                    let needs_parens = matches!(&field.pat.ppat_desc,
                        PatternDesc::Ppat_alias(..) | PatternDesc::Ppat_or(..)
                    );
                    if needs_parens {
                        f.string("(");
                    }
                    print_pattern(f, arena, &field.pat);
                    if needs_parens {
                        f.string(")");
                    }
                    f.close_box();
                }
            }
            if matches!(closed, ClosedFlag::Open) {
                f.string(";_}");
            } else {
                f.space(); // @; before }
                f.string("}");
            }
            f.close_box();
        }
        PatternDesc::Ppat_array(pats) => {
            // OCaml: pp f "@[<2>[|%a|]@]" with sep ";"
            f.open_box(BoxKind::Box, 2);
            f.string("[|");
            for (i, p) in pats.iter().enumerate() {
                if i > 0 {
                    f.string(";");
                }
                print_pattern(f, arena, p);
            }
            f.string("|]");
            f.close_box();
        }
        PatternDesc::Ppat_or(p1, p2) => {
            print_pattern(f, arena, p1);
            f.string("|");
            print_pattern(f, arena, p2);
        }
        PatternDesc::Ppat_constraint(p, t) => {
            // OCaml: pp f "@[<2>(%a@;:@;%a)@]" (pattern1 ctxt) p (core_type ctxt) ct
            f.open_box(BoxKind::Box, 2);
            f.string("(");
            // Only alias and or patterns need extra parens (cons is handled by pattern1 without parens)
            let needs_inner_parens = matches!(
                p.ppat_desc,
                PatternDesc::Ppat_alias(..) | PatternDesc::Ppat_or(..)
            );
            if needs_inner_parens {
                f.string("(");
            }
            print_pattern(f, arena, p);
            if needs_inner_parens {
                f.string(")");
            }
            f.space(); // @; before :
            f.string(":");
            f.space(); // @; after :
            print_core_type(f, arena, t);
            f.string(")");
            f.close_box();
        }
        PatternDesc::Ppat_type(lid) => {
            f.string("#");
            print_longident_idx(f, arena, lid.txt);
        }
        PatternDesc::Ppat_unpack(name) => {
            // OCaml: pp f "(module@ %s)@ " s.txt
            f.string("(module");
            f.space(); // @ after "(module"
            f.string(&name.txt);
            f.string(")");
            f.space(); // @ trailing break
        }
        PatternDesc::Ppat_exception(p) => {
            f.string("exception ");
            // Or-patterns and alias patterns inside exception need parentheses
            let needs_parens = matches!(
                p.ppat_desc,
                PatternDesc::Ppat_or(..) | PatternDesc::Ppat_alias(..)
            );
            if needs_parens {
                f.string("(");
            }
            print_pattern(f, arena, p);
            if needs_parens {
                f.string(")");
            }
        }
        PatternDesc::Ppat_extension((name, payload)) => {
            f.open_box(BoxKind::H, 2);
            f.string("[%");
            f.string(&name.txt);
            f.space();
            print_payload(f, arena, payload);
            f.string("]");
            f.close_box();
        }
        PatternDesc::Ppat_open(lid, p) => {
            print_longident_idx(f, arena, lid.txt);
            f.string(".(");
            print_pattern(f, arena, p);
            f.string(")");
        }
    }
}

// ============================================================================
// Types
// ============================================================================

/// Print core_type at level 1 (wraps arrow and alias types in parens).
/// Matches OCaml's core_type1 which falls through to paren true (core_type ctxt) for those.
fn print_core_type1<W: Write>(f: &mut Formatter<W>, arena: &ParseArena, typ: &CoreType) {
    let needs_parens = matches!(
        typ.ptyp_desc,
        CoreTypeDesc::Ptyp_arrow { .. } | CoreTypeDesc::Ptyp_alias(..)
    );
    if needs_parens {
        f.string("(");
        print_core_type(f, arena, typ);
        f.string(")");
    } else {
        print_core_type(f, arena, typ);
    }
}

fn print_core_type<W: Write>(f: &mut Formatter<W>, arena: &ParseArena, typ: &CoreType) {
    // Types with attributes are wrapped: ((type)[@attr ])
    let has_attrs = printable_attributes(&typ.ptyp_attributes).len() > 0;
    if has_attrs {
        f.string("((");
    }
    // OCaml's core_type wraps fallthrough types (those handled by core_type1) in @[<2>...@]
    // Types that already handle their own Box(2): Ptyp_arrow, Ptyp_alias, Ptyp_poly (non-empty vars)
    // Ptyp_poly with empty vars just recurses into core_type, so no extra box needed
    let needs_box = match &typ.ptyp_desc {
        CoreTypeDesc::Ptyp_arrow { .. } => false,
        CoreTypeDesc::Ptyp_alias(..) => false,
        CoreTypeDesc::Ptyp_poly(vars, _) if !vars.is_empty() => false,
        CoreTypeDesc::Ptyp_poly(_, _) => false, // empty vars recurses into core_type
        _ => true,
    };
    if needs_box {
        f.open_box(BoxKind::Box, 2);
    }
    print_core_type_inner(f, arena, typ);
    if needs_box {
        f.close_box();
    }
    if has_attrs {
        f.string(")");
        print_attributes(f, arena, &typ.ptyp_attributes);
        f.string(")");
    }
}

fn print_core_type_inner<W: Write>(f: &mut Formatter<W>, arena: &ParseArena, typ: &CoreType) {
    match &typ.ptyp_desc {
        CoreTypeDesc::Ptyp_any => {
            f.string("_");
        }
        CoreTypeDesc::Ptyp_var(name) => {
            f.string("'");
            f.string(name);
        }
        CoreTypeDesc::Ptyp_arrow { arg, ret, arity } => {
            // OCaml: pp f "@[<2>%a@;->@;%a%s@]"
            // Structural box with indent 2, break hints around ->
            f.open_box(BoxKind::Box, 2);
            print_arg_label(f, arena, &arg.lbl);
            // Arrow and alias types as args need parentheses (OCaml's core_type1
            // wraps them via the `| _ -> paren true (core_type ctxt) f x` fallthrough)
            let arg_needs_parens = matches!(
                arg.typ.ptyp_desc,
                CoreTypeDesc::Ptyp_arrow { .. } | CoreTypeDesc::Ptyp_alias(..)
            );
            if arg_needs_parens {
                f.string("(");
            }
            print_core_type(f, arena, &arg.typ);
            if arg_needs_parens {
                f.string(")");
            }
            // Print attributes on the argument (after the type)
            print_attributes(f, arena, &arg.attrs);
            f.space(); // Break hint before ->
            f.string("->");
            f.space(); // Break hint after ->
            print_core_type(f, arena, ret);
            if let Arity::Full(n) = arity {
                f.string(&format!(" (a:{})", n));
            }
            f.close_box();
        }
        CoreTypeDesc::Ptyp_tuple(types) => {
            // OCaml: pp f "(%a)" (list (core_type1 ctxt) ~sep:"@;*@;") l
            f.string("(");
            for (i, t) in types.iter().enumerate() {
                if i > 0 {
                    f.space(); // @; before *
                    f.string("*");
                    f.space(); // @; after *
                }
                // Alias types inside tuple need parens for precedence
                let needs_parens = matches!(t.ptyp_desc, CoreTypeDesc::Ptyp_alias(..));
                if needs_parens {
                    f.string("(");
                }
                print_core_type(f, arena, t);
                if needs_parens {
                    f.string(")");
                }
            }
            f.string(")");
        }
        CoreTypeDesc::Ptyp_constr(lid, args) => {
            // OCaml: | [x] -> pp f "%a@;" (core_type1 ctxt) x
            //        | _ -> list ~first:"(" ~last:")@;" (core_type ctxt) ~sep:",@;" f l
            if !args.is_empty() {
                if args.len() == 1 {
                    // Single arg uses core_type1 (no extra Box wrapping)
                    print_core_type_inner(f, arena, &args[0]);
                    f.space(); // @;
                } else {
                    f.string("(");
                    for (i, t) in args.iter().enumerate() {
                        if i > 0 {
                            f.string(",");
                            f.space(); // ,@;
                        }
                        print_core_type(f, arena, t);
                    }
                    f.string(")");
                    f.space(); // )@;
                }
            }
            print_longident_idx(f, arena, lid.txt);
        }
        CoreTypeDesc::Ptyp_object(fields, closed) => {
            // OCaml: pp f "@[<hov2><@ %a%a@ > @]"
            f.open_box(BoxKind::HOV, 2);
            f.string("<");
            f.space(); // @ after <
            for (i, field) in fields.iter().enumerate() {
                if i > 0 {
                    f.string(";");
                }
                match field {
                    ObjectField::Otag(name, attrs, typ) => {
                        // OCaml: pp f "@[<hov2>%s: %a@ %a@ @]"
                        f.open_box(BoxKind::HOV, 2);
                        f.string(&name.txt);
                        f.string(": ");
                        print_core_type(f, arena, typ);
                        f.space(); // @ after type
                        let printable = printable_attributes(attrs);
                        if !printable.is_empty() {
                            print_attributes(f, arena, attrs);
                        }
                        f.space(); // @ before close
                        f.close_box();
                    }
                    ObjectField::Oinherit(typ) => {
                        // OCaml: pp f "@[<hov2>%a@ @]"
                        f.open_box(BoxKind::HOV, 2);
                        print_core_type(f, arena, typ);
                        f.space(); // @ after type
                        f.close_box();
                    }
                }
            }
            // Open object suffix
            // OCaml field_var: if fields empty then ".." else " ;.."
            if matches!(closed, ClosedFlag::Open) {
                if fields.is_empty() {
                    f.string("..");
                } else {
                    f.string(" ;..");
                }
            }
            f.space(); // @ before >
            f.string(">");
            f.space(); // @ after >
            f.close_box();
        }
        CoreTypeDesc::Ptyp_alias(t, name) => {
            // OCaml: pp f "@[<2>%a@;as@;'%s@]" (core_type1 ctxt) ct s
            f.open_box(BoxKind::Box, 2);
            // Arrow types inside alias need parens for precedence
            let needs_parens = matches!(t.ptyp_desc, CoreTypeDesc::Ptyp_arrow { .. });
            if needs_parens {
                f.string("(");
            }
            print_core_type(f, arena, t);
            if needs_parens {
                f.string(")");
            }
            f.space();
            f.string("as");
            f.space();
            f.string("'");
            f.string(name);
            f.close_box();
        }
        CoreTypeDesc::Ptyp_variant(rows, closed, labels) => {
            // OCaml: pp f "@[<2>[%a%a]@]" ...
            f.open_box(BoxKind::Box, 2);
            f.string("[");

            // Empty variant cases
            if rows.is_empty() {
                match closed {
                    ClosedFlag::Open => f.string(">"),
                    ClosedFlag::Closed => {}
                }
            } else {
                // Non-empty: prefix + break, then list
                // OCaml: pp f "%s@;%a" prefix (list type_variant_helper ~sep:"@;<1 -2>| ") rows
                match (closed, &labels) {
                    (ClosedFlag::Closed, None) => {
                        // Just break hint (no prefix character)
                        f.space();
                    }
                    (ClosedFlag::Closed, Some(_)) => {
                        f.string("<");
                        f.space();
                    }
                    (ClosedFlag::Open, _) => {
                        f.string(">");
                        f.space();
                    }
                }

                for (i, row) in rows.iter().enumerate() {
                    if i > 0 {
                        // Separator: @;<1 -2>| = break with offset -2, then "| "
                        // The -2 offset causes the "|" to align with the "["
                        f.break_(1, -2);
                        f.string("| ");
                    }
                    match row {
                        RowField::Rtag(label, attrs, empty, types) => {
                            // OCaml: @[<2>%a%a@;%a@]
                            f.open_box(BoxKind::Box, 2);
                            f.string("`");
                            f.string(&label.txt);
                            if !*empty || !types.is_empty() {
                                // of@; = " of " with break hint
                                f.space();
                                f.string("of");
                                f.space();
                                for (j, t) in types.iter().enumerate() {
                                    if j > 0 {
                                        f.string("&");
                                    }
                                    print_core_type(f, arena, t);
                                }
                            }
                            // Trailing break (@;) then attrs
                            f.space();
                            print_attributes(f, arena, attrs);
                            f.close_box();
                        }
                        RowField::Rinherit(typ) => {
                            print_core_type(f, arena, typ);
                        }
                    }
                }
            }

            // Low labels (present for some polymorphic variants)
            // OCaml: Some [] | None -> () (do nothing)
            //        Some xs -> print "> " then labels
            if let Some(lbls) = labels {
                if !lbls.is_empty() {
                    f.string("> ");
                    for (i, lbl) in lbls.iter().enumerate() {
                        if i > 0 {
                            f.string(" ");
                        }
                        f.string("`");
                        f.string(lbl);
                    }
                }
            }
            f.string("]");
            f.close_box();
        }
        CoreTypeDesc::Ptyp_poly(vars, t) => {
            if vars.is_empty() {
                // OCaml: Ptyp_poly ([], ct) -> core_type ctxt f ct
                print_core_type(f, arena, t);
            } else {
                // OCaml: @[<2>%a@;.@;%a@] with tyvars separated by @;
                f.open_box(BoxKind::Box, 2);
                for (i, var) in vars.iter().enumerate() {
                    if i > 0 {
                        f.space(); // @; between tyvars
                    }
                    f.string("'");
                    f.string(&var.txt);
                }
                f.space(); // @; before .
                f.string(".");
                f.space(); // @; after .
                print_core_type(f, arena, t);
                f.close_box();
            }
        }
        CoreTypeDesc::Ptyp_package((lid, constraints)) => {
            // OCaml: @[<hov2>(module@ %a@ with@ %a)@]
            f.open_box(BoxKind::HOV, 2);
            f.string("(module");
            f.space(); // @ after "(module"
            print_longident_idx(f, arena, lid.txt);
            for (i, (path, typ)) in constraints.iter().enumerate() {
                if i == 0 {
                    f.space(); // @ before "with"
                    f.string("with");
                    f.space(); // @ after "with"
                } else {
                    f.space(); // @ before "and"
                    f.string("and");
                    f.space(); // @ after "and"
                }
                // OCaml: "type %a@ =@ %a"
                f.string("type");
                f.space();
                print_longident_idx(f, arena, path.txt);
                f.space(); // @ before "="
                f.string("=");
                f.space(); // @ after "="
                print_core_type(f, arena, typ);
            }
            f.string(")");
            f.close_box();
        }
        CoreTypeDesc::Ptyp_extension((name, payload)) => {
            f.open_box(BoxKind::H, 2);
            f.string("[%");
            f.string(&name.txt);
            f.space();
            print_payload(f, arena, payload);
            f.string("]");
            f.close_box();
        }
    }
    // Note: attributes handled in print_core_type wrapper
}

// ============================================================================
// Labels and arguments
// ============================================================================

fn print_arg_label<W: Write>(f: &mut Formatter<W>, arena: &ParseArena, label: &ArgLabel) {
    // In OCaml types, labeled arguments are printed as "label:type" without ~
    match label {
        ArgLabel::Nolabel => {}
        ArgLabel::Labelled(s) => {
            f.string(arena.get_string(s.txt));
            f.string(":");
        }
        ArgLabel::Optional(s) => {
            f.string("?");
            f.string(arena.get_string(s.txt));
            f.string(":");
        }
    }
}

/// Print arg label for expression context (uses ~)
fn print_arg_label_expr<W: Write>(f: &mut Formatter<W>, arena: &ParseArena, label: &ArgLabel) {
    match label {
        ArgLabel::Nolabel => {}
        ArgLabel::Labelled(s) => {
            f.string("~");
            f.string(arena.get_string(s.txt));
            f.string(":");
        }
        ArgLabel::Optional(s) => {
            f.string("?");
            f.string(arena.get_string(s.txt));
            f.string(":");
        }
    }
}

fn print_arg_with_label<W: Write>(f: &mut Formatter<W>, arena: &ParseArena, label: &ArgLabel, arg: &Expression) {
    print_arg_with_label_ctx(f, arena, label, arg, PrintCtx::reset());
}

/// Context-aware version of print_arg_with_label.
/// OCaml's `label_x_expression_param ctxt` passes context through to simple_expr and expression2.
fn print_arg_with_label_ctx<W: Write>(f: &mut Formatter<W>, arena: &ParseArena, label: &ArgLabel, arg: &Expression, ctx: PrintCtx) {
    match label {
        ArgLabel::Labelled(name) => {
            let name_str = arena.get_string(name.txt);
            if let ExpressionDesc::Pexp_ident(lid) = &arg.pexp_desc {
                if let Longident::Lident(arg_name_idx) = arena.get_longident(lid.txt) {
                    let arg_name = arena.get_string(*arg_name_idx);
                    if arg_name == name_str && arg.pexp_attributes.is_empty() {
                        f.string("~");
                        f.string(name_str);
                        return;
                    }
                }
            }
            f.string("~");
            f.string(name_str);
            f.string(":");
            // OCaml uses simple_expr ctxt for labeled args
            print_expression_simple_ctx(f, arena, arg, ctx);
        }
        ArgLabel::Optional(name) => {
            let name_str = arena.get_string(name.txt);
            if let ExpressionDesc::Pexp_ident(lid) = &arg.pexp_desc {
                if let Longident::Lident(arg_name_idx) = arena.get_longident(lid.txt) {
                    let arg_name = arena.get_string(*arg_name_idx);
                    if arg_name == name_str && arg.pexp_attributes.is_empty() {
                        f.string("?");
                        f.string(name_str);
                        return;
                    }
                }
            }
            f.string("?");
            f.string(name_str);
            f.string(":");
            // OCaml uses simple_expr ctxt for optional args
            print_expression_simple_ctx(f, arena, arg, ctx);
        }
        ArgLabel::Nolabel => {
            // OCaml uses expression2 ctxt for unlabeled args (label_x_expression_param -> Nolabel -> expression2)
            print_expression2_ctx(f, arena, arg, ctx);
        }
    }
}

// ============================================================================
// Constants
// ============================================================================

fn print_constant<W: Write>(f: &mut Formatter<W>, c: &Constant) {
    match c {
        Constant::Integer(s, suffix) => {
            f.string(s);
            if let Some(c) = suffix {
                f.string(&c.to_string());
            }
        }
        Constant::Char(i) => {
            let escaped = match *i {
                0x5C => "\\\\".to_string(),
                0x27 => "\\'".to_string(),
                0x0A => "\\n".to_string(),
                0x09 => "\\t".to_string(),
                0x08 => "\\b".to_string(),
                0x0D => "\\r".to_string(),
                n if (0x20..=0x7E).contains(&n) => {
                    if let Some(c) = char::from_u32(n as u32) {
                        c.to_string()
                    } else {
                        format!("\\{}", n)
                    }
                }
                n => format!("\\{}", n),
            };
            f.string("'");
            f.string(&escaped);
            f.string("'");
        }
        Constant::String(s, delim) => {
            // Match OCaml's pprintast.ml:
            // - None -> %S (quoted string with escaping)
            // - Some(delim) -> {delim|...|delim} (delimited string)
            if let Some(d) = delim {
                f.string("{");
                f.string(d);
                f.string("|");
                f.string(s);
                f.string("|");
                f.string(d);
                f.string("}");
            } else {
                f.string("\"");
                f.string(&escape_string(s));
                f.string("\"");
            }
        }
        Constant::Float(s, suffix) => {
            f.string(s);
            if let Some(c) = suffix {
                f.string(&c.to_string());
            }
        }
    }
}

// ============================================================================
// Longident
// ============================================================================

fn print_longident<W: Write>(f: &mut Formatter<W>, arena: &ParseArena, lid: &Longident) {
    match lid {
        Longident::Lident(name_idx) => {
            let name = arena.get_string(*name_idx);
            f.string(name);
        }
        Longident::Ldot(prefix, name_idx) => {
            print_longident(f, arena, prefix);
            f.string(".");
            let name = arena.get_string(*name_idx);
            f.string(name);
        }
        Longident::Lapply(m1, m2) => {
            print_longident(f, arena, m1);
            f.string("(");
            print_longident(f, arena, m2);
            f.string(")");
        }
    }
}

/// Print longident from index using arena
fn print_longident_idx<W: Write>(f: &mut Formatter<W>, arena: &ParseArena, lid_idx: LidentIdx) {
    print_longident(f, arena, arena.get_longident(lid_idx));
}

// ============================================================================
// Payload
// ============================================================================

fn print_payload<W: Write>(f: &mut Formatter<W>, arena: &ParseArena, payload: &Payload) {
    match payload {
        Payload::PStr(items) => {
            // Special case: single Pstr_eval prints just the expression
            // OCaml: pp f "@[<2>%a@]%a" (expression ctxt) e (item_attributes ctxt) attrs
            if items.len() == 1 {
                if let StructureItemDesc::Pstr_eval(expr, attrs) = &items[0].pstr_desc {
                    f.open_box(BoxKind::Box, 2);
                    print_expression(f, arena, expr);
                    f.close_box();
                    print_item_attributes(f, arena, attrs);
                    return;
                }
            }
            for (i, item) in items.iter().enumerate() {
                print_structure_item(f, arena, item);
                if i < items.len() - 1 {
                    f.string("\n  ");
                }
            }
        }
        Payload::PSig(sig_items) => {
            // Print signature items with : prefix and newlines between items
            f.string(":");
            for (i, item) in sig_items.iter().enumerate() {
                print_signature_item(f, arena, item);
                if i < sig_items.len() - 1 {
                    f.string("\n    ");
                }
            }
        }
        Payload::PTyp(typ) => {
            f.string(":");
            print_core_type(f, arena, typ);
        }
        Payload::PPat(pat, guard) => {
            f.string("?");
            print_pattern(f, arena, pat);
            if let Some(g) = guard {
                f.string(" when ");
                print_expression(f, arena, g);
            }
        }
    }
}

// ============================================================================
// Type declarations
// ============================================================================

/// Print record declaration: {field1: type; field2: type}
/// Matches OCaml's record_declaration: pp f "{@\n%a}" (list type_record_field ~sep:";@\n") lbls
/// Each field: @[<2>mutable? name?: type @; attrs@]
fn print_record_declaration<W: Write>(f: &mut Formatter<W>, arena: &ParseArena, fields: &[LabelDeclaration]) {
    // OCaml: pp f "{@\n%a}" (list type_record_field ~sep:";@\n") lbls
    f.string("{");
    f.newline();
    if fields.is_empty() {
        f.string("}");
        return;
    }
    for (i, field) in fields.iter().enumerate() {
        // OCaml: @[<2>%a%s%a:@;%a@;%a@]
        // mutable_flag, name, optional_flag, core_type, attributes
        f.open_box(BoxKind::Box, 2);
        if matches!(field.pld_mutable, MutableFlag::Mutable) {
            f.string("mutable ");
        }
        f.string(&field.pld_name.txt);
        if field.pld_optional {
            f.string("?");
        }
        f.string(":");
        f.space();
        print_core_type(f, arena, &field.pld_type);
        f.space();
        print_attributes(f, arena, &field.pld_attributes);
        f.close_box();
        if i < fields.len() - 1 {
            f.string(";");
            f.newline();
        } else {
            f.string("}");
        }
    }
}

fn print_type_declaration<W: Write>(f: &mut Formatter<W>, arena: &ParseArena, decl: &TypeDeclaration) {
    if !decl.ptype_params.is_empty() {
        if decl.ptype_params.len() == 1 {
            // Print variance prefix
            match decl.ptype_params[0].1 {
                Variance::Covariant => f.string("+"),
                Variance::Contravariant => f.string("-"),
                Variance::Invariant => {}
            }
            print_core_type(f, arena, &decl.ptype_params[0].0);
            f.string(" ");
        } else {
            f.string("(");
            for (i, (t, variance)) in decl.ptype_params.iter().enumerate() {
                if i > 0 {
                    f.string(", ");
                }
                // Print variance prefix
                match variance {
                    Variance::Covariant => f.string("+"),
                    Variance::Contravariant => f.string("-"),
                    Variance::Invariant => {}
                }
                print_core_type(f, arena, t);
            }
            f.string(") ");
        }
    }
    f.string(&decl.ptype_name.txt);

    if let Some(manifest) = &decl.ptype_manifest {
        // OCaml: type_def_list prints " =" then type_declaration prints "@;private @;type"
        // The break hint allows long types to wrap to the next line
        f.string(" =");
        if matches!(decl.ptype_private, PrivateFlag::Private) {
            f.space();  // Break hint before private
            f.string("private");
        }
        f.space();  // Break hint before type body (OCaml's @;)
        print_core_type(f, arena, manifest);
    }

    match &decl.ptype_kind {
        TypeKind::Ptype_abstract => {}
        TypeKind::Ptype_variant(ctors) => {
            f.string(" =");
            if matches!(decl.ptype_private, PrivateFlag::Private) {
                f.string(" private");
            }
            // OCaml uses: pp f "%t%t@\n%a" intro priv (list ~sep:"@\n" constructor_declaration) xs
            // Where constructor_declaration is: pp f "|@;"; ...
            // This means each constructor gets @\n (force newline with indent) + |@; (break hint)
            for ctor in ctors.iter() {
                // OCaml: pp f "|@;"; constructor_declaration ...
                // Each constructor gets @\n (force newline) + |@; (| then break hint)
                f.newline();
                f.string("|");
                f.space(); // @; after |
                f.string(&ctor.pcd_name.txt);
                // OCaml constructor_declaration: "%s%a@;%a" name args attrs
                match &ctor.pcd_res {
                    None => {
                        // Non-GADT: pp f "@;of@;%a" (list (core_type1 ctxt) ~sep:"@;*@;") l
                        match &ctor.pcd_args {
                            ConstructorArguments::Pcstr_tuple(args) if !args.is_empty() => {
                                f.space(); // @; before "of"
                                f.string("of");
                                f.space(); // @; after "of"
                                for (j, arg) in args.iter().enumerate() {
                                    if j > 0 {
                                        f.space(); // @; before *
                                        f.string("*");
                                        f.space(); // @; after *
                                    }
                                    print_core_type1(f, arena, arg);
                                }
                            }
                            ConstructorArguments::Pcstr_record(fields) => {
                                f.space(); // @; before "of"
                                f.string("of");
                                f.space(); // @; after "of"
                                print_record_declaration(f, arena, fields);
                            }
                            _ => {}
                        }
                    }
                    Some(res) => {
                        // GADT: "%s:@;%a@;%a" name args attrs
                        f.string(":");
                        f.space(); // @; after :
                        match &ctor.pcd_args {
                            ConstructorArguments::Pcstr_tuple(args) if !args.is_empty() => {
                                // "%a@;->@;%a" (list core_type1 ~sep:"@;*@;") l core_type1 r
                                for (j, arg) in args.iter().enumerate() {
                                    if j > 0 {
                                        f.space(); // @; before *
                                        f.string("*");
                                        f.space(); // @; after *
                                    }
                                    print_core_type1(f, arena, arg);
                                }
                                f.space(); // @; before ->
                                f.string("->");
                                f.space(); // @; after ->
                                print_core_type1(f, arena, res);
                            }
                            ConstructorArguments::Pcstr_record(fields) => {
                                print_record_declaration(f, arena, fields);
                                f.space(); // @; before ->
                                f.string("->");
                                f.space(); // @; after ->
                                print_core_type1(f, arena, res);
                            }
                            _ => {
                                print_core_type1(f, arena, res);
                            }
                        }
                    }
                }
                // OCaml: @;%a for attrs (break hint before attrs)
                f.space();
                print_attributes(f, arena, &ctor.pcd_attributes);
            }
        }
        TypeKind::Ptype_record(fields) => {
            // OCaml: pp f "%t%t@;%a" intro priv (record_declaration ctxt) l
            // record_declaration: pp f "{@\n%a}" outputs { then forced newline then fields then }
            f.string(" =");
            if matches!(decl.ptype_private, PrivateFlag::Private) {
                f.string(" private");
            }
            f.space(); // @; break hint
            print_record_declaration(f, arena, fields);
        }
        TypeKind::Ptype_open => {
            f.string(" =");
            if matches!(decl.ptype_private, PrivateFlag::Private) {
                f.string(" private");
            }
            f.string(" ..");
        }
    }

    // Type constraints
    for (ct1, ct2, _) in &decl.ptype_cstrs {
        f.string(" constraint ");
        print_core_type(f, arena, ct1);
        f.string(" = ");
        print_core_type(f, arena, ct2);
    }

    // NOTE: Type attributes (ptype_attributes) are NOT printed here.
    // In OCaml, item attributes are printed OUTSIDE the @[<2>...@] box
    // by the caller (Pstr_type/Psig_type).
}

fn print_extension_constructor<W: Write>(f: &mut Formatter<W>, arena: &ParseArena, ext: &ExtensionConstructor) {
    f.string(&ext.pext_name.txt);
    match &ext.pext_kind {
        ExtensionConstructorKind::Pext_decl(args, res) => {
            // OCaml: extension_constructor delegates to constructor_declaration
            // Same format strings as constructor_declaration
            match res {
                None => {
                    // Non-GADT: "%s%a@;%a" name args attrs
                    match args {
                        ConstructorArguments::Pcstr_tuple(args) if !args.is_empty() => {
                            f.space(); // @; before "of"
                            f.string("of");
                            f.space(); // @; after "of"
                            for (i, arg) in args.iter().enumerate() {
                                if i > 0 {
                                    f.space(); // @; before *
                                    f.string("*");
                                    f.space(); // @; after *
                                }
                                print_core_type1(f, arena, arg);
                            }
                        }
                        ConstructorArguments::Pcstr_record(fields) => {
                            f.space(); // @; before "of"
                            f.string("of");
                            f.space(); // @; after "of"
                            print_record_declaration(f, arena, fields);
                        }
                        _ => {}
                    }
                    f.space(); // @; before attrs
                }
                Some(r) => {
                    // GADT: "%s:@;%a@;%a" name args attrs
                    f.string(":");
                    f.space(); // @; after :
                    match args {
                        ConstructorArguments::Pcstr_tuple(args) if !args.is_empty() => {
                            for (i, arg) in args.iter().enumerate() {
                                if i > 0 {
                                    f.space(); // @; before *
                                    f.string("*");
                                    f.space(); // @; after *
                                }
                                print_core_type1(f, arena, arg);
                            }
                            f.space(); // @; before ->
                            f.string("->");
                            f.space(); // @; after ->
                            print_core_type1(f, arena, r);
                        }
                        ConstructorArguments::Pcstr_record(fields) => {
                            print_record_declaration(f, arena, fields);
                            f.space(); // @; before ->
                            f.string("->");
                            f.space(); // @; after ->
                            print_core_type1(f, arena, r);
                        }
                        _ => {
                            print_core_type1(f, arena, r);
                        }
                    }
                    f.space(); // @; before attrs
                }
            }
        }
        ExtensionConstructorKind::Pext_rebind(lid) => {
            // Print attributes before rebind
            print_attributes(f, arena, &ext.pext_attributes);
            f.string(" = ");
            print_longident_idx(f, arena, lid.txt);
            return; // Don't print attributes twice
        }
    }
    // Print attributes
    print_attributes(f, arena, &ext.pext_attributes);
}

// ============================================================================
// Modules
// ============================================================================

fn print_module_expr<W: Write>(f: &mut Formatter<W>, arena: &ParseArena, mexpr: &ModuleExpr) {
    // Print attributes on module expressions: ((mexpr)[@attr ])
    let has_attrs = printable_attributes(&mexpr.pmod_attributes).len() > 0;
    if has_attrs {
        f.string("((");
    }
    print_module_expr_inner(f, arena, mexpr);
    if has_attrs {
        f.string(")");
        print_attributes(f, arena, &mexpr.pmod_attributes);
        f.string(")");
    }
}

fn print_module_expr_inner<W: Write>(f: &mut Formatter<W>, arena: &ParseArena, mexpr: &ModuleExpr) {
    match &mexpr.pmod_desc {
        ModuleExprDesc::Pmod_ident(lid) => {
            print_longident_idx(f, arena, lid.txt);
        }
        ModuleExprDesc::Pmod_structure(items) => {
            // OCaml: pp f "@[<hv2>struct@;@[<0>%a@]@;<1 -2>end@]" with items separated by @\n
            f.open_box(BoxKind::HV, 2);
            f.string("struct");
            f.space(); // @;
            f.open_box(BoxKind::Box, 0);
            for (i, item) in items.iter().enumerate() {
                if i > 0 {
                    f.newline(); // @\n forced newline between structure items
                }
                print_structure_item(f, arena, item);
            }
            f.close_box();
            f.break_(1, -2);
            f.string("end");
            f.close_box();
        }
        ModuleExprDesc::Pmod_functor(name, mtype, body) => {
            if let Some(mt) = mtype {
                // OCaml: pp f "functor@ (%s@ :@ %a)@;->@;%a"
                f.string("functor");
                f.space(); // @ after "functor"
                f.string("(");
                f.string(&name.txt);
                f.space(); // @ after name
                f.string(":");
                f.space(); // @ after ":"
                print_module_type(f, arena, mt);
                f.string(")");
                f.space(); // @; after ")"
                f.string("->");
                f.space(); // @; after "->"
            } else {
                // OCaml: pp f "functor ()@;->@;%a"
                f.string("functor ()");
                f.space(); // @; after "()"
                f.string("->");
                f.space(); // @; after "->"
            }
            print_module_expr(f, arena, body);
        }
        ModuleExprDesc::Pmod_apply(m1, m2) => {
            // OCaml: pp f "(%a)(%a)" - both functor and argument get parens
            f.string("(");
            print_module_expr(f, arena, m1);
            f.string(")(");
            print_module_expr(f, arena, m2);
            f.string(")");
        }
        ModuleExprDesc::Pmod_constraint(m, mt) => {
            // OCaml: pp f "@[<hov2>(%a@ :@ %a)@]"
            f.open_box(BoxKind::HOV, 2);
            f.string("(");
            print_module_expr(f, arena, m);
            f.space(); // @ before ":"
            f.string(":");
            f.space(); // @ after ":"
            print_module_type(f, arena, mt);
            f.string(")");
            f.close_box();
        }
        ModuleExprDesc::Pmod_unpack(e) => {
            // OCaml: (val@ %a) — break hint between val and expression
            f.string("(val");
            f.space();
            print_expression(f, arena, e);
            f.string(")");
        }
        ModuleExprDesc::Pmod_extension((name, payload)) => {
            // OCaml: @[<2>[%%%s@ %a]@]
            f.open_box(BoxKind::Box, 2);
            f.string("[%");
            f.string(&name.txt);
            f.space();
            print_payload(f, arena, payload);
            f.string("]");
            f.close_box();
        }
    }
}

fn print_module_type<W: Write>(f: &mut Formatter<W>, arena: &ParseArena, mtype: &ModuleType) {
    // Print attributes on module types: ((mtype)[@attr ])
    let has_attrs = printable_attributes(&mtype.pmty_attributes).len() > 0;
    if has_attrs {
        f.string("((");
    }
    print_module_type_inner(f, arena, mtype);
    if has_attrs {
        f.string(")");
        print_attributes(f, arena, &mtype.pmty_attributes);
        f.string(")");
    }
}

fn print_module_type_inner<W: Write>(f: &mut Formatter<W>, arena: &ParseArena, mtype: &ModuleType) {
    match &mtype.pmty_desc {
        ModuleTypeDesc::Pmty_ident(lid) => {
            print_longident_idx(f, arena, lid.txt);
        }
        ModuleTypeDesc::Pmty_signature(items) => {
            // OCaml: pp f "@[<hv0>@[<hv2>sig@ %a@]@ end@]"
            // Items separated by default "@ " = Break(1,0) (list without ~sep)
            f.open_box(BoxKind::HV, 0);
            f.open_box(BoxKind::HV, 2);
            f.string("sig");
            f.space();  // @ break hint after sig
            for (i, item) in items.iter().enumerate() {
                if i > 0 {
                    f.space();  // @ break hint between items
                }
                print_signature_item(f, arena, item);
            }
            f.close_box();
            f.space();  // @ break hint before end
            f.string("end");
            f.close_box();
        }
        ModuleTypeDesc::Pmty_functor(name, arg_type, ret_type) => {
            if arg_type.is_none() {
                // OCaml: pp f "@[<hov2>functor () ->@ %a@]"
                f.open_box(BoxKind::HOV, 2);
                f.string("functor ()");
                f.space();
                f.string("->");
                f.space();
                print_module_type(f, arena, ret_type);
                f.close_box();
            } else if name.txt == "_" {
                // OCaml: pp f "@[<hov2>%a@ ->@ %a@]"
                f.open_box(BoxKind::HOV, 2);
                if let Some(mt) = arg_type {
                    print_module_type(f, arena, mt);
                }
                f.space();
                f.string("->");
                f.space();
                print_module_type(f, arena, ret_type);
                f.close_box();
            } else {
                // OCaml: pp f "@[<hov2>functor@ (%s@ :@ %a)@ ->@ %a@]"
                f.open_box(BoxKind::HOV, 2);
                f.string("functor");
                f.space();
                f.string("(");
                f.string(&name.txt);
                if let Some(mt) = arg_type {
                    f.space();
                    f.string(":");
                    f.space();
                    print_module_type(f, arena, mt);
                }
                f.string(")");
                f.space();
                f.string("->");
                f.space();
                print_module_type(f, arena, ret_type);
                f.close_box();
            }
        }
        ModuleTypeDesc::Pmty_with(mt, constraints) => {
            // OCaml: pp f "@[<hov2>(%a@ with@ %a)@]" when there are constraints
            // or pp f "@[<hov2>%a@]" when empty
            if constraints.is_empty() {
                f.open_box(BoxKind::HOV, 2);
                print_module_type(f, arena, mt);
                f.close_box();
            } else {
                f.open_box(BoxKind::HOV, 2);
                f.string("(");
                print_module_type(f, arena, mt);
                f.space();
                f.string("with");
                for (i, constraint) in constraints.iter().enumerate() {
                    if i > 0 {
                        f.space();
                        f.string("and");
                    }
                    f.space();
                    match constraint {
                        WithConstraint::Pwith_type(lid, decl) => {
                            // OCaml: pp f "type@ %a %a =@ %a" params lid type_decl
                            f.string("type");
                            f.space();  // @ after type
                            // Print type params
                            if !decl.ptype_params.is_empty() {
                                if decl.ptype_params.len() == 1 {
                                    print_core_type(f, arena, &decl.ptype_params[0].0);
                                    f.string(" ");
                                } else {
                                    f.string("(");
                                    for (i, (t, _)) in decl.ptype_params.iter().enumerate() {
                                        if i > 0 {
                                            f.string(",");
                                        }
                                        print_core_type(f, arena, t);
                                    }
                                    f.string(") ");
                                }
                            } else {
                                f.string(" ");  // space before lid when params empty
                            }
                            print_longident_idx(f, arena, lid.txt);
                            f.string(" =");
                            f.space();  // @ after =
                            // type_declaration prints @; then private then manifest then constraints
                            f.space();  // @; in type_declaration
                            if decl.ptype_private == PrivateFlag::Private {
                                f.string("private ");
                            }
                            if let Some(manifest) = &decl.ptype_manifest {
                                print_core_type(f, arena, manifest);
                            }
                            // Print type constraints
                            for (ct1, ct2, _) in &decl.ptype_cstrs {
                                f.string(" constraint ");
                                print_core_type(f, arena, ct1);
                                f.string(" = ");
                                print_core_type(f, arena, ct2);
                            }
                        }
                        WithConstraint::Pwith_module(lid1, lid2) => {
                            f.string("module ");
                            print_longident_idx(f, arena, lid1.txt);
                            f.string(" = ");
                            print_longident_idx(f, arena, lid2.txt);
                        }
                        WithConstraint::Pwith_typesubst(lid, decl) => {
                            // Same pattern as Pwith_type but with :=
                            f.string("type");
                            f.space();
                            // Print type params
                            if !decl.ptype_params.is_empty() {
                                if decl.ptype_params.len() == 1 {
                                    print_core_type(f, arena, &decl.ptype_params[0].0);
                                    f.string(" ");
                                } else {
                                    f.string("(");
                                    for (i, (t, _)) in decl.ptype_params.iter().enumerate() {
                                        if i > 0 {
                                            f.string(",");
                                        }
                                        print_core_type(f, arena, t);
                                    }
                                    f.string(") ");
                                }
                            } else {
                                f.string(" ");
                            }
                            print_longident_idx(f, arena, lid.txt);
                            f.string(" :=");
                            f.space();
                            f.space();
                            if decl.ptype_private == PrivateFlag::Private {
                                f.string("private ");
                            }
                            if let Some(manifest) = &decl.ptype_manifest {
                                print_core_type(f, arena, manifest);
                            }
                            // Print type constraints
                            for (ct1, ct2, _) in &decl.ptype_cstrs {
                                f.string(" constraint ");
                                print_core_type(f, arena, ct1);
                                f.string(" = ");
                                print_core_type(f, arena, ct2);
                            }
                        }
                        WithConstraint::Pwith_modsubst(lid1, lid2) => {
                            f.string("module ");
                            print_longident_idx(f, arena, lid1.txt);
                            f.string(" := ");
                            print_longident_idx(f, arena, lid2.txt);
                        }
                    }
                }
                f.string(")");
                f.close_box();
            }
        }
        ModuleTypeDesc::Pmty_typeof(mexpr) => {
            f.string("module type of ");
            print_module_expr(f, arena, mexpr);
        }
        ModuleTypeDesc::Pmty_extension((name, payload)) => {
            f.open_box(BoxKind::H, 2);
            f.string("[%");
            f.string(&name.txt);
            f.space();
            print_payload(f, arena, payload);
            f.string("]");
            f.close_box();
        }
        ModuleTypeDesc::Pmty_alias(lid) => {
            f.string("(module ");
            print_longident_idx(f, arena, lid.txt);
            f.string(")");
        }
    }
}

// ============================================================================
// Signature items
// ============================================================================

fn print_signature_item<W: Write>(f: &mut Formatter<W>, arena: &ParseArena, item: &SignatureItem) {
    match &item.psig_desc {
        SignatureItemDesc::Psig_value(vd) => {
            // OCaml: pp f "@[<2>%s@ %a@ :@ %a@]%a"
            f.open_box(BoxKind::Box, 2);
            if vd.pval_prim.is_empty() {
                f.string("val");
            } else {
                f.string("external");
            }
            f.space();
            f.string(&vd.pval_name.txt);
            f.space();
            f.string(":");
            f.space();
            // OCaml value_description: pp f "@[<hov2>%a%a@]"
            f.open_box(BoxKind::HOV, 2);
            print_core_type(f, arena, &vd.pval_type);
            if !vd.pval_prim.is_empty() {
                f.space();
                f.string("=");
                f.space();
                for (i, prim) in vd.pval_prim.iter().enumerate() {
                    if i > 0 {
                        f.space();
                    }
                    f.string("\"");
                    f.string(prim);
                    f.string("\"");
                }
            }
            f.close_box();
            f.close_box();
            print_item_attributes(f, arena, &vd.pval_attributes);
        }
        SignatureItemDesc::Psig_type(rec_flag, decls) => {
            // OCaml: @[<2>type %a%a%s%s%a@]%a for first, @[<2>and %a@]%a for others
            // Each declaration is wrapped in a box with indent 2
            let rec_str = match rec_flag {
                RecFlag::Recursive => "",
                RecFlag::Nonrecursive => " nonrec",
            };
            for (i, decl) in decls.iter().enumerate() {
                if i > 0 {
                    f.newline();
                }
                // OCaml: @[<2> - Open structural box with indent 2 for type declaration
                f.open_box(BoxKind::Box, 2);
                if i == 0 {
                    f.string("type");
                    f.string(rec_str);
                    f.string(" ");
                } else {
                    f.string("and ");
                }
                print_type_declaration(f, arena, decl);
                f.close_box();
                // OCaml: item attributes printed OUTSIDE the box
                print_item_attributes(f, arena, &decl.ptype_attributes);
            }
        }
        SignatureItemDesc::Psig_typext(ext) => {
            // Same format as Pstr_typext
            f.open_box(BoxKind::HV, 2);
            f.string("type ");
            // Print type params if present
            if !ext.ptyext_params.is_empty() {
                if ext.ptyext_params.len() == 1 {
                    // Single param: no parens, like OCaml's "type _ Tid.t +="
                    print_core_type(f, arena, &ext.ptyext_params[0].0);
                    f.string(" ");
                } else {
                    f.string("(");
                    for (i, (ty, _variance)) in ext.ptyext_params.iter().enumerate() {
                        if i > 0 {
                            f.string(",");
                        }
                        print_core_type(f, arena, ty);
                    }
                    f.string(") ");
                }
            }
            print_longident_idx(f, arena, ext.ptyext_path.txt);
            f.string(" += ");
            if matches!(ext.ptyext_private, PrivateFlag::Private) {
                f.string("private ");
            }
            // Extra space before line break to match OCaml's "+=  \n"
            f.string(" ");
            for ctor in ext.ptyext_constructors.iter() {
                f.newline();
                f.string("| ");
                print_extension_constructor(f, arena, ctor);
            }
            f.close_box();
            print_item_attributes(f, arena, &ext.ptyext_attributes);
        }
        SignatureItemDesc::Psig_exception(ext) => {
            // OCaml: pp f "@[<hov2>exception@ %a@]"
            f.open_box(BoxKind::HOV, 2);
            f.string("exception");
            f.space(); // @ after "exception"
            print_extension_constructor(f, arena, ext);
            f.close_box();
        }
        SignatureItemDesc::Psig_module(md) => {
            // OCaml has special case: if pmd_type is Pmty_alias with no attrs, use = syntax
            f.string("module ");
            f.string(&md.pmd_name.txt);
            if let ModuleTypeDesc::Pmty_alias(lid) = &md.pmd_type.pmty_desc {
                if md.pmd_type.pmty_attributes.is_empty() {
                    // module Name = Alias
                    f.string(" = ");
                    print_longident_idx(f, arena, lid.txt);
                    print_item_attributes(f, arena, &md.pmd_attributes);
                    return;
                }
            }
            // module Name : ModuleType
            f.string(" : ");
            print_module_type(f, arena, &md.pmd_type);
            print_item_attributes(f, arena, &md.pmd_attributes);
        }
        SignatureItemDesc::Psig_recmodule(mds) => {
            // OCaml: First decl is @[<hov2>module@ rec@ %s:@ %a@]%a
            //        Others are @ @[<hov2>and@ %s:@ %a@]%a
            for (i, md) in mds.iter().enumerate() {
                if i > 0 {
                    // Break before "and" for subsequent declarations
                    f.break_(1, 0);
                }
                f.open_box(BoxKind::HOV, 2);
                if i == 0 {
                    f.string("module");
                    f.space();
                    f.string("rec");
                    f.space();
                } else {
                    f.string("and");
                    f.space();
                }
                f.string(&md.pmd_name.txt);
                f.string(":");
                f.space();
                print_module_type(f, arena, &md.pmd_type);
                f.close_box();
                print_item_attributes(f, arena, &md.pmd_attributes);
            }
        }
        SignatureItemDesc::Psig_modtype(mtd) => {
            // OCaml: pp f "@[<hov2>module@ type@ %s%a@]%a"
            // where inner is: pp_print_space f (); pp f "@ =@ %a" (module_type ctxt) mt
            // This produces two spaces before = when not breaking: "Sig  ="
            f.open_box(BoxKind::HOV, 2);
            f.string("module");
            f.space();
            f.string("type");
            f.space();
            f.string(&mtd.pmtd_name.txt);
            if let Some(mt) = &mtd.pmtd_type {
                // pp_print_space + "@ =" gives two spaces before =
                f.string(" ");  // literal space from pp_print_space
                f.space();      // break hint before =
                f.string("=");
                f.space();
                print_module_type(f, arena, mt);
            }
            f.close_box();
            print_item_attributes(f, arena, &mtd.pmtd_attributes);
        }
        SignatureItemDesc::Psig_open(od) => {
            f.string("open");
            if matches!(od.popen_override, OverrideFlag::Override) {
                f.string("!");
            }
            f.string(" ");
            print_longident_idx(f, arena, od.popen_lid.txt);
            print_item_attributes(f, arena, &od.popen_attributes);
        }
        SignatureItemDesc::Psig_include(incl) => {
            // OCaml: @[<hov2>include@ %a@]%a
            f.open_box(BoxKind::HOV, 2);
            f.string("include");
            f.space();
            print_module_type(f, arena, &incl.pincl_mod);
            f.close_box();
            print_item_attributes(f, arena, &incl.pincl_attributes);
        }
        SignatureItemDesc::Psig_attribute((name, payload)) => {
            // Standalone/floating attributes use [@@@...] (3 @)
            f.string("[@@@");
            f.string(&name.txt);
            if !payload_is_empty(payload) {
                f.string(" ");
                print_payload(f, arena, payload);
            } else {
                f.string(" ");
            }
            f.string("]");
        }
        SignatureItemDesc::Psig_extension((name, payload), attrs) => {
            f.open_box(BoxKind::H, 2);
            f.string("[%%");
            f.string(&name.txt);
            f.space();
            print_payload(f, arena, payload);
            f.string("]");
            print_item_attributes(f, arena, attrs);
            f.close_box();
        }
    }
}

fn print_item_attributes<W: Write>(f: &mut Formatter<W>, arena: &ParseArena, attrs: &[(Located<String>, Payload)]) {
    // OCaml: and item_attribute ctxt f (s, e) = pp f "@[<2>[@@@@%s@ %a]@]" s.txt (payload ctxt) e
    // Each attribute is wrapped in a box with indent 2, with a break hint after the name
    let attrs = printable_attributes(attrs);
    for (name, payload) in attrs {
        f.open_box(BoxKind::Box, 2);
        f.string("[@@");
        f.string(&name.txt);
        f.space();  // Break hint after name (matches OCaml's `@ `)
        if !payload_is_empty(payload) {
            print_payload(f, arena, payload);
        }
        f.string("]");
        f.close_box();
    }
}

/// Print attributes with single @ (for expressions, types, patterns)
/// OCaml: and attribute ctxt f (s, e) = pp f "@[<2>[@@%s@ %a]@]" s.txt (payload ctxt) e
fn print_attributes<W: Write>(f: &mut Formatter<W>, arena: &ParseArena, attrs: &[(Located<String>, Payload)]) {
    let attrs = printable_attributes(attrs);
    for (name, payload) in attrs {
        f.open_box(BoxKind::Box, 2);
        f.string("[@");
        f.string(&name.txt);
        f.space();  // Break hint after name (matches OCaml's `@ `)
        if !payload_is_empty(payload) {
            print_payload(f, arena, payload);
        }
        f.string("]");
        f.close_box();
    }
}
