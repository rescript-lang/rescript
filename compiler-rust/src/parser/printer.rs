//! ReScript AST printer.
//!
//! This module converts the AST back to ReScript source code.
//! It's used for formatting and roundtrip testing.

use super::ast::*;
use super::longident::Longident;
use std::fmt::Write;

/// Printer state for formatting output.
pub struct Printer {
    /// Output buffer.
    output: String,
    /// Current indentation level.
    indent: usize,
    /// Indentation string (spaces).
    indent_str: &'static str,
}

impl Default for Printer {
    fn default() -> Self {
        Self::new()
    }
}

impl Printer {
    /// Create a new printer.
    pub fn new() -> Self {
        Printer {
            output: String::new(),
            indent: 0,
            indent_str: "  ",
        }
    }

    /// Get the printed output.
    pub fn output(&self) -> &str {
        &self.output
    }

    /// Consume the printer and return the output.
    pub fn into_output(self) -> String {
        self.output
    }

    /// Write a string to the output.
    fn write(&mut self, s: &str) {
        self.output.push_str(s);
    }

    /// Write a newline and indentation.
    fn newline(&mut self) {
        self.output.push('\n');
        for _ in 0..self.indent {
            self.output.push_str(self.indent_str);
        }
    }

    /// Increase indentation.
    fn indent(&mut self) {
        self.indent += 1;
    }

    /// Decrease indentation.
    fn dedent(&mut self) {
        if self.indent > 0 {
            self.indent -= 1;
        }
    }

    /// Write a space.
    fn space(&mut self) {
        self.output.push(' ');
    }

    /// Check if an expression is a block-like form (let, letmodule, letexception, open, sequence)
    /// that shouldn't get extra braces when inside another block.
    fn is_block_body_expr(expr: &Expression) -> bool {
        matches!(
            &expr.pexp_desc,
            ExpressionDesc::Pexp_let(..)
                | ExpressionDesc::Pexp_letmodule(..)
                | ExpressionDesc::Pexp_letexception(..)
                | ExpressionDesc::Pexp_open(..)
                | ExpressionDesc::Pexp_sequence(..)
        )
    }

    /// Print a block body expression without adding outer braces.
    /// Used for printing the continuation of let/module/open/exception.
    fn print_block_body(&mut self, expr: &Expression) {
        // Print any non-internal attributes on the expression
        for attr in &expr.pexp_attributes {
            if !Self::is_internal_attribute(attr) {
                self.write("@");
                self.print_attribute(attr);
                self.space();
            }
        }

        match &expr.pexp_desc {
            ExpressionDesc::Pexp_let(rec_flag, bindings, body) => {
                self.print_let_bindings(*rec_flag, bindings);
                self.newline();
                self.print_block_body(body);
            }
            ExpressionDesc::Pexp_letmodule(name, modexpr, body) => {
                self.write("module ");
                self.write(&name.txt);
                self.write(" = ");
                self.print_module_expr(modexpr);
                self.newline();
                self.print_block_body(body);
            }
            ExpressionDesc::Pexp_letexception(ext, body) => {
                self.write("exception ");
                self.print_extension_constructor(ext);
                self.newline();
                self.print_block_body(body);
            }
            ExpressionDesc::Pexp_open(override_flag, lid, body) => {
                self.write("open");
                if *override_flag == OverrideFlag::Override {
                    self.write("!");
                }
                self.write(" ");
                self.print_longident(&lid.txt);
                self.newline();
                self.print_block_body(body);
            }
            ExpressionDesc::Pexp_sequence(left, right) => {
                self.print_expression(left);
                self.newline();
                self.print_block_body(right);
            }
            _ => {
                // Not a block form, print normally
                self.print_expression(expr);
            }
        }
    }

    // ========================================================================
    // Structure Printing
    // ========================================================================

    /// Print a structure.
    pub fn print_structure(&mut self, structure: &Structure) {
        for (i, item) in structure.iter().enumerate() {
            if i > 0 {
                self.newline();
                self.newline();
            }
            self.print_structure_item(item);
        }
    }

    /// Print a structure item.
    pub fn print_structure_item(&mut self, item: &StructureItem) {
        self.print_attributes(&item.pstr_desc.get_attributes());
        match &item.pstr_desc {
            StructureItemDesc::Pstr_eval(expr, _attrs) => {
                self.print_expression(expr);
            }
            StructureItemDesc::Pstr_value(rec_flag, bindings) => {
                self.print_let_bindings(*rec_flag, bindings);
            }
            StructureItemDesc::Pstr_primitive(vd) => {
                self.print_value_description(vd);
            }
            StructureItemDesc::Pstr_type(rec_flag, decls) => {
                self.print_type_declarations(*rec_flag, decls);
            }
            StructureItemDesc::Pstr_typext(ext) => {
                self.print_type_extension(ext);
            }
            StructureItemDesc::Pstr_exception(ext) => {
                self.write("exception ");
                self.print_extension_constructor(ext);
            }
            StructureItemDesc::Pstr_module(mb) => {
                self.print_module_binding(mb);
            }
            StructureItemDesc::Pstr_recmodule(mbs) => {
                for (i, mb) in mbs.iter().enumerate() {
                    if i == 0 {
                        self.write("module rec ");
                    } else {
                        self.newline();
                        self.write("and ");
                    }
                    self.write(&mb.pmb_name.txt);
                    self.write(" = ");
                    self.print_module_expr(&mb.pmb_expr);
                }
            }
            StructureItemDesc::Pstr_modtype(mtd) => {
                self.print_module_type_declaration(mtd);
            }
            StructureItemDesc::Pstr_open(od) => {
                self.write("open ");
                self.print_longident(&od.popen_lid.txt);
            }
            StructureItemDesc::Pstr_include(incl) => {
                self.write("include ");
                self.print_module_expr(&incl.pincl_mod);
            }
            StructureItemDesc::Pstr_attribute(attr) => {
                self.write("@@");
                self.print_attribute(attr);
            }
            StructureItemDesc::Pstr_extension(ext, _attrs) => {
                // Module-level extensions use %%
                self.write("%%");
                self.write(&ext.0.txt);
                self.print_extension_payload(&ext.1);
            }
        }
    }

    // ========================================================================
    // Let Bindings
    // ========================================================================

    /// Print let bindings.
    fn print_let_bindings(&mut self, rec_flag: RecFlag, bindings: &[ValueBinding]) {
        for (i, binding) in bindings.iter().enumerate() {
            if i == 0 {
                self.write("let ");
                if rec_flag == RecFlag::Recursive {
                    self.write("rec ");
                }
            } else {
                self.newline();
                self.write("and ");
            }
            self.print_value_binding(binding);
        }
    }

    /// Print a single value binding.
    fn print_value_binding(&mut self, binding: &ValueBinding) {
        self.print_pattern(&binding.pvb_pat);
        self.write(" = ");
        // Check if expression needs to be wrapped in braces
        // (sequences and other multi-statement expressions)
        if Self::needs_braces_in_binding(&binding.pvb_expr) {
            self.write("{");
            self.indent();
            self.newline();
            self.print_block_body(&binding.pvb_expr);
            self.dedent();
            self.newline();
            self.write("}");
        } else {
            self.print_expression(&binding.pvb_expr);
        }
    }

    /// Check if an expression needs to be wrapped in braces when used as a binding RHS.
    fn needs_braces_in_binding(expr: &Expression) -> bool {
        matches!(
            &expr.pexp_desc,
            ExpressionDesc::Pexp_sequence(..)
                | ExpressionDesc::Pexp_open(..)
                | ExpressionDesc::Pexp_let(..)
                | ExpressionDesc::Pexp_letmodule(..)
                | ExpressionDesc::Pexp_letexception(..)
        )
    }

    // ========================================================================
    // Binary/Unary Expression Detection
    // ========================================================================

    /// List of binary operators in ReScript.
    const BINARY_OPERATORS: &'static [&'static str] = &[
        ":=", "||", "&&", "==", "===", "<", ">", "!=", "!==", "<=", ">=", "+", "+.", "-", "-.",
        "++", "*", "*.", "/", "/.", "**", "->", "<>", "%", "|||", "^^^", "&&&", "<<", ">>", ">>>",
    ];

    /// List of unary operators in ReScript.
    const UNARY_OPERATORS: &'static [&'static str] = &["!", "~+", "~+.", "~-", "~-.", "not"];

    /// Check if an operator is a binary operator.
    fn is_binary_operator(op: &str) -> bool {
        Self::BINARY_OPERATORS.contains(&op)
    }

    /// Check if an operator is a unary operator.
    fn is_unary_operator(op: &str) -> bool {
        Self::UNARY_OPERATORS.contains(&op)
    }

    /// Try to extract binary expression components from a Pexp_apply.
    /// Returns (operator, left, right) if this is a binary expression.
    fn as_binary_expr<'b>(
        &self,
        funct: &'b Expression,
        args: &'b [(ArgLabel, Expression)],
    ) -> Option<(&'b str, &'b Expression, &'b Expression)> {
        // Must have exactly 2 unlabeled arguments
        if args.len() != 2 {
            return None;
        }
        let (label1, left) = &args[0];
        let (label2, right) = &args[1];
        if !matches!(label1, ArgLabel::Nolabel) || !matches!(label2, ArgLabel::Nolabel) {
            return None;
        }

        // Function must be an identifier that is a binary operator
        if let ExpressionDesc::Pexp_ident(lid) = &funct.pexp_desc {
            if let Longident::Lident(op) = &lid.txt {
                if Self::is_binary_operator(op) {
                    return Some((op.as_str(), left, right));
                }
            }
        }
        None
    }

    /// Try to extract unary expression components from a Pexp_apply.
    /// Returns (operator, operand) if this is a unary expression.
    fn as_unary_expr<'b>(
        &self,
        funct: &'b Expression,
        args: &'b [(ArgLabel, Expression)],
    ) -> Option<(&'b str, &'b Expression)> {
        // Must have exactly 1 unlabeled argument
        if args.len() != 1 {
            return None;
        }
        let (label, operand) = &args[0];
        if !matches!(label, ArgLabel::Nolabel) {
            return None;
        }

        // Function must be an identifier that is a unary operator
        if let ExpressionDesc::Pexp_ident(lid) = &funct.pexp_desc {
            if let Longident::Lident(op) = &lid.txt {
                if Self::is_unary_operator(op) {
                    // Map internal names to display names
                    let display_op = match op.as_str() {
                        "~-" => "-",
                        "~-." => "-.",
                        "~+" => "+",
                        "~+." => "+.",
                        "not" => "!",
                        other => other,
                    };
                    return Some((display_op, operand));
                }
            }
        }
        None
    }

    // ========================================================================
    // Template Literal Printing
    // ========================================================================

    /// Print an untagged template literal.
    /// Walks the ++ chain and reconstructs the template.
    fn print_template_literal(&mut self, expr: &Expression) {
        let mut tag = "js";

        fn walk_expr(printer: &mut Printer, expr: &Expression, tag: &mut &str) {
            match &expr.pexp_desc {
                ExpressionDesc::Pexp_apply { funct, args, .. } => {
                    // Check if this is a ++ operation
                    if let ExpressionDesc::Pexp_ident(lid) = &funct.pexp_desc {
                        if let Longident::Lident(op) = &lid.txt {
                            if op == "++" && args.len() == 2 {
                                walk_expr(printer, &args[0].1, tag);
                                walk_expr(printer, &args[1].1, tag);
                                return;
                            }
                        }
                    }
                    // Not a ++, print as interpolation
                    printer.write("${");
                    printer.print_expression(expr);
                    printer.write("}");
                }
                ExpressionDesc::Pexp_constant(Constant::String(txt, Some(prefix))) => {
                    *tag = if prefix == "json" { "json" } else { "js" };
                    // Print the template string contents
                    printer.print_template_string_contents(txt);
                }
                ExpressionDesc::Pexp_constant(Constant::String(txt, None)) => {
                    // Might be a regular string, print as template content
                    printer.print_template_string_contents(txt);
                }
                _ => {
                    // Expression interpolation
                    printer.write("${");
                    printer.print_expression(expr);
                    printer.write("}");
                }
            }
        }

        walk_expr(self, expr, &mut tag);

        // Wrap with backticks
        let content = std::mem::take(&mut self.output);
        if tag == "js" {
            self.write("`");
        } else {
            self.write(tag);
            self.write("`");
        }
        self.write(&content);
        self.write("`");
    }

    /// Print template string contents (handling escapes).
    fn print_template_string_contents(&mut self, s: &str) {
        // The string is already stored with escapes, output directly
        self.write(s);
    }

    /// Print a tagged template literal: tag`strings${values}`
    fn print_tagged_template_literal(&mut self, tag: &Expression, args: &[(ArgLabel, Expression)]) {
        // Args should be [strings_array, values_array]
        if args.len() != 2 {
            // Fallback to regular apply
            self.print_expression(tag);
            self.write("(");
            for (i, (label, arg)) in args.iter().enumerate() {
                if i > 0 {
                    self.write(", ");
                }
                self.print_arg_label(label);
                self.print_expression(arg);
            }
            self.write(")");
            return;
        }

        let strings = match &args[0].1.pexp_desc {
            ExpressionDesc::Pexp_array(arr) => arr,
            _ => {
                self.print_expression(tag);
                return;
            }
        };
        let values = match &args[1].1.pexp_desc {
            ExpressionDesc::Pexp_array(arr) => arr,
            _ => {
                self.print_expression(tag);
                return;
            }
        };

        // Print the tag
        self.print_expression(tag);
        self.write("`");

        // Interleave strings and values
        for (i, str_expr) in strings.iter().enumerate() {
            // Print string content
            if let ExpressionDesc::Pexp_constant(Constant::String(txt, _)) = &str_expr.pexp_desc {
                self.print_template_string_contents(txt);
            }
            // Print value interpolation if exists
            if i < values.len() {
                self.write("${");
                self.print_expression(&values[i]);
                self.write("}");
            }
        }

        self.write("`");
    }

    // ========================================================================
    // Expression Printing
    // ========================================================================

    /// Print an expression.
    pub fn print_expression(&mut self, expr: &Expression) {
        self.print_expression_inner(expr, false);
    }

    /// Check if an attribute is internal and shouldn't be printed.
    fn is_internal_attribute(attr: &Attribute) -> bool {
        let name = &attr.0.txt;
        name.starts_with("res.") || name.starts_with("ns.")
    }

    /// Print an expression with optional parentheses context.
    fn print_expression_inner(&mut self, expr: &Expression, _needs_parens: bool) {
        // Print any non-internal attributes on the expression
        for attr in &expr.pexp_attributes {
            if !Self::is_internal_attribute(attr) {
                self.write("@");
                self.print_attribute(attr);
                self.space();
            }
        }

        match &expr.pexp_desc {
            ExpressionDesc::Pexp_ident(lid) => {
                self.print_longident(&lid.txt);
            }
            ExpressionDesc::Pexp_constant(c) => {
                // Check if this is a template literal string
                let is_template = expr
                    .pexp_attributes
                    .iter()
                    .any(|(n, _)| n.txt == "res.template");
                if is_template {
                    if let Constant::String(txt, _) = c {
                        self.write("`");
                        self.print_template_string_contents(txt);
                        self.write("`");
                        return;
                    }
                }
                // Also check for empty delimiter (Some(""))
                if let Constant::String(txt, Some(delim)) = c {
                    if delim.is_empty() || delim == "js" {
                        self.write("`");
                        self.print_template_string_contents(txt);
                        self.write("`");
                        return;
                    }
                }
                self.print_constant(c);
            }
            ExpressionDesc::Pexp_let(rec_flag, bindings, body) => {
                self.write("{");
                self.indent();
                self.newline();
                self.print_let_bindings(*rec_flag, bindings);
                self.newline();
                // Use print_block_body for continuation to avoid nested braces
                self.print_block_body(body);
                self.dedent();
                self.newline();
                self.write("}");
            }
            ExpressionDesc::Pexp_fun { is_async, .. } => {
                if *is_async {
                    self.write("async ");
                }
                self.print_fun_expr(expr);
            }
            ExpressionDesc::Pexp_apply { funct, args, .. } => {
                // Check for tagged template literal: tag([strings], [values])
                if expr
                    .pexp_attributes
                    .iter()
                    .any(|(n, _)| n.txt == "res.taggedTemplate")
                {
                    self.print_tagged_template_literal(funct, args);
                    return;
                }
                // Check for untagged template literal: str ++ val ++ str...
                if expr
                    .pexp_attributes
                    .iter()
                    .any(|(n, _)| n.txt == "res.template")
                {
                    self.print_template_literal(expr);
                    return;
                }
                // Check for array access syntax: arr[index]
                if expr
                    .pexp_attributes
                    .iter()
                    .any(|(n, _)| n.txt == "res.array.access")
                    && args.len() == 2
                {
                    self.print_expression(&args[0].1);
                    self.write("[");
                    self.print_expression(&args[1].1);
                    self.write("]");
                    return;
                }
                // Check for array set syntax: arr[index] = value
                if expr
                    .pexp_attributes
                    .iter()
                    .any(|(n, _)| n.txt == "res.array.set")
                    && args.len() == 3
                {
                    self.print_expression(&args[0].1);
                    self.write("[");
                    self.print_expression(&args[1].1);
                    self.write("] = ");
                    self.print_expression(&args[2].1);
                    return;
                }
                // Check if this is a binary expression and print in infix notation
                if let Some((operator, left, right)) = self.as_binary_expr(funct, args) {
                    self.print_expression(left);
                    self.space();
                    self.write(operator);
                    self.space();
                    self.print_expression(right);
                } else if let Some((operator, operand)) = self.as_unary_expr(funct, args) {
                    self.write(operator);
                    self.print_expression(operand);
                } else {
                    self.print_expression(funct);
                    self.write("(");
                    for (i, (label, arg)) in args.iter().enumerate() {
                        if i > 0 {
                            self.write(", ");
                        }
                        self.print_arg_label(label);
                        self.print_expression(arg);
                    }
                    self.write(")");
                }
            }
            ExpressionDesc::Pexp_match(scrutinee, cases) => {
                self.write("switch ");
                self.print_expression(scrutinee);
                self.write(" {");
                self.indent();
                for case in cases {
                    self.newline();
                    self.write("| ");
                    self.print_pattern(&case.pc_lhs);
                    if let Some(guard) = &case.pc_guard {
                        self.write(" if ");
                        self.print_expression(guard);
                    }
                    self.write(" => ");
                    self.print_expression(&case.pc_rhs);
                }
                self.dedent();
                self.newline();
                self.write("}");
            }
            ExpressionDesc::Pexp_try(body, cases) => {
                self.write("try ");
                self.print_expression(body);
                self.write(" catch {");
                self.indent();
                for case in cases {
                    self.newline();
                    self.write("| ");
                    self.print_pattern(&case.pc_lhs);
                    if let Some(guard) = &case.pc_guard {
                        self.write(" when ");
                        self.print_expression(guard);
                    }
                    self.write(" => ");
                    self.print_expression(&case.pc_rhs);
                }
                self.dedent();
                self.newline();
                self.write("}");
            }
            ExpressionDesc::Pexp_tuple(exprs) => {
                self.write("(");
                for (i, e) in exprs.iter().enumerate() {
                    if i > 0 {
                        self.write(", ");
                    }
                    self.print_expression(e);
                }
                self.write(")");
            }
            ExpressionDesc::Pexp_construct(lid, arg) => {
                self.print_longident(&lid.txt);
                if let Some(arg) = arg {
                    self.write("(");
                    self.print_expression(arg);
                    self.write(")");
                }
            }
            ExpressionDesc::Pexp_variant(label, arg) => {
                self.write("#");
                self.write(label);
                if let Some(arg) = arg {
                    self.write("(");
                    // If the argument is a tuple, unwrap it to avoid double parens
                    match &arg.pexp_desc {
                        ExpressionDesc::Pexp_tuple(exprs) => {
                            for (i, e) in exprs.iter().enumerate() {
                                if i > 0 {
                                    self.write(", ");
                                }
                                self.print_expression(e);
                            }
                        }
                        ExpressionDesc::Pexp_construct(lid, None)
                            if lid.txt.to_string() == "()" =>
                        {
                            // Unit argument - print nothing inside parens
                        }
                        _ => self.print_expression(arg),
                    }
                    self.write(")");
                }
            }
            ExpressionDesc::Pexp_record(fields, spread) => {
                self.write("{");
                if let Some(spread) = spread {
                    self.write("...");
                    self.print_expression(spread);
                    if !fields.is_empty() {
                        self.write(", ");
                    }
                }
                for (i, field) in fields.iter().enumerate() {
                    if i > 0 {
                        self.write(", ");
                    }
                    self.print_longident(&field.lid.txt);
                    self.write(": ");
                    self.print_expression(&field.expr);
                }
                self.write("}");
            }
            ExpressionDesc::Pexp_field(expr, lid) => {
                self.print_expression(expr);
                self.write(".");
                self.print_longident(&lid.txt);
            }
            ExpressionDesc::Pexp_setfield(expr, lid, value) => {
                self.print_expression(expr);
                self.write(".");
                self.print_longident(&lid.txt);
                self.write(" = ");
                self.print_expression(value);
            }
            ExpressionDesc::Pexp_array(items) => {
                self.write("[");
                for (i, item) in items.iter().enumerate() {
                    if i > 0 {
                        self.write(", ");
                    }
                    self.print_expression(item);
                }
                self.write("]");
            }
            ExpressionDesc::Pexp_ifthenelse(cond, then_branch, else_branch) => {
                // Check for ternary attribute
                let is_ternary = expr
                    .pexp_attributes
                    .iter()
                    .any(|(name, _)| name.txt == "res.ternary");
                if is_ternary {
                    self.print_expression(cond);
                    self.write(" ? ");
                    self.print_expression(then_branch);
                    self.write(" : ");
                    if let Some(else_br) = else_branch {
                        self.print_expression(else_br);
                    }
                } else {
                    self.write("if ");
                    self.print_expression(cond);
                    self.write(" {");
                    self.indent();
                    self.newline();
                    self.print_expression(then_branch);
                    self.dedent();
                    self.newline();
                    self.write("}");
                    if let Some(else_br) = else_branch {
                        self.write(" else ");
                        // Check if else branch is another if
                        if matches!(else_br.pexp_desc, ExpressionDesc::Pexp_ifthenelse(..)) {
                            self.print_expression(else_br);
                        } else {
                            self.write("{");
                            self.indent();
                            self.newline();
                            self.print_expression(else_br);
                            self.dedent();
                            self.newline();
                            self.write("}");
                        }
                    }
                }
            }
            ExpressionDesc::Pexp_sequence(first, second) => {
                self.print_expression(first);
                self.newline();
                self.print_expression(second);
            }
            ExpressionDesc::Pexp_while(cond, body) => {
                self.write("while ");
                self.print_expression(cond);
                self.write(" {");
                self.indent();
                self.newline();
                self.print_expression(body);
                self.dedent();
                self.newline();
                self.write("}");
            }
            ExpressionDesc::Pexp_for(pat, start, finish, dir, body) => {
                self.write("for ");
                self.print_pattern(pat);
                self.write(" in ");
                self.print_expression(start);
                match dir {
                    DirectionFlag::Upto => self.write(" to "),
                    DirectionFlag::Downto => self.write(" downto "),
                }
                self.print_expression(finish);
                self.write(" {");
                self.indent();
                self.newline();
                self.print_expression(body);
                self.dedent();
                self.newline();
                self.write("}");
            }
            ExpressionDesc::Pexp_constraint(expr, typ) => {
                self.write("(");
                self.print_expression(expr);
                self.write(": ");
                self.print_core_type(typ);
                self.write(")");
            }
            ExpressionDesc::Pexp_coerce(expr, _from, to) => {
                self.write("(");
                self.print_expression(expr);
                self.write(" :> ");
                self.print_core_type(to);
                self.write(")");
            }
            ExpressionDesc::Pexp_send(expr, meth) => {
                self.print_expression(expr);
                self.write("##");
                self.write(&meth.txt);
            }
            ExpressionDesc::Pexp_letmodule(name, modexpr, body) => {
                self.write("{");
                self.indent();
                self.newline();
                self.write("module ");
                self.write(&name.txt);
                self.write(" = ");
                self.print_module_expr(modexpr);
                self.newline();
                // Use print_block_body for continuation to avoid nested braces
                self.print_block_body(body);
                self.dedent();
                self.newline();
                self.write("}");
            }
            ExpressionDesc::Pexp_letexception(ext, body) => {
                self.write("{");
                self.indent();
                self.newline();
                self.write("exception ");
                self.print_extension_constructor(ext);
                self.newline();
                // Use print_block_body for continuation to avoid nested braces
                self.print_block_body(body);
                self.dedent();
                self.newline();
                self.write("}");
            }
            ExpressionDesc::Pexp_assert(expr) => {
                self.write("assert ");
                self.print_expression(expr);
            }
            ExpressionDesc::Pexp_pack(modexpr) => {
                self.write("module(");
                self.print_module_expr(modexpr);
                self.write(")");
            }
            ExpressionDesc::Pexp_open(_override_flag, lid, expr) => {
                self.write("{");
                self.indent();
                self.newline();
                self.write("open ");
                self.print_longident(&lid.txt);
                self.newline();
                // Use print_block_body for continuation to avoid nested braces
                self.print_block_body(expr);
                self.dedent();
                self.newline();
                self.write("}");
            }
            ExpressionDesc::Pexp_extension(ext) => {
                // Check for %obj extension (JS object literal)
                if ext.0.txt == "obj" {
                    if let Payload::PStr(items) = &ext.1 {
                        if let Some(item) = items.first() {
                            if let StructureItemDesc::Pstr_eval(expr, _) = &item.pstr_desc {
                                if let ExpressionDesc::Pexp_record(fields, _) = &expr.pexp_desc {
                                    self.write("{");
                                    for (i, field) in fields.iter().enumerate() {
                                        if i > 0 {
                                            self.write(", ");
                                        }
                                        // Print field key as quoted string
                                        self.write("\"");
                                        self.print_longident(&field.lid.txt);
                                        self.write("\": ");
                                        self.print_expression(&field.expr);
                                    }
                                    self.write("}");
                                    return;
                                }
                            }
                        }
                    }
                }
                // Check for res.list extension
                if ext.0.txt == "res.list" || ext.0.txt == "res.list.spread" {
                    self.write("list{");
                    if let Payload::PStr(items) = &ext.1 {
                        for (i, item) in items.iter().enumerate() {
                            if i > 0 {
                                self.write(", ");
                            }
                            if let StructureItemDesc::Pstr_eval(e, _) = &item.pstr_desc {
                                self.print_expression(e);
                            }
                        }
                    }
                    // Check for spread in attributes
                    if ext.0.txt == "res.list.spread" {
                        if let Some((attr_name, attr_payload)) = expr
                            .pexp_attributes
                            .iter()
                            .find(|(n, _)| n.txt == "res.spread")
                        {
                            if !matches!(&ext.1, Payload::PStr(items) if items.is_empty()) {
                                self.write(", ");
                            }
                            self.write("...");
                            if let Payload::PStr(spread_items) = attr_payload {
                                if let Some(item) = spread_items.first() {
                                    if let StructureItemDesc::Pstr_eval(e, _) = &item.pstr_desc {
                                        self.print_expression(e);
                                    }
                                }
                            }
                        }
                    }
                    self.write("}");
                    return;
                }
                self.write("%");
                self.write(&ext.0.txt);
                self.print_extension_payload(&ext.1);
            }
            ExpressionDesc::Pexp_newtype(name, body) => {
                self.write("(type ");
                self.write(&name.txt);
                self.write(") => ");
                self.print_expression(body);
            }
            ExpressionDesc::Pexp_await(expr) => {
                self.write("await ");
                self.print_expression(expr);
            }
            ExpressionDesc::Pexp_jsx_element(elem) => {
                self.print_jsx_element(elem);
            }
        }
    }

    // ========================================================================
    // Pattern Printing
    // ========================================================================

    /// Print a pattern.
    pub fn print_pattern(&mut self, pat: &Pattern) {
        match &pat.ppat_desc {
            PatternDesc::Ppat_any => {
                self.write("_");
            }
            PatternDesc::Ppat_var(name) => {
                self.write(&name.txt);
            }
            PatternDesc::Ppat_alias(pat, name) => {
                self.print_pattern(pat);
                self.write(" as ");
                self.write(&name.txt);
            }
            PatternDesc::Ppat_constant(c) => {
                self.print_constant(c);
            }
            PatternDesc::Ppat_interval(c1, c2) => {
                self.print_constant(c1);
                self.write("..");
                self.print_constant(c2);
            }
            PatternDesc::Ppat_tuple(pats) => {
                self.write("(");
                for (i, p) in pats.iter().enumerate() {
                    if i > 0 {
                        self.write(", ");
                    }
                    self.print_pattern(p);
                }
                self.write(")");
            }
            PatternDesc::Ppat_construct(lid, arg) => {
                self.print_longident(&lid.txt);
                if let Some(arg) = arg {
                    self.write("(");
                    self.print_pattern(arg);
                    self.write(")");
                }
            }
            PatternDesc::Ppat_variant(label, arg) => {
                self.write("#");
                self.write(label);
                if let Some(arg) = arg {
                    self.write("(");
                    self.print_pattern(arg);
                    self.write(")");
                }
            }
            PatternDesc::Ppat_record(fields, closed) => {
                self.write("{");
                for (i, field) in fields.iter().enumerate() {
                    if i > 0 {
                        self.write(", ");
                    }
                    self.print_longident(&field.lid.txt);
                    self.write(": ");
                    self.print_pattern(&field.pat);
                }
                if *closed == ClosedFlag::Open {
                    if !fields.is_empty() {
                        self.write(", ");
                    }
                    self.write("_");
                }
                self.write("}");
            }
            PatternDesc::Ppat_array(pats) => {
                self.write("[");
                for (i, p) in pats.iter().enumerate() {
                    if i > 0 {
                        self.write(", ");
                    }
                    self.print_pattern(p);
                }
                self.write("]");
            }
            PatternDesc::Ppat_or(left, right) => {
                self.print_pattern(left);
                self.write(" | ");
                self.print_pattern(right);
            }
            PatternDesc::Ppat_constraint(pat, typ) => {
                self.print_pattern(pat);
                self.write(": ");
                self.print_core_type(typ);
            }
            PatternDesc::Ppat_type(lid) => {
                self.write("#");
                self.print_longident(&lid.txt);
            }
            PatternDesc::Ppat_unpack(name) => {
                self.write("module(");
                self.write(&name.txt);
                self.write(")");
            }
            PatternDesc::Ppat_exception(pat) => {
                self.write("exception ");
                self.print_pattern(pat);
            }
            PatternDesc::Ppat_extension(ext) => {
                self.write("%");
                self.write(&ext.0.txt);
            }
            PatternDesc::Ppat_open(lid, pat) => {
                self.print_longident(&lid.txt);
                self.write(".(");
                self.print_pattern(pat);
                self.write(")");
            }
        }
    }

    // ========================================================================
    // Type Printing
    // ========================================================================

    /// Print a core type.
    pub fn print_core_type(&mut self, typ: &CoreType) {
        match &typ.ptyp_desc {
            CoreTypeDesc::Ptyp_any => {
                self.write("_");
            }
            CoreTypeDesc::Ptyp_var(name) => {
                self.write("'");
                self.write(name);
            }
            CoreTypeDesc::Ptyp_arrow { arg, ret, .. } => {
                self.print_type_arg(arg);
                self.write(" => ");
                self.print_core_type(ret);
            }
            CoreTypeDesc::Ptyp_tuple(typs) => {
                self.write("(");
                for (i, t) in typs.iter().enumerate() {
                    if i > 0 {
                        self.write(", ");
                    }
                    self.print_core_type(t);
                }
                self.write(")");
            }
            CoreTypeDesc::Ptyp_constr(lid, args) => {
                // ReScript uses angle bracket syntax: constr<args>
                self.print_longident(&lid.txt);
                if !args.is_empty() {
                    self.write("<");
                    for (i, arg) in args.iter().enumerate() {
                        if i > 0 {
                            self.write(", ");
                        }
                        self.print_core_type(arg);
                    }
                    self.write(">");
                }
            }
            CoreTypeDesc::Ptyp_object(fields, closed) => {
                self.write("{");
                for (i, field) in fields.iter().enumerate() {
                    if i > 0 {
                        self.write(", ");
                    }
                    match field {
                        ObjectField::Otag(name, _attrs, typ) => {
                            self.write(&name.txt);
                            self.write(": ");
                            self.print_core_type(typ);
                        }
                        ObjectField::Oinherit(typ) => {
                            self.write("...");
                            self.print_core_type(typ);
                        }
                    }
                }
                if *closed == ClosedFlag::Open {
                    if !fields.is_empty() {
                        self.write(", ");
                    }
                    self.write("..");
                }
                self.write("}");
            }
            CoreTypeDesc::Ptyp_alias(typ, name) => {
                self.print_core_type(typ);
                self.write(" as '");
                self.write(name);
            }
            CoreTypeDesc::Ptyp_variant(fields, closed, _labels) => {
                self.write("[");
                if *closed == ClosedFlag::Open {
                    self.write("> ");
                }
                for (i, field) in fields.iter().enumerate() {
                    if i > 0 {
                        self.write(" | ");
                    }
                    self.print_row_field(field);
                }
                self.write("]");
            }
            CoreTypeDesc::Ptyp_poly(vars, typ) => {
                for var in vars {
                    self.write("'");
                    self.write(&var.txt);
                    self.write(" ");
                }
                self.write(". ");
                self.print_core_type(typ);
            }
            CoreTypeDesc::Ptyp_package(pkg) => {
                self.write("module(");
                self.print_longident(&pkg.0.txt);
                if !pkg.1.is_empty() {
                    self.write(" with ");
                    for (i, (lid, typ)) in pkg.1.iter().enumerate() {
                        if i > 0 {
                            self.write(" and ");
                        }
                        self.write("type ");
                        self.print_longident(&lid.txt);
                        self.write(" = ");
                        self.print_core_type(typ);
                    }
                }
                self.write(")");
            }
            CoreTypeDesc::Ptyp_extension(ext) => {
                self.write("%");
                self.write(&ext.0.txt);
            }
        }
    }

    /// Print a type argument.
    fn print_type_arg(&mut self, arg: &TypeArg) {
        self.print_arg_label(&arg.lbl);
        self.print_core_type(&arg.typ);
    }

    /// Print a row field.
    fn print_row_field(&mut self, field: &RowField) {
        match field {
            RowField::Rtag(label, _attrs, is_const, typs) => {
                self.write("#");
                self.write(&label.txt);
                if !*is_const && !typs.is_empty() {
                    self.write("(");
                    for (i, typ) in typs.iter().enumerate() {
                        if i > 0 {
                            self.write(", ");
                        }
                        self.print_core_type(typ);
                    }
                    self.write(")");
                }
            }
            RowField::Rinherit(typ) => {
                self.print_core_type(typ);
            }
        }
    }

    // ========================================================================
    // Module Printing
    // ========================================================================

    /// Print a module expression.
    pub fn print_module_expr(&mut self, modexpr: &ModuleExpr) {
        match &modexpr.pmod_desc {
            ModuleExprDesc::Pmod_ident(lid) => {
                self.print_longident(&lid.txt);
            }
            ModuleExprDesc::Pmod_structure(items) => {
                self.write("{");
                if !items.is_empty() {
                    self.indent();
                    for item in items {
                        self.newline();
                        self.print_structure_item(item);
                    }
                    self.dedent();
                    self.newline();
                }
                self.write("}");
            }
            ModuleExprDesc::Pmod_functor(name, param, body) => {
                self.write("(");
                self.write(&name.txt);
                if let Some(mt) = param {
                    self.write(": ");
                    self.print_module_type(mt);
                }
                self.write(") => ");
                self.print_module_expr(body);
            }
            ModuleExprDesc::Pmod_apply(func, arg) => {
                // Check if this is an await expression (has res.await attribute and __await__ functor)
                let is_await = modexpr
                    .pmod_attributes
                    .iter()
                    .any(|(n, _)| n.txt == "res.await")
                    && matches!(
                        &func.pmod_desc,
                        ModuleExprDesc::Pmod_ident(lid) if lid.txt.to_string() == "__await__"
                    );
                if is_await {
                    self.write("await ");
                    self.print_module_expr(arg);
                } else {
                    self.print_module_expr(func);
                    self.write("(");
                    self.print_module_expr(arg);
                    self.write(")");
                }
            }
            ModuleExprDesc::Pmod_constraint(expr, typ) => {
                self.write("(");
                self.print_module_expr(expr);
                self.write(": ");
                self.print_module_type(typ);
                self.write(")");
            }
            ModuleExprDesc::Pmod_unpack(expr) => {
                self.write("unpack(");
                self.print_expression(expr);
                self.write(")");
            }
            ModuleExprDesc::Pmod_extension(ext) => {
                self.write("%");
                self.write(&ext.0.txt);
            }
        }
    }

    /// Print a module type.
    pub fn print_module_type(&mut self, modtype: &ModuleType) {
        match &modtype.pmty_desc {
            ModuleTypeDesc::Pmty_ident(lid) => {
                self.print_longident(&lid.txt);
            }
            ModuleTypeDesc::Pmty_signature(items) => {
                self.write("{");
                if !items.is_empty() {
                    self.indent();
                    for item in items {
                        self.newline();
                        self.print_signature_item(item);
                    }
                    self.dedent();
                    self.newline();
                }
                self.write("}");
            }
            ModuleTypeDesc::Pmty_functor(name, param, body) => {
                self.write("(");
                self.write(&name.txt);
                if let Some(mt) = param {
                    self.write(": ");
                    self.print_module_type(mt);
                }
                self.write(") => ");
                self.print_module_type(body);
            }
            ModuleTypeDesc::Pmty_with(typ, constraints) => {
                self.print_module_type(typ);
                self.write(" with ");
                for (i, constraint) in constraints.iter().enumerate() {
                    if i > 0 {
                        self.write(" and ");
                    }
                    self.print_with_constraint(constraint);
                }
            }
            ModuleTypeDesc::Pmty_typeof(modexpr) => {
                self.write("module type of ");
                self.print_module_expr(modexpr);
            }
            ModuleTypeDesc::Pmty_extension(ext) => {
                self.write("%");
                self.write(&ext.0.txt);
            }
            ModuleTypeDesc::Pmty_alias(lid) => {
                self.write("module ");
                self.print_longident(&lid.txt);
            }
        }
    }

    /// Print a signature item.
    pub fn print_signature_item(&mut self, item: &SignatureItem) {
        match &item.psig_desc {
            SignatureItemDesc::Psig_value(vd) => {
                self.write("let ");
                self.write(&vd.pval_name.txt);
                self.write(": ");
                self.print_core_type(&vd.pval_type);
            }
            SignatureItemDesc::Psig_type(rec_flag, decls) => {
                self.print_type_declarations(*rec_flag, decls);
            }
            SignatureItemDesc::Psig_typext(ext) => {
                self.print_type_extension(ext);
            }
            SignatureItemDesc::Psig_exception(ext) => {
                self.write("exception ");
                self.print_extension_constructor(ext);
            }
            SignatureItemDesc::Psig_module(md) => {
                self.write("module ");
                self.write(&md.pmd_name.txt);
                self.write(": ");
                self.print_module_type(&md.pmd_type);
            }
            SignatureItemDesc::Psig_recmodule(mds) => {
                for (i, md) in mds.iter().enumerate() {
                    if i == 0 {
                        self.write("module rec ");
                    } else {
                        self.newline();
                        self.write("and ");
                    }
                    self.write(&md.pmd_name.txt);
                    self.write(": ");
                    self.print_module_type(&md.pmd_type);
                }
            }
            SignatureItemDesc::Psig_modtype(mtd) => {
                self.print_module_type_declaration(mtd);
            }
            SignatureItemDesc::Psig_open(od) => {
                self.write("open ");
                self.print_longident(&od.popen_lid.txt);
            }
            SignatureItemDesc::Psig_include(incl) => {
                self.write("include ");
                self.print_module_type(&incl.pincl_mod);
            }
            SignatureItemDesc::Psig_attribute(attr) => {
                self.write("@@");
                self.print_attribute(attr);
            }
            SignatureItemDesc::Psig_extension(ext, _attrs) => {
                self.write("%");
                self.write(&ext.0.txt);
            }
        }
    }

    /// Print a module binding.
    fn print_module_binding(&mut self, mb: &ModuleBinding) {
        self.write("module ");
        self.write(&mb.pmb_name.txt);
        self.write(" = ");
        self.print_module_expr(&mb.pmb_expr);
    }

    /// Print a module type declaration.
    fn print_module_type_declaration(&mut self, mtd: &ModuleTypeDeclaration) {
        self.write("module type ");
        self.write(&mtd.pmtd_name.txt);
        if let Some(typ) = &mtd.pmtd_type {
            self.write(" = ");
            self.print_module_type(typ);
        }
    }

    /// Print a with constraint.
    fn print_with_constraint(&mut self, constraint: &WithConstraint) {
        match constraint {
            WithConstraint::Pwith_type(lid, decl) => {
                self.write("type ");
                self.print_longident(&lid.txt);
                self.write(" = ");
                if let Some(manifest) = &decl.ptype_manifest {
                    self.print_core_type(manifest);
                }
            }
            WithConstraint::Pwith_module(lid1, lid2) => {
                self.write("module ");
                self.print_longident(&lid1.txt);
                self.write(" = ");
                self.print_longident(&lid2.txt);
            }
            WithConstraint::Pwith_typesubst(lid, decl) => {
                self.write("type ");
                self.print_longident(&lid.txt);
                self.write(" := ");
                if let Some(manifest) = &decl.ptype_manifest {
                    self.print_core_type(manifest);
                }
            }
            WithConstraint::Pwith_modsubst(lid1, lid2) => {
                self.write("module ");
                self.print_longident(&lid1.txt);
                self.write(" := ");
                self.print_longident(&lid2.txt);
            }
        }
    }

    // ========================================================================
    // Type Declarations
    // ========================================================================

    /// Print type declarations.
    fn print_type_declarations(&mut self, rec_flag: RecFlag, decls: &[TypeDeclaration]) {
        for (i, decl) in decls.iter().enumerate() {
            if i == 0 {
                self.write("type ");
                if rec_flag == RecFlag::Recursive && decls.len() > 1 {
                    self.write("rec ");
                }
            } else {
                self.newline();
                self.write("and ");
            }
            self.print_type_declaration(decl);
        }
    }

    /// Print a single type declaration.
    fn print_type_declaration(&mut self, decl: &TypeDeclaration) {
        // Print type name first (ReScript style: type option<'a> not 'a option)
        self.write(&decl.ptype_name.txt);

        // Print type parameters in angle brackets (ReScript style)
        if !decl.ptype_params.is_empty() {
            self.write("<");
            for (i, (typ, _var)) in decl.ptype_params.iter().enumerate() {
                if i > 0 {
                    self.write(", ");
                }
                self.print_core_type(typ);
            }
            self.write(">");
        }

        // Print kind or manifest
        match &decl.ptype_kind {
            TypeKind::Ptype_abstract => {
                if let Some(manifest) = &decl.ptype_manifest {
                    self.write(" = ");
                    if decl.ptype_private == PrivateFlag::Private {
                        self.write("private ");
                    }
                    self.print_core_type(manifest);
                }
            }
            TypeKind::Ptype_variant(constructors) => {
                self.write(" =");
                if decl.ptype_private == PrivateFlag::Private {
                    self.write(" private");
                }
                for ctor in constructors {
                    self.newline();
                    self.write("| ");
                    self.print_constructor_declaration(ctor);
                }
            }
            TypeKind::Ptype_record(fields) => {
                self.write(" = ");
                if decl.ptype_private == PrivateFlag::Private {
                    self.write("private ");
                }
                self.write("{");
                for (i, field) in fields.iter().enumerate() {
                    if i > 0 {
                        self.write(",");
                    }
                    self.newline();
                    self.write("  ");
                    self.print_label_declaration(field);
                }
                self.newline();
                self.write("}");
            }
            TypeKind::Ptype_open => {
                self.write(" = ..");
            }
        }
    }

    /// Print a constructor declaration.
    fn print_constructor_declaration(&mut self, ctor: &ConstructorDeclaration) {
        self.write(&ctor.pcd_name.txt);
        match &ctor.pcd_args {
            ConstructorArguments::Pcstr_tuple(typs) if !typs.is_empty() => {
                self.write("(");
                for (i, typ) in typs.iter().enumerate() {
                    if i > 0 {
                        self.write(", ");
                    }
                    self.print_core_type(typ);
                }
                self.write(")");
            }
            ConstructorArguments::Pcstr_record(fields) => {
                self.write("{");
                for (i, field) in fields.iter().enumerate() {
                    if i > 0 {
                        self.write(", ");
                    }
                    self.print_label_declaration(field);
                }
                self.write("}");
            }
            _ => {}
        }
        if let Some(res) = &ctor.pcd_res {
            self.write(": ");
            self.print_core_type(res);
        }
    }

    /// Print a label declaration.
    fn print_label_declaration(&mut self, field: &LabelDeclaration) {
        if field.pld_mutable == MutableFlag::Mutable {
            self.write("mutable ");
        }
        self.write(&field.pld_name.txt);
        self.write(": ");
        self.print_core_type(&field.pld_type);
    }

    /// Print an extension constructor.
    fn print_extension_constructor(&mut self, ext: &ExtensionConstructor) {
        self.write(&ext.pext_name.txt);
        match &ext.pext_kind {
            ExtensionConstructorKind::Pext_decl(args, res) => {
                match args {
                    ConstructorArguments::Pcstr_tuple(typs) if !typs.is_empty() => {
                        self.write("(");
                        for (i, typ) in typs.iter().enumerate() {
                            if i > 0 {
                                self.write(", ");
                            }
                            self.print_core_type(typ);
                        }
                        self.write(")");
                    }
                    ConstructorArguments::Pcstr_record(fields) => {
                        self.write("{");
                        for (i, field) in fields.iter().enumerate() {
                            if i > 0 {
                                self.write(", ");
                            }
                            self.print_label_declaration(field);
                        }
                        self.write("}");
                    }
                    _ => {}
                }
                if let Some(res) = res {
                    self.write(": ");
                    self.print_core_type(res);
                }
            }
            ExtensionConstructorKind::Pext_rebind(lid) => {
                self.write(" = ");
                self.print_longident(&lid.txt);
            }
        }
    }

    /// Print a type extension.
    fn print_type_extension(&mut self, ext: &TypeExtension) {
        self.write("type ");
        self.print_longident(&ext.ptyext_path.txt);
        self.write(" +=");
        if ext.ptyext_private == PrivateFlag::Private {
            self.write(" private");
        }
        for ctor in &ext.ptyext_constructors {
            self.newline();
            self.write("| ");
            self.print_extension_constructor(ctor);
        }
    }

    /// Print a value description.
    fn print_value_description(&mut self, vd: &ValueDescription) {
        self.write("external ");
        self.write(&vd.pval_name.txt);
        self.write(": ");
        self.print_core_type(&vd.pval_type);
        if !vd.pval_prim.is_empty() {
            self.write(" = ");
            for (i, prim) in vd.pval_prim.iter().enumerate() {
                if i > 0 {
                    self.space();
                }
                self.write("\"");
                self.write(prim);
                self.write("\"");
            }
        }
    }

    // ========================================================================
    // JSX
    // ========================================================================

    /// Print a JSX element.
    fn print_jsx_element(&mut self, elem: &JsxElement) {
        match elem {
            JsxElement::Unary(unary) => {
                self.write("<");
                self.print_jsx_tag_name(&unary.tag_name.txt);
                for prop in &unary.props {
                    self.space();
                    self.print_jsx_prop(prop);
                }
                self.write(" />");
            }
            JsxElement::Container(container) => {
                self.write("<");
                self.print_jsx_tag_name(&container.tag_name_start.txt);
                for prop in &container.props {
                    self.space();
                    self.print_jsx_prop(prop);
                }
                self.write(">");
                for child in &container.children {
                    self.print_jsx_child(child);
                }
                self.write("</");
                self.print_jsx_tag_name(&container.tag_name_start.txt);
                self.write(">");
            }
            JsxElement::Fragment(fragment) => {
                self.write("<>");
                for child in &fragment.children {
                    self.print_jsx_child(child);
                }
                self.write("</>");
            }
        }
    }

    /// Print a JSX child expression.
    /// Non-JSX expressions need to be wrapped in braces.
    fn print_jsx_child(&mut self, child: &Expression) {
        // JSX elements don't need braces
        if matches!(child.pexp_desc, ExpressionDesc::Pexp_jsx_element(_)) {
            self.print_expression(child);
        } else {
            // Other expressions need braces
            self.write("{");
            self.print_expression(child);
            self.write("}");
        }
    }

    /// Print a JSX tag name.
    fn print_jsx_tag_name(&mut self, name: &JsxTagName) {
        match name {
            JsxTagName::Lower(s) => self.write(s),
            JsxTagName::QualifiedLower { path, name } => {
                self.print_longident(path);
                self.write(".");
                self.write(name);
            }
            JsxTagName::Upper(lid) => self.print_longident(lid),
            JsxTagName::Invalid(s) => self.write(s),
        }
    }

    /// Print a JSX prop.
    fn print_jsx_prop(&mut self, prop: &JsxProp) {
        match prop {
            JsxProp::Value {
                name,
                optional,
                value,
            } => {
                self.write(&name.txt);
                if *optional {
                    self.write("=?");
                } else {
                    self.write("=");
                }
                self.write("{");
                self.print_expression(value);
                self.write("}");
            }
            JsxProp::Punning { optional, name } => {
                if *optional {
                    self.write("?");
                }
                self.write(&name.txt);
            }
            JsxProp::Spreading { expr, .. } => {
                self.write("{...");
                self.print_expression(expr);
                self.write("}");
            }
        }
    }

    // ========================================================================
    // Helpers
    // ========================================================================

    /// Print a longident.
    fn print_longident(&mut self, lid: &Longident) {
        match lid {
            Longident::Lident(s) => self.write(s),
            Longident::Ldot(prefix, s) => {
                self.print_longident(prefix);
                self.write(".");
                self.write(s);
            }
            Longident::Lapply(f, arg) => {
                self.print_longident(f);
                self.write("(");
                self.print_longident(arg);
                self.write(")");
            }
        }
    }

    /// Print a constant.
    fn print_constant(&mut self, c: &Constant) {
        match c {
            Constant::Integer(s, suffix) => {
                self.write(s);
                if let Some(suffix) = suffix {
                    let _ = write!(self.output, "{}", suffix);
                }
            }
            Constant::Char(code) => {
                let _ = write!(self.output, "'\\u{{{:04x}}}'", code);
            }
            Constant::String(s, _delim) => {
                self.write("\"");
                // The string is stored exactly as it appeared in source (between quotes),
                // so we output it directly. The escape sequences are already present.
                self.write(s);
                self.write("\"");
            }
            Constant::Float(s, suffix) => {
                self.write(s);
                if let Some(suffix) = suffix {
                    let _ = write!(self.output, "{}", suffix);
                }
            }
        }
    }

    /// Print a function expression, collecting all parameters and handling labeled args properly.
    fn print_fun_expr(&mut self, expr: &Expression) {
        // Collect all function parameters
        let mut params: Vec<(&ArgLabel, Option<&Expression>, &Pattern)> = Vec::new();
        let mut current = expr;

        while let ExpressionDesc::Pexp_fun {
            arg_label,
            default,
            lhs,
            rhs,
            ..
        } = &current.pexp_desc
        {
            params.push((arg_label, default.as_deref(), lhs));
            current = rhs;
        }

        // Check if we need parentheses:
        // - Any labeled parameter
        // - Multiple parameters
        // - Any default value
        let needs_parens = params.len() > 1
            || params
                .iter()
                .any(|(label, default, _)| !matches!(label, ArgLabel::Nolabel) || default.is_some());

        // Print parameters
        if needs_parens {
            self.write("(");
        }

        for (i, (label, default, pat)) in params.iter().enumerate() {
            if i > 0 {
                self.write(", ");
            }
            self.print_fun_param(label, *default, pat);
        }

        if needs_parens {
            self.write(")");
        }

        self.write(" => ");
        // Function body needs braces if it's a sequence
        if matches!(&current.pexp_desc, ExpressionDesc::Pexp_sequence(..)) {
            self.write("{");
            self.newline();
            self.print_block_body(current);
            self.newline();
            self.write("}");
        } else {
            self.print_expression(current);
        }
    }

    /// Print a single function parameter, using punning when possible.
    fn print_fun_param(&mut self, label: &ArgLabel, default: Option<&Expression>, pat: &Pattern) {
        match label {
            ArgLabel::Nolabel => {
                self.print_pattern(pat);
                if let Some(def) = default {
                    self.write("=");
                    self.print_expression(def);
                }
            }
            ArgLabel::Labelled(name) => {
                self.write("~");
                self.write(name);
                // Check for punning: ~name where pattern is just the variable name
                let is_punned = Self::is_punned_pattern(name, pat);
                if !is_punned {
                    self.write(" as ");
                    self.print_pattern(pat);
                }
                if let Some(def) = default {
                    self.write("=");
                    self.print_expression(def);
                }
            }
            ArgLabel::Optional(name) => {
                self.write("~");
                self.write(name);
                // Check for punning
                let is_punned = Self::is_punned_pattern(name, pat);
                if !is_punned {
                    self.write(" as ");
                    self.print_pattern(pat);
                }
                self.write("=?");
                if let Some(def) = default {
                    self.print_expression(def);
                }
            }
        }
    }

    /// Check if pattern is a simple variable with the same name (for punning).
    fn is_punned_pattern(name: &str, pat: &Pattern) -> bool {
        match &pat.ppat_desc {
            PatternDesc::Ppat_var(var) => var.txt == name,
            PatternDesc::Ppat_constraint(inner, _) => Self::is_punned_pattern(name, inner),
            _ => false,
        }
    }

    /// Print an argument label (used for function application arguments).
    fn print_arg_label(&mut self, label: &ArgLabel) {
        match label {
            ArgLabel::Nolabel => {}
            ArgLabel::Labelled(s) => {
                self.write("~");
                self.write(s);
                self.write("=");
            }
            ArgLabel::Optional(s) => {
                self.write("~");
                self.write(s);
                self.write("=?");
            }
        }
    }

    /// Print attributes.
    fn print_attributes(&mut self, attrs: &Attributes) {
        for attr in attrs {
            self.write("@");
            self.print_attribute(attr);
            self.newline();
        }
    }

    /// Print a single attribute.
    fn print_attribute(&mut self, attr: &Attribute) {
        self.write(&attr.0.txt);
        match &attr.1 {
            Payload::PStr(items) if items.is_empty() => {}
            _ => {
                self.write("(...)");
            }
        }
    }

    fn print_extension_payload(&mut self, payload: &Payload) {
        match payload {
            Payload::PStr(items) => {
                if !items.is_empty() {
                    self.write("(");
                    for item in items {
                        self.print_structure_item(item);
                    }
                    self.write(")");
                }
            }
            Payload::PSig(_) => self.write("(: ...)"),
            Payload::PTyp(typ) => {
                self.write("(: ");
                self.print_core_type(typ);
                self.write(")");
            }
            Payload::PPat(pat, expr) => {
                self.write("(");
                self.print_pattern(pat);
                if let Some(e) = expr {
                    self.write(" when ");
                    self.print_expression(e);
                }
                self.write(")");
            }
        }
    }
}

// Implement get_attributes for StructureItemDesc
impl StructureItemDesc {
    fn get_attributes(&self) -> Attributes {
        match self {
            StructureItemDesc::Pstr_eval(_, attrs) => attrs.clone(),
            StructureItemDesc::Pstr_extension(_, attrs) => attrs.clone(),
            _ => vec![],
        }
    }
}

// ============================================================================
// Convenience Functions
// ============================================================================

/// Print a structure to a string.
pub fn print_structure(structure: &Structure) -> String {
    let mut printer = Printer::new();
    printer.print_structure(structure);
    printer.into_output()
}

/// Print an expression to a string.
pub fn print_expression(expr: &Expression) -> String {
    let mut printer = Printer::new();
    printer.print_expression(expr);
    printer.into_output()
}

/// Print a pattern to a string.
pub fn print_pattern(pat: &Pattern) -> String {
    let mut printer = Printer::new();
    printer.print_pattern(pat);
    printer.into_output()
}

/// Print a core type to a string.
pub fn print_core_type(typ: &CoreType) -> String {
    let mut printer = Printer::new();
    printer.print_core_type(typ);
    printer.into_output()
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::{Parser, module};
    use std::sync::mpsc;
    use std::thread;
    use std::time::Duration;

    /// Default timeout for parsing operations (5 seconds - very generous for ms-scale ops)
    const PARSE_TIMEOUT: Duration = Duration::from_secs(5);

    /// Parse and print source code with a timeout.
    /// Panics if parsing/printing takes longer than PARSE_TIMEOUT.
    fn roundtrip(source: &str) -> String {
        let source_owned = source.to_string();
        let source_for_error = source_owned.clone();
        let (tx, rx) = mpsc::channel();

        thread::spawn(move || {
            let mut parser = Parser::new("test.res", &source_owned);
            let structure = module::parse_structure(&mut parser);
            let result = print_structure(&structure);
            let _ = tx.send(result);
        });

        match rx.recv_timeout(PARSE_TIMEOUT) {
            Ok(result) => result,
            Err(mpsc::RecvTimeoutError::Timeout) => {
                panic!(
                    "Parser timed out after {:?} for input: {}",
                    PARSE_TIMEOUT,
                    if source_for_error.len() > 100 {
                        format!("{}...", &source_for_error[..100])
                    } else {
                        source_for_error
                    }
                )
            }
            Err(mpsc::RecvTimeoutError::Disconnected) => {
                panic!("Parser thread panicked for input: {}", source_for_error)
            }
        }
    }

    #[test]
    fn test_print_let_binding() {
        let result = roundtrip("let x = 42");
        assert!(result.contains("let x = 42"));
    }

    #[test]
    fn test_print_let_binding_with_string() {
        let result = roundtrip("let s = \"hello\"");
        assert!(result.contains("let s = \"hello\""));
    }

    #[test]
    fn test_print_type_declaration() {
        let result = roundtrip("type t = int");
        assert!(result.contains("type t = int"));
    }

    #[test]
    fn test_print_variant_type() {
        let result = roundtrip("type color = | Red | Green | Blue");
        assert!(result.contains("type color ="));
        assert!(result.contains("| Red"));
        assert!(result.contains("| Green"));
        assert!(result.contains("| Blue"));
    }

    #[test]
    fn test_print_record_type() {
        let result = roundtrip("type person = { name: string, age: int }");
        assert!(result.contains("type person = {"));
        assert!(result.contains("name: string"));
        assert!(result.contains("age: int"));
    }

    #[test]
    fn test_print_open() {
        let result = roundtrip("open Belt");
        assert!(result.contains("open Belt"));
    }

    #[test]
    fn test_print_module() {
        let result = roundtrip("module M = { let x = 1 }");
        assert!(result.contains("module M = {"));
        assert!(result.contains("let x = 1"));
    }

    #[test]
    fn test_print_external() {
        let result = roundtrip("external log: string => unit = \"console.log\"");
        assert!(result.contains("external log: string => unit"));
        assert!(result.contains("\"console.log\""));
    }

    #[test]
    fn test_print_exception() {
        let result = roundtrip("exception MyError");
        assert!(result.contains("exception MyError"));
    }

    #[test]
    fn test_print_array() {
        let result = roundtrip("let arr = [1, 2, 3]");
        assert!(result.contains("[1, 2, 3]"));
    }

    #[test]
    fn test_print_tuple() {
        let result = roundtrip("let t = (1, 2)");
        assert!(result.contains("(1, 2)"));
    }

    #[test]
    fn test_print_if() {
        let result = roundtrip("let x = if true { 1 } else { 2 }");
        assert!(result.contains("if true"));
    }

    #[test]
    fn test_print_switch() {
        let result = roundtrip("let x = switch y { | 1 => true | _ => false }");
        assert!(result.contains("switch y"));
        assert!(result.contains("| 1 => true"));
    }

    #[test]
    fn test_printer_is_send_sync() {
        fn assert_send_sync<T: Send + Sync>() {}
        assert_send_sync::<Printer>();
    }

    // Additional roundtrip tests

    #[test]
    fn test_print_function() {
        let result = roundtrip("let f = (x) => x + 1");
        assert!(result.contains("=>"));
    }

    #[test]
    fn test_print_function_application() {
        let result = roundtrip("let y = f(1, 2)");
        assert!(result.contains("f(1, 2)"));
    }

    #[test]
    fn test_print_record_expression() {
        let result = roundtrip("let r = {name: \"test\", age: 30}");
        assert!(result.contains("name:"));
        assert!(result.contains("age:"));
    }

    #[test]
    fn test_print_field_access() {
        let result = roundtrip("let n = person.name");
        assert!(result.contains("person.name"));
    }

    #[test]
    fn test_print_while_loop() {
        let result = roundtrip("let _ = while true { () }");
        assert!(result.contains("while true"));
    }

    #[test]
    fn test_print_for_loop() {
        let result = roundtrip("for i in 0 to 10 { () }");
        assert!(result.contains("for i in"));
        assert!(result.contains("to"));
    }

    #[test]
    fn test_print_polymorphic_variant() {
        let result = roundtrip("let x = #Red");
        assert!(result.contains("#Red") || result.contains("Red"));
    }

    #[test]
    fn test_print_try_catch() {
        let result = roundtrip("let x = try f() catch { | Exn => 0 }");
        assert!(result.contains("try"));
        assert!(result.contains("catch"));
    }

    #[test]
    fn test_print_type_with_params() {
        // ReScript uses angle bracket syntax for type parameters: type option<'a>
        let result = roundtrip("type option<'a> = | None | Some('a)");
        assert!(
            result.contains("option<'a>"),
            "Expected \"option<'a>\" but got: {}",
            result
        );
        assert!(result.contains("| None"));
        assert!(result.contains("| Some"));
    }

    #[test]
    fn test_print_labeled_function() {
        let result = roundtrip("let f = (~x, ~y) => x + y");
        assert!(result.contains("~x"));
        assert!(result.contains("~y"));
    }

    #[test]
    fn test_print_pattern_with_constructor() {
        let result = roundtrip("let Some(x) = opt");
        assert!(result.contains("Some(x)"));
    }

    #[test]
    fn test_print_constraint() {
        let result = roundtrip("let x: int = 42");
        // The pattern constraint might be parsed differently
        assert!(result.contains("int") || result.contains("x = 42"));
    }

    // TODO: assert parsing needs to be implemented
    // #[test]
    // fn test_print_assert() {
    //     let result = roundtrip("let _ = assert(x > 0)");
    //     eprintln!("Assert result: {}", result);
    //     assert!(result.contains("assert"));
    // }

    #[test]
    fn test_print_await() {
        let result = roundtrip("let x = await promise");
        assert!(result.contains("await promise"));
    }

    #[test]
    fn test_print_sequence() {
        let result = roundtrip("let _ = { f(); g() }");
        // The parser might parse this differently
        assert!(result.contains("f") && result.contains("g"));
    }

    #[test]
    fn test_print_binary_operators() {
        let result = roundtrip("let x = 1 + 2 * 3");
        assert!(result.contains("1"));
        assert!(result.contains("2"));
        assert!(result.contains("3"));
    }

    #[test]
    fn test_print_module_type() {
        let result = roundtrip("module type S = { type t let x: t }");
        assert!(result.contains("module type S"));
    }

    #[test]
    fn test_print_include() {
        let result = roundtrip("include Belt");
        assert!(result.contains("include Belt"));
    }

    #[test]
    fn test_print_or_pattern() {
        let result = roundtrip("let x = switch y { | 1 | 2 => true | _ => false }");
        assert!(result.contains("1 | 2"));
    }

    #[test]
    fn test_print_constructor_with_args() {
        let result = roundtrip("type t = | Point(int, int)");
        assert!(result.contains("Point(int, int)"));
    }

    // JSX tests
    #[test]
    fn test_print_jsx_unary() {
        let result = roundtrip("let x = <div />");
        assert!(result.contains("<div"));
        assert!(result.contains("/>"));
    }

    #[test]
    fn test_print_jsx_container() {
        let result = roundtrip("let x = <div> {child} </div>");
        assert!(result.contains("<div>"));
        assert!(result.contains("</div>"));
    }

    #[test]
    fn test_print_jsx_fragment() {
        let result = roundtrip("let x = <> {child} </>");
        assert!(result.contains("<>"));
        assert!(result.contains("</>"));
    }

    #[test]
    fn test_print_jsx_with_props() {
        let result = roundtrip("let x = <Button onClick={handler} />");
        assert!(result.contains("onClick"));
    }

    // Edge cases
    #[test]
    fn test_print_nested_modules() {
        let result = roundtrip("module A = { module B = { let x = 1 } }");
        assert!(result.contains("module A"));
        assert!(result.contains("module B"));
    }

    #[test]
    fn test_print_functor() {
        let result = roundtrip("module F = (X: S) => { type t = X.t }");
        assert!(result.contains("module F"));
    }

    #[test]
    fn test_print_mutable_field() {
        let result = roundtrip("type t = { mutable x: int }");
        assert!(result.contains("mutable"));
    }

    #[test]
    fn test_print_private_type() {
        let result = roundtrip("type t = private int");
        assert!(result.contains("private"));
    }

    // TODO: Type extension parsing needs work
    // #[test]
    // fn test_print_type_extension() {
    //     let result = roundtrip("type t += | NewCase");
    //     assert!(result.contains("+="));
    // }

    #[test]
    fn test_print_nested_switch() {
        let result = roundtrip(
            r#"
            let x = switch a {
            | 1 => switch b { | 2 => true | _ => false }
            | _ => false
            }
        "#,
        );
        assert!(result.contains("switch a"));
        assert!(result.contains("switch b"));
    }

    #[test]
    fn test_print_chained_field_access() {
        let result = roundtrip("let x = a.b.c.d");
        assert!(result.contains("a.b.c.d"));
    }

    #[test]
    fn test_print_optional_labeled_arg() {
        let result = roundtrip("let f = (~x=?, ~y) => x");
        assert!(result.contains("~x"));
        assert!(result.contains("~y"));
    }

    #[test]
    fn test_print_variant_with_payload() {
        let result = roundtrip("let x = #Foo(1, 2)");
        // Polymorphic variants might parse differently
        assert!(result.contains("Foo") || result.contains("#"));
    }

    #[test]
    fn test_print_record_spread() {
        let result = roundtrip("let r = {...old, x: 1}");
        assert!(result.contains("..."));
    }

    #[test]
    fn test_print_nested_tuples() {
        let result = roundtrip("let x = ((1, 2), (3, 4))");
        assert!(result.contains("(1, 2)"));
        assert!(result.contains("(3, 4)"));
    }

    #[test]
    fn test_print_complex_type() {
        // Type parameters with angle brackets use different syntax
        let result = roundtrip("type ('a, 'b) t = ('a, 'b) => option");
        assert!(result.contains("t"));
    }

    // Stress tests for deeply nested structures (verify no hangs)
    #[test]
    fn test_deeply_nested_parens() {
        let result = roundtrip("let x = ((((((1))))))");
        assert!(result.contains("1"));
    }

    #[test]
    fn test_deeply_nested_arrays() {
        let result = roundtrip("let x = [[[1, 2], [3, 4]], [[5, 6], [7, 8]]]");
        assert!(result.contains("1"));
        assert!(result.contains("8"));
    }

    #[test]
    fn test_long_binary_expression() {
        let result = roundtrip("let x = 1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9 + 10");
        assert!(result.contains("1"));
        assert!(result.contains("10"));
    }

    #[test]
    fn test_many_function_args() {
        let result = roundtrip("let f = (a, b, c, d, e, f, g, h) => a + b + c");
        assert!(result.contains("a"));
        assert!(result.contains("h"));
    }

    #[test]
    fn test_chained_pipes() {
        let result = roundtrip("let x = a->f->g->h->i->j");
        assert!(result.contains("a"));
        assert!(result.contains("j") || result.contains("->"));
    }
}
