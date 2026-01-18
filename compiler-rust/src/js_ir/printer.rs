//! JavaScript IR Printer - Generates JavaScript source code from JS IR.
//!
//! This module provides functionality to convert the JavaScript IR
//! into formatted JavaScript source code.

use super::op::{BinOp, Number, PropertyName};
use super::{
    Block, CaseClause, Delim, Expression, ExpressionDesc, ForDirection, JsRawInfo, Program,
    Statement, StatementDesc, StringClause, VIdent, VariableDeclaration,
};
use crate::ident::Ident;
use std::collections::HashSet;
use std::fmt::Write;

/// JavaScript printer state
#[derive(Debug)]
pub struct JsPrinter {
    /// Output buffer
    output: String,
    /// Current indentation level
    indent: usize,
    /// Indentation string (2 spaces)
    indent_str: &'static str,
    /// Used identifiers (for scope management)
    used_idents: HashSet<String>,
}

impl Default for JsPrinter {
    fn default() -> Self {
        Self::new()
    }
}

impl JsPrinter {
    /// Create a new printer
    pub fn new() -> Self {
        Self {
            output: String::new(),
            indent: 0,
            indent_str: "  ",
            used_idents: HashSet::new(),
        }
    }

    /// Get the generated JavaScript code
    pub fn into_output(self) -> String {
        self.output
    }

    /// Get a reference to the output
    pub fn output(&self) -> &str {
        &self.output
    }

    /// Write a string
    fn write(&mut self, s: &str) {
        self.output.push_str(s);
    }

    /// Write a character
    fn write_char(&mut self, c: char) {
        self.output.push(c);
    }

    /// Write a newline with current indentation
    fn newline(&mut self) {
        self.output.push('\n');
        for _ in 0..self.indent {
            self.output.push_str(self.indent_str);
        }
    }

    /// Write a space
    fn space(&mut self) {
        self.output.push(' ');
    }

    /// Write a semicolon
    fn semi(&mut self) {
        self.output.push(';');
    }

    /// Increase indentation
    fn indent(&mut self) {
        self.indent += 1;
    }

    /// Decrease indentation
    fn dedent(&mut self) {
        self.indent = self.indent.saturating_sub(1);
    }

    /// Get a unique name for an identifier
    fn ident_name(&self, id: &Ident) -> String {
        if id.stamp() == 0 {
            // Persistent identifier
            id.name().to_string()
        } else {
            // Local identifier - use name with stamp
            format!("{}${}", id.name(), id.stamp())
        }
    }

    /// Print a program
    pub fn print_program(&mut self, program: &Program) {
        self.print_statements(&program.block, true);
    }

    /// Print statements
    pub fn print_statements(&mut self, stmts: &Block, top_level: bool) {
        for (i, stmt) in stmts.iter().enumerate() {
            if i > 0 {
                if top_level {
                    self.newline();
                }
                self.newline();
            }
            self.print_statement(stmt, top_level);
        }
    }

    /// Print a statement
    pub fn print_statement(&mut self, stmt: &Statement, _top_level: bool) {
        // Print comment if present
        if let Some(ref comment) = stmt.comment {
            self.write("/* ");
            self.write(comment);
            self.write(" */");
            self.newline();
        }

        self.print_statement_desc(&stmt.desc);
    }

    /// Print a statement description
    fn print_statement_desc(&mut self, desc: &StatementDesc) {
        match desc {
            StatementDesc::Block(stmts) => {
                if stmts.is_empty() {
                    return;
                }
                self.print_statements(stmts, false);
            }

            StatementDesc::Variable(decl) => {
                self.print_variable_declaration(decl);
            }

            StatementDesc::Exp(expr) => {
                // Skip pure variable expressions
                if matches!(expr.desc, ExpressionDesc::Var(_)) {
                    return;
                }
                let needs_paren = self.expr_needs_paren(expr);
                if needs_paren {
                    self.write_char('(');
                }
                self.print_expression(expr, 0);
                if needs_paren {
                    self.write_char(')');
                }
                self.semi();
            }

            StatementDesc::If(cond, then_block, else_block) => {
                self.write("if ");
                self.write_char('(');
                self.print_expression(cond, 0);
                self.write(") ");
                self.print_brace_block(then_block);

                if !else_block.is_empty() {
                    self.write(" else ");
                    // Check if else block is just another if
                    if else_block.len() == 1 {
                        if let StatementDesc::If(_, _, _) = &else_block[0].desc {
                            self.print_statement(&else_block[0], false);
                            return;
                        }
                    }
                    self.print_brace_block(else_block);
                }
            }

            StatementDesc::While(cond, body) => {
                self.write("while ");
                self.write_char('(');
                self.print_expression(cond, 0);
                self.write(") ");
                self.print_brace_block(body);
                self.semi();
            }

            StatementDesc::ForRange(init, end, id, direction, body) => {
                let id_name = self.ident_name(id);
                self.write("for ");
                self.write_char('(');

                // Initialization
                if let Some(init_expr) = init {
                    self.write("let ");
                    self.write(&id_name);
                    self.write(" = ");
                    self.print_expression(init_expr, 0);
                }
                self.semi();
                self.space();

                // Condition
                self.write(&id_name);
                match direction {
                    ForDirection::Upto => self.write(" <= "),
                    ForDirection::Downto => self.write(" >= "),
                    ForDirection::Up => self.write(" < "),
                }
                self.print_expression(end, 0);
                self.semi();
                self.space();

                // Increment/decrement
                match direction {
                    ForDirection::Upto | ForDirection::Up => {
                        self.write("++");
                        self.write(&id_name);
                    }
                    ForDirection::Downto => {
                        self.write("--");
                        self.write(&id_name);
                    }
                }

                self.write(") ");
                self.print_brace_block(body);
            }

            StatementDesc::Continue => {
                self.write("continue");
                self.semi();
            }

            StatementDesc::Break => {
                self.write("break");
                self.semi();
            }

            StatementDesc::Return(expr) => {
                // Check for undefined return
                if matches!(expr.desc, ExpressionDesc::Undefined { .. }) {
                    self.write("return");
                    self.semi();
                    return;
                }

                self.write("return ");
                self.print_expression(expr, 0);
                self.semi();
            }

            StatementDesc::IntSwitch(expr, cases, default) => {
                self.write("switch ");
                self.write_char('(');
                self.print_expression(expr, 0);
                self.write(") {");
                self.indent();

                for (value, clause) in cases {
                    self.print_int_case(*value, clause);
                }

                if let Some(default_block) = default {
                    self.newline();
                    self.write("default:");
                    self.indent();
                    self.newline();
                    self.print_statements(default_block, false);
                    self.dedent();
                }

                self.dedent();
                self.newline();
                self.write_char('}');
            }

            StatementDesc::StringSwitch(expr, cases, default) => {
                self.write("switch ");
                self.write_char('(');
                self.print_expression(expr, 0);
                self.write(") {");
                self.indent();

                for clause in cases {
                    self.print_string_case(clause);
                }

                if let Some(default_block) = default {
                    self.newline();
                    self.write("default:");
                    self.indent();
                    self.newline();
                    self.print_statements(default_block, false);
                    self.dedent();
                }

                self.dedent();
                self.newline();
                self.write_char('}');
            }

            StatementDesc::Throw(expr) => {
                self.write("throw ");
                self.print_expression(expr, 0);
                self.semi();
            }

            StatementDesc::Try(body, catch, finally) => {
                self.write("try ");
                self.print_brace_block(body);

                if let Some((exn_id, catch_block)) = catch {
                    let exn_name = self.ident_name(exn_id);
                    self.write(" catch (");
                    self.write(&exn_name);
                    self.write(") ");
                    self.print_brace_block(catch_block);
                }

                if let Some(finally_block) = finally {
                    self.write(" finally ");
                    self.print_brace_block(finally_block);
                }
            }

            StatementDesc::Debugger => {
                self.write("debugger");
                self.semi();
            }
        }
    }

    /// Print a brace-enclosed block
    fn print_brace_block(&mut self, stmts: &Block) {
        self.write_char('{');
        if !stmts.is_empty() {
            self.indent();
            self.newline();
            self.print_statements(stmts, false);
            self.dedent();
            self.newline();
        }
        self.write_char('}');
    }

    /// Print a variable declaration
    fn print_variable_declaration(&mut self, decl: &VariableDeclaration) {
        let var_name = self.ident_name(&decl.ident);
        self.write("let ");
        self.write(&var_name);

        if let Some(value) = &decl.value {
            self.write(" = ");
            self.print_expression(value, 1);
        }

        self.semi();
    }

    /// Print an integer case
    fn print_int_case(&mut self, value: i32, clause: &CaseClause) {
        self.newline();
        self.write("case ");
        write!(self.output, "{}", value).unwrap();
        self.write_char(':');

        if !clause.body.is_empty() {
            self.indent();
            self.newline();
            self.print_statements(&clause.body, false);
            if clause.should_break {
                self.newline();
                self.write("break");
                self.semi();
            }
            self.dedent();
        }
    }

    /// Print a string case
    fn print_string_case(&mut self, clause: &StringClause) {
        self.newline();
        self.write("case ");
        self.print_string(&clause.tag_type);
        self.write_char(':');

        if !clause.clause.body.is_empty() {
            self.indent();
            self.newline();
            self.print_statements(&clause.clause.body, false);
            if clause.clause.should_break {
                self.newline();
                self.write("break");
                self.semi();
            }
            self.dedent();
        }
    }

    /// Check if an expression needs parentheses when used as a statement
    fn expr_needs_paren(&self, expr: &Expression) -> bool {
        matches!(
            expr.desc,
            ExpressionDesc::Fun { .. } | ExpressionDesc::Object(_, _)
        )
    }

    /// Print an expression
    pub fn print_expression(&mut self, expr: &Expression, level: u8) {
        // Print comment if present
        if let Some(ref comment) = expr.comment {
            self.write("/* ");
            self.write(comment);
            self.write(" */ ");
        }

        self.print_expression_desc(&expr.desc, level);
    }

    /// Print an expression description
    fn print_expression_desc(&mut self, desc: &ExpressionDesc, level: u8) {
        match desc {
            ExpressionDesc::Null => {
                self.write("null");
            }

            ExpressionDesc::Undefined { .. } => {
                self.write("undefined");
            }

            ExpressionDesc::Var(vident) => {
                self.print_vident(vident);
            }

            ExpressionDesc::Bool(b) => {
                self.write(if *b { "true" } else { "false" });
            }

            ExpressionDesc::Number(n) => {
                self.print_number(n, level);
            }

            ExpressionDesc::Str { delim, txt } => {
                self.print_string_with_delim(txt, *delim);
            }

            ExpressionDesc::Array(elements, _) => {
                self.write_char('[');
                for (i, elem) in elements.iter().enumerate() {
                    if i > 0 {
                        self.write(", ");
                    }
                    self.print_expression(elem, 1);
                }
                self.write_char(']');
            }

            ExpressionDesc::Object(dup, props) => {
                let needs_paren = level > 1;
                if needs_paren {
                    self.write_char('(');
                }

                self.write_char('{');
                if !props.is_empty() || dup.is_some() {
                    self.indent();

                    // Handle spread/duplicate
                    if let Some(spread) = dup {
                        self.newline();
                        self.write("...");
                        self.print_expression(spread, 1);
                        if !props.is_empty() {
                            self.write_char(',');
                        }
                    }

                    for (i, (name, value)) in props.iter().enumerate() {
                        if i > 0 || dup.is_some() {
                            self.newline();
                        } else {
                            self.newline();
                        }
                        self.print_property_name(name);
                        self.write(": ");
                        self.print_expression(value, 1);
                        if i < props.len() - 1 {
                            self.write_char(',');
                        }
                    }

                    self.dedent();
                    self.newline();
                }
                self.write_char('}');

                if needs_paren {
                    self.write_char(')');
                }
            }

            ExpressionDesc::Seq(e1, e2) => {
                let needs_paren = level > 0;
                if needs_paren {
                    self.write_char('(');
                }
                self.print_expression(e1, 0);
                self.write(", ");
                self.print_expression(e2, 0);
                if needs_paren {
                    self.write_char(')');
                }
            }

            ExpressionDesc::Cond(cond, then_, else_) => {
                let needs_paren = level > 2;
                if needs_paren {
                    self.write_char('(');
                }
                self.print_expression(cond, 3);
                self.write(" ? ");
                self.print_expression(then_, 3);
                self.write(" : ");
                self.print_expression(else_, 3);
                if needs_paren {
                    self.write_char(')');
                }
            }

            ExpressionDesc::Bin(op, left, right) => {
                let (out, lft, rght) = op_precedence(*op);
                let needs_paren = level > out;
                if needs_paren {
                    self.write_char('(');
                }
                self.print_expression(left, lft);
                self.space();
                self.write(op.to_js_string());
                self.space();
                self.print_expression(right, rght);
                if needs_paren {
                    self.write_char(')');
                }
            }

            ExpressionDesc::JsNot(e) => {
                let needs_paren = level > 13;
                if needs_paren {
                    self.write_char('(');
                }
                self.write_char('!');
                self.print_expression(e, 13);
                if needs_paren {
                    self.write_char(')');
                }
            }

            ExpressionDesc::JsBnot(e) => {
                let needs_paren = level > 13;
                if needs_paren {
                    self.write_char('(');
                }
                self.write_char('~');
                self.print_expression(e, 13);
                if needs_paren {
                    self.write_char(')');
                }
            }

            ExpressionDesc::Typeof(e) => {
                self.write("typeof ");
                self.print_expression(e, 13);
            }

            ExpressionDesc::In(prop, obj) => {
                let needs_paren = level > 12;
                if needs_paren {
                    self.write_char('(');
                }
                self.print_expression(prop, 0);
                self.write(" in ");
                self.print_expression(obj, 0);
                if needs_paren {
                    self.write_char(')');
                }
            }

            ExpressionDesc::Call(func, args, _info) => {
                let needs_paren = level > 15;
                if needs_paren {
                    self.write_char('(');
                }

                // Wrap function expressions in parens
                if matches!(func.desc, ExpressionDesc::Fun { .. }) {
                    self.write_char('(');
                    self.print_expression(func, 15);
                    self.write_char(')');
                } else {
                    self.print_expression(func, 15);
                }

                self.write_char('(');
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        self.write(", ");
                    }
                    self.print_expression(arg, 1);
                }
                self.write_char(')');

                if needs_paren {
                    self.write_char(')');
                }
            }

            ExpressionDesc::FlatCall(func, args) => {
                self.print_expression(func, 15);
                self.write(".apply(null, ");
                self.print_expression(args, 1);
                self.write_char(')');
            }

            ExpressionDesc::New(ctor, args) => {
                let needs_paren = level > 15;
                if needs_paren {
                    self.write_char('(');
                }
                self.write("new ");
                self.print_expression(ctor, 16);
                self.write_char('(');
                if let Some(args) = args {
                    for (i, arg) in args.iter().enumerate() {
                        if i > 0 {
                            self.write(", ");
                        }
                        self.print_expression(arg, 1);
                    }
                }
                self.write_char(')');
                if needs_paren {
                    self.write_char(')');
                }
            }

            ExpressionDesc::ArrayIndex(arr, idx) => {
                let needs_paren = level > 15;
                if needs_paren {
                    self.write_char('(');
                }
                self.print_expression(arr, 15);
                self.write_char('[');
                self.print_expression(idx, 0);
                self.write_char(']');
                if needs_paren {
                    self.write_char(')');
                }
            }

            ExpressionDesc::StringIndex(str_expr, idx) => {
                self.print_expression(str_expr, 15);
                self.write(".codePointAt(");
                self.print_expression(idx, 0);
                self.write_char(')');
            }

            ExpressionDesc::StaticIndex(obj, field, _) => {
                let needs_paren = level > 15;
                if needs_paren {
                    self.write_char('(');
                }
                self.print_expression(obj, 15);
                self.print_property_access(field);
                if needs_paren {
                    self.write_char(')');
                }
            }

            ExpressionDesc::Length(e, obj_type) => {
                let needs_paren = level > 15;
                if needs_paren {
                    self.write_char('(');
                }
                self.print_expression(e, 15);
                self.write(".length");
                if needs_paren {
                    self.write_char(')');
                }
                let _ = obj_type; // Used for type-specific optimizations
            }

            ExpressionDesc::IsNullOrUndefined(e) => {
                let needs_paren = level > 0;
                if needs_paren {
                    self.write_char('(');
                }
                self.print_expression(e, 1);
                self.write(" == null");
                if needs_paren {
                    self.write_char(')');
                }
            }

            ExpressionDesc::StringAppend(e1, e2) => {
                let (out, lft, rght) = op_precedence(BinOp::Plus);
                let needs_paren = level > out;
                if needs_paren {
                    self.write_char('(');
                }
                self.print_expression(e1, lft);
                self.write(" + ");
                self.print_expression(e2, rght);
                if needs_paren {
                    self.write_char(')');
                }
            }

            ExpressionDesc::Fun {
                is_method,
                params,
                body,
                async_,
                directive,
                ..
            } => {
                let use_arrow = !*is_method;

                // Collect param names upfront to avoid borrow issues
                let param_names: Vec<String> = params.iter().map(|p| self.ident_name(p)).collect();

                // Async prefix
                if *async_ {
                    self.write("async ");
                }

                // Function keyword or arrow
                if !use_arrow {
                    self.write("function");
                }

                // Parameters
                if param_names.len() == 1 && use_arrow {
                    self.write(&param_names[0]);
                } else {
                    self.write_char('(');
                    for (i, name) in param_names.iter().enumerate() {
                        if i > 0 {
                            self.write(", ");
                        }
                        self.write(name);
                    }
                    self.write_char(')');
                }

                self.space();

                if use_arrow {
                    self.write("=> ");
                }

                // Body
                if use_arrow && body.len() == 1 {
                    if let StatementDesc::Return(expr) = &body[0].desc {
                        if !matches!(expr.desc, ExpressionDesc::Undefined { .. }) {
                            // Single expression arrow function
                            let needs_paren = self.expr_needs_paren(expr);
                            if needs_paren {
                                self.write_char('(');
                            }
                            self.print_expression(expr, 0);
                            if needs_paren {
                                self.write_char(')');
                            }
                            return;
                        }
                    }
                }

                self.write_char('{');
                if !body.is_empty() {
                    self.indent();
                    if let Some(dir) = directive {
                        self.newline();
                        self.write_char('"');
                        self.write(dir);
                        self.write("\";");
                    }
                    self.newline();
                    self.print_function_body(body);
                    self.dedent();
                    self.newline();
                }
                self.write_char('}');
            }

            ExpressionDesc::RawJsCode(info) => {
                self.print_raw_js(info);
            }

            ExpressionDesc::Await(e) => {
                let needs_paren = level > 13;
                if needs_paren {
                    self.write_char('(');
                }
                self.write("await ");
                self.print_expression(e, 13);
                if needs_paren {
                    self.write_char(')');
                }
            }

            ExpressionDesc::Spread(e) => {
                let needs_paren = level > 13;
                if needs_paren {
                    self.write_char('(');
                }
                self.write("...");
                self.print_expression(e, 13);
                if needs_paren {
                    self.write_char(')');
                }
            }

            ExpressionDesc::TaggedTemplate(tag, strings, values) => {
                self.print_expression(tag, level);
                self.write_char('`');
                for (i, s) in strings.iter().enumerate() {
                    if let ExpressionDesc::Str { txt, .. } = &s.desc {
                        self.write(txt);
                    }
                    if i < values.len() {
                        self.write("${");
                        self.print_expression(&values[i], 0);
                        self.write_char('}');
                    }
                }
                self.write_char('`');
            }

            ExpressionDesc::OptionalBlock(e, identity) => {
                if *identity {
                    self.print_expression(e, level);
                } else {
                    // Would call runtime "some" function
                    self.write("Caml_option.some(");
                    self.print_expression(e, 1);
                    self.write_char(')');
                }
            }

            ExpressionDesc::CamlBlock(elements, _mutable, _tag, tag_info) => {
                // For now, just output as array (can be refined for records/variants)
                self.write_char('[');
                for (i, elem) in elements.iter().enumerate() {
                    if i > 0 {
                        self.write(", ");
                    }
                    self.print_expression(elem, 1);
                }
                self.write_char(']');
                let _ = tag_info;
            }

            ExpressionDesc::CamlBlockTag(e, tag) => {
                self.print_expression(e, 15);
                self.write_char('.');
                self.write(tag);
            }
        }
    }

    /// Print a variable identifier
    fn print_vident(&mut self, vident: &VIdent) {
        match vident {
            VIdent::Id(id) => {
                let name = self.ident_name(id);
                self.write(&name);
            }
            VIdent::Qualified(module, member) => {
                let mod_name = self.ident_name(&module.id);
                self.write(&mod_name);
                if let Some(m) = member {
                    self.write_char('.');
                    self.write(m);
                }
            }
        }
    }

    /// Print a property name
    fn print_property_name(&mut self, name: &PropertyName) {
        match name {
            PropertyName::Lit(s) => {
                if is_valid_js_ident(s) {
                    self.write(s);
                } else {
                    self.print_string(s);
                }
            }
            PropertyName::Symbol => {
                self.write("[Symbol.for(\"name\")]");
            }
        }
    }

    /// Print property access
    fn print_property_access(&mut self, name: &str) {
        if is_valid_js_ident(name) {
            self.write_char('.');
            self.write(name);
        } else {
            self.write_char('[');
            self.print_string(name);
            self.write_char(']');
        }
    }

    /// Print a number
    fn print_number(&mut self, n: &Number, level: u8) {
        let s = match n {
            Number::Float(f) => f.f.clone(),
            Number::Int { i, c: Some(c) } => {
                format!("/* {} */{}", char::from_u32(*c as u32).unwrap_or('?'), i)
            }
            Number::Int { i, c: None } => format!("{}", i),
            Number::BigInt(b) => {
                format!("{}{}n", if b.positive { "" } else { "-" }, b.value)
            }
        };

        // Negative numbers may need parens
        let needs_paren = (s.starts_with('-') && level > 13)
            || (level == 15 && !s.starts_with('I') && !s.starts_with('N'));

        if needs_paren {
            self.write_char('(');
        }
        self.write(&s);
        if needs_paren {
            self.write_char(')');
        }
    }

    /// Print a string literal
    fn print_string(&mut self, s: &str) {
        self.write_char('"');
        for c in s.chars() {
            match c {
                '"' => self.write("\\\""),
                '\\' => self.write("\\\\"),
                '\n' => self.write("\\n"),
                '\r' => self.write("\\r"),
                '\t' => self.write("\\t"),
                c if c < ' ' => {
                    write!(self.output, "\\x{:02x}", c as u32).unwrap();
                }
                c => self.write_char(c),
            }
        }
        self.write_char('"');
    }

    /// Print a string with specific delimiter
    fn print_string_with_delim(&mut self, s: &str, delim: Delim) {
        match delim {
            Delim::None => self.print_string(s),
            Delim::NoQuotes => self.write(s),
            Delim::BackQuotes => {
                self.write_char('`');
                self.write(s);
                self.write_char('`');
            }
            Delim::StarJ => {
                self.write_char('"');
                self.write(s);
                self.write_char('"');
            }
        }
    }

    /// Print raw JavaScript code
    fn print_raw_js(&mut self, info: &JsRawInfo) {
        if info.is_stmt {
            self.newline();
            self.write(&info.code);
            self.newline();
        } else {
            self.write_char('(');
            self.write(&info.code);
            self.write_char(')');
        }
    }

    /// Print a function body (handles trailing return undefined)
    fn print_function_body(&mut self, stmts: &Block) {
        if stmts.is_empty() {
            return;
        }

        // Handle trailing return undefined
        let len = stmts.len();
        for (i, stmt) in stmts.iter().enumerate() {
            if i > 0 {
                self.newline();
            }

            // Skip trailing "return undefined"
            if i == len - 1 {
                if let StatementDesc::Return(ref e) = stmt.desc {
                    if matches!(e.desc, ExpressionDesc::Undefined { .. }) {
                        continue;
                    }
                }
            }

            self.print_statement(stmt, false);
        }
    }
}

/// Get operator precedence (output_level, left_level, right_level)
fn op_precedence(op: BinOp) -> (u8, u8, u8) {
    match op {
        BinOp::Eq => (1, 14, 1),
        BinOp::Or => (3, 3, 4),
        BinOp::And => (4, 4, 5),
        BinOp::Bor => (6, 6, 7),
        BinOp::Bxor => (7, 7, 8),
        BinOp::Band => (8, 8, 9),
        BinOp::EqEqEq | BinOp::NotEqEq => (9, 9, 10),
        BinOp::Lt | BinOp::Le | BinOp::Gt | BinOp::Ge | BinOp::InstanceOf => (10, 10, 11),
        BinOp::Lsl | BinOp::Lsr | BinOp::Asr => (11, 11, 12),
        BinOp::Plus | BinOp::Minus => (12, 12, 13),
        BinOp::Mul | BinOp::Div | BinOp::Mod => (13, 13, 14),
        BinOp::Pow => (14, 15, 14), // Right-associative
        BinOp::Bnot => (14, 14, 14),
    }
}

/// Check if a string is a valid JavaScript identifier
fn is_valid_js_ident(s: &str) -> bool {
    if s.is_empty() {
        return false;
    }

    let mut chars = s.chars();
    match chars.next() {
        Some(c) if c.is_ascii_alphabetic() || c == '_' || c == '$' => {}
        _ => return false,
    }

    for c in chars {
        if !c.is_ascii_alphanumeric() && c != '_' && c != '$' {
            return false;
        }
    }

    !is_reserved_word(s)
}

/// Check if a string is a JavaScript reserved word
fn is_reserved_word(s: &str) -> bool {
    matches!(
        s,
        "break"
            | "case"
            | "catch"
            | "continue"
            | "debugger"
            | "default"
            | "delete"
            | "do"
            | "else"
            | "finally"
            | "for"
            | "function"
            | "if"
            | "in"
            | "instanceof"
            | "new"
            | "return"
            | "switch"
            | "this"
            | "throw"
            | "try"
            | "typeof"
            | "var"
            | "void"
            | "while"
            | "with"
            | "class"
            | "const"
            | "enum"
            | "export"
            | "extends"
            | "import"
            | "super"
            | "null"
            | "true"
            | "false"
    )
}

/// Print a program to a string
pub fn print_program(program: &Program) -> String {
    let mut printer = JsPrinter::new();
    printer.print_program(program);
    printer.into_output()
}

/// Print an expression to a string
pub fn print_expression(expr: &Expression) -> String {
    let mut printer = JsPrinter::new();
    printer.print_expression(expr, 0);
    printer.into_output()
}

/// Print a block to a string
pub fn print_block(block: &Block) -> String {
    let mut printer = JsPrinter::new();
    printer.print_statements(block, true);
    printer.into_output()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::js_ir::op::MutableFlag;
    use crate::js_ir::{CallInfo, Expression, Statement};

    #[test]
    fn test_print_bool() {
        let expr = Expression::bool(true);
        assert_eq!(print_expression(&expr), "true");

        let expr = Expression::bool(false);
        assert_eq!(print_expression(&expr), "false");
    }

    #[test]
    fn test_print_null_undefined() {
        let expr = Expression::null();
        assert_eq!(print_expression(&expr), "null");

        let expr = Expression::undefined(true);
        assert_eq!(print_expression(&expr), "undefined");
    }

    #[test]
    fn test_print_number() {
        let expr = Expression::number(Number::int(42));
        assert_eq!(print_expression(&expr), "42");

        let expr = Expression::number(Number::float("3.14"));
        assert_eq!(print_expression(&expr), "3.14");
    }

    #[test]
    fn test_print_string() {
        let expr = Expression::string("hello");
        assert_eq!(print_expression(&expr), "\"hello\"");

        let expr = Expression::string("hello\nworld");
        assert_eq!(print_expression(&expr), "\"hello\\nworld\"");
    }

    #[test]
    fn test_print_array() {
        let expr = Expression::array(
            vec![
                Expression::number(Number::int(1)),
                Expression::number(Number::int(2)),
                Expression::number(Number::int(3)),
            ],
            MutableFlag::Immutable,
        );
        assert_eq!(print_expression(&expr), "[1, 2, 3]");
    }

    #[test]
    fn test_print_binary() {
        let left = Expression::number(Number::int(1));
        let right = Expression::number(Number::int(2));
        let expr = Expression::bin(BinOp::Plus, left, right);
        assert_eq!(print_expression(&expr), "1 + 2");
    }

    #[test]
    fn test_print_conditional() {
        let cond = Expression::bool(true);
        let then_ = Expression::number(Number::int(1));
        let else_ = Expression::number(Number::int(2));
        let expr = Expression::cond(cond, then_, else_);
        assert_eq!(print_expression(&expr), "true ? 1 : 2");
    }

    #[test]
    fn test_print_call() {
        let id = Ident::create_local("foo");
        let func = Expression::var(id);
        let args = vec![Expression::number(Number::int(42))];
        let expr = Expression::call(func, args, CallInfo::default());
        assert!(print_expression(&expr).contains("(42)"));
    }

    #[test]
    fn test_print_return() {
        let stmt = Statement::return_(Expression::number(Number::int(42)));
        let block = vec![stmt];
        assert_eq!(print_block(&block), "return 42;");
    }

    #[test]
    fn test_print_if() {
        let stmt = Statement::if_(
            Expression::bool(true),
            vec![Statement::return_(Expression::number(Number::int(1)))],
            vec![Statement::return_(Expression::number(Number::int(2)))],
        );
        let result = print_block(&vec![stmt]);
        assert!(result.contains("if (true)"));
        assert!(result.contains("return 1;"));
        assert!(result.contains("else"));
        assert!(result.contains("return 2;"));
    }

    #[test]
    fn test_print_while() {
        let stmt = Statement::while_(Expression::bool(true), vec![Statement::break_()]);
        let result = print_block(&vec![stmt]);
        assert!(result.contains("while (true)"));
        assert!(result.contains("break;"));
    }
}
