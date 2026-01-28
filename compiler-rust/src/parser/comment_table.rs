//! Comment table for the ReScript printer.
//!
//! This module implements comment attachment to AST nodes. Comments are organized
//! into three categories: leading, inside, and trailing comments for each location.
//!
//! This is a 1:1 port of OCaml's `res_comments_table.ml`.

use crate::location::Location;
use crate::parse_arena::{LocIdx, ParseArena};
use crate::parser::ast::{
    self, ArgLabel, Attribute, Case, ConstructorArguments, ConstructorDeclaration, CoreType,
    CoreTypeDesc, Expression, ExpressionDesc, ExpressionRecordField, Extension,
    ExtensionConstructor, ExtensionConstructorKind, IncludeDeclaration, IncludeDescription,
    JsxClosingTag, JsxContainerElement, JsxElement, JsxFragment, JsxProp, JsxUnaryElement,
    LabelDeclaration, Loc, ModuleBinding, ModuleDeclaration, ModuleExpr, ModuleExprDesc,
    ModuleType, ModuleTypeDeclaration, ModuleTypeDesc, ObjectField, OpenDescription, Pattern,
    PatternDesc, PatternRecordField, Payload, RowField, Signature, SignatureItem,
    SignatureItemDesc, Structure, StructureItem, StructureItemDesc, TypeDeclaration,
    TypeExtension, TypeKind, ValueBinding, ValueDescription, WithConstraint,
};
use crate::parser::ast::Payload::{PPat, PSig, PStr, PTyp};
use crate::parser::comment::Comment;
use crate::parser::longident::Longident;
use std::collections::HashMap;

/// A comment table that maps locations to their associated comments.
#[derive(Debug, Clone, Default)]
pub struct CommentTable {
    /// Comments that appear before a location.
    pub leading: HashMap<Location, Vec<Comment>>,
    /// Comments that appear inside a location (e.g., inside a block).
    pub inside: HashMap<Location, Vec<Comment>>,
    /// Comments that appear after a location.
    pub trailing: HashMap<Location, Vec<Comment>>,
}

impl CommentTable {
    /// Create a new empty comment table.
    pub fn new() -> Self {
        Self {
            leading: HashMap::with_capacity(100),
            inside: HashMap::with_capacity(100),
            trailing: HashMap::with_capacity(100),
        }
    }

    /// Create a copy of the comment table.
    pub fn copy(&self) -> Self {
        Self {
            leading: self.leading.clone(),
            inside: self.inside.clone(),
            trailing: self.trailing.clone(),
        }
    }

    /// Attach comments to a location in the given table.
    fn attach(table: &mut HashMap<Location, Vec<Comment>>, loc: Location, comments: Vec<Comment>) {
        if !comments.is_empty() {
            table.insert(loc, comments);
        }
    }

    /// Check if there are leading comments for a location.
    pub fn has_leading_comments(&self, loc: &Location) -> bool {
        self.leading.contains_key(loc)
    }

    /// Check if there are trailing comments for a location.
    pub fn has_trailing_comments(&self, loc: &Location) -> bool {
        self.trailing.contains_key(loc)
    }

    /// Check if there are inside comments for a location.
    pub fn has_inside_comments(&self, loc: &Location) -> bool {
        self.inside.contains_key(loc)
    }

    /// Get leading comments for a location.
    pub fn get_leading_comments(&self, loc: &Location) -> Option<&Vec<Comment>> {
        self.leading.get(loc)
    }

    /// Get trailing comments for a location.
    pub fn get_trailing_comments(&self, loc: &Location) -> Option<&Vec<Comment>> {
        self.trailing.get(loc)
    }

    /// Get inside comments for a location.
    pub fn get_inside_comments(&self, loc: &Location) -> Option<&Vec<Comment>> {
        self.inside.get(loc)
    }

    /// Remove and return leading comments for a location.
    pub fn remove_leading_comments(&mut self, loc: &Location) -> Vec<Comment> {
        self.leading.remove(loc).unwrap_or_default()
    }

    /// Remove and return trailing comments for a location.
    pub fn remove_trailing_comments(&mut self, loc: &Location) -> Vec<Comment> {
        self.trailing.remove(loc).unwrap_or_default()
    }

    /// Remove and return inside comments for a location.
    pub fn remove_inside_comments(&mut self, loc: &Location) -> Vec<Comment> {
        self.inside.remove(loc).unwrap_or_default()
    }

    /// Get the first leading comment for a location (if any).
    pub fn get_first_leading_comment(&self, loc: &Location) -> Option<&Comment> {
        self.leading.get(loc).and_then(|comments| comments.first())
    }

    /// Walk a structure and attach comments to appropriate AST nodes.
    ///
    /// This is a simplified implementation that attaches comments based on position.
    /// A full implementation would walk the entire AST tree.
    pub fn walk_structure_simple(&mut self, structure: &[StructureItem], comments: Vec<Comment>, arena: &ParseArena) {
        if comments.is_empty() || structure.is_empty() {
            if !comments.is_empty() {
                // Attach remaining comments to "inside" of the file
                Self::attach(&mut self.inside, Location::none(), comments);
            }
            return;
        }

        // Simple implementation: attach comments to the nearest structure item
        let mut remaining_comments = comments;

        for (i, item) in structure.iter().enumerate() {
            let loc = arena.to_location(item.pstr_loc);

            // Find comments that belong to this item
            let (leading, rest): (Vec<Comment>, Vec<Comment>) = remaining_comments
                .into_iter()
                .partition(|c| c.loc().loc_end.cnum <= loc.loc_start.cnum);

            // Find comments that are after this item but before the next
            let next_start = if i + 1 < structure.len() {
                arena.to_location(structure[i + 1].pstr_loc).loc_start.cnum
            } else {
                i32::MAX
            };

            let (trailing, rest): (Vec<_>, Vec<_>) = rest
                .into_iter()
                .partition(|c| {
                    c.loc().loc_start.cnum >= loc.loc_end.cnum
                        && c.loc().loc_end.cnum <= next_start
                        && c.loc().loc_start.line == loc.loc_end.line
                });

            // Attach leading comments
            if !leading.is_empty() {
                Self::attach(&mut self.leading, loc.clone(), leading);
            }

            // Attach trailing comments
            if !trailing.is_empty() {
                Self::attach(&mut self.trailing, loc.clone(), trailing);
            }

            remaining_comments = rest;
        }

        // Any remaining comments go to "inside" at end of file
        if !remaining_comments.is_empty() {
            Self::attach(&mut self.inside, Location::none(), remaining_comments);
        }
    }
}

/// Partitions a list of comments into three groups based on their position relative to a location:
/// - leading: comments that end before the location's start position
/// - inside: comments that overlap with the location
/// - trailing: comments that start after the location's end position
pub fn partition_by_loc(
    comments: Vec<Comment>,
    loc: &Location,
) -> (Vec<Comment>, Vec<Comment>, Vec<Comment>) {
    let mut leading = Vec::new();
    let mut inside = Vec::new();
    let mut trailing = Vec::new();

    for comment in comments {
        let cmt_loc = comment.loc();
        if cmt_loc.loc_end.cnum <= loc.loc_start.cnum {
            leading.push(comment);
        } else if cmt_loc.loc_start.cnum >= loc.loc_end.cnum {
            trailing.push(comment);
        } else {
            inside.push(comment);
        }
    }

    (leading, inside, trailing)
}

/// Splits a list of comments into two groups based on their position relative to a location:
/// - leading: comments that end before the location's start
/// - trailing: comments that start at or after the location's start
pub fn partition_leading_trailing(
    comments: Vec<Comment>,
    loc: &Location,
) -> (Vec<Comment>, Vec<Comment>) {
    let mut leading = Vec::new();
    let mut trailing = Vec::new();

    for comment in comments {
        let cmt_loc = comment.loc();
        if cmt_loc.loc_end.cnum <= loc.loc_start.cnum {
            leading.push(comment);
        } else {
            trailing.push(comment);
        }
    }

    (leading, trailing)
}

/// Splits comments into two groups based on whether they start on the same line as a location's end position.
pub fn partition_by_on_same_line(
    loc: &Location,
    comments: Vec<Comment>,
) -> (Vec<Comment>, Vec<Comment>) {
    let mut on_same_line = Vec::new();
    let mut on_other_line = Vec::new();

    for comment in comments {
        let cmt_loc = comment.loc();
        if cmt_loc.loc_start.line == loc.loc_end.line {
            on_same_line.push(comment);
        } else {
            on_other_line.push(comment);
        }
    }

    (on_same_line, on_other_line)
}

/// Partitions comments that are adjacent to a location (appear right after it without blank lines).
pub fn partition_adjacent_trailing(
    loc: &Location,
    comments: Vec<Comment>,
) -> (Vec<Comment>, Vec<Comment>) {
    let mut after_loc = Vec::new();
    let mut remaining = Vec::new();
    let mut prev_end_pos = loc.loc_end.cnum;
    let mut found_gap = false;

    for comment in comments {
        if found_gap {
            remaining.push(comment);
        } else {
            let cmt_prev_end_pos = comment.prev_tok_end_pos().cnum;
            if prev_end_pos == cmt_prev_end_pos {
                prev_end_pos = comment.loc().loc_end.cnum;
                after_loc.push(comment);
            } else {
                found_gap = true;
                remaining.push(comment);
            }
        }
    }

    (after_loc, remaining)
}

/// Splits comments that follow a location but come before another token on the same line.
pub fn partition_adjacent_trailing_before_next_token_on_same_line(
    loc: &Location,
    next_token: &Location,
    comments: Vec<Comment>,
) -> (Vec<Comment>, Vec<Comment>) {
    let mut after_loc = Vec::new();
    let mut rest = Vec::new();
    let mut done = false;

    for comment in comments {
        if done {
            rest.push(comment);
            continue;
        }

        let cmt_loc = comment.loc();
        // Check if the comment is on the same line as the loc, and is entirely before the next_token
        if cmt_loc.loc_start.line == loc.loc_end.line
            && cmt_loc.loc_start.cnum > loc.loc_end.cnum
            && cmt_loc.loc_end.cnum <= next_token.loc_start.cnum
        {
            after_loc.push(comment);
        } else {
            done = true;
            rest.push(comment);
        }
    }

    (after_loc, rest)
}

/// Extracts comments that appear between two specified line numbers.
pub fn partition_between_lines(
    start_line: i32,
    end_line: i32,
    comments: Vec<Comment>,
) -> (Vec<Comment>, Vec<Comment>) {
    let mut between_comments = Vec::new();
    let mut rest = Vec::new();
    let mut done = false;

    for comment in comments {
        if done {
            rest.push(comment);
            continue;
        }

        let cmt_loc = comment.loc();
        if cmt_loc.loc_start.line > start_line && cmt_loc.loc_end.line < end_line {
            between_comments.push(comment);
        } else {
            done = true;
            rest.push(comment);
        }
    }

    (between_comments, rest)
}

/// AST node types for the walker.
#[derive(Debug)]
pub enum Node<'a> {
    Case(&'a Case),
    CoreType(&'a CoreType),
    ExprArgument { expr: &'a Expression, loc: Location },
    Expression(&'a Expression),
    ExprRecordRow(&'a Loc<Longident>, &'a Expression),
    ExtensionConstructor(&'a ExtensionConstructor),
    LabelDeclaration(&'a LabelDeclaration),
    ModuleBinding(&'a ModuleBinding),
    ModuleDeclaration(&'a ModuleDeclaration),
    ModuleExpr(&'a ModuleExpr),
    ObjectField(&'a ObjectField),
    PackageConstraint(&'a Loc<Longident>, &'a CoreType),
    Pattern(&'a Pattern),
    PatternRecordRow(&'a Loc<Longident>, &'a Pattern),
    RowField(&'a RowField),
    SignatureItem(&'a SignatureItem),
    StructureItem(&'a StructureItem),
    TypeDeclaration(&'a TypeDeclaration),
    ValueBinding(&'a ValueBinding),
    JsxProp(&'a JsxProp),
}

impl<'a> Node<'a> {
    /// Get the location of a node.
    pub fn get_loc(&self, arena: &ParseArena) -> Location {
        match self {
            Node::Case(case) => {
                // The case location extends from pattern to the end of the expression
                let start = arena.loc_start(case.pc_lhs.ppat_loc);
                let end = arena.loc_end(case.pc_rhs.pexp_loc);
                Location::from_positions(start.clone(), end.clone())
            }
            Node::CoreType(ct) => arena.to_location(ct.ptyp_loc),
            Node::ExprArgument { loc, .. } => arena.to_location(*loc),
            Node::Expression(e) => {
                // Check for braces attribute
                for attr in &e.pexp_attributes {
                    if attr.0.txt == "res.braces" || attr.0.txt == "ns.braces" {
                        return arena.to_location(attr.0.loc);
                    }
                }
                arena.to_location(e.pexp_loc)
            }
            Node::ExprRecordRow(li, e) => {
                let start = arena.loc_start(li.loc);
                let end = arena.loc_end(e.pexp_loc);
                Location::from_positions(start.clone(), end.clone())
            }
            Node::ExtensionConstructor(ec) => arena.to_location(ec.pext_loc),
            Node::LabelDeclaration(ld) => arena.to_location(ld.pld_loc),
            Node::ModuleBinding(mb) => arena.to_location(mb.pmb_loc),
            Node::ModuleDeclaration(md) => arena.to_location(md.pmd_loc),
            Node::ModuleExpr(me) => arena.to_location(me.pmod_loc),
            Node::ObjectField(field) => match field {
                ObjectField::Otag(lbl, _attrs, typ) => {
                    let start = arena.loc_start(lbl.loc);
                    let end = arena.loc_end(typ.ptyp_loc);
                    Location::from_positions(start.clone(), end.clone())
                }
                ObjectField::Oinherit(typ) => arena.to_location(typ.ptyp_loc),
            },
            Node::PackageConstraint(li, te) => {
                let start = arena.loc_start(li.loc);
                let end = arena.loc_end(te.ptyp_loc);
                Location::from_positions(start.clone(), end.clone())
            }
            Node::Pattern(p) => arena.to_location(p.ppat_loc),
            Node::PatternRecordRow(li, p) => {
                let start = arena.loc_start(li.loc);
                let end = arena.loc_end(p.ppat_loc);
                Location::from_positions(start.clone(), end.clone())
            }
            Node::RowField(rf) => match rf {
                RowField::Rtag(tag, _attrs, _flag, _args) => arena.to_location(tag.loc),
                RowField::Rinherit(typ) => arena.to_location(typ.ptyp_loc),
            },
            Node::SignatureItem(si) => arena.to_location(si.psig_loc),
            Node::StructureItem(si) => arena.to_location(si.pstr_loc),
            Node::TypeDeclaration(td) => arena.to_location(td.ptype_loc),
            Node::ValueBinding(vb) => arena.to_location(vb.pvb_loc),
            Node::JsxProp(prop) => get_jsx_prop_loc(prop, arena),
        }
    }
}

/// Get the location of a JSX prop.
fn get_jsx_prop_loc(prop: &JsxProp, arena: &ParseArena) -> Location {
    match prop {
        JsxProp::Punning { name, .. } => arena.to_location(name.loc),
        JsxProp::Value { name, value, .. } => {
            let start = arena.loc_start(name.loc);
            let end = arena.loc_end(value.pexp_loc);
            Location::from_positions(start.clone(), end.clone())
        }
        JsxProp::Spreading { loc, .. } => arena.to_location(*loc),
    }
}

/// Check if an expression is a block expression.
fn is_block_expr(expr: &Expression) -> bool {
    match &expr.pexp_desc {
        ExpressionDesc::Pexp_letmodule(..)
        | ExpressionDesc::Pexp_letexception(..)
        | ExpressionDesc::Pexp_let(..)
        | ExpressionDesc::Pexp_open(..)
        | ExpressionDesc::Pexp_sequence(..) => true,
        ExpressionDesc::Pexp_apply { funct, .. } if is_block_expr(funct) => true,
        ExpressionDesc::Pexp_constraint(inner, _) if is_block_expr(inner) => true,
        ExpressionDesc::Pexp_field(inner, _) if is_block_expr(inner) => true,
        ExpressionDesc::Pexp_setfield(inner, _, _) if is_block_expr(inner) => true,
        _ => false,
    }
}

/// Check if an expression is an if-then-else.
fn is_if_then_else_expr(expr: &Expression) -> bool {
    matches!(&expr.pexp_desc, ExpressionDesc::Pexp_ifthenelse(..))
}

/// Collect list patterns from a cons pattern.
fn collect_list_patterns<'a>(pattern: &'a Pattern) -> Vec<&'a Pattern> {
    let mut acc = Vec::new();
    let mut current = pattern;

    loop {
        match &current.ppat_desc {
            PatternDesc::Ppat_construct(name, Some(args))
                if name.txt == Longident::Lident("::".to_string()) =>
            {
                if let PatternDesc::Ppat_tuple(tuple) = &args.ppat_desc {
                    if tuple.len() == 2 {
                        acc.push(&tuple[0]);
                        current = &tuple[1];
                        continue;
                    }
                }
                acc.push(current);
                break;
            }
            PatternDesc::Ppat_construct(name, None)
                if name.txt == Longident::Lident("[]".to_string()) =>
            {
                break;
            }
            _ => {
                acc.push(current);
                break;
            }
        }
    }

    acc
}

/// Collect list expressions from a cons expression.
fn collect_list_exprs<'a>(expr: &'a Expression) -> Vec<&'a Expression> {
    let mut acc = Vec::new();
    let mut current = expr;

    loop {
        match &current.pexp_desc {
            ExpressionDesc::Pexp_construct(name, Some(args))
                if name.txt == Longident::Lident("::".to_string()) =>
            {
                if let ExpressionDesc::Pexp_tuple(tuple) = &args.pexp_desc {
                    if tuple.len() == 2 {
                        acc.push(&tuple[0]);
                        current = &tuple[1];
                        continue;
                    }
                }
                acc.push(current);
                break;
            }
            ExpressionDesc::Pexp_construct(name, _)
                if name.txt == Longident::Lident("[]".to_string()) =>
            {
                break;
            }
            _ => {
                acc.push(current);
                break;
            }
        }
    }

    acc
}

// ============================================================================
// Walk functions for populating the comment table
// ============================================================================

/// Walk a structure and attach comments to AST nodes.
pub fn walk_structure(structure: &Structure, t: &mut CommentTable, comments: Vec<Comment>, arena: &ParseArena) {
    if comments.is_empty() {
        return;
    }
    if structure.is_empty() {
        CommentTable::attach(&mut t.inside, Location::none(), comments);
        return;
    }

    let nodes: Vec<Node<'_>> = structure.iter().map(|si| Node::StructureItem(si)).collect();
    walk_list(&nodes, t, comments, arena);
}

/// Walk a signature and attach comments to AST nodes.
pub fn walk_signature(signature: &Signature, t: &mut CommentTable, comments: Vec<Comment>, arena: &ParseArena) {
    if comments.is_empty() {
        return;
    }
    if signature.is_empty() {
        CommentTable::attach(&mut t.inside, Location::none(), comments);
        return;
    }

    let nodes: Vec<Node<'_>> = signature.iter().map(|si| Node::SignatureItem(si)).collect();
    walk_list(&nodes, t, comments, arena);
}

/// Walk a list of nodes.
fn walk_list(nodes: &[Node<'_>], t: &mut CommentTable, comments: Vec<Comment>, arena: &ParseArena) {
    walk_list_impl(nodes, t, comments, None, arena);
}

fn walk_list_impl(nodes: &[Node<'_>], t: &mut CommentTable, comments: Vec<Comment>, prev_loc: Option<Location>, arena: &ParseArena) {
    if comments.is_empty() {
        return;
    }
    if nodes.is_empty() {
        if let Some(loc) = prev_loc {
            CommentTable::attach(&mut t.trailing, loc, comments);
        }
        return;
    }

    let node = &nodes[0];
    let rest_nodes = &nodes[1..];
    let curr_loc = node.get_loc(arena);

    let (leading, inside, trailing) = partition_by_loc(comments, &curr_loc);

    match &prev_loc {
        None => {
            // First node, all leading comments attach here
            CommentTable::attach(&mut t.leading, curr_loc.clone(), leading);
        }
        Some(prev_loc) => {
            // Check if on same line
            if prev_loc.loc_end.line == curr_loc.loc_start.line {
                let (after_prev, before_curr) = partition_adjacent_trailing(prev_loc, leading);
                CommentTable::attach(&mut t.trailing, prev_loc.clone(), after_prev);
                CommentTable::attach(&mut t.leading, curr_loc.clone(), before_curr);
            } else {
                let (on_same_line_as_prev, after_prev) = partition_by_on_same_line(prev_loc, leading);
                CommentTable::attach(&mut t.trailing, prev_loc.clone(), on_same_line_as_prev);
                let (before_curr, _, _) = partition_by_loc(after_prev, &curr_loc);
                CommentTable::attach(&mut t.leading, curr_loc.clone(), before_curr);
            }
        }
    }

    walk_node(node, t, inside, arena);
    walk_list_impl(rest_nodes, t, trailing, Some(curr_loc), arena);
}

/// Walk a node and its children.
fn walk_node(node: &Node<'_>, t: &mut CommentTable, comments: Vec<Comment>, arena: &ParseArena) {
    match node {
        Node::Case(c) => walk_case(c, t, comments, arena),
        Node::CoreType(ct) => walk_core_type(ct, t, comments, arena),
        Node::ExprArgument { expr, loc } => walk_expr_argument(expr, loc, t, comments, arena),
        Node::Expression(e) => walk_expression(e, t, comments, arena),
        Node::ExprRecordRow(li, e) => walk_expr_record_row(li, e, t, comments, arena),
        Node::ExtensionConstructor(ec) => walk_extension_constructor(ec, t, comments, arena),
        Node::LabelDeclaration(ld) => walk_label_declaration(ld, t, comments, arena),
        Node::ModuleBinding(mb) => walk_module_binding(mb, t, comments, arena),
        Node::ModuleDeclaration(md) => walk_module_declaration(md, t, comments, arena),
        Node::ModuleExpr(me) => walk_module_expr(me, t, comments, arena),
        Node::ObjectField(f) => walk_object_field(f, t, comments, arena),
        Node::PackageConstraint(li, te) => walk_package_constraint(li, te, t, comments, arena),
        Node::Pattern(p) => walk_pattern(p, t, comments, arena),
        Node::PatternRecordRow(li, p) => walk_pattern_record_row(li, p, t, comments, arena),
        Node::RowField(rf) => walk_row_field(rf, t, comments, arena),
        Node::SignatureItem(si) => walk_signature_item(si, t, comments, arena),
        Node::StructureItem(si) => walk_structure_item(si, t, comments, arena),
        Node::TypeDeclaration(td) => walk_type_declaration(td, t, comments, arena),
        Node::ValueBinding(vb) => walk_value_binding(vb, t, comments, arena),
        Node::JsxProp(prop) => walk_jsx_prop(prop, t, comments, arena),
    }
}

fn walk_structure_item(si: &StructureItem, t: &mut CommentTable, comments: Vec<Comment>, arena: &ParseArena) {
    if comments.is_empty() {
        return;
    }

    match &si.pstr_desc {
        StructureItemDesc::Pstr_primitive(vd) => walk_value_description(vd, t, comments, arena),
        StructureItemDesc::Pstr_open(od) => walk_open_description(od, t, comments, arena),
        StructureItemDesc::Pstr_value(_, bindings) => walk_value_bindings(bindings, t, comments, arena),
        StructureItemDesc::Pstr_type(_, declarations) => {
            walk_type_declarations(declarations, t, comments, arena)
        }
        StructureItemDesc::Pstr_eval(expr, _) => walk_expression(expr, t, comments, arena),
        StructureItemDesc::Pstr_module(mb) => walk_module_binding(mb, t, comments, arena),
        StructureItemDesc::Pstr_recmodule(mbs) => {
            let nodes: Vec<Node<'_>> = mbs.iter().map(|mb| Node::ModuleBinding(mb)).collect();
            walk_list(&nodes, t, comments, arena);
        }
        StructureItemDesc::Pstr_modtype(mtd) => walk_module_type_declaration(mtd, t, comments, arena),
        StructureItemDesc::Pstr_attribute(attr) => walk_attribute(attr, t, comments, arena),
        StructureItemDesc::Pstr_extension(ext, _) => walk_extension(ext, t, comments, arena),
        StructureItemDesc::Pstr_include(id) => walk_include_declaration(id, t, comments, arena),
        StructureItemDesc::Pstr_exception(ec) => walk_extension_constructor(ec, t, comments, arena),
        StructureItemDesc::Pstr_typext(te) => walk_type_extension(te, t, comments, arena),
    }
}

fn walk_signature_item(si: &SignatureItem, t: &mut CommentTable, comments: Vec<Comment>, arena: &ParseArena) {
    if comments.is_empty() {
        return;
    }

    match &si.psig_desc {
        SignatureItemDesc::Psig_value(vd) => walk_value_description(vd, t, comments, arena),
        SignatureItemDesc::Psig_type(_, declarations) => {
            walk_type_declarations(declarations, t, comments, arena)
        }
        SignatureItemDesc::Psig_typext(te) => walk_type_extension(te, t, comments, arena),
        SignatureItemDesc::Psig_exception(ec) => walk_extension_constructor(ec, t, comments, arena),
        SignatureItemDesc::Psig_module(md) => walk_module_declaration(md, t, comments, arena),
        SignatureItemDesc::Psig_recmodule(mds) => {
            let nodes: Vec<Node<'_>> = mds.iter().map(|md| Node::ModuleDeclaration(md)).collect();
            walk_list(&nodes, t, comments, arena);
        }
        SignatureItemDesc::Psig_modtype(mtd) => walk_module_type_declaration(mtd, t, comments, arena),
        SignatureItemDesc::Psig_open(od) => walk_open_description(od, t, comments, arena),
        SignatureItemDesc::Psig_include(id) => walk_include_description(id, t, comments, arena),
        SignatureItemDesc::Psig_attribute(attr) => walk_attribute(attr, t, comments, arena),
        SignatureItemDesc::Psig_extension(ext, _) => walk_extension(ext, t, comments, arena),
    }
}

fn walk_value_description(vd: &ValueDescription, t: &mut CommentTable, comments: Vec<Comment>, arena: &ParseArena) {
    let (leading, trailing) = partition_leading_trailing(comments, &arena.to_location(vd.pval_name.loc));
    CommentTable::attach(&mut t.leading, arena.to_location(vd.pval_name.loc), leading);

    let (after_name, rest) = partition_adjacent_trailing(&arena.to_location(vd.pval_name.loc), trailing);
    CommentTable::attach(&mut t.trailing, arena.to_location(vd.pval_name.loc), after_name);

    let (before, inside, after) = partition_by_loc(rest, &vd.pval_type.ptyp_loc);
    CommentTable::attach(&mut t.leading, vd.pval_type.ptyp_loc.clone(), before);
    walk_core_type(&vd.pval_type, t, inside, arena);
    CommentTable::attach(&mut t.trailing, vd.pval_type.ptyp_loc.clone(), after);
}

fn walk_type_extension(te: &TypeExtension, t: &mut CommentTable, comments: Vec<Comment>, arena: &ParseArena) {
    let (leading, trailing) = partition_leading_trailing(comments, &arena.to_location(te.ptyext_path.loc));
    CommentTable::attach(&mut t.leading, arena.to_location(te.ptyext_path.loc), leading);

    let (after_path, rest) = partition_adjacent_trailing(&arena.to_location(te.ptyext_path.loc), trailing);
    CommentTable::attach(&mut t.trailing, arena.to_location(te.ptyext_path.loc), after_path);

    // Type params
    let rest = if te.ptyext_params.is_empty() {
        rest
    } else {
        visit_list_but_continue_with_remaining_comments(
            &te.ptyext_params,
            |param, _arena| param.0.ptyp_loc,
            |param, t, comments| walk_core_type(&param.0, t, comments, arena),
            t,
            rest,
            false,
            arena,
        )
    };

    let nodes: Vec<Node<'_>> = te
        .ptyext_constructors
        .iter()
        .map(|ec| Node::ExtensionConstructor(ec))
        .collect();
    walk_list(&nodes, t, rest, arena);
}

fn walk_include_declaration(id: &IncludeDeclaration, t: &mut CommentTable, comments: Vec<Comment>, arena: &ParseArena) {
    let (before, inside, after) = partition_by_loc(comments, &arena.to_location(id.pincl_mod.pmod_loc));
    CommentTable::attach(&mut t.leading, arena.to_location(id.pincl_mod.pmod_loc), before);
    walk_module_expr(&id.pincl_mod, t, inside, arena);
    CommentTable::attach(&mut t.trailing, arena.to_location(id.pincl_mod.pmod_loc), after);
}

fn walk_include_description(id: &IncludeDescription, t: &mut CommentTable, comments: Vec<Comment>, arena: &ParseArena) {
    let (before, inside, after) = partition_by_loc(comments, &id.pincl_mod.pmty_loc);
    CommentTable::attach(&mut t.leading, id.pincl_mod.pmty_loc.clone(), before);
    walk_mod_type(&id.pincl_mod, t, inside, arena);
    CommentTable::attach(&mut t.trailing, id.pincl_mod.pmty_loc.clone(), after);
}

fn walk_module_type_declaration(
    mtd: &ModuleTypeDeclaration,
    t: &mut CommentTable,
    comments: Vec<Comment>,
    arena: &ParseArena,
) {
    let (leading, trailing) = partition_leading_trailing(comments, &mtd.pmtd_name.loc);
    CommentTable::attach(&mut t.leading, mtd.pmtd_name.loc.clone(), leading);

    match &mtd.pmtd_type {
        None => {
            CommentTable::attach(&mut t.trailing, mtd.pmtd_name.loc.clone(), trailing);
        }
        Some(mod_type) => {
            let (after_name, rest) = partition_adjacent_trailing(&mtd.pmtd_name.loc, trailing);
            CommentTable::attach(&mut t.trailing, mtd.pmtd_name.loc.clone(), after_name);

            let (before, inside, after) = partition_by_loc(rest, &mod_type.pmty_loc);
            CommentTable::attach(&mut t.leading, mod_type.pmty_loc.clone(), before);
            walk_mod_type(mod_type, t, inside, arena);
            CommentTable::attach(&mut t.trailing, mod_type.pmty_loc.clone(), after);
        }
    }
}

fn walk_module_binding(mb: &ModuleBinding, t: &mut CommentTable, comments: Vec<Comment>, arena: &ParseArena) {
    let (leading, trailing) = partition_leading_trailing(comments, &mb.pmb_name.loc);
    CommentTable::attach(&mut t.leading, mb.pmb_name.loc.clone(), leading);

    let (after_name, rest) = partition_adjacent_trailing(&mb.pmb_name.loc, trailing);
    CommentTable::attach(&mut t.trailing, mb.pmb_name.loc.clone(), after_name);

    let (leading, inside, trailing) = partition_by_loc(rest, &mb.pmb_expr.pmod_loc);

    match &mb.pmb_expr.pmod_desc {
        ModuleExprDesc::Pmod_constraint { .. } => {
            let combined: Vec<Comment> = leading.into_iter().chain(inside.into_iter()).collect();
            walk_module_expr(&mb.pmb_expr, t, combined, arena);
        }
        _ => {
            CommentTable::attach(&mut t.leading, arena.to_location(mb.pmb_expr.pmod_loc), leading);
            walk_module_expr(&mb.pmb_expr, t, inside, arena);
        }
    }
    CommentTable::attach(&mut t.trailing, arena.to_location(mb.pmb_expr.pmod_loc), trailing);
}

fn walk_module_declaration(md: &ModuleDeclaration, t: &mut CommentTable, comments: Vec<Comment>, arena: &ParseArena) {
    let (leading, trailing) = partition_leading_trailing(comments, &md.pmd_name.loc);
    CommentTable::attach(&mut t.leading, md.pmd_name.loc.clone(), leading);

    let (after_name, rest) = partition_adjacent_trailing(&md.pmd_name.loc, trailing);
    CommentTable::attach(&mut t.trailing, md.pmd_name.loc.clone(), after_name);

    let (leading, inside, trailing) = partition_by_loc(rest, &md.pmd_type.pmty_loc);
    CommentTable::attach(&mut t.leading, md.pmd_type.pmty_loc.clone(), leading);
    walk_mod_type(&md.pmd_type, t, inside, arena);
    CommentTable::attach(&mut t.trailing, md.pmd_type.pmty_loc.clone(), trailing);
}

fn walk_open_description(od: &OpenDescription, t: &mut CommentTable, comments: Vec<Comment>, arena: &ParseArena) {
    let loc = &od.popen_lid.loc;
    let (leading, trailing) = partition_leading_trailing(comments, loc);
    CommentTable::attach(&mut t.leading, loc.clone(), leading);
    CommentTable::attach(&mut t.trailing, loc.clone(), trailing);
}

fn walk_value_bindings(vbs: &[ValueBinding], t: &mut CommentTable, comments: Vec<Comment>, arena: &ParseArena) {
    let nodes: Vec<Node<'_>> = vbs.iter().map(|vb| Node::ValueBinding(vb)).collect();
    walk_list(&nodes, t, comments, arena);
}

fn walk_type_declarations(tds: &[TypeDeclaration], t: &mut CommentTable, comments: Vec<Comment>, arena: &ParseArena) {
    let nodes: Vec<Node<'_>> = tds.iter().map(|td| Node::TypeDeclaration(td)).collect();
    walk_list(&nodes, t, comments, arena);
}

fn walk_value_binding(vb: &ValueBinding, t: &mut CommentTable, comments: Vec<Comment>, arena: &ParseArena) {
    let pattern_loc = &vb.pvb_pat.ppat_loc;
    let expr_loc = &vb.pvb_expr.pexp_loc;

    let (leading, inside, trailing) = partition_by_loc(comments, pattern_loc);

    // Everything before start of pattern can only be leading on the pattern
    CommentTable::attach(&mut t.leading, pattern_loc.clone(), leading);
    walk_pattern(&vb.pvb_pat, t, inside, arena);

    let (after_pat, surrounding_expr) = partition_adjacent_trailing(pattern_loc, trailing);
    CommentTable::attach(&mut t.trailing, pattern_loc.clone(), after_pat);

    let (before_expr, inside_expr, after_expr) = partition_by_loc(surrounding_expr, expr_loc);

    if is_block_expr(&vb.pvb_expr) {
        let combined: Vec<Comment> = before_expr
            .into_iter()
            .chain(inside_expr.into_iter())
            .chain(after_expr.into_iter())
            .collect();
        walk_expression(&vb.pvb_expr, t, combined);
    } else {
        CommentTable::attach(&mut t.leading, expr_loc.clone(), before_expr);
        walk_expression(&vb.pvb_expr, t, inside_expr);
        CommentTable::attach(&mut t.trailing, expr_loc.clone(), after_expr);
    }
}

fn walk_type_declaration(td: &TypeDeclaration, t: &mut CommentTable, comments: Vec<Comment>, arena: &ParseArena) {
    let (before_name, rest) = partition_leading_trailing(comments, &td.ptype_name.loc);
    CommentTable::attach(&mut t.leading, td.ptype_name.loc.clone(), before_name);

    let (after_name, rest) = partition_adjacent_trailing(&td.ptype_name.loc, rest);
    CommentTable::attach(&mut t.trailing, td.ptype_name.loc.clone(), after_name);

    // Type params
    let rest = if td.ptype_params.is_empty() {
        rest
    } else {
        visit_list_but_continue_with_remaining_comments(
            &td.ptype_params,
            |param, _arena| param.0.ptyp_loc,
            |param, t, comments| walk_core_type(&param.0, t, comments, arena),
            t,
            rest,
            false,
            arena,
        )
    };

    // Manifest: = typexpr
    let rest = match &td.ptype_manifest {
        Some(typexpr) => {
            let (before_typ, inside_typ, after_typ) = partition_by_loc(rest, &arena.to_location(typexpr.ptyp_loc));
            CommentTable::attach(&mut t.leading, arena.to_location(typexpr.ptyp_loc), before_typ);
            walk_core_type(typexpr, t, inside_typ, arena);
            let (after_typ_adj, rest) = partition_adjacent_trailing(&arena.to_location(typexpr.ptyp_loc), after_typ);
            CommentTable::attach(&mut t.trailing, arena.to_location(typexpr.ptyp_loc), after_typ_adj);
            rest
        }
        None => rest,
    };

    let rest = match &td.ptype_kind {
        TypeKind::Ptype_abstract | TypeKind::Ptype_open => rest,
        TypeKind::Ptype_record(lds) => {
            if lds.is_empty() {
                CommentTable::attach(&mut t.inside, arena.to_location(td.ptype_loc), rest);
                Vec::new()
            } else {
                let nodes: Vec<Node<'_>> = lds.iter().map(|ld| Node::LabelDeclaration(ld)).collect();
                walk_list(&nodes, t, rest, arena);
                Vec::new()
            }
        }
        TypeKind::Ptype_variant(cds) => walk_constructor_declarations(cds, t, rest, arena),
    };

    CommentTable::attach(&mut t.trailing, arena.to_location(td.ptype_loc), rest);
}

fn walk_label_declaration(ld: &LabelDeclaration, t: &mut CommentTable, comments: Vec<Comment>, arena: &ParseArena) {
    let (before_name, rest) = partition_leading_trailing(comments, &ld.pld_name.loc);
    CommentTable::attach(&mut t.leading, ld.pld_name.loc.clone(), before_name);

    let (after_name, rest) = partition_adjacent_trailing(&ld.pld_name.loc, rest);
    CommentTable::attach(&mut t.trailing, ld.pld_name.loc.clone(), after_name);

    let (before_typ, inside_typ, after_typ) = partition_by_loc(rest, &ld.pld_type.ptyp_loc);
    CommentTable::attach(&mut t.leading, ld.pld_type.ptyp_loc.clone(), before_typ);
    walk_core_type(&ld.pld_type, t, inside_typ, arena);
    CommentTable::attach(&mut t.trailing, ld.pld_type.ptyp_loc.clone(), after_typ);
}

fn walk_constructor_declarations(
    cds: &[ConstructorDeclaration],
    t: &mut CommentTable,
    comments: Vec<Comment>,
    arena: &ParseArena,
) -> Vec<Comment> {
    visit_list_but_continue_with_remaining_comments(
        cds,
        |cd, _arena| cd.pcd_loc,
        |cd, t, comments| walk_constructor_declaration(cd, t, comments, arena),
        t,
        comments,
        false,
        arena,
    )
}

fn walk_constructor_declaration(
    cd: &ConstructorDeclaration,
    t: &mut CommentTable,
    comments: Vec<Comment>,
    arena: &ParseArena,
) {
    let (before_name, rest) = partition_leading_trailing(comments, &cd.pcd_name.loc);
    CommentTable::attach(&mut t.leading, cd.pcd_name.loc.clone(), before_name);

    let (after_name, rest) = partition_adjacent_trailing(&cd.pcd_name.loc, rest);
    CommentTable::attach(&mut t.trailing, cd.pcd_name.loc.clone(), after_name);

    let rest = walk_constructor_arguments(&cd.pcd_args, t, rest, arena);

    let rest = match &cd.pcd_res {
        Some(typexpr) => {
            let (before_typ, inside_typ, after_typ) = partition_by_loc(rest, &arena.to_location(typexpr.ptyp_loc));
            CommentTable::attach(&mut t.leading, arena.to_location(typexpr.ptyp_loc), before_typ);
            walk_core_type(typexpr, t, inside_typ, arena);
            let (after_typ_adj, rest) = partition_adjacent_trailing(&arena.to_location(typexpr.ptyp_loc), after_typ);
            CommentTable::attach(&mut t.trailing, arena.to_location(typexpr.ptyp_loc), after_typ_adj);
            rest
        }
        None => rest,
    };

    CommentTable::attach(&mut t.trailing, arena.to_location(cd.pcd_loc), rest);
}

fn walk_constructor_arguments(
    args: &ConstructorArguments,
    t: &mut CommentTable,
    comments: Vec<Comment>,
    arena: &ParseArena,
) -> Vec<Comment> {
    match args {
        ConstructorArguments::Pcstr_tuple(typexprs) => {
            visit_list_but_continue_with_remaining_comments(
                typexprs,
                |te, _arena| te.ptyp_loc,
                |te, t, comments| walk_core_type(te, t, comments, arena),
                t,
                comments,
                false,
                arena,
            )
        }
        ConstructorArguments::Pcstr_record(lds) => {
            visit_list_but_continue_with_remaining_comments(
                lds,
                |ld, _arena| ld.pld_loc,
                |ld, t, comments| walk_label_declaration(ld, t, comments, arena),
                t,
                comments,
                false,
                arena,
            )
        }
    }
}

fn walk_extension_constructor(
    ec: &ExtensionConstructor,
    t: &mut CommentTable,
    comments: Vec<Comment>,
    arena: &ParseArena,
) {
    let (leading, trailing) = partition_leading_trailing(comments, &ec.pext_name.loc);
    CommentTable::attach(&mut t.leading, ec.pext_name.loc.clone(), leading);

    let (after_name, rest) = partition_adjacent_trailing(&ec.pext_name.loc, trailing);
    CommentTable::attach(&mut t.trailing, ec.pext_name.loc.clone(), after_name);

    match &ec.pext_kind {
        ExtensionConstructorKind::Pext_decl(args, res) => {
            let rest = walk_constructor_arguments(args, t, rest, arena);
            if let Some(typexpr) = res {
                let (before_typ, inside_typ, after_typ) =
                    partition_by_loc(rest, &arena.to_location(typexpr.ptyp_loc));
                CommentTable::attach(&mut t.leading, arena.to_location(typexpr.ptyp_loc), before_typ);
                walk_core_type(typexpr, t, inside_typ, arena);
                CommentTable::attach(&mut t.trailing, arena.to_location(typexpr.ptyp_loc), after_typ);
            }
        }
        ExtensionConstructorKind::Pext_rebind(lid) => {
            let (leading, trailing) = partition_leading_trailing(rest, &arena.to_location(lid.loc));
            CommentTable::attach(&mut t.leading, arena.to_location(lid.loc), leading);
            CommentTable::attach(&mut t.trailing, arena.to_location(lid.loc), trailing);
        }
    }
}

fn walk_expression(expr: &Expression, t: &mut CommentTable, comments: Vec<Comment>, arena: &ParseArena) {
    if comments.is_empty() {
        return;
    }

    match &expr.pexp_desc {
        ExpressionDesc::Pexp_constant { .. } => {
            let (leading, trailing) = partition_leading_trailing(comments, &arena.to_location(expr.pexp_loc));
            CommentTable::attach(&mut t.leading, arena.to_location(expr.pexp_loc), leading);
            CommentTable::attach(&mut t.trailing, arena.to_location(expr.pexp_loc), trailing);
        }
        ExpressionDesc::Pexp_ident(longident) => {
            let (leading, trailing) = partition_leading_trailing(comments, &arena.to_location(longident.loc));
            CommentTable::attach(&mut t.leading, arena.to_location(longident.loc), leading);
            CommentTable::attach(&mut t.trailing, arena.to_location(longident.loc), trailing);
        }
        ExpressionDesc::Pexp_let(_, bindings, body) => {
            // Check if body is unit
            if matches!(&body.pexp_desc, ExpressionDesc::Pexp_construct(name, None) if name.txt == Longident::Lident("()".to_string()))
            {
                walk_value_bindings(bindings, t, comments, arena);
            } else {
                let comments = visit_list_but_continue_with_remaining_comments(
                    bindings,
                    |vb, arena| {
                        if arena.loc_ghost(vb.pvb_pat.ppat_loc) {
                            vb.pvb_expr.pexp_loc
                        } else {
                            vb.pvb_loc
                        }
                    },
                    |vb, t, comments| walk_value_binding(vb, t, comments, arena),
                    t,
                    comments,
                    true,
                    arena,
                );

                if is_block_expr(body) {
                    walk_expression(body, t, comments, arena);
                } else {
                    let (leading, inside, trailing) = partition_by_loc(comments, &arena.to_location(body.pexp_loc));
                    CommentTable::attach(&mut t.leading, arena.to_location(body.pexp_loc), leading);
                    walk_expression(body, t, inside, arena);
                    CommentTable::attach(&mut t.trailing, arena.to_location(body.pexp_loc), trailing);
                }
            }
        }
        ExpressionDesc::Pexp_sequence(lhs, rhs) => {
            let (leading, inside, trailing) = partition_by_loc(comments, &arena.to_location(lhs.pexp_loc));

            let comments = if is_block_expr(lhs) {
                let (after_expr, comments) = partition_by_on_same_line(&arena.to_location(lhs.pexp_loc), trailing);
                let combined: Vec<Comment> = leading
                    .into_iter()
                    .chain(inside.into_iter())
                    .chain(after_expr.into_iter())
                    .collect();
                walk_expression(lhs, t, combined);
                comments
            } else {
                CommentTable::attach(&mut t.leading, arena.to_location(lhs.pexp_loc), leading);
                walk_expression(lhs, t, inside, arena);
                let (after_expr, comments) = partition_by_on_same_line(&arena.to_location(lhs.pexp_loc), trailing);
                CommentTable::attach(&mut t.trailing, arena.to_location(lhs.pexp_loc), after_expr);
                comments
            };

            if is_block_expr(rhs) {
                walk_expression(rhs, t, comments, arena);
            } else {
                let (leading, inside, trailing) = partition_by_loc(comments, &arena.to_location(rhs.pexp_loc));
                CommentTable::attach(&mut t.leading, arena.to_location(rhs.pexp_loc), leading);
                walk_expression(rhs, t, inside, arena);
                CommentTable::attach(&mut t.trailing, arena.to_location(rhs.pexp_loc), trailing);
            }
        }
        ExpressionDesc::Pexp_open(_, lid, body) => {
            let (leading, comments) = partition_leading_trailing(comments, &arena.to_location(expr.pexp_loc));
            let combined_loc = Location::from_positions(
                arena.loc_start(expr.pexp_loc).clone(),
                arena.loc_end(lid.loc).clone(),
            );
            CommentTable::attach(&mut t.leading, combined_loc, leading);

            let (leading, trailing) = partition_leading_trailing(comments, &arena.to_location(lid.loc));
            CommentTable::attach(&mut t.leading, arena.to_location(lid.loc), leading);

            let (after_longident, rest) = partition_by_on_same_line(&lid.loc, trailing);
            CommentTable::attach(&mut t.trailing, arena.to_location(lid.loc), after_longident);

            if is_block_expr(body) {
                walk_expression(body, t, rest, arena);
            } else {
                let (leading, inside, trailing) = partition_by_loc(rest, &arena.to_location(body.pexp_loc));
                CommentTable::attach(&mut t.leading, arena.to_location(body.pexp_loc), leading);
                walk_expression(body, t, inside, arena);
                CommentTable::attach(&mut t.trailing, arena.to_location(body.pexp_loc), trailing);
            }
        }
        ExpressionDesc::Pexp_extension(ext) => {
            // Check for %obj extension
            if ext.0.txt == "obj" {
                if let PStr(items) = &ext.1 {
                    if items.len() == 1 {
                        if let StructureItemDesc::Pstr_eval(inner, _) = &items[0].pstr_desc
                        {
                            if let ExpressionDesc::Pexp_record(fields, _) = &inner.pexp_desc {
                                let nodes: Vec<Node<'_>> = fields
                                    .iter()
                                    .map(|f| Node::ExprRecordRow(&f.lid, &f.expr))
                                    .collect();
                                walk_list(&nodes, t, comments, arena);
                                return;
                            }
                        }
                    }
                }
            }
            walk_extension(ext, t, comments, arena);
        }
        ExpressionDesc::Pexp_letexception(ext, body) => {
            let (leading, comments) = partition_leading_trailing(comments, &arena.to_location(expr.pexp_loc));
            let combined_loc = Location::from_positions(
                arena.loc_start(expr.pexp_loc).clone(),
                arena.loc_end(ext.pext_loc).clone(),
            );
            CommentTable::attach(&mut t.leading, combined_loc, leading);

            let (leading, inside, trailing) = partition_by_loc(comments, &arena.to_location(ext.pext_loc));
            CommentTable::attach(&mut t.leading, arena.to_location(ext.pext_loc), leading);
            walk_extension_constructor(ext, t, inside, arena);

            let (after_ext_constr, rest) = partition_by_on_same_line(&arena.to_location(ext.pext_loc), trailing);
            CommentTable::attach(&mut t.trailing, arena.to_location(ext.pext_loc), after_ext_constr);

            if is_block_expr(body) {
                walk_expression(body, t, rest, arena);
            } else {
                let (leading, inside, trailing) = partition_by_loc(rest, &arena.to_location(body.pexp_loc));
                CommentTable::attach(&mut t.leading, arena.to_location(body.pexp_loc), leading);
                walk_expression(body, t, inside, arena);
                CommentTable::attach(&mut t.trailing, arena.to_location(body.pexp_loc), trailing);
            }
        }
        ExpressionDesc::Pexp_letmodule(name, module, body) => {
            let (leading, comments) = partition_leading_trailing(comments, &arena.to_location(expr.pexp_loc));
            let combined_loc = Location::from_positions(
                arena.loc_start(expr.pexp_loc).clone(),
                arena.loc_end(module.pmod_loc).clone(),
            );
            CommentTable::attach(&mut t.leading, combined_loc, leading);

            let (leading, trailing) = partition_leading_trailing(comments, &arena.to_location(name.loc));
            CommentTable::attach(&mut t.leading, arena.to_location(name.loc), leading);

            let (after_string, rest) = partition_adjacent_trailing(&arena.to_location(name.loc), trailing);
            CommentTable::attach(&mut t.trailing, arena.to_location(name.loc), after_string);

            let (before_mod_expr, inside_mod_expr, after_mod_expr) =
                partition_by_loc(rest, &arena.to_location(module.pmod_loc));
            CommentTable::attach(&mut t.leading, arena.to_location(module.pmod_loc), before_mod_expr);
            walk_module_expr(module, t, inside_mod_expr);

            let (after_mod_expr_adj, rest) = partition_by_on_same_line(&arena.to_location(module.pmod_loc), after_mod_expr);
            CommentTable::attach(&mut t.trailing, arena.to_location(module.pmod_loc), after_mod_expr_adj);

            if is_block_expr(body) {
                walk_expression(body, t, rest, arena);
            } else {
                let (leading, inside, trailing) = partition_by_loc(rest, &arena.to_location(body.pexp_loc));
                CommentTable::attach(&mut t.leading, arena.to_location(body.pexp_loc), leading);
                walk_expression(body, t, inside, arena);
                CommentTable::attach(&mut t.trailing, arena.to_location(body.pexp_loc), trailing);
            }
        }
        ExpressionDesc::Pexp_assert(inner) => {
            if is_block_expr(inner) {
                walk_expression(inner, t, comments, arena);
            } else {
                let (leading, inside, trailing) = partition_by_loc(comments, &arena.to_location(inner.pexp_loc));
                CommentTable::attach(&mut t.leading, arena.to_location(inner.pexp_loc), leading);
                walk_expression(inner, t, inside, arena);
                CommentTable::attach(&mut t.trailing, arena.to_location(inner.pexp_loc), trailing);
            }
        }
        ExpressionDesc::Pexp_coerce(inner, _, typ) => {
            let (leading, inside, trailing) = partition_by_loc(comments, &arena.to_location(inner.pexp_loc));
            CommentTable::attach(&mut t.leading, arena.to_location(inner.pexp_loc), leading);
            walk_expression(inner, t, inside, arena);

            let (after_expr, rest) = partition_adjacent_trailing(&arena.to_location(inner.pexp_loc), trailing);
            CommentTable::attach(&mut t.trailing, arena.to_location(inner.pexp_loc), after_expr);

            let (leading, inside, trailing) = partition_by_loc(rest, &arena.to_location(typ.ptyp_loc));
            CommentTable::attach(&mut t.leading, arena.to_location(typ.ptyp_loc), leading);
            walk_core_type(typ, t, inside, arena);
            CommentTable::attach(&mut t.trailing, arena.to_location(typ.ptyp_loc), trailing);
        }
        ExpressionDesc::Pexp_constraint(inner, typ) => {
            let (leading, inside, trailing) = partition_by_loc(comments, &arena.to_location(inner.pexp_loc));
            CommentTable::attach(&mut t.leading, arena.to_location(inner.pexp_loc), leading);
            walk_expression(inner, t, inside, arena);

            let (after_expr, rest) = partition_adjacent_trailing(&arena.to_location(inner.pexp_loc), trailing);
            CommentTable::attach(&mut t.trailing, arena.to_location(inner.pexp_loc), after_expr);

            let (leading, inside, trailing) = partition_by_loc(rest, &arena.to_location(typ.ptyp_loc));
            CommentTable::attach(&mut t.leading, arena.to_location(typ.ptyp_loc), leading);
            walk_core_type(typ, t, inside, arena);
            CommentTable::attach(&mut t.trailing, arena.to_location(typ.ptyp_loc), trailing);
        }
        ExpressionDesc::Pexp_tuple(exprs) if exprs.is_empty() => {
            CommentTable::attach(&mut t.inside, arena.to_location(expr.pexp_loc), comments);
        }
        ExpressionDesc::Pexp_array(exprs) if exprs.is_empty() => {
            CommentTable::attach(&mut t.inside, arena.to_location(expr.pexp_loc), comments);
        }
        ExpressionDesc::Pexp_construct(name, None)
            if name.txt == Longident::Lident("[]".to_string()) =>
        {
            CommentTable::attach(&mut t.inside, arena.to_location(expr.pexp_loc), comments);
        }
        ExpressionDesc::Pexp_construct(name, Some(_))
            if name.txt == Longident::Lident("::".to_string()) =>
        {
            let list_exprs = collect_list_exprs(expr);
            let nodes: Vec<Node<'_>> = list_exprs.iter().map(|e| Node::Expression(e)).collect();
            walk_list(&nodes, t, comments, arena);
        }
        ExpressionDesc::Pexp_construct(name, args) => {
            let (leading, trailing) = partition_leading_trailing(comments, &arena.to_location(name.loc));
            CommentTable::attach(&mut t.leading, arena.to_location(name.loc), leading);

            match args {
                Some(arg_expr) => {
                    let (after_longident, rest) = partition_adjacent_trailing(&arena.to_location(name.loc), trailing);
                    CommentTable::attach(&mut t.trailing, arena.to_location(name.loc), after_longident);
                    walk_expression(arg_expr, t, rest, arena);
                }
                None => {
                    CommentTable::attach(&mut t.trailing, arena.to_location(name.loc), trailing);
                }
            }
        }
        ExpressionDesc::Pexp_variant(_, None) => {}
        ExpressionDesc::Pexp_variant(_, Some(arg)) => {
            walk_expression(arg, t, comments, arena);
        }
        ExpressionDesc::Pexp_array(exprs) | ExpressionDesc::Pexp_tuple(exprs) => {
            let nodes: Vec<Node<'_>> = exprs.iter().map(|e| Node::Expression(e)).collect();
            walk_list(&nodes, t, comments, arena);
        }
        ExpressionDesc::Pexp_record(fields, spread) => {
            if fields.is_empty() {
                CommentTable::attach(&mut t.inside, arena.to_location(expr.pexp_loc), comments);
            } else {
                let comments = match spread {
                    None => comments,
                    Some(spread_expr) => {
                        let (leading, inside, trailing) =
                            partition_by_loc(comments, &arena.to_location(spread_expr.pexp_loc));
                        CommentTable::attach(&mut t.leading, arena.to_location(spread_expr.pexp_loc), leading);
                        walk_expression(spread_expr, t, inside, arena);
                        let (after_expr, rest) =
                            partition_adjacent_trailing(&arena.to_location(spread_expr.pexp_loc), trailing);
                        CommentTable::attach(&mut t.trailing, arena.to_location(spread_expr.pexp_loc), after_expr);
                        rest
                    }
                };

                let nodes: Vec<Node<'_>> = fields
                    .iter()
                    .map(|f| Node::ExprRecordRow(&f.lid, &f.expr))
                    .collect();
                walk_list(&nodes, t, comments, arena);
            }
        }
        ExpressionDesc::Pexp_field(inner, lid) => {
            let (leading, inside, trailing) = partition_by_loc(comments, &arena.to_location(inner.pexp_loc));

            let trailing = if is_block_expr(inner) {
                let (after_expr, rest) = partition_adjacent_trailing(&arena.to_location(inner.pexp_loc), trailing);
                let combined: Vec<Comment> = leading
                    .into_iter()
                    .chain(inside.into_iter())
                    .chain(after_expr.into_iter())
                    .collect();
                walk_expression(inner, t, combined);
                rest
            } else {
                CommentTable::attach(&mut t.leading, arena.to_location(inner.pexp_loc), leading);
                walk_expression(inner, t, inside, arena);
                trailing
            };

            let (after_expr, rest) = partition_adjacent_trailing(&arena.to_location(inner.pexp_loc), trailing);
            CommentTable::attach(&mut t.trailing, arena.to_location(inner.pexp_loc), after_expr);

            let (leading, trailing) = partition_leading_trailing(rest, &arena.to_location(lid.loc));
            CommentTable::attach(&mut t.leading, arena.to_location(lid.loc), leading);
            CommentTable::attach(&mut t.trailing, arena.to_location(lid.loc), trailing);
        }
        ExpressionDesc::Pexp_setfield(obj, lid, value) => {
            let (leading, inside, trailing) = partition_by_loc(comments, &arena.to_location(obj.pexp_loc));

            let rest = if is_block_expr(obj) {
                let (after_expr, rest) = partition_adjacent_trailing(&arena.to_location(obj.pexp_loc), trailing);
                let combined: Vec<Comment> = leading
                    .into_iter()
                    .chain(inside.into_iter())
                    .chain(after_expr.into_iter())
                    .collect();
                walk_expression(obj, t, combined);
                rest
            } else {
                let (after_expr, rest) = partition_adjacent_trailing(&arena.to_location(obj.pexp_loc), trailing);
                CommentTable::attach(&mut t.leading, arena.to_location(obj.pexp_loc), leading);
                walk_expression(obj, t, inside, arena);
                CommentTable::attach(&mut t.trailing, arena.to_location(obj.pexp_loc), after_expr);
                rest
            };

            let (before_longident, after_longident) = partition_leading_trailing(rest, &lid.loc);
            CommentTable::attach(&mut t.leading, arena.to_location(lid.loc), before_longident);

            let (after_longident_adj, rest) = partition_adjacent_trailing(&arena.to_location(lid.loc), after_longident);
            CommentTable::attach(&mut t.trailing, arena.to_location(lid.loc), after_longident_adj);

            if is_block_expr(value) {
                walk_expression(value, t, rest, arena);
            } else {
                let (leading, inside, trailing) = partition_by_loc(rest, &arena.to_location(value.pexp_loc));
                CommentTable::attach(&mut t.leading, arena.to_location(value.pexp_loc), leading);
                walk_expression(value, t, inside, arena);
                CommentTable::attach(&mut t.trailing, arena.to_location(value.pexp_loc), trailing);
            }
        }
        ExpressionDesc::Pexp_ifthenelse(cond, then_branch, else_branch) => {
            let (leading, rest) = partition_leading_trailing(comments, &arena.to_location(expr.pexp_loc));
            CommentTable::attach(&mut t.leading, arena.to_location(expr.pexp_loc), leading);

            let (leading, inside, trailing) = partition_by_loc(rest, &arena.to_location(cond.pexp_loc));

            let comments = if is_block_expr(cond) {
                let (after_expr, comments) = partition_adjacent_trailing(&arena.to_location(cond.pexp_loc), trailing);
                let combined: Vec<Comment> = leading
                    .into_iter()
                    .chain(inside.into_iter())
                    .chain(after_expr.into_iter())
                    .collect();
                walk_expression(cond, t, combined);
                comments
            } else {
                CommentTable::attach(&mut t.leading, arena.to_location(cond.pexp_loc), leading);
                walk_expression(cond, t, inside, arena);
                let (after_expr, comments) = partition_adjacent_trailing(&arena.to_location(cond.pexp_loc), trailing);
                CommentTable::attach(&mut t.trailing, arena.to_location(cond.pexp_loc), after_expr);
                comments
            };

            let (leading, inside, trailing) = partition_by_loc(comments, &arena.to_location(then_branch.pexp_loc));

            let comments = if is_block_expr(then_branch) {
                let (after_expr, trailing) =
                    partition_adjacent_trailing(&arena.to_location(then_branch.pexp_loc), trailing);
                let combined: Vec<Comment> = leading
                    .into_iter()
                    .chain(inside.into_iter())
                    .chain(after_expr.into_iter())
                    .collect();
                walk_expression(then_branch, t, combined);
                trailing
            } else {
                CommentTable::attach(&mut t.leading, arena.to_location(then_branch.pexp_loc), leading);
                walk_expression(then_branch, t, inside, arena);
                let (after_expr, comments) =
                    partition_adjacent_trailing(&arena.to_location(then_branch.pexp_loc), trailing);
                CommentTable::attach(&mut t.trailing, arena.to_location(then_branch.pexp_loc), after_expr);
                comments
            };

            if let Some(else_expr) = else_branch {
                if is_block_expr(else_expr) || is_if_then_else_expr(else_expr) {
                    walk_expression(else_expr, t, comments, arena);
                } else {
                    let (leading, inside, trailing) =
                        partition_by_loc(comments, &arena.to_location(else_expr.pexp_loc));
                    CommentTable::attach(&mut t.leading, arena.to_location(else_expr.pexp_loc), leading);
                    walk_expression(else_expr, t, inside, arena);
                    CommentTable::attach(&mut t.trailing, arena.to_location(else_expr.pexp_loc), trailing);
                }
            }
        }
        ExpressionDesc::Pexp_while(cond, body) => {
            let (leading, inside, trailing) = partition_by_loc(comments, &arena.to_location(cond.pexp_loc));

            let rest = if is_block_expr(cond) {
                let (after_expr, rest) = partition_adjacent_trailing(&arena.to_location(cond.pexp_loc), trailing);
                let combined: Vec<Comment> = leading
                    .into_iter()
                    .chain(inside.into_iter())
                    .chain(after_expr.into_iter())
                    .collect();
                walk_expression(cond, t, combined);
                rest
            } else {
                CommentTable::attach(&mut t.leading, arena.to_location(cond.pexp_loc), leading);
                walk_expression(cond, t, inside, arena);
                let (after_expr, rest) = partition_adjacent_trailing(&arena.to_location(cond.pexp_loc), trailing);
                CommentTable::attach(&mut t.trailing, arena.to_location(cond.pexp_loc), after_expr);
                rest
            };

            if is_block_expr(body) {
                walk_expression(body, t, rest, arena);
            } else {
                let (leading, inside, trailing) = partition_by_loc(rest, &arena.to_location(body.pexp_loc));
                CommentTable::attach(&mut t.leading, arena.to_location(body.pexp_loc), leading);
                walk_expression(body, t, inside, arena);
                CommentTable::attach(&mut t.trailing, arena.to_location(body.pexp_loc), trailing);
            }
        }
        ExpressionDesc::Pexp_for(pat, from, to, _, body) => {
            let (leading, inside, trailing) = partition_by_loc(comments, &arena.to_location(pat.ppat_loc));
            CommentTable::attach(&mut t.leading, arena.to_location(pat.ppat_loc), leading);
            walk_pattern(pat, t, inside, arena);

            let (after_pat, rest) = partition_adjacent_trailing(&arena.to_location(pat.ppat_loc), trailing);
            CommentTable::attach(&mut t.trailing, arena.to_location(pat.ppat_loc), after_pat);

            let (leading, inside, trailing) = partition_by_loc(rest, &arena.to_location(from.pexp_loc));
            CommentTable::attach(&mut t.leading, arena.to_location(from.pexp_loc), leading);
            walk_expression(from, t, inside, arena);

            let (after_expr, rest) = partition_adjacent_trailing(&arena.to_location(from.pexp_loc), trailing);
            CommentTable::attach(&mut t.trailing, arena.to_location(from.pexp_loc), after_expr);

            let (leading, inside, trailing) = partition_by_loc(rest, &arena.to_location(to.pexp_loc));
            CommentTable::attach(&mut t.leading, arena.to_location(to.pexp_loc), leading);
            walk_expression(to, t, inside, arena);

            let (after_expr, rest) = partition_adjacent_trailing(&arena.to_location(to.pexp_loc), trailing);
            CommentTable::attach(&mut t.trailing, arena.to_location(to.pexp_loc), after_expr);

            if is_block_expr(body) {
                walk_expression(body, t, rest, arena);
            } else {
                let (leading, inside, trailing) = partition_by_loc(rest, &arena.to_location(body.pexp_loc));
                CommentTable::attach(&mut t.leading, arena.to_location(body.pexp_loc), leading);
                walk_expression(body, t, inside, arena);
                CommentTable::attach(&mut t.trailing, arena.to_location(body.pexp_loc), trailing);
            }
        }
        ExpressionDesc::Pexp_pack(mod_expr) => {
            let (before, inside, after) = partition_by_loc(comments, &arena.to_location(mod_expr.pmod_loc));
            CommentTable::attach(&mut t.leading, arena.to_location(mod_expr.pmod_loc), before);
            walk_module_expr(mod_expr, t, inside, arena);
            CommentTable::attach(&mut t.trailing, arena.to_location(mod_expr.pmod_loc), after);
        }
        ExpressionDesc::Pexp_match(match_expr, cases)
        | ExpressionDesc::Pexp_try(match_expr, cases) => {
            let (before, inside, after) = partition_by_loc(comments, &arena.to_location(match_expr.pexp_loc));

            let after = if is_block_expr(match_expr) {
                let (after_expr, rest) =
                    partition_adjacent_trailing(&arena.to_location(match_expr.pexp_loc), after);
                let combined: Vec<Comment> = before
                    .into_iter()
                    .chain(inside.into_iter())
                    .chain(after_expr.into_iter())
                    .collect();
                walk_expression(match_expr, t, combined);
                rest
            } else {
                CommentTable::attach(&mut t.leading, arena.to_location(match_expr.pexp_loc), before);
                walk_expression(match_expr, t, inside, arena);
                after
            };

            let (after_expr, rest) = partition_adjacent_trailing(&arena.to_location(match_expr.pexp_loc), after);
            CommentTable::attach(&mut t.trailing, arena.to_location(match_expr.pexp_loc), after_expr);

            let nodes: Vec<Node<'_>> = cases.iter().map(|c| Node::Case(c)).collect();
            walk_list(&nodes, t, rest, arena);
        }
        ExpressionDesc::Pexp_apply { funct, args, .. } => {
            // Check for unary expressions
            if args.len() == 1 {
                if let ExpressionDesc::Pexp_ident(ident) = &funct.pexp_desc {
                    if let Longident::Lident(op) = &ident.txt {
                        if matches!(
                            op.as_str(),
                            "~+" | "~+." | "~-" | "~-." | "~~~" | "not" | "!"
                        ) {
                            let (_, arg_expr) = &args[0];
                            let (before, inside, after) =
                                partition_by_loc(comments, &arena.to_location(arg_expr.pexp_loc));
                            CommentTable::attach(&mut t.leading, arena.to_location(arg_expr.pexp_loc), before);
                            walk_expression(arg_expr, t, inside, arena);
                            CommentTable::attach(&mut t.trailing, arena.to_location(arg_expr.pexp_loc), after);
                            return;
                        }
                    }
                }
            }

            // Regular apply
            walk_apply_expr(funct, args, t, comments);
        }
        ExpressionDesc::Pexp_fun { rhs: body, .. } | ExpressionDesc::Pexp_newtype(_, body) => {
            // Walk through the function parameters and body
            walk_expression(body, t, comments, arena);
        }
        ExpressionDesc::Pexp_send(inner, _) | ExpressionDesc::Pexp_await(inner) => {
            walk_expression(inner, t, comments, arena);
        }
        ExpressionDesc::Pexp_jsx_element(jsx) => {
            walk_jsx_element(jsx, t, comments, arena);
        }
    }
}

fn walk_jsx_element(jsx: &JsxElement, t: &mut CommentTable, comments: Vec<Comment>, arena: &ParseArena) {
    if comments.is_empty() {
        return;
    }

    match jsx {
        JsxElement::Fragment(fragment) => {
            let nodes: Vec<Node<'_>> = fragment.children.iter().map(|e| Node::Expression(e)).collect();
            walk_list(&nodes, t, comments, arena);
        }
        JsxElement::Unary(unary) => {
            // Walk the props
            for prop in &unary.props {
                walk_jsx_prop(prop, t, vec![]);
            }
            // Remaining comments attach to the element
            let loc = &unary.tag_name.loc;
            CommentTable::attach(&mut t.trailing, loc.clone(), comments);
        }
        JsxElement::Container(container) => {
            // Walk props and children
            for prop in &container.props {
                walk_jsx_prop(prop, t, vec![]);
            }
            let nodes: Vec<Node<'_>> = container.children.iter().map(|e| Node::Expression(e)).collect();
            walk_list(&nodes, t, comments, arena);
        }
    }
}

fn walk_jsx_prop(prop: &JsxProp, t: &mut CommentTable, comments: Vec<Comment>, arena: &ParseArena) {
    match prop {
        JsxProp::Punning { name, .. } => {
            let (leading, trailing) = partition_leading_trailing(comments, &arena.to_location(name.loc));
            CommentTable::attach(&mut t.leading, arena.to_location(name.loc), leading);
            CommentTable::attach(&mut t.trailing, arena.to_location(name.loc), trailing);
        }
        JsxProp::Value { name, value, .. } => {
            let (leading, trailing) = partition_leading_trailing(comments, &arena.to_location(name.loc));
            CommentTable::attach(&mut t.leading, arena.to_location(name.loc), leading);

            let (after_lbl, rest) = partition_adjacent_trailing(&arena.to_location(name.loc), trailing);
            CommentTable::attach(&mut t.trailing, arena.to_location(name.loc), after_lbl);

            let (before, inside, after) = partition_by_loc(rest, &arena.to_location(value.pexp_loc));
            CommentTable::attach(&mut t.leading, arena.to_location(value.pexp_loc), before);
            walk_expression(value, t, inside, arena);
            CommentTable::attach(&mut t.trailing, arena.to_location(value.pexp_loc), after);
        }
        JsxProp::Spreading { expr, .. } => {
            walk_expression(expr, t, comments, arena);
        }
    }
}

fn walk_apply_expr(
    call_expr: &Expression,
    arguments: &[(ArgLabel, Expression)],
    t: &mut CommentTable,
    comments: Vec<Comment>,
    arena: &ParseArena,
) {
    let (before, inside, after) = partition_by_loc(comments, &arena.to_location(call_expr.pexp_loc));

    let after = if is_block_expr(call_expr) {
        let (after_expr, rest) = partition_adjacent_trailing(&arena.to_location(call_expr.pexp_loc), after);
        let combined: Vec<Comment> = before
            .into_iter()
            .chain(inside.into_iter())
            .chain(after_expr.into_iter())
            .collect();
        walk_expression(call_expr, t, combined, arena);
        rest
    } else {
        CommentTable::attach(&mut t.leading, arena.to_location(call_expr.pexp_loc), before);
        walk_expression(call_expr, t, inside, arena);
        after
    };

    let (after_expr, rest) = partition_adjacent_trailing(&arena.to_location(call_expr.pexp_loc), after);
    CommentTable::attach(&mut t.trailing, arena.to_location(call_expr.pexp_loc), after_expr);

    let nodes: Vec<Node<'_>> = arguments
        .iter()
        .map(|(lbl, expr)| {
            let loc = match lbl {
                ArgLabel::Labelled(lbl_loc) | ArgLabel::Optional(lbl_loc) => {
                    let start = arena.loc_start(lbl_loc.loc);
                    let end = arena.loc_end(expr.pexp_loc);
                    arena.from_location(&Location::from_positions(start.clone(), end.clone()))
                }
                ArgLabel::Nolabel => expr.pexp_loc,
            };
            Node::ExprArgument { expr, loc }
        })
        .collect();

    walk_list(&nodes, t, rest, arena);
}

fn walk_expr_argument(
    expr: &Expression,
    _loc: &LocIdx,
    t: &mut CommentTable,
    comments: Vec<Comment>,
    arena: &ParseArena,
) {
    walk_expression(expr, t, comments, arena);
}

fn walk_expr_record_row(
    li: &Loc<Longident>,
    expr: &Expression,
    t: &mut CommentTable,
    comments: Vec<Comment>,
    arena: &ParseArena,
) {
    let li_loc = arena.to_location(li.loc);
    let (leading, trailing) = partition_leading_trailing(comments, &li_loc);
    CommentTable::attach(&mut t.leading, li_loc.clone(), leading);

    let (after_li, rest) = partition_adjacent_trailing(&li_loc, trailing);
    CommentTable::attach(&mut t.trailing, li_loc, after_li);

    let (before, inside, after) = partition_by_loc(rest, &arena.to_location(expr.pexp_loc));
    CommentTable::attach(&mut t.leading, arena.to_location(expr.pexp_loc), before);
    walk_expression(expr, t, inside, arena);
    CommentTable::attach(&mut t.trailing, arena.to_location(expr.pexp_loc), after);
}

fn walk_case(case: &Case, t: &mut CommentTable, comments: Vec<Comment>, arena: &ParseArena) {
    let (before, inside, after) = partition_by_loc(comments, &arena.to_location(case.pc_lhs.ppat_loc));
    CommentTable::attach(&mut t.leading, arena.to_location(case.pc_lhs.ppat_loc), before);
    walk_pattern(&case.pc_lhs, t, inside, arena);

    let (after_pat, rest) = partition_adjacent_trailing(&arena.to_location(case.pc_lhs.ppat_loc), after);
    CommentTable::attach(&mut t.trailing, arena.to_location(case.pc_lhs.ppat_loc), after_pat);

    // Handle guard
    let rest = match &case.pc_guard {
        Some(guard) => {
            let (before, inside, after) = partition_by_loc(rest, &arena.to_location(guard.pexp_loc));
            CommentTable::attach(&mut t.leading, arena.to_location(guard.pexp_loc), before);
            walk_expression(guard, t, inside, arena);
            let (after_guard, rest) = partition_adjacent_trailing(&arena.to_location(guard.pexp_loc), after);
            CommentTable::attach(&mut t.trailing, arena.to_location(guard.pexp_loc), after_guard);
            rest
        }
        None => rest,
    };

    if is_block_expr(&case.pc_rhs) {
        walk_expression(&case.pc_rhs, t, rest, arena);
    } else {
        let (before, inside, after) = partition_by_loc(rest, &arena.to_location(case.pc_rhs.pexp_loc));
        CommentTable::attach(&mut t.leading, arena.to_location(case.pc_rhs.pexp_loc), before);
        walk_expression(&case.pc_rhs, t, inside, arena);
        CommentTable::attach(&mut t.trailing, arena.to_location(case.pc_rhs.pexp_loc), after);
    }
}

fn walk_pattern(pattern: &Pattern, t: &mut CommentTable, comments: Vec<Comment>, arena: &ParseArena) {
    if comments.is_empty() {
        return;
    }

    match &pattern.ppat_desc {
        PatternDesc::Ppat_any | PatternDesc::Ppat_constant { .. } | PatternDesc::Ppat_interval { .. } => {
            let (leading, trailing) = partition_leading_trailing(comments, &arena.to_location(pattern.ppat_loc));
            CommentTable::attach(&mut t.leading, arena.to_location(pattern.ppat_loc), leading);
            CommentTable::attach(&mut t.trailing, arena.to_location(pattern.ppat_loc), trailing);
        }
        PatternDesc::Ppat_var(name) => {
            let (leading, trailing) = partition_leading_trailing(comments, &arena.to_location(name.loc));
            CommentTable::attach(&mut t.leading, arena.to_location(name.loc), leading);
            CommentTable::attach(&mut t.trailing, arena.to_location(name.loc), trailing);
        }
        PatternDesc::Ppat_alias(pat, name) => {
            let (before, inside, after) = partition_by_loc(comments, &arena.to_location(pat.ppat_loc));
            CommentTable::attach(&mut t.leading, arena.to_location(pat.ppat_loc), before);
            walk_pattern(pat, t, inside, arena);

            let (after_pat, rest) = partition_adjacent_trailing(&arena.to_location(pat.ppat_loc), after);
            CommentTable::attach(&mut t.trailing, arena.to_location(pat.ppat_loc), after_pat);

            let (leading, trailing) = partition_leading_trailing(rest, &name.loc);
            CommentTable::attach(&mut t.leading, arena.to_location(name.loc), leading);
            CommentTable::attach(&mut t.trailing, arena.to_location(name.loc), trailing);
        }
        PatternDesc::Ppat_tuple(pats) if pats.is_empty() => {
            CommentTable::attach(&mut t.inside, arena.to_location(pattern.ppat_loc), comments);
        }
        PatternDesc::Ppat_array(pats) if pats.is_empty() => {
            CommentTable::attach(&mut t.inside, arena.to_location(pattern.ppat_loc), comments);
        }
        PatternDesc::Ppat_construct(name, None)
            if name.txt == Longident::Lident("[]".to_string()) =>
        {
            CommentTable::attach(&mut t.inside, arena.to_location(pattern.ppat_loc), comments);
        }
        PatternDesc::Ppat_construct(name, Some(_))
            if name.txt == Longident::Lident("::".to_string()) =>
        {
            let list_pats = collect_list_patterns(pattern);
            let nodes: Vec<Node<'_>> = list_pats.iter().map(|p| Node::Pattern(p)).collect();
            walk_list(&nodes, t, comments, arena);
        }
        PatternDesc::Ppat_construct(name, args) => {
            let (leading, trailing) = partition_leading_trailing(comments, &arena.to_location(name.loc));
            CommentTable::attach(&mut t.leading, arena.to_location(name.loc), leading);

            match args {
                Some(arg_pat) => {
                    let (after_longident, rest) = partition_adjacent_trailing(&arena.to_location(name.loc), trailing);
                    CommentTable::attach(&mut t.trailing, arena.to_location(name.loc), after_longident);
                    walk_pattern(arg_pat, t, rest, arena);
                }
                None => {
                    CommentTable::attach(&mut t.trailing, arena.to_location(name.loc), trailing);
                }
            }
        }
        PatternDesc::Ppat_variant(_, None) => {}
        PatternDesc::Ppat_variant(_, Some(arg)) => {
            walk_pattern(arg, t, comments, arena);
        }
        PatternDesc::Ppat_tuple(pats) | PatternDesc::Ppat_array(pats) => {
            let nodes: Vec<Node<'_>> = pats.iter().map(|p| Node::Pattern(p)).collect();
            walk_list(&nodes, t, comments, arena);
        }
        PatternDesc::Ppat_record(fields, _) => {
            let nodes: Vec<Node<'_>> = fields
                .iter()
                .map(|f| Node::PatternRecordRow(&f.lid, &f.pat))
                .collect();
            walk_list(&nodes, t, comments, arena);
        }
        PatternDesc::Ppat_or(lhs, rhs) => {
            let (before, inside, after) = partition_by_loc(comments, &arena.to_location(lhs.ppat_loc));
            CommentTable::attach(&mut t.leading, arena.to_location(lhs.ppat_loc), before);
            walk_pattern(lhs, t, inside, arena);

            let (after_lhs, rest) = partition_adjacent_trailing(&arena.to_location(lhs.ppat_loc), after);
            CommentTable::attach(&mut t.trailing, arena.to_location(lhs.ppat_loc), after_lhs);

            let (before, inside, after) = partition_by_loc(rest, &arena.to_location(rhs.ppat_loc));
            CommentTable::attach(&mut t.leading, arena.to_location(rhs.ppat_loc), before);
            walk_pattern(rhs, t, inside, arena);
            CommentTable::attach(&mut t.trailing, arena.to_location(rhs.ppat_loc), after);
        }
        PatternDesc::Ppat_constraint(pat, typ) => {
            let (before, inside, after) = partition_by_loc(comments, &arena.to_location(pat.ppat_loc));
            CommentTable::attach(&mut t.leading, arena.to_location(pat.ppat_loc), before);
            walk_pattern(pat, t, inside, arena);

            let (after_pat, rest) = partition_adjacent_trailing(&arena.to_location(pat.ppat_loc), after);
            CommentTable::attach(&mut t.trailing, arena.to_location(pat.ppat_loc), after_pat);

            let (before, inside, after) = partition_by_loc(rest, &arena.to_location(typ.ptyp_loc));
            CommentTable::attach(&mut t.leading, arena.to_location(typ.ptyp_loc), before);
            walk_core_type(typ, t, inside, arena);
            CommentTable::attach(&mut t.trailing, arena.to_location(typ.ptyp_loc), after);
        }
        PatternDesc::Ppat_type(lid) => {
            let (leading, trailing) = partition_leading_trailing(comments, &arena.to_location(lid.loc));
            CommentTable::attach(&mut t.leading, arena.to_location(lid.loc), leading);
            CommentTable::attach(&mut t.trailing, arena.to_location(lid.loc), trailing);
        }
        PatternDesc::Ppat_exception(inner) => {
            walk_pattern(inner, t, comments, arena);
        }
        PatternDesc::Ppat_unpack(name) => {
            let (leading, trailing) = partition_leading_trailing(comments, &arena.to_location(name.loc));
            CommentTable::attach(&mut t.leading, arena.to_location(name.loc), leading);
            CommentTable::attach(&mut t.trailing, arena.to_location(name.loc), trailing);
        }
        PatternDesc::Ppat_extension(ext) => {
            walk_extension(ext, t, comments, arena);
        }
        PatternDesc::Ppat_open(lid, pat) => {
            let (leading, trailing) = partition_leading_trailing(comments, &arena.to_location(lid.loc));
            CommentTable::attach(&mut t.leading, arena.to_location(lid.loc), leading);

            let (after_lid, rest) = partition_adjacent_trailing(&arena.to_location(lid.loc), trailing);
            CommentTable::attach(&mut t.trailing, arena.to_location(lid.loc), after_lid);

            walk_pattern(pat, t, rest, arena);
        }
    }
}

fn walk_pattern_record_row(
    li: &Loc<Longident>,
    pat: &Pattern,
    t: &mut CommentTable,
    comments: Vec<Comment>,
    arena: &ParseArena,
) {
    let li_loc = arena.to_location(li.loc);
    let (leading, trailing) = partition_leading_trailing(comments, &li_loc);
    CommentTable::attach(&mut t.leading, li_loc.clone(), leading);

    let (after_li, rest) = partition_adjacent_trailing(&li_loc, trailing);
    CommentTable::attach(&mut t.trailing, li_loc, after_li);

    let (before, inside, after) = partition_by_loc(rest, &arena.to_location(pat.ppat_loc));
    CommentTable::attach(&mut t.leading, arena.to_location(pat.ppat_loc), before);
    walk_pattern(pat, t, inside, arena);
    CommentTable::attach(&mut t.trailing, arena.to_location(pat.ppat_loc), after);
}

fn walk_core_type(ct: &CoreType, t: &mut CommentTable, comments: Vec<Comment>, arena: &ParseArena) {
    if comments.is_empty() {
        return;
    }

    match &ct.ptyp_desc {
        CoreTypeDesc::Ptyp_any | CoreTypeDesc::Ptyp_var(_) => {
            let (leading, trailing) = partition_leading_trailing(comments, &arena.to_location(ct.ptyp_loc));
            CommentTable::attach(&mut t.leading, arena.to_location(ct.ptyp_loc), leading);
            CommentTable::attach(&mut t.trailing, arena.to_location(ct.ptyp_loc), trailing);
        }
        CoreTypeDesc::Ptyp_constr(name, args) => {
            let (leading, trailing) = partition_leading_trailing(comments, &arena.to_location(name.loc));
            CommentTable::attach(&mut t.leading, arena.to_location(name.loc), leading);

            if args.is_empty() {
                CommentTable::attach(&mut t.trailing, arena.to_location(name.loc), trailing);
            } else {
                let (after_name, rest) = partition_adjacent_trailing(&arena.to_location(name.loc), trailing);
                CommentTable::attach(&mut t.trailing, arena.to_location(name.loc), after_name);

                visit_list_but_continue_with_remaining_comments(
                    args,
                    |arg, _arena| arg.ptyp_loc,
                    |arg, t, comments| walk_core_type(arg, t, comments, arena),
                    t,
                    rest,
                    false,
                    arena,
                );
            }
        }
        CoreTypeDesc::Ptyp_arrow { arg, ret, .. } => {
            let (before, inside, after) = partition_by_loc(comments, &arg.typ.ptyp_loc);
            CommentTable::attach(&mut t.leading, arg.typ.ptyp_loc.clone(), before);
            walk_core_type(&arg.typ, t, inside, arena);

            let (after_arg, rest) = partition_adjacent_trailing(&arg.typ.ptyp_loc, after);
            CommentTable::attach(&mut t.trailing, arg.typ.ptyp_loc.clone(), after_arg);

            let (before, inside, after) = partition_by_loc(rest, &arena.to_location(ret.ptyp_loc));
            CommentTable::attach(&mut t.leading, arena.to_location(ret.ptyp_loc), before);
            walk_core_type(ret, t, inside, arena);
            CommentTable::attach(&mut t.trailing, arena.to_location(ret.ptyp_loc), after);
        }
        CoreTypeDesc::Ptyp_tuple(types) => {
            let nodes: Vec<Node<'_>> = types.iter().map(|ty| Node::CoreType(ty)).collect();
            walk_list(&nodes, t, comments, arena);
        }
        CoreTypeDesc::Ptyp_object(fields, _) => {
            let nodes: Vec<Node<'_>> = fields.iter().map(|f| Node::ObjectField(f)).collect();
            walk_list(&nodes, t, comments, arena);
        }
        CoreTypeDesc::Ptyp_poly(_, typ) => {
            walk_core_type(typ, t, comments, arena);
        }
        CoreTypeDesc::Ptyp_variant(fields, _, _) => {
            let nodes: Vec<Node<'_>> = fields.iter().map(|f| Node::RowField(f)).collect();
            walk_list(&nodes, t, comments, arena);
        }
        CoreTypeDesc::Ptyp_package((name, constraints)) => {
            let (leading, trailing) = partition_leading_trailing(comments, &arena.to_location(name.loc));
            CommentTable::attach(&mut t.leading, arena.to_location(name.loc), leading);

            if constraints.is_empty() {
                CommentTable::attach(&mut t.trailing, arena.to_location(name.loc), trailing);
            } else {
                let (after_name, rest) = partition_adjacent_trailing(&arena.to_location(name.loc), trailing);
                CommentTable::attach(&mut t.trailing, arena.to_location(name.loc), after_name);

                let nodes: Vec<Node<'_>> = constraints
                    .iter()
                    .map(|(li, ty)| Node::PackageConstraint(li, ty))
                    .collect();
                walk_list(&nodes, t, rest, arena);
            }
        }
        CoreTypeDesc::Ptyp_alias(typ, _name) => {
            // The alias name is just a String without location info
            let (before, inside, after) = partition_by_loc(comments, &arena.to_location(typ.ptyp_loc));
            CommentTable::attach(&mut t.leading, arena.to_location(typ.ptyp_loc), before);
            walk_core_type(typ, t, inside, arena);
            CommentTable::attach(&mut t.trailing, arena.to_location(typ.ptyp_loc), after);
        }
        CoreTypeDesc::Ptyp_extension(ext) => {
            walk_extension(ext, t, comments, arena);
        }
    }
}

fn walk_object_field(field: &ObjectField, t: &mut CommentTable, comments: Vec<Comment>, arena: &ParseArena) {
    match field {
        ObjectField::Otag(lbl, _attrs, typ) => {
            let (leading, trailing) = partition_leading_trailing(comments, &arena.to_location(lbl.loc));
            CommentTable::attach(&mut t.leading, arena.to_location(lbl.loc), leading);

            let (after_lbl, rest) = partition_adjacent_trailing(&arena.to_location(lbl.loc), trailing);
            CommentTable::attach(&mut t.trailing, arena.to_location(lbl.loc), after_lbl);

            let (before, inside, after) = partition_by_loc(rest, &arena.to_location(typ.ptyp_loc));
            CommentTable::attach(&mut t.leading, arena.to_location(typ.ptyp_loc), before);
            walk_core_type(typ, t, inside, arena);
            CommentTable::attach(&mut t.trailing, arena.to_location(typ.ptyp_loc), after);
        }
        ObjectField::Oinherit(typ) => {
            walk_core_type(typ, t, comments, arena);
        }
    }
}

fn walk_row_field(field: &RowField, t: &mut CommentTable, comments: Vec<Comment>, arena: &ParseArena) {
    match field {
        RowField::Rtag(tag, _attrs, _flag, args) => {
            let (leading, trailing) = partition_leading_trailing(comments, &arena.to_location(tag.loc));
            CommentTable::attach(&mut t.leading, arena.to_location(tag.loc), leading);

            if args.is_empty() {
                CommentTable::attach(&mut t.trailing, arena.to_location(tag.loc), trailing);
            } else {
                let (after_tag, rest) = partition_adjacent_trailing(&tag.loc, trailing);
                CommentTable::attach(&mut t.trailing, arena.to_location(tag.loc), after_tag);

                visit_list_but_continue_with_remaining_comments(
                    args,
                    |arg, _arena| arg.ptyp_loc,
                    |arg, t, comments| walk_core_type(arg, t, comments, arena),
                    t,
                    rest,
                    false,
                    arena,
                );
            }
        }
        RowField::Rinherit(typ) => {
            walk_core_type(typ, t, comments, arena);
        }
    }
}

fn walk_package_constraint(
    li: &Loc<Longident>,
    typ: &CoreType,
    t: &mut CommentTable,
    comments: Vec<Comment>,
    arena: &ParseArena,
) {
    let li_loc = arena.to_location(li.loc);
    let (leading, trailing) = partition_leading_trailing(comments, &li_loc);
    CommentTable::attach(&mut t.leading, li_loc.clone(), leading);

    let (after_li, rest) = partition_adjacent_trailing(&li_loc, trailing);
    CommentTable::attach(&mut t.trailing, li_loc, after_li);

    let (before, inside, after) = partition_by_loc(rest, &arena.to_location(typ.ptyp_loc));
    CommentTable::attach(&mut t.leading, arena.to_location(typ.ptyp_loc), before);
    walk_core_type(typ, t, inside, arena);
    CommentTable::attach(&mut t.trailing, arena.to_location(typ.ptyp_loc), after);
}

fn walk_module_expr(me: &ModuleExpr, t: &mut CommentTable, comments: Vec<Comment>, arena: &ParseArena) {
    if comments.is_empty() {
        return;
    }

    match &me.pmod_desc {
        ModuleExprDesc::Pmod_ident(lid) => {
            let (leading, trailing) = partition_leading_trailing(comments, &arena.to_location(lid.loc));
            CommentTable::attach(&mut t.leading, arena.to_location(lid.loc), leading);
            CommentTable::attach(&mut t.trailing, arena.to_location(lid.loc), trailing);
        }
        ModuleExprDesc::Pmod_structure(structure) => {
            walk_structure(structure, t, comments);
        }
        ModuleExprDesc::Pmod_functor(name, arg_type, body) => {
            // Handle functor name
            let (leading, trailing) = partition_leading_trailing(comments, &arena.to_location(name.loc));
            CommentTable::attach(&mut t.leading, arena.to_location(name.loc), leading);

            let (after_name, rest) = partition_adjacent_trailing(&arena.to_location(name.loc), trailing);
            CommentTable::attach(&mut t.trailing, arena.to_location(name.loc), after_name);

            // Handle optional argument type
            let rest = match arg_type {
                Some(mod_type) => {
                    let (before, inside, after) = partition_by_loc(rest, &mod_type.pmty_loc);
                    CommentTable::attach(&mut t.leading, mod_type.pmty_loc.clone(), before);
                    walk_mod_type(mod_type, t, inside, arena);
                    let (after_mod_type, rest) =
                        partition_adjacent_trailing(&mod_type.pmty_loc, after);
                    CommentTable::attach(&mut t.trailing, mod_type.pmty_loc.clone(), after_mod_type);
                    rest
                }
                None => rest,
            };

            let (before, inside, after) = partition_by_loc(rest, &arena.to_location(body.pmod_loc));
            CommentTable::attach(&mut t.leading, arena.to_location(body.pmod_loc), before);
            walk_module_expr(body, t, inside, arena);
            CommentTable::attach(&mut t.trailing, arena.to_location(body.pmod_loc), after);
        }
        ModuleExprDesc::Pmod_apply(funct, arg) => {
            let (before, inside, after) = partition_by_loc(comments, &arena.to_location(funct.pmod_loc));
            CommentTable::attach(&mut t.leading, arena.to_location(funct.pmod_loc), before);
            walk_module_expr(funct, t, inside, arena);

            let (after_funct, rest) = partition_adjacent_trailing(&arena.to_location(funct.pmod_loc), after);
            CommentTable::attach(&mut t.trailing, arena.to_location(funct.pmod_loc), after_funct);

            let (before, inside, after) = partition_by_loc(rest, &arena.to_location(arg.pmod_loc));
            CommentTable::attach(&mut t.leading, arena.to_location(arg.pmod_loc), before);
            walk_module_expr(arg, t, inside, arena);
            CommentTable::attach(&mut t.trailing, arena.to_location(arg.pmod_loc), after);
        }
        ModuleExprDesc::Pmod_constraint(module_expr, mod_type) => {
            let (before, inside, after) = partition_by_loc(comments, &arena.to_location(module_expr.pmod_loc));
            CommentTable::attach(&mut t.leading, arena.to_location(module_expr.pmod_loc), before);
            walk_module_expr(module_expr, t, inside, arena);

            let (after_mod, rest) = partition_adjacent_trailing(&arena.to_location(module_expr.pmod_loc), after);
            CommentTable::attach(&mut t.trailing, arena.to_location(module_expr.pmod_loc), after_mod);

            let (before, inside, after) = partition_by_loc(rest, &mod_type.pmty_loc);
            CommentTable::attach(&mut t.leading, mod_type.pmty_loc.clone(), before);
            walk_mod_type(mod_type, t, inside, arena);
            CommentTable::attach(&mut t.trailing, mod_type.pmty_loc.clone(), after);
        }
        ModuleExprDesc::Pmod_unpack(expr) => {
            walk_expression(expr, t, comments, arena);
        }
        ModuleExprDesc::Pmod_extension(ext) => {
            walk_extension(ext, t, comments, arena);
        }
    }
}

fn walk_mod_type(mt: &ModuleType, t: &mut CommentTable, comments: Vec<Comment>, arena: &ParseArena) {
    if comments.is_empty() {
        return;
    }

    match &mt.pmty_desc {
        ModuleTypeDesc::Pmty_ident(lid) => {
            let (leading, trailing) = partition_leading_trailing(comments, &arena.to_location(lid.loc));
            CommentTable::attach(&mut t.leading, arena.to_location(lid.loc), leading);
            CommentTable::attach(&mut t.trailing, arena.to_location(lid.loc), trailing);
        }
        ModuleTypeDesc::Pmty_signature(sig) => {
            walk_signature(sig, t, comments);
        }
        ModuleTypeDesc::Pmty_functor(name, arg_type, ret) => {
            // Handle functor name
            let (leading, trailing) = partition_leading_trailing(comments, &arena.to_location(name.loc));
            CommentTable::attach(&mut t.leading, arena.to_location(name.loc), leading);

            let (after_name, rest) = partition_adjacent_trailing(&arena.to_location(name.loc), trailing);
            CommentTable::attach(&mut t.trailing, arena.to_location(name.loc), after_name);

            // Handle optional argument type
            let rest = match arg_type {
                Some(mod_type) => {
                    let (before, inside, after) = partition_by_loc(rest, &mod_type.pmty_loc);
                    CommentTable::attach(&mut t.leading, mod_type.pmty_loc.clone(), before);
                    walk_mod_type(mod_type, t, inside, arena);
                    let (after_mod_type, rest) =
                        partition_adjacent_trailing(&mod_type.pmty_loc, after);
                    CommentTable::attach(&mut t.trailing, mod_type.pmty_loc.clone(), after_mod_type);
                    rest
                }
                None => rest,
            };

            let (before, inside, after) = partition_by_loc(rest, &ret.pmty_loc);
            CommentTable::attach(&mut t.leading, ret.pmty_loc.clone(), before);
            walk_mod_type(ret, t, inside, arena);
            CommentTable::attach(&mut t.trailing, ret.pmty_loc.clone(), after);
        }
        ModuleTypeDesc::Pmty_with(typ, constraints) => {
            let (before, inside, after) = partition_by_loc(comments, &typ.pmty_loc);
            CommentTable::attach(&mut t.leading, typ.pmty_loc.clone(), before);
            walk_mod_type(typ, t, inside, arena);

            let (after_typ, rest) = partition_adjacent_trailing(&typ.pmty_loc, after);
            CommentTable::attach(&mut t.trailing, typ.pmty_loc.clone(), after_typ);

            // Walk with constraints if needed
            for constraint in constraints {
                match constraint {
                    WithConstraint::Pwith_type(lid, td)
                    | WithConstraint::Pwith_typesubst(lid, td) => {
                        let (leading, trailing) = partition_leading_trailing(rest.clone(), &lid.loc);
                        CommentTable::attach(&mut t.leading, arena.to_location(lid.loc), leading);
                        walk_type_declaration(td, t, trailing, arena);
                    }
                    WithConstraint::Pwith_module(mod_lid, target_lid)
                    | WithConstraint::Pwith_modsubst(mod_lid, target_lid) => {
                        let (leading, trailing) = partition_leading_trailing(rest.clone(), &arena.to_location(mod_lid.loc));
                        CommentTable::attach(&mut t.leading, arena.to_location(mod_lid.loc), leading);
                        let (after_mod, rest2) = partition_adjacent_trailing(&arena.to_location(mod_lid.loc), trailing);
                        CommentTable::attach(&mut t.trailing, arena.to_location(mod_lid.loc), after_mod);
                        let (leading, trailing) = partition_leading_trailing(rest2, &arena.to_location(target_lid.loc));
                        CommentTable::attach(&mut t.leading, arena.to_location(target_lid.loc), leading);
                        CommentTable::attach(&mut t.trailing, arena.to_location(target_lid.loc), trailing);
                    }
                }
            }
        }
        ModuleTypeDesc::Pmty_typeof(me) => {
            walk_module_expr(me, t, comments, arena);
        }
        ModuleTypeDesc::Pmty_extension(ext) => {
            walk_extension(ext, t, comments, arena);
        }
        ModuleTypeDesc::Pmty_alias(lid) => {
            let (leading, trailing) = partition_leading_trailing(comments, &arena.to_location(lid.loc));
            CommentTable::attach(&mut t.leading, arena.to_location(lid.loc), leading);
            CommentTable::attach(&mut t.trailing, arena.to_location(lid.loc), trailing);
        }
    }
}

fn walk_attribute(_attr: &Attribute, _t: &mut CommentTable, _comments: Vec<Comment>) {
    // Attributes don't have much structure to walk
}

fn walk_extension(ext: &Extension, t: &mut CommentTable, comments: Vec<Comment>, arena: &ParseArena) {
    let (name, payload) = ext;
    let (leading, trailing) = partition_leading_trailing(comments, &arena.to_location(name.loc));
    CommentTable::attach(&mut t.leading, arena.to_location(name.loc), leading);

    match payload {
        PStr(items) => {
            if !items.is_empty() {
                let (after_name, rest) = partition_adjacent_trailing(&arena.to_location(name.loc), trailing);
                CommentTable::attach(&mut t.trailing, arena.to_location(name.loc), after_name);
                walk_structure(items, t, rest);
            } else {
                CommentTable::attach(&mut t.trailing, arena.to_location(name.loc), trailing);
            }
        }
        PSig(items) => {
            if !items.is_empty() {
                let (after_name, rest) = partition_adjacent_trailing(&arena.to_location(name.loc), trailing);
                CommentTable::attach(&mut t.trailing, arena.to_location(name.loc), after_name);
                walk_signature(items, t, rest);
            } else {
                CommentTable::attach(&mut t.trailing, arena.to_location(name.loc), trailing);
            }
        }
        PTyp(typ) => {
            let (after_name, rest) = partition_adjacent_trailing(&arena.to_location(name.loc), trailing);
            CommentTable::attach(&mut t.trailing, arena.to_location(name.loc), after_name);
            walk_core_type(typ, t, rest, arena);
        }
        PPat(pat, guard) => {
            let (after_name, rest) = partition_adjacent_trailing(&arena.to_location(name.loc), trailing);
            CommentTable::attach(&mut t.trailing, arena.to_location(name.loc), after_name);
            walk_pattern(pat, t, rest.clone());
            if let Some(guard_expr) = guard {
                walk_expression(guard_expr, t, Vec::new());
            }
        }
    }
}

/// Visit a list of nodes but return any remaining comments.
fn visit_list_but_continue_with_remaining_comments<'a, T>(
    nodes: &'a [T],
    get_loc: impl Fn(&'a T, &ParseArena) -> LocIdx,
    walk_node: impl Fn(&'a T, &mut CommentTable, Vec<Comment>),
    t: &mut CommentTable,
    comments: Vec<Comment>,
    newline_delimited: bool,
    arena: &ParseArena,
) -> Vec<Comment> {
    visit_list_impl(nodes, get_loc, walk_node, t, comments, newline_delimited, None, arena)
}

fn visit_list_impl<'a, T>(
    nodes: &'a [T],
    get_loc: impl Fn(&'a T, &ParseArena) -> LocIdx,
    walk_node: impl Fn(&'a T, &mut CommentTable, Vec<Comment>),
    t: &mut CommentTable,
    comments: Vec<Comment>,
    newline_delimited: bool,
    prev_loc: Option<Location>,
    arena: &ParseArena,
) -> Vec<Comment> {
    if comments.is_empty() {
        return Vec::new();
    }
    if nodes.is_empty() {
        return match prev_loc {
            Some(loc) => {
                let (after_prev, rest) = if newline_delimited {
                    partition_by_on_same_line(&loc, comments)
                } else {
                    partition_adjacent_trailing(&loc, comments)
                };
                CommentTable::attach(&mut t.trailing, loc, after_prev);
                rest
            }
            None => comments,
        };
    }

    let node = &nodes[0];
    let rest_nodes = &nodes[1..];
    let curr_loc = arena.to_location(get_loc(node, arena));

    let (leading, inside, trailing) = partition_by_loc(comments, &curr_loc);

    match &prev_loc {
        None => {
            // First node, all leading comments attach here
            CommentTable::attach(&mut t.leading, curr_loc.clone(), leading);
        }
        Some(prev_loc) => {
            // Check if on same line
            if prev_loc.loc_end.line == curr_loc.loc_start.line {
                let (after_prev, before_curr) = partition_adjacent_trailing(prev_loc, leading);
                CommentTable::attach(&mut t.trailing, prev_loc.clone(), after_prev);
                CommentTable::attach(&mut t.leading, curr_loc.clone(), before_curr);
            } else {
                let (on_same_line_as_prev, after_prev) = partition_by_on_same_line(prev_loc, leading);
                CommentTable::attach(&mut t.trailing, prev_loc.clone(), on_same_line_as_prev);
                let (before_curr, _, _) = partition_by_loc(after_prev, &curr_loc);
                CommentTable::attach(&mut t.leading, curr_loc.clone(), before_curr);
            }
        }
    }

    walk_node(node, t, inside);
    visit_list_impl(
        rest_nodes,
        get_loc,
        walk_node,
        t,
        trailing,
        newline_delimited,
        Some(curr_loc),
        arena,
    )
}

/// Create a comment table from a structure and comments.
pub fn make_for_structure(structure: &Structure, comments: Vec<Comment>) -> CommentTable {
    let mut table = CommentTable::new();
    walk_structure(structure, &mut table, comments);
    table
}

/// Create a comment table from a signature and comments.
pub fn make_for_signature(signature: &Signature, comments: Vec<Comment>) -> CommentTable {
    let mut table = CommentTable::new();
    walk_signature(signature, &mut table, comments);
    table
}
