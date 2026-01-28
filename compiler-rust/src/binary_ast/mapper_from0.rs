//! Mapper from parsetree0 to current parsetree
//!
//! This module converts the frozen parsetree0 format back to the current Rust
//! parsetree types. This is the inverse of `mapper_to0`.
//!
//! Key transformations:
//! - [@res.arity N] attribute + Function$ wrapper → function arity
//! - function$ type constructor → arrow type arity
//! - [@res.async] attribute → async flag
//! - [@res.await] attribute → Pexp_await
//! - [@res.partial] attribute → partial flag
//! - [@res.optional] attribute → optional record fields
//!
//! This mirrors `compiler/ml/ast_mapper_from0.ml`.

use crate::location::Location as FullLocation;
use crate::location::Located as OldLocated;
use crate::parse_arena::{Located, LocIdx, ParseArena};
use crate::parser::ast::{self as current};
use crate::parser::longident::Longident;

use super::parsetree0 as pt0;

// ========== Public API ==========

/// Convert a structure from parsetree0 to current parsetree
pub fn map_structure(arena: &mut ParseArena, str: &[pt0::StructureItem]) -> Vec<current::StructureItem> {
    let mut mapper = Mapper::new(arena);
    mapper.map_structure(str)
}

/// Convert a signature from parsetree0 to current parsetree
pub fn map_signature(arena: &mut ParseArena, sig: &[pt0::SignatureItem]) -> Vec<current::SignatureItem> {
    let mut mapper = Mapper::new(arena);
    mapper.map_signature(sig)
}

// ========== Helper functions ==========

fn map_closed_flag(flag: &pt0::ClosedFlag) -> current::ClosedFlag {
    match flag {
        pt0::ClosedFlag::Closed => current::ClosedFlag::Closed,
        pt0::ClosedFlag::Open => current::ClosedFlag::Open,
    }
}

fn map_arity(arity: &pt0::Arity) -> current::Arity {
    match arity {
        Some(n) => current::Arity::Full(*n as usize),
        None => current::Arity::Unknown,
    }
}

// ========== Attribute helpers ==========

fn has_await_attribute(attrs: &[pt0::Attribute]) -> bool {
    attrs.iter().any(|(name, _)| name.txt == "res.await")
}

fn remove_await_attribute(attrs: &[pt0::Attribute]) -> Vec<pt0::Attribute> {
    attrs
        .iter()
        .filter(|(name, _)| name.txt != "res.await")
        .cloned()
        .collect()
}

fn has_async_attribute(attrs: &[current::Attribute]) -> bool {
    attrs.iter().any(|(name, _)| name.txt == "res.async")
}

fn has_partial_attribute(attrs: &[pt0::Attribute]) -> bool {
    attrs.iter().any(|(name, _)| name.txt == "res.partial")
}

fn remove_partial_attribute(attrs: &[pt0::Attribute]) -> Vec<pt0::Attribute> {
    attrs
        .iter()
        .filter(|(name, _)| name.txt != "res.partial")
        .cloned()
        .collect()
}

fn has_optional_attribute(attrs: &[current::Attribute]) -> bool {
    attrs.iter().any(|(name, _)| name.txt == "res.optional")
}

fn remove_optional_attribute(attrs: &[current::Attribute]) -> Vec<current::Attribute> {
    attrs
        .iter()
        .filter(|(name, _)| name.txt != "res.optional")
        .cloned()
        .collect()
}

fn get_arity_from_attributes(attrs: &[current::Attribute]) -> Option<i32> {
    for (name, payload) in attrs {
        if name.txt == "res.arity" {
            if let current::Payload::PStr(items) = payload {
                if let Some(item) = items.first() {
                    if let current::StructureItemDesc::Pstr_eval(expr, _) = &item.pstr_desc {
                        if let current::ExpressionDesc::Pexp_constant(current::Constant::Integer(
                            s,
                            _,
                        )) = &expr.pexp_desc
                        {
                            return s.parse().ok();
                        }
                    }
                }
            }
        }
    }
    None
}

fn remove_arity_attribute(attrs: &[current::Attribute]) -> Vec<current::Attribute> {
    attrs
        .iter()
        .filter(|(name, _)| name.txt != "res.arity")
        .cloned()
        .collect()
}

// ========== JSX helpers ==========

fn has_jsx_attribute(attrs: &[pt0::Attribute]) -> bool {
    attrs.iter().any(|(name, _)| name.txt == "JSX")
}

fn remove_jsx_attribute(attrs: &[pt0::Attribute]) -> Vec<pt0::Attribute> {
    attrs
        .iter()
        .filter(|(name, _)| name.txt != "JSX")
        .cloned()
        .collect()
}

// ========== Flag conversions ==========

fn map_rec_flag(flag: pt0::RecFlag) -> current::RecFlag {
    match flag {
        pt0::RecFlag::Nonrecursive => current::RecFlag::Nonrecursive,
        pt0::RecFlag::Recursive => current::RecFlag::Recursive,
    }
}

fn map_direction_flag(flag: pt0::DirectionFlag) -> current::DirectionFlag {
    match flag {
        pt0::DirectionFlag::Upto => current::DirectionFlag::Upto,
        pt0::DirectionFlag::Downto => current::DirectionFlag::Downto,
    }
}

fn map_private_flag(flag: pt0::PrivateFlag) -> current::PrivateFlag {
    match flag {
        pt0::PrivateFlag::Private => current::PrivateFlag::Private,
        pt0::PrivateFlag::Public => current::PrivateFlag::Public,
    }
}

fn map_mutable_flag(flag: pt0::MutableFlag) -> current::MutableFlag {
    match flag {
        pt0::MutableFlag::Immutable => current::MutableFlag::Immutable,
        pt0::MutableFlag::Mutable => current::MutableFlag::Mutable,
    }
}

fn map_override_flag(flag: pt0::OverrideFlag) -> current::OverrideFlag {
    match flag {
        pt0::OverrideFlag::Override => current::OverrideFlag::Override,
        pt0::OverrideFlag::Fresh => current::OverrideFlag::Fresh,
    }
}

fn map_variance(v: pt0::Variance) -> current::Variance {
    match v {
        pt0::Variance::Covariant => current::Variance::Covariant,
        pt0::Variance::Contravariant => current::Variance::Contravariant,
        pt0::Variance::Invariant => current::Variance::Invariant,
    }
}

// ========== Constant conversion ==========

fn map_constant(c: &pt0::Constant) -> current::Constant {
    match c {
        pt0::Constant::Integer(s, suffix) => current::Constant::Integer(s.clone(), suffix.clone()),
        pt0::Constant::Char(c) => current::Constant::Char(*c),
        pt0::Constant::String(s, delim) => {
            // Reverse the "*j" transformation from mapper_to0:
            // - Some("*j") -> None (regular double-quoted string)
            // - Other delimiters -> keep as is
            let mapped_delim = match delim {
                Some(d) if d == "*j" => None,
                other => other.clone(),
            };
            current::Constant::String(s.clone(), mapped_delim)
        }
        pt0::Constant::Float(s, suffix) => current::Constant::Float(s.clone(), suffix.clone()),
    }
}

/// Mapper from parsetree0 to current parsetree.
/// Holds a mutable reference to the arena for creating LocIdx values.
pub struct Mapper<'a> {
    arena: &'a mut ParseArena,
}

impl<'a> Mapper<'a> {
    pub fn new(arena: &'a mut ParseArena) -> Self {
        Self { arena }
    }

    /// Convert a full Location to LocIdx
    fn loc(&mut self, loc: &FullLocation) -> LocIdx {
        self.arena.from_location(loc)
    }

    /// Create a Located value from old Located
    fn map_loc<T: Clone>(&mut self, old: &OldLocated<T>) -> Located<T> {
        Located {
            txt: old.txt.clone(),
            loc: self.loc(&old.loc),
        }
    }

    /// Create a Located value with given text and location
    fn mkloc<T>(&mut self, txt: T, loc: &FullLocation) -> Located<T> {
        Located {
            txt,
            loc: self.loc(loc),
        }
    }

    // ========== Public API ==========

    /// Convert a structure from parsetree0 to current parsetree
    pub fn map_structure(&mut self, str: &[pt0::StructureItem]) -> Vec<current::StructureItem> {
        str.iter().map(|item| self.map_structure_item(item)).collect()
    }

    /// Convert a signature from parsetree0 to current parsetree
    pub fn map_signature(&mut self, sig: &[pt0::SignatureItem]) -> Vec<current::SignatureItem> {
        sig.iter().map(|item| self.map_signature_item(item)).collect()
    }

    // ========== Arg label mapping ==========

    fn map_arg_label(&mut self, lbl: &pt0::ArgLabel) -> current::ArgLabel {
        match lbl {
            pt0::ArgLabel::Nolabel => current::ArgLabel::Nolabel,
            // parsetree0 doesn't have location info, so use mknoloc
            pt0::ArgLabel::Labelled(s) => current::ArgLabel::Labelled(Located::mknoloc(s.clone())),
            pt0::ArgLabel::Optional(s) => current::ArgLabel::Optional(Located::mknoloc(s.clone())),
        }
    }

    // ========== Attribute mapping ==========

    fn map_attribute(&mut self, attr: &pt0::Attribute) -> current::Attribute {
        (self.map_loc(&attr.0), self.map_payload(&attr.1))
    }

    fn map_attributes(&mut self, attrs: &[pt0::Attribute]) -> Vec<current::Attribute> {
        attrs.iter().map(|a| self.map_attribute(a)).collect()
    }

    fn map_extension(&mut self, ext: &pt0::Extension) -> current::Extension {
        (self.map_loc(&ext.0), self.map_payload(&ext.1))
    }

    fn map_payload(&mut self, payload: &pt0::Payload) -> current::Payload {
        match payload {
            pt0::Payload::PStr(str) => current::Payload::PStr(self.map_structure(str)),
            pt0::Payload::PSig(sig) => current::Payload::PSig(self.map_signature(sig)),
            pt0::Payload::PTyp(t) => current::Payload::PTyp(Box::new(self.map_core_type(t))),
            pt0::Payload::PPat(p, guard) => {
                current::Payload::PPat(Box::new(self.map_pattern(p)), guard.as_ref().map(|g| Box::new(self.map_expression(g))))
            }
        }
    }

    // ========== Core type mapping ==========

    fn map_core_type(&mut self, typ: &pt0::CoreType) -> current::CoreType {
        let loc = self.loc(&typ.ptyp_loc);
        let attrs = self.map_attributes(&typ.ptyp_attributes);

        let desc = match &typ.ptyp_desc {
            pt0::CoreTypeDesc::Any => current::CoreTypeDesc::Ptyp_any,
            pt0::CoreTypeDesc::Var(s) => current::CoreTypeDesc::Ptyp_var(s.clone()),
            pt0::CoreTypeDesc::Arrow(lbl, t1, t2) => {
                let lbl = self.map_arg_label(lbl);
                return current::CoreType {
                    ptyp_desc: current::CoreTypeDesc::Ptyp_arrow {
                        arg: Box::new(current::TypeArg {
                            attrs,
                            lbl,
                            typ: self.map_core_type(t1),
                        }),
                        ret: Box::new(self.map_core_type(t2)),
                        arity: current::Arity::Unknown,
                    },
                    ptyp_loc: loc,
                    ptyp_attributes: vec![],
                };
            }
            pt0::CoreTypeDesc::Tuple(tl) => {
                current::CoreTypeDesc::Ptyp_tuple(tl.iter().map(|t| self.map_core_type(t)).collect())
            }
            pt0::CoreTypeDesc::Constr(lid, tl) => {
                let mapped_types: Vec<_> = tl.iter().map(|t| self.map_core_type(t)).collect();

                if let Longident::Lident(name) = &lid.txt {
                    if name == "function$" && mapped_types.len() == 2 {
                        if let current::CoreTypeDesc::Ptyp_arrow { arg, ret, .. } = mapped_types[0].ptyp_desc.clone() {
                            if let current::CoreTypeDesc::Ptyp_variant(rows, _, _) = &mapped_types[1].ptyp_desc {
                                if let Some(row) = rows.first() {
                                    if let current::RowField::Rtag(tag, _, _, _) = row {
                                        if tag.txt.starts_with("Has_arity") {
                                            if let Ok(arity) = tag.txt[9..].parse::<usize>() {
                                                return current::CoreType {
                                                    ptyp_desc: current::CoreTypeDesc::Ptyp_arrow {
                                                        arg,
                                                        ret,
                                                        arity: current::Arity::Full(arity),
                                                    },
                                                    ptyp_loc: loc,
                                                    ptyp_attributes: attrs,
                                                };
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }

                current::CoreTypeDesc::Ptyp_constr(self.map_loc(lid), mapped_types)
            }
            pt0::CoreTypeDesc::Object(fields, closed) => {
                let fields = fields.iter().map(|f| self.map_object_field(f)).collect();
                current::CoreTypeDesc::Ptyp_object(fields, map_closed_flag(closed))
            }
            pt0::CoreTypeDesc::Alias(t, s) => {
                current::CoreTypeDesc::Ptyp_alias(Box::new(self.map_core_type(t)), s.clone())
            }
            pt0::CoreTypeDesc::Variant(rows, closed, labels) => {
                let rows = rows.iter().map(|r| self.map_row_field(r)).collect();
                current::CoreTypeDesc::Ptyp_variant(rows, map_closed_flag(closed), labels.clone())
            }
            pt0::CoreTypeDesc::Poly(vars, t) => {
                current::CoreTypeDesc::Ptyp_poly(
                    vars.iter().map(|v| self.map_loc(v)).collect(),
                    Box::new(self.map_core_type(t)),
                )
            }
            pt0::CoreTypeDesc::Package(pkg) => {
                let (lid, constraints) = pkg;
                let constraints = constraints
                    .iter()
                    .map(|(l, t)| (self.map_loc(l), self.map_core_type(t)))
                    .collect();
                current::CoreTypeDesc::Ptyp_package((self.map_loc(lid), constraints))
            }
            pt0::CoreTypeDesc::Extension(ext) => {
                current::CoreTypeDesc::Ptyp_extension(self.map_extension(ext))
            }
            pt0::CoreTypeDesc::Class => {
                current::CoreTypeDesc::Ptyp_any
            }
        };

        current::CoreType {
            ptyp_desc: desc,
            ptyp_loc: loc,
            ptyp_attributes: attrs,
        }
    }

    fn map_row_field(&mut self, rf: &pt0::RowField) -> current::RowField {
        match rf {
            pt0::RowField::Rtag(label, attrs, empty, types) => current::RowField::Rtag(
                self.map_loc(label),
                self.map_attributes(attrs),
                *empty,
                types.iter().map(|t| self.map_core_type(t)).collect(),
            ),
            pt0::RowField::Rinherit(t) => current::RowField::Rinherit(self.map_core_type(t)),
        }
    }

    fn map_object_field(&mut self, of: &pt0::ObjectField) -> current::ObjectField {
        match of {
            pt0::ObjectField::Otag(label, attrs, t) => {
                current::ObjectField::Otag(self.map_loc(label), self.map_attributes(attrs), self.map_core_type(t))
            }
            pt0::ObjectField::Oinherit(t) => current::ObjectField::Oinherit(self.map_core_type(t)),
        }
    }

    // ========== Pattern mapping ==========

    fn map_pattern(&mut self, pat: &pt0::Pattern) -> current::Pattern {
        let loc = self.loc(&pat.ppat_loc);
        let attrs = self.map_attributes(&pat.ppat_attributes);

        let desc = match &pat.ppat_desc {
            pt0::PatternDesc::Any => current::PatternDesc::Ppat_any,
            pt0::PatternDesc::Var(s) => current::PatternDesc::Ppat_var(self.map_loc(s)),
            pt0::PatternDesc::Alias(p, s) => {
                current::PatternDesc::Ppat_alias(Box::new(self.map_pattern(p)), self.map_loc(s))
            }
            pt0::PatternDesc::Constant(c) => current::PatternDesc::Ppat_constant(map_constant(c)),
            pt0::PatternDesc::Interval(c1, c2) => {
                current::PatternDesc::Ppat_interval(map_constant(c1), map_constant(c2))
            }
            pt0::PatternDesc::Tuple(pl) => {
                current::PatternDesc::Ppat_tuple(pl.iter().map(|p| self.map_pattern(p)).collect())
            }
            pt0::PatternDesc::Construct(lid, arg) => {
                current::PatternDesc::Ppat_construct(
                    self.map_loc(lid),
                    arg.as_ref().map(|p| Box::new(self.map_pattern(p)))
                )
            }
            pt0::PatternDesc::Variant(tag, arg) => {
                current::PatternDesc::Ppat_variant(tag.clone(), arg.as_ref().map(|p| Box::new(self.map_pattern(p))))
            }
            pt0::PatternDesc::Record(fields, closed) => {
                let fields = fields.iter().map(|pt0::RecordElement { lid, x: pat, opt }| {
                    let lid = self.map_loc(lid);
                    let mapped_attrs = self.map_attributes(&pat.ppat_attributes);
                    let optional = *opt || has_optional_attribute(&mapped_attrs);
                    let pat = if has_optional_attribute(&mapped_attrs) {
                        let mut mapped = self.map_pattern(pat);
                        mapped.ppat_attributes = remove_optional_attribute(&mapped.ppat_attributes);
                        mapped
                    } else {
                        self.map_pattern(pat)
                    };
                    current::PatternRecordField { lid, pat, opt: optional }
                }).collect();
                current::PatternDesc::Ppat_record(fields, map_closed_flag(closed))
            }
            pt0::PatternDesc::Array(pl) => {
                current::PatternDesc::Ppat_array(pl.iter().map(|p| self.map_pattern(p)).collect())
            }
            pt0::PatternDesc::Or(p1, p2) => {
                current::PatternDesc::Ppat_or(Box::new(self.map_pattern(p1)), Box::new(self.map_pattern(p2)))
            }
            pt0::PatternDesc::Constraint(p, t) => {
                current::PatternDesc::Ppat_constraint(Box::new(self.map_pattern(p)), self.map_core_type(t))
            }
            pt0::PatternDesc::Type(lid) => {
                current::PatternDesc::Ppat_type(self.map_loc(lid))
            }
            pt0::PatternDesc::Unpack(name) => {
                current::PatternDesc::Ppat_unpack(self.map_loc(name))
            }
            pt0::PatternDesc::Open(lid, p) => {
                current::PatternDesc::Ppat_open(self.map_loc(lid), Box::new(self.map_pattern(p)))
            }
            pt0::PatternDesc::Exception(p) => {
                current::PatternDesc::Ppat_exception(Box::new(self.map_pattern(p)))
            }
            pt0::PatternDesc::Extension(ext) => {
                current::PatternDesc::Ppat_extension(self.map_extension(ext))
            }
            pt0::PatternDesc::Lazy(_) => {
                panic!("OCaml 'lazy' patterns are not supported in ReScript")
            }
        };

        current::Pattern {
            ppat_desc: desc,
            ppat_loc: loc,
            ppat_attributes: attrs,
        }
    }

    // ========== Expression mapping ==========

    /// Extracts JSX children from a cons-list expression
    fn extract_jsx_children(&mut self, expr: &pt0::Expression) -> Vec<current::Expression> {
        let mut children = Vec::new();
        let mut current_expr = expr;
        loop {
            match &current_expr.pexp_desc {
                pt0::ExpressionDesc::Construct(lid, Some(arg))
                    if matches!(&lid.txt, Longident::Lident(s) if s == "::") =>
                {
                    if let pt0::ExpressionDesc::Tuple(items) = &arg.pexp_desc {
                        if items.len() == 2 {
                            children.push(self.map_expression(&items[0]));
                            current_expr = &items[1];
                            continue;
                        }
                    }
                    break;
                }
                pt0::ExpressionDesc::Construct(lid, None)
                    if matches!(&lid.txt, Longident::Lident(s) if s == "[]") =>
                {
                    break;
                }
                _ => {
                    // If it's not a proper list, just include this as a child
                    children.push(self.map_expression(current_expr));
                    break;
                }
            }
        }
        children
    }

    /// Tries to extract a JSX prop from a labeled argument
    fn try_map_jsx_prop(&mut self, label: &pt0::ArgLabel, expr: &pt0::Expression) -> Option<current::JsxProp> {
        match label {
            pt0::ArgLabel::Labelled(name) if name == "_spreadProps" => {
                let expr_loc = self.loc(&expr.pexp_loc);
                Some(current::JsxProp::Spreading {
                    loc: expr_loc,
                    expr: self.map_expression(expr),
                })
            }
            pt0::ArgLabel::Labelled(name) => {
                // Check for punning: labelled arg with ident of same name
                if let pt0::ExpressionDesc::Ident(lid) = &expr.pexp_desc {
                    if let Longident::Lident(v) = &lid.txt {
                        if v == name {
                            return Some(current::JsxProp::Punning {
                                optional: false,
                                name: self.map_loc(lid).map(|_| name.clone()),
                            });
                        }
                    }
                }
                Some(current::JsxProp::Value {
                    name: Located::mknoloc(name.clone()),
                    optional: false,
                    value: self.map_expression(expr),
                })
            }
            pt0::ArgLabel::Optional(name) => {
                // Check for punning: optional arg with ident of same name
                if let pt0::ExpressionDesc::Ident(lid) = &expr.pexp_desc {
                    if let Longident::Lident(v) = &lid.txt {
                        if v == name {
                            return Some(current::JsxProp::Punning {
                                optional: true,
                                name: self.map_loc(lid).map(|_| name.clone()),
                            });
                        }
                    }
                }
                Some(current::JsxProp::Value {
                    name: Located::mknoloc(name.clone()),
                    optional: true,
                    value: self.map_expression(expr),
                })
            }
            pt0::ArgLabel::Nolabel => None,
        }
    }

    /// Extracts props and children from JSX apply arguments
    fn extract_jsx_props_and_children(
        &mut self,
        args: &[(pt0::ArgLabel, pt0::Expression)],
    ) -> (Vec<current::JsxProp>, Vec<current::Expression>) {
        let mut props = Vec::new();
        let mut children = Vec::new();

        for (label, expr) in args {
            match label {
                pt0::ArgLabel::Labelled(name) if name == "children" => {
                    children = self.extract_jsx_children(expr);
                }
                pt0::ArgLabel::Nolabel => {
                    // Unit argument at the end, skip
                }
                _ => {
                    if let Some(prop) = self.try_map_jsx_prop(label, expr) {
                        props.push(prop);
                    }
                }
            }
        }

        (props, children)
    }

    /// Converts a longident to a JSX tag name
    fn longident_to_jsx_tag(&self, lid: &Longident) -> current::JsxTagName {
        match lid {
            Longident::Lident(s) if !s.is_empty() && s.chars().next().unwrap().is_lowercase() => {
                current::JsxTagName::Lower(s.clone())
            }
            Longident::Lident(s) => current::JsxTagName::Upper(Longident::Lident(s.clone())),
            Longident::Ldot(path, name)
                if !name.is_empty() && name.chars().next().unwrap().is_lowercase() =>
            {
                current::JsxTagName::QualifiedLower {
                    path: (**path).clone(),
                    name: name.clone(),
                }
            }
            other => current::JsxTagName::Upper(other.clone()),
        }
    }

    fn map_expression(&mut self, expr: &pt0::Expression) -> current::Expression {
        let loc = self.loc(&expr.pexp_loc);
        let attrs = self.map_attributes(&expr.pexp_attributes);

        // Check for res.await attribute - convert to Pexp_await
        if has_await_attribute(&expr.pexp_attributes) {
            let new_attrs = remove_await_attribute(&expr.pexp_attributes);
            let inner_expr = pt0::Expression {
                pexp_desc: expr.pexp_desc.clone(),
                pexp_loc: expr.pexp_loc.clone(),
                pexp_attributes: new_attrs,
            };
            return current::Expression {
                pexp_desc: current::ExpressionDesc::Pexp_await(Box::new(self.map_expression(&inner_expr))),
                pexp_loc: loc,
                pexp_attributes: vec![],
            };
        }

        let desc = match &expr.pexp_desc {
            pt0::ExpressionDesc::Ident(lid) => current::ExpressionDesc::Pexp_ident(self.map_loc(lid)),
            pt0::ExpressionDesc::Constant(c) => current::ExpressionDesc::Pexp_constant(map_constant(c)),
            pt0::ExpressionDesc::Let(rec_flag, bindings, body) => current::ExpressionDesc::Pexp_let(
                map_rec_flag(*rec_flag),
                bindings.iter().map(|vb| self.map_value_binding(vb)).collect(),
                Box::new(self.map_expression(body)),
            ),
            pt0::ExpressionDesc::Fun { arg_label: lbl, default, lhs: pat, rhs: body, arity, is_async } => {
                current::ExpressionDesc::Pexp_fun {
                    arg_label: self.map_arg_label(lbl),
                    default: default.as_ref().map(|e| Box::new(self.map_expression(e))),
                    lhs: self.map_pattern(pat),
                    rhs: Box::new(self.map_expression(body)),
                    arity: map_arity(arity),
                    is_async: *is_async,
                }
            }
            pt0::ExpressionDesc::Apply { funct: func, args, partial: pt0_partial, transformed_jsx } => {
                // Check for JSX attribute - convert back to JSX element
                if has_jsx_attribute(&expr.pexp_attributes) {
                    if let pt0::ExpressionDesc::Ident(tag_lid) = &func.pexp_desc {
                        let jsx_attrs = self.map_attributes(&remove_jsx_attribute(&expr.pexp_attributes));
                        let (props, children) = self.extract_jsx_props_and_children(args);
                        let tag_name = self.longident_to_jsx_tag(&tag_lid.txt);
                        let tag_loc = Located::new(tag_name, self.loc(&tag_lid.loc));

                        // Get positions for JSX element structure
                        let loc_start = self.arena.loc_start(loc).clone();
                        let loc_end = self.arena.loc_end(loc).clone();

                        let jsx_element = if children.is_empty() {
                            current::JsxElement::Unary(current::JsxUnaryElement {
                                tag_name: tag_loc,
                                props,
                            })
                        } else {
                            current::JsxElement::Container(current::JsxContainerElement {
                                tag_name_start: tag_loc.clone(),
                                opening_end: loc_start.clone(),
                                props,
                                children,
                                closing_tag: Some(current::JsxClosingTag {
                                    start: loc_end.clone(),
                                    name: tag_loc,
                                    end: loc_end,
                                }),
                            })
                        };

                        return current::Expression {
                            pexp_desc: current::ExpressionDesc::Pexp_jsx_element(jsx_element),
                            pexp_loc: loc,
                            pexp_attributes: jsx_attrs,
                        };
                    }
                }

                // Check for partial application attribute (legacy) or use pt0_partial
                let partial = *pt0_partial || has_partial_attribute(&expr.pexp_attributes);
                let attrs = if has_partial_attribute(&expr.pexp_attributes) {
                    self.map_attributes(&remove_partial_attribute(&expr.pexp_attributes))
                } else {
                    attrs.clone()
                };

                // Map operator names
                let func = if let pt0::ExpressionDesc::Ident(lid) = &func.pexp_desc {
                    match &lid.txt {
                        Longident::Lident(name) => {
                            let new_name = match name.as_str() {
                                "|." => "->",
                                "^" => "++",
                                "<>" => "!=",
                                "!=" => "!==",
                                "=" => "==",
                                "==" => "===",
                                _ => name.as_str(),
                            };
                            if new_name != name {
                                pt0::Expression {
                                    pexp_desc: pt0::ExpressionDesc::Ident(OldLocated {
                                        txt: Longident::Lident(new_name.to_string()),
                                        loc: lid.loc.clone(),
                                    }),
                                    pexp_loc: func.pexp_loc.clone(),
                                    pexp_attributes: func.pexp_attributes.clone(),
                                }
                            } else {
                                func.as_ref().clone()
                            }
                        }
                        _ => func.as_ref().clone(),
                    }
                } else {
                    func.as_ref().clone()
                };

                return current::Expression {
                    pexp_desc: current::ExpressionDesc::Pexp_apply {
                        funct: Box::new(self.map_expression(&func)),
                        args: args
                            .iter()
                            .map(|(lbl, e)| (self.map_arg_label(lbl), self.map_expression(e)))
                            .collect(),
                        partial,
                        transformed_jsx: *transformed_jsx,
                    },
                    pexp_loc: loc,
                    pexp_attributes: attrs,
                };
            }
            pt0::ExpressionDesc::Match(e, cases) => current::ExpressionDesc::Pexp_match(
                Box::new(self.map_expression(e)),
                cases.iter().map(|c| self.map_case(c)).collect(),
            ),
            pt0::ExpressionDesc::Try(e, cases) => current::ExpressionDesc::Pexp_try(
                Box::new(self.map_expression(e)),
                cases.iter().map(|c| self.map_case(c)).collect(),
            ),
            pt0::ExpressionDesc::Tuple(el) => {
                current::ExpressionDesc::Pexp_tuple(el.iter().map(|e| self.map_expression(e)).collect())
            }
            pt0::ExpressionDesc::Construct(lid, arg) => {
                // Check for JSX fragment (list with [@JSX] attribute)
                if has_jsx_attribute(&expr.pexp_attributes) {
                    if let Longident::Lident(name) = &lid.txt {
                        if name == "[]" || name == "::" {
                            let jsx_attrs = self.map_attributes(&remove_jsx_attribute(&expr.pexp_attributes));
                            let children = self.extract_jsx_children(expr);
                            let loc_start = self.arena.loc_start(loc).clone();
                            let loc_end = self.arena.loc_end(loc).clone();
                            let jsx_element = current::JsxElement::Fragment(current::JsxFragment {
                                opening: loc_start,
                                children,
                                closing: loc_end,
                            });
                            return current::Expression {
                                pexp_desc: current::ExpressionDesc::Pexp_jsx_element(jsx_element),
                                pexp_loc: loc,
                                pexp_attributes: jsx_attrs,
                            };
                        }
                    }
                }

                // Check for Function$ wrapper (arity encoding)
                if let Longident::Lident(name) = &lid.txt {
                    if name == "Function$" {
                        if let Some(inner) = arg {
                            let mapped = self.map_expression(inner);
                            if let current::ExpressionDesc::Pexp_fun { arg_label, default, lhs, rhs, is_async, .. } = mapped.pexp_desc {
                                // Extract arity from attributes
                                let arity_opt = get_arity_from_attributes(&attrs);
                                let arity = match arity_opt {
                                    Some(n) => current::Arity::Full(n as usize),
                                    None => current::Arity::Unknown,
                                };
                                let new_attrs = remove_arity_attribute(&attrs);
                                return current::Expression {
                                    pexp_desc: current::ExpressionDesc::Pexp_fun {
                                        arg_label,
                                        default,
                                        lhs,
                                        rhs,
                                        arity,
                                        is_async,
                                    },
                                    pexp_loc: loc,
                                    pexp_attributes: new_attrs,
                                };
                            }
                        }
                    }
                }
                current::ExpressionDesc::Pexp_construct(
                    self.map_loc(lid),
                    arg.as_ref().map(|e| Box::new(self.map_expression(e))),
                )
            }
            pt0::ExpressionDesc::Variant(label, arg) => current::ExpressionDesc::Pexp_variant(
                label.clone(),
                arg.as_ref().map(|e| Box::new(self.map_expression(e))),
            ),
            pt0::ExpressionDesc::Record(fields, base) => {
                let fields = fields
                    .iter()
                    .map(|pt0::RecordElement { lid, x: e, opt }| {
                        let mapped_expr = self.map_expression(e);
                        // Preserve opt from parsetree0 if present, or check for optional attribute
                        let optional = *opt || has_optional_attribute(&mapped_expr.pexp_attributes);
                        let expr_attrs = if has_optional_attribute(&mapped_expr.pexp_attributes) {
                            remove_optional_attribute(&mapped_expr.pexp_attributes)
                        } else {
                            mapped_expr.pexp_attributes.clone()
                        };
                        current::ExpressionRecordField {
                            lid: self.map_loc(lid),
                            expr: current::Expression {
                                pexp_desc: mapped_expr.pexp_desc,
                                pexp_loc: mapped_expr.pexp_loc,
                                pexp_attributes: expr_attrs,
                            },
                            opt: optional,
                        }
                    })
                    .collect();
                current::ExpressionDesc::Pexp_record(
                    fields,
                    base.as_ref().map(|e| Box::new(self.map_expression(e))),
                )
            }
            pt0::ExpressionDesc::Field(e, lid) => {
                current::ExpressionDesc::Pexp_field(Box::new(self.map_expression(e)), self.map_loc(lid))
            }
            pt0::ExpressionDesc::Setfield(e1, lid, e2) => current::ExpressionDesc::Pexp_setfield(
                Box::new(self.map_expression(e1)),
                self.map_loc(lid),
                Box::new(self.map_expression(e2)),
            ),
            pt0::ExpressionDesc::Array(el) => {
                current::ExpressionDesc::Pexp_array(el.iter().map(|e| self.map_expression(e)).collect())
            }
            pt0::ExpressionDesc::Ifthenelse(e1, e2, e3) => current::ExpressionDesc::Pexp_ifthenelse(
                Box::new(self.map_expression(e1)),
                Box::new(self.map_expression(e2)),
                e3.as_ref().map(|e| Box::new(self.map_expression(e))),
            ),
            pt0::ExpressionDesc::Sequence(e1, e2) => current::ExpressionDesc::Pexp_sequence(
                Box::new(self.map_expression(e1)),
                Box::new(self.map_expression(e2)),
            ),
            pt0::ExpressionDesc::While(e1, e2) => current::ExpressionDesc::Pexp_while(
                Box::new(self.map_expression(e1)),
                Box::new(self.map_expression(e2)),
            ),
            pt0::ExpressionDesc::For(pat, e1, e2, dir, e3) => current::ExpressionDesc::Pexp_for(
                self.map_pattern(pat),
                Box::new(self.map_expression(e1)),
                Box::new(self.map_expression(e2)),
                map_direction_flag(*dir),
                Box::new(self.map_expression(e3)),
            ),
            pt0::ExpressionDesc::Coerce(e, _, t) => {
                current::ExpressionDesc::Pexp_coerce(Box::new(self.map_expression(e)), None, self.map_core_type(t))
            }
            pt0::ExpressionDesc::Constraint(e, t) => {
                current::ExpressionDesc::Pexp_constraint(Box::new(self.map_expression(e)), self.map_core_type(t))
            }
            pt0::ExpressionDesc::Send(e, method) => {
                current::ExpressionDesc::Pexp_send(Box::new(self.map_expression(e)), self.map_loc(method))
            }
            pt0::ExpressionDesc::Letmodule(name, me, body) => current::ExpressionDesc::Pexp_letmodule(
                self.map_loc(name),
                self.map_module_expr(me),
                Box::new(self.map_expression(body)),
            ),
            pt0::ExpressionDesc::Letexception(ec, body) => current::ExpressionDesc::Pexp_letexception(
                self.map_extension_constructor(ec),
                Box::new(self.map_expression(body)),
            ),
            pt0::ExpressionDesc::Assert(e) => {
                current::ExpressionDesc::Pexp_assert(Box::new(self.map_expression(e)))
            }
            pt0::ExpressionDesc::Newtype(name, body) => {
                current::ExpressionDesc::Pexp_newtype(self.map_loc(name), Box::new(self.map_expression(body)))
            }
            pt0::ExpressionDesc::Pack(me) => current::ExpressionDesc::Pexp_pack(self.map_module_expr(me)),
            pt0::ExpressionDesc::Open(ovf, lid, body) => current::ExpressionDesc::Pexp_open(
                map_override_flag(*ovf),
                self.map_loc(lid),
                Box::new(self.map_expression(body)),
            ),
            pt0::ExpressionDesc::Extension(ext) => {
                current::ExpressionDesc::Pexp_extension(self.map_extension(ext))
            }
            // OCaml-specific expressions that don't exist in ReScript
            pt0::ExpressionDesc::New(_) => {
                panic!("OCaml 'new' expressions are not supported in ReScript")
            }
            pt0::ExpressionDesc::Setinstvar(_, _) => {
                panic!("OCaml 'setinstvar' expressions are not supported in ReScript")
            }
            pt0::ExpressionDesc::Override(_) => {
                panic!("OCaml 'override' expressions are not supported in ReScript")
            }
            pt0::ExpressionDesc::Lazy(_) => {
                panic!("OCaml 'lazy' expressions are not supported in ReScript")
            }
            pt0::ExpressionDesc::Poly(_, _) => {
                panic!("OCaml 'poly' expressions are not supported in ReScript")
            }
            pt0::ExpressionDesc::Object => {
                panic!("OCaml 'object' expressions are not supported in ReScript")
            }
            pt0::ExpressionDesc::Unreachable => {
                // Unreachable is represented by a `.` pattern
                // Map to an extension point that represents unreachable
                current::ExpressionDesc::Pexp_extension((
                    self.mkloc("rescript.exprhole".to_string(), &expr.pexp_loc),
                    current::Payload::PStr(vec![]),
                ))
            }
        };

        current::Expression {
            pexp_desc: desc,
            pexp_loc: loc,
            pexp_attributes: attrs,
        }
    }

    fn map_case(&mut self, c: &pt0::Case) -> current::Case {
        current::Case {
            pc_bar: None,
            pc_lhs: self.map_pattern(&c.pc_lhs),
            pc_guard: c.pc_guard.as_ref().map(|g| self.map_expression(g)),
            pc_rhs: self.map_expression(&c.pc_rhs),
        }
    }

    fn map_value_binding(&mut self, vb: &pt0::ValueBinding) -> current::ValueBinding {
        current::ValueBinding {
            pvb_pat: self.map_pattern(&vb.pvb_pat),
            pvb_expr: self.map_expression(&vb.pvb_expr),
            pvb_attributes: self.map_attributes(&vb.pvb_attributes),
            pvb_loc: self.loc(&vb.pvb_loc),
        }
    }

    // ========== Module expressions ==========

    fn map_module_expr(&mut self, me: &pt0::ModuleExpr) -> current::ModuleExpr {
        let loc = self.loc(&me.pmod_loc);
        let attrs = self.map_attributes(&me.pmod_attributes);

        let desc = match &me.pmod_desc {
            pt0::ModuleExprDesc::Ident(lid) => current::ModuleExprDesc::Pmod_ident(self.map_loc(lid)),
            pt0::ModuleExprDesc::Structure(str) => {
                current::ModuleExprDesc::Pmod_structure(self.map_structure(str))
            }
            pt0::ModuleExprDesc::Functor(name, mt, body) => current::ModuleExprDesc::Pmod_functor(
                self.map_loc(name),
                mt.as_ref().map(|mt| Box::new(self.map_module_type(mt))),
                Box::new(self.map_module_expr(body)),
            ),
            pt0::ModuleExprDesc::Apply(m1, m2) => current::ModuleExprDesc::Pmod_apply(
                Box::new(self.map_module_expr(m1)),
                Box::new(self.map_module_expr(m2)),
            ),
            pt0::ModuleExprDesc::Constraint(m, mt) => {
                current::ModuleExprDesc::Pmod_constraint(Box::new(self.map_module_expr(m)), Box::new(self.map_module_type(mt)))
            }
            pt0::ModuleExprDesc::Unpack(e) => {
                current::ModuleExprDesc::Pmod_unpack(Box::new(self.map_expression(e)))
            }
            pt0::ModuleExprDesc::Extension(ext) => {
                current::ModuleExprDesc::Pmod_extension(self.map_extension(ext))
            }
        };

        current::ModuleExpr {
            pmod_desc: desc,
            pmod_loc: loc,
            pmod_attributes: attrs,
        }
    }

    // ========== Module types ==========

    fn map_module_type(&mut self, mt: &pt0::ModuleType) -> current::ModuleType {
        let loc = self.loc(&mt.pmty_loc);
        let attrs = self.map_attributes(&mt.pmty_attributes);

        let desc = match &mt.pmty_desc {
            pt0::ModuleTypeDesc::Ident(lid) => current::ModuleTypeDesc::Pmty_ident(self.map_loc(lid)),
            pt0::ModuleTypeDesc::Alias(lid) => current::ModuleTypeDesc::Pmty_alias(self.map_loc(lid)),
            pt0::ModuleTypeDesc::Signature(sig) => {
                current::ModuleTypeDesc::Pmty_signature(self.map_signature(sig))
            }
            pt0::ModuleTypeDesc::Functor(name, mt1, mt2) => current::ModuleTypeDesc::Pmty_functor(
                self.map_loc(name),
                mt1.as_ref().map(|m| Box::new(self.map_module_type(m))),
                Box::new(self.map_module_type(mt2)),
            ),
            pt0::ModuleTypeDesc::With(mt, constraints) => current::ModuleTypeDesc::Pmty_with(
                Box::new(self.map_module_type(mt)),
                constraints.iter().map(|wc| self.map_with_constraint(wc)).collect(),
            ),
            pt0::ModuleTypeDesc::Typeof(me) => {
                current::ModuleTypeDesc::Pmty_typeof(Box::new(self.map_module_expr(me)))
            }
            pt0::ModuleTypeDesc::Extension(ext) => {
                current::ModuleTypeDesc::Pmty_extension(self.map_extension(ext))
            }
        };

        current::ModuleType {
            pmty_desc: desc,
            pmty_loc: loc,
            pmty_attributes: attrs,
        }
    }

    fn map_with_constraint(&mut self, wc: &pt0::WithConstraint) -> current::WithConstraint {
        match wc {
            pt0::WithConstraint::Type(lid, td) => {
                current::WithConstraint::Pwith_type(self.map_loc(lid), self.map_type_declaration(td))
            }
            pt0::WithConstraint::Module(lid1, lid2) => {
                current::WithConstraint::Pwith_module(self.map_loc(lid1), self.map_loc(lid2))
            }
            pt0::WithConstraint::TypeSubst(lid, td) => {
                current::WithConstraint::Pwith_typesubst(self.map_loc(lid), self.map_type_declaration(td))
            }
            pt0::WithConstraint::ModSubst(lid1, lid2) => {
                current::WithConstraint::Pwith_modsubst(self.map_loc(lid1), self.map_loc(lid2))
            }
        }
    }

    // ========== Structure items ==========

    fn map_structure_item(&mut self, item: &pt0::StructureItem) -> current::StructureItem {
        let loc = self.loc(&item.pstr_loc);

        let desc = match &item.pstr_desc {
            pt0::StructureItemDesc::Eval(e, attrs) => {
                current::StructureItemDesc::Pstr_eval(self.map_expression(e), self.map_attributes(attrs))
            }
            pt0::StructureItemDesc::Value(rec_flag, bindings) => current::StructureItemDesc::Pstr_value(
                map_rec_flag(*rec_flag),
                bindings.iter().map(|vb| self.map_value_binding(vb)).collect(),
            ),
            pt0::StructureItemDesc::Primitive(vd) => {
                current::StructureItemDesc::Pstr_primitive(self.map_value_description(vd))
            }
            pt0::StructureItemDesc::Type(rec_flag, decls) => current::StructureItemDesc::Pstr_type(
                map_rec_flag(*rec_flag),
                decls.iter().map(|td| self.map_type_declaration(td)).collect(),
            ),
            pt0::StructureItemDesc::Typext(te) => {
                current::StructureItemDesc::Pstr_typext(self.map_type_extension(te))
            }
            pt0::StructureItemDesc::Exception(ec) => {
                current::StructureItemDesc::Pstr_exception(self.map_extension_constructor(ec))
            }
            pt0::StructureItemDesc::Module(mb) => {
                current::StructureItemDesc::Pstr_module(self.map_module_binding(mb))
            }
            pt0::StructureItemDesc::Recmodule(mbs) => {
                current::StructureItemDesc::Pstr_recmodule(mbs.iter().map(|mb| self.map_module_binding(mb)).collect())
            }
            pt0::StructureItemDesc::Modtype(mtd) => {
                current::StructureItemDesc::Pstr_modtype(self.map_module_type_declaration(mtd))
            }
            pt0::StructureItemDesc::Open(od) => {
                current::StructureItemDesc::Pstr_open(self.map_open_description(od))
            }
            pt0::StructureItemDesc::Include(id) => {
                current::StructureItemDesc::Pstr_include(self.map_include_declaration(id))
            }
            pt0::StructureItemDesc::Extension(ext, attrs) => {
                current::StructureItemDesc::Pstr_extension(self.map_extension(ext), self.map_attributes(attrs))
            }
            pt0::StructureItemDesc::Attribute(attr) => {
                current::StructureItemDesc::Pstr_attribute(self.map_attribute(attr))
            }
            pt0::StructureItemDesc::Class => {
                panic!("OCaml 'class' definitions are not supported in ReScript")
            }
            pt0::StructureItemDesc::ClassType => {
                panic!("OCaml 'class type' definitions are not supported in ReScript")
            }
        };

        current::StructureItem {
            pstr_desc: desc,
            pstr_loc: loc,
        }
    }

    // ========== Signature items ==========

    fn map_signature_item(&mut self, item: &pt0::SignatureItem) -> current::SignatureItem {
        let loc = self.loc(&item.psig_loc);

        let desc = match &item.psig_desc {
            pt0::SignatureItemDesc::Value(vd) => {
                current::SignatureItemDesc::Psig_value(self.map_value_description(vd))
            }
            pt0::SignatureItemDesc::Type(rec_flag, decls) => current::SignatureItemDesc::Psig_type(
                map_rec_flag(*rec_flag),
                decls.iter().map(|td| self.map_type_declaration(td)).collect(),
            ),
            pt0::SignatureItemDesc::Typext(te) => {
                current::SignatureItemDesc::Psig_typext(self.map_type_extension(te))
            }
            pt0::SignatureItemDesc::Exception(ec) => {
                current::SignatureItemDesc::Psig_exception(self.map_extension_constructor(ec))
            }
            pt0::SignatureItemDesc::Module(md) => {
                current::SignatureItemDesc::Psig_module(self.map_module_declaration(md))
            }
            pt0::SignatureItemDesc::Recmodule(mds) => current::SignatureItemDesc::Psig_recmodule(
                mds.iter().map(|md| self.map_module_declaration(md)).collect(),
            ),
            pt0::SignatureItemDesc::Modtype(mtd) => {
                current::SignatureItemDesc::Psig_modtype(self.map_module_type_declaration(mtd))
            }
            pt0::SignatureItemDesc::Open(od) => {
                current::SignatureItemDesc::Psig_open(self.map_open_description(od))
            }
            pt0::SignatureItemDesc::Include(id) => {
                current::SignatureItemDesc::Psig_include(self.map_include_description(id))
            }
            pt0::SignatureItemDesc::Extension(ext, attrs) => {
                current::SignatureItemDesc::Psig_extension(self.map_extension(ext), self.map_attributes(attrs))
            }
            pt0::SignatureItemDesc::Attribute(attr) => {
                current::SignatureItemDesc::Psig_attribute(self.map_attribute(attr))
            }
            pt0::SignatureItemDesc::Class => {
                panic!("OCaml 'class' declarations are not supported in ReScript")
            }
            pt0::SignatureItemDesc::ClassType => {
                panic!("OCaml 'class type' declarations are not supported in ReScript")
            }
        };

        current::SignatureItem {
            psig_desc: desc,
            psig_loc: loc,
        }
    }

    // ========== Type declarations ==========

    fn map_type_declaration(&mut self, td: &pt0::TypeDeclaration) -> current::TypeDeclaration {
        current::TypeDeclaration {
            ptype_name: self.map_loc(&td.ptype_name),
            ptype_params: td
                .ptype_params
                .iter()
                .map(|(t, v)| (self.map_core_type(t), map_variance(*v)))
                .collect(),
            ptype_cstrs: td
                .ptype_cstrs
                .iter()
                .map(|(t1, t2, loc)| (self.map_core_type(t1), self.map_core_type(t2), self.loc(loc)))
                .collect(),
            ptype_kind: self.map_type_kind(&td.ptype_kind),
            ptype_private: map_private_flag(td.ptype_private),
            ptype_manifest: td.ptype_manifest.as_ref().map(|t| self.map_core_type(t)),
            ptype_attributes: self.map_attributes(&td.ptype_attributes),
            ptype_loc: self.loc(&td.ptype_loc),
        }
    }

    fn map_type_kind(&mut self, tk: &pt0::TypeKind) -> current::TypeKind {
        match tk {
            pt0::TypeKind::Abstract => current::TypeKind::Ptype_abstract,
            pt0::TypeKind::Variant(cds) => {
                current::TypeKind::Ptype_variant(cds.iter().map(|cd| self.map_constructor_declaration(cd)).collect())
            }
            pt0::TypeKind::Record(lds) => {
                current::TypeKind::Ptype_record(lds.iter().map(|ld| self.map_label_declaration(ld)).collect())
            }
            pt0::TypeKind::Open => current::TypeKind::Ptype_open,
        }
    }

    fn map_constructor_declaration(&mut self, cd: &pt0::ConstructorDeclaration) -> current::ConstructorDeclaration {
        current::ConstructorDeclaration {
            pcd_name: self.map_loc(&cd.pcd_name),
            pcd_args: self.map_constructor_arguments(&cd.pcd_args),
            pcd_res: cd.pcd_res.as_ref().map(|t| self.map_core_type(t)),
            pcd_loc: self.loc(&cd.pcd_loc),
            pcd_attributes: self.map_attributes(&cd.pcd_attributes),
        }
    }

    fn map_constructor_arguments(&mut self, ca: &pt0::ConstructorArguments) -> current::ConstructorArguments {
        match ca {
            pt0::ConstructorArguments::Tuple(tl) => {
                current::ConstructorArguments::Pcstr_tuple(tl.iter().map(|t| self.map_core_type(t)).collect())
            }
            pt0::ConstructorArguments::Record(lds) => {
                current::ConstructorArguments::Pcstr_record(lds.iter().map(|ld| self.map_label_declaration(ld)).collect())
            }
        }
    }

    fn map_label_declaration(&mut self, ld: &pt0::LabelDeclaration) -> current::LabelDeclaration {
        let attrs = self.map_attributes(&ld.pld_attributes);
        let optional = has_optional_attribute(&attrs);
        let final_attrs = if optional {
            remove_optional_attribute(&attrs)
        } else {
            attrs
        };

        current::LabelDeclaration {
            pld_name: self.map_loc(&ld.pld_name),
            pld_mutable: map_mutable_flag(ld.pld_mutable),
            pld_type: self.map_core_type(&ld.pld_type),
            pld_loc: self.loc(&ld.pld_loc),
            pld_attributes: final_attrs,
            pld_optional: optional,
        }
    }

    fn map_type_extension(&mut self, te: &pt0::TypeExtension) -> current::TypeExtension {
        current::TypeExtension {
            ptyext_path: self.map_loc(&te.ptyext_path),
            ptyext_params: te
                .ptyext_params
                .iter()
                .map(|(t, v)| (self.map_core_type(t), map_variance(*v)))
                .collect(),
            ptyext_constructors: te
                .ptyext_constructors
                .iter()
                .map(|ec| self.map_extension_constructor(ec))
                .collect(),
            ptyext_private: map_private_flag(te.ptyext_private),
            ptyext_attributes: self.map_attributes(&te.ptyext_attributes),
        }
    }

    fn map_extension_constructor(&mut self, ec: &pt0::ExtensionConstructor) -> current::ExtensionConstructor {
        current::ExtensionConstructor {
            pext_name: self.map_loc(&ec.pext_name),
            pext_kind: self.map_extension_constructor_kind(&ec.pext_kind),
            pext_loc: self.loc(&ec.pext_loc),
            pext_attributes: self.map_attributes(&ec.pext_attributes),
        }
    }

    fn map_extension_constructor_kind(
        &mut self,
        eck: &pt0::ExtensionConstructorKind,
    ) -> current::ExtensionConstructorKind {
        match eck {
            pt0::ExtensionConstructorKind::Decl(args, ret) => current::ExtensionConstructorKind::Pext_decl(
                self.map_constructor_arguments(args),
                ret.as_ref().map(|t| self.map_core_type(t)),
            ),
            pt0::ExtensionConstructorKind::Rebind(lid) => {
                current::ExtensionConstructorKind::Pext_rebind(self.map_loc(lid))
            }
        }
    }

    // ========== Other declarations ==========

    fn map_value_description(&mut self, vd: &pt0::ValueDescription) -> current::ValueDescription {
        current::ValueDescription {
            pval_name: self.map_loc(&vd.pval_name),
            pval_type: self.map_core_type(&vd.pval_type),
            pval_prim: vd.pval_prim.clone(),
            pval_attributes: self.map_attributes(&vd.pval_attributes),
            pval_loc: self.loc(&vd.pval_loc),
        }
    }

    fn map_module_declaration(&mut self, md: &pt0::ModuleDeclaration) -> current::ModuleDeclaration {
        current::ModuleDeclaration {
            pmd_name: self.map_loc(&md.pmd_name),
            pmd_type: self.map_module_type(&md.pmd_type),
            pmd_attributes: self.map_attributes(&md.pmd_attributes),
            pmd_loc: self.loc(&md.pmd_loc),
        }
    }

    fn map_module_type_declaration(&mut self, mtd: &pt0::ModuleTypeDeclaration) -> current::ModuleTypeDeclaration {
        current::ModuleTypeDeclaration {
            pmtd_name: self.map_loc(&mtd.pmtd_name),
            pmtd_type: mtd.pmtd_type.as_ref().map(|mt| self.map_module_type(mt)),
            pmtd_attributes: self.map_attributes(&mtd.pmtd_attributes),
            pmtd_loc: self.loc(&mtd.pmtd_loc),
        }
    }

    fn map_module_binding(&mut self, mb: &pt0::ModuleBinding) -> current::ModuleBinding {
        current::ModuleBinding {
            pmb_name: self.map_loc(&mb.pmb_name),
            pmb_expr: self.map_module_expr(&mb.pmb_expr),
            pmb_attributes: self.map_attributes(&mb.pmb_attributes),
            pmb_loc: self.loc(&mb.pmb_loc),
        }
    }

    fn map_open_description(&mut self, od: &pt0::OpenDescription) -> current::OpenDescription {
        current::OpenDescription {
            popen_lid: self.map_loc(&od.popen_lid),
            popen_override: map_override_flag(od.popen_override),
            popen_loc: self.loc(&od.popen_loc),
            popen_attributes: self.map_attributes(&od.popen_attributes),
        }
    }

    fn map_include_declaration(&mut self, id: &pt0::IncludeDeclaration) -> current::IncludeDeclaration {
        current::IncludeDeclaration {
            pincl_mod: self.map_module_expr(&id.pincl_mod),
            pincl_loc: self.loc(&id.pincl_loc),
            pincl_attributes: self.map_attributes(&id.pincl_attributes),
        }
    }

    fn map_include_description(&mut self, id: &pt0::IncludeDescription) -> current::IncludeDescription {
        current::IncludeDescription {
            pincl_mod: self.map_module_type(&id.pincl_mod),
            pincl_loc: self.loc(&id.pincl_loc),
            pincl_attributes: self.map_attributes(&id.pincl_attributes),
        }
    }
}
