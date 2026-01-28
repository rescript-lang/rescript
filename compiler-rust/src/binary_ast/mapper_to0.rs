//! Mapper from current parsetree to parsetree0
//!
//! This module converts the current Rust parsetree types to the frozen parsetree0
//! format used for binary AST serialization.
//!
//! Key transformations:
//! - Function arity → [@res.arity N] attribute + Function$ wrapper
//! - Arrow type arity → function$ type constructor
//! - async flag → [@res.async] attribute
//! - Pexp_await → [@res.await] attribute on expression
//! - partial flag → [@res.partial] attribute
//! - optional record fields → [@res.optional] attribute
//! - JSX elements → already handled by JSX PPX
//!
//! This mirrors `compiler/common/ast_mapper_to0.ml`.

use crate::location::{Location as FullLocation, Located as OldLocated};
use crate::parse_arena::{Located, LocIdx, ParseArena};
use crate::parser::ast::{self as current};

use super::parsetree0 as pt0;

// ========== Public API ==========

/// Convert a structure from current parsetree to parsetree0
pub fn map_structure(arena: &ParseArena, str: &[current::StructureItem]) -> pt0::Structure {
    let mapper = Mapper::new(arena);
    mapper.map_structure(str)
}

/// Convert a signature from current parsetree to parsetree0
pub fn map_signature(arena: &ParseArena, sig: &[current::SignatureItem]) -> pt0::Signature {
    let mapper = Mapper::new(arena);
    mapper.map_signature(sig)
}

// ========== Mapper struct ==========

/// Mapper from current parsetree (with LocIdx) to parsetree0 (with full Location).
/// Holds a reference to the arena for converting LocIdx values to Location.
pub struct Mapper<'a> {
    arena: &'a ParseArena,
}

impl<'a> Mapper<'a> {
    pub fn new(arena: &'a ParseArena) -> Self {
        Self { arena }
    }

    /// Convert a LocIdx to full Location
    fn loc(&self, idx: LocIdx) -> FullLocation {
        self.arena.to_location(idx)
    }

    /// Convert an arena-based Located<T> to a full Located<T>
    fn map_loc<T: Clone>(&self, located: &Located<T>) -> OldLocated<T> {
        OldLocated {
            txt: located.txt.clone(),
            loc: self.loc(located.loc),
        }
    }

    /// Create an OldLocated from text and location index
    #[allow(dead_code)]
    fn mkloc<T>(&self, txt: T, loc: LocIdx) -> OldLocated<T> {
        OldLocated {
            txt,
            loc: self.loc(loc),
        }
    }

    // ========== Public API methods ==========

    /// Convert a structure from current parsetree to parsetree0
    pub fn map_structure(&self, str: &[current::StructureItem]) -> pt0::Structure {
        str.iter().map(|item| self.map_structure_item(item)).collect()
    }

    /// Convert a signature from current parsetree to parsetree0
    pub fn map_signature(&self, sig: &[current::SignatureItem]) -> pt0::Signature {
        sig.iter().map(|item| self.map_signature_item(item)).collect()
    }

    // ========== Helper functions ==========

    #[allow(dead_code)]
    fn mk_optional_attr(&self, loc: LocIdx) -> pt0::Attribute {
        (
            OldLocated::new("res.optional".to_string(), self.loc(loc)),
            pt0::Payload::PStr(vec![]),
        )
    }

    #[allow(dead_code)]
    fn mk_async_attr(&self, loc: LocIdx) -> pt0::Attribute {
        (
            OldLocated::new("res.async".to_string(), self.loc(loc)),
            pt0::Payload::PStr(vec![]),
        )
    }

    fn mk_await_attr(&self, loc: LocIdx) -> pt0::Attribute {
        (
            OldLocated::new("res.await".to_string(), self.loc(loc)),
            pt0::Payload::PStr(vec![]),
        )
    }

    #[allow(dead_code)]
    fn mk_partial_attr(&self, loc: LocIdx) -> pt0::Attribute {
        (
            OldLocated::new("res.partial".to_string(), self.loc(loc)),
            pt0::Payload::PStr(vec![]),
        )
    }

    #[allow(dead_code)]
    fn mk_arity_attr(&self, arity: usize, loc: LocIdx) -> pt0::Attribute {
        let full_loc = self.loc(loc);
        (
            OldLocated::new("res.arity".to_string(), full_loc.clone()),
            pt0::Payload::PStr(vec![pt0::StructureItem {
                pstr_desc: pt0::StructureItemDesc::Eval(
                    pt0::Expression {
                        pexp_desc: pt0::ExpressionDesc::Constant(pt0::Constant::Integer(
                            arity.to_string(),
                            None,
                        )),
                        pexp_loc: full_loc.clone(),
                        pexp_attributes: vec![],
                    },
                    vec![],
                ),
                pstr_loc: full_loc,
            }]),
        )
    }

    // ========== JSX helpers ==========

    fn mk_jsx_attr(&self, loc: LocIdx) -> pt0::Attribute {
        (
            OldLocated::new("JSX".to_string(), self.loc(loc)),
            pt0::Payload::PStr(vec![]),
        )
    }

    fn mk_unit_expr(&self, loc: LocIdx) -> pt0::Expression {
        let full_loc = self.loc(loc);
        pt0::Expression {
            pexp_desc: pt0::ExpressionDesc::Construct(
                OldLocated::new(
                    crate::parser::longident::Longident::Lident("()".to_string()),
                    full_loc.clone(),
                ),
                None,
            ),
            pexp_loc: full_loc,
            pexp_attributes: vec![],
        }
    }

    fn mk_nil_expr(&self, loc: LocIdx) -> pt0::Expression {
        let full_loc = self.loc(loc);
        pt0::Expression {
            pexp_desc: pt0::ExpressionDesc::Construct(
                OldLocated::new(
                    crate::parser::longident::Longident::Lident("[]".to_string()),
                    full_loc.clone(),
                ),
                None,
            ),
            pexp_loc: full_loc,
            pexp_attributes: vec![],
        }
    }

    fn map_jsx_props(&self, props: &[current::JsxProp], _loc: LocIdx) -> Vec<(pt0::ArgLabel, pt0::Expression)> {
        props
            .iter()
            .map(|prop| match prop {
                current::JsxProp::Punning { optional, name } => {
                    let name_loc = self.loc(name.loc);
                    let ident_expr = pt0::Expression {
                        pexp_desc: pt0::ExpressionDesc::Ident(OldLocated::new(
                            crate::parser::longident::Longident::Lident(name.txt.clone()),
                            name_loc.clone(),
                        )),
                        pexp_loc: name_loc,
                        pexp_attributes: vec![],
                    };
                    let label = if *optional {
                        pt0::ArgLabel::Optional(name.txt.clone())
                    } else {
                        pt0::ArgLabel::Labelled(name.txt.clone())
                    };
                    (label, ident_expr)
                }
                current::JsxProp::Value { name, optional, value } => {
                    let label = if *optional {
                        pt0::ArgLabel::Optional(name.txt.clone())
                    } else {
                        pt0::ArgLabel::Labelled(name.txt.clone())
                    };
                    (label, self.map_expression(value))
                }
                current::JsxProp::Spreading { expr, .. } => {
                    (pt0::ArgLabel::Labelled("_spreadProps".to_string()), self.map_expression(expr))
                }
            })
            .collect()
    }

    fn jsx_tag_to_longident(&self, tag: &current::JsxTagName) -> crate::parser::longident::Longident {
        match tag {
            current::JsxTagName::Lower(name) => {
                crate::parser::longident::Longident::Lident(name.clone())
            }
            current::JsxTagName::QualifiedLower { path, name } => {
                crate::parser::longident::Longident::Ldot(Box::new(path.clone()), name.clone())
            }
            current::JsxTagName::Upper(lid) => lid.clone(),
            current::JsxTagName::Invalid(name) => {
                crate::parser::longident::Longident::Lident(name.clone())
            }
        }
    }

    fn map_jsx_children_to_list(&self, children: &[current::Expression], loc: LocIdx) -> pt0::Expression {
        // Convert children to a list expression (cons cells ending with [])
        let nil = self.mk_nil_expr(loc);
        let full_loc = self.loc(loc);
        children.iter().rev().fold(nil, |acc, child| {
            let child_expr = self.map_expression(child);
            pt0::Expression {
                pexp_desc: pt0::ExpressionDesc::Construct(
                    OldLocated::new(
                        crate::parser::longident::Longident::Lident("::".to_string()),
                        full_loc.clone(),
                    ),
                    Some(Box::new(pt0::Expression {
                        pexp_desc: pt0::ExpressionDesc::Tuple(vec![child_expr, acc]),
                        pexp_loc: full_loc.clone(),
                        pexp_attributes: vec![],
                    })),
                ),
                pexp_loc: full_loc.clone(),
                pexp_attributes: vec![],
            }
        })
    }

    // ========== Flag conversions ==========

    fn map_rec_flag(&self, flag: current::RecFlag) -> pt0::RecFlag {
        match flag {
            current::RecFlag::Nonrecursive => pt0::RecFlag::Nonrecursive,
            current::RecFlag::Recursive => pt0::RecFlag::Recursive,
        }
    }

    fn map_direction_flag(&self, flag: current::DirectionFlag) -> pt0::DirectionFlag {
        match flag {
            current::DirectionFlag::Upto => pt0::DirectionFlag::Upto,
            current::DirectionFlag::Downto => pt0::DirectionFlag::Downto,
        }
    }

    fn map_closed_flag(&self, flag: current::ClosedFlag) -> pt0::ClosedFlag {
        match flag {
            current::ClosedFlag::Closed => pt0::ClosedFlag::Closed,
            current::ClosedFlag::Open => pt0::ClosedFlag::Open,
        }
    }

    fn map_override_flag(&self, flag: current::OverrideFlag) -> pt0::OverrideFlag {
        match flag {
            current::OverrideFlag::Fresh => pt0::OverrideFlag::Fresh,
            current::OverrideFlag::Override => pt0::OverrideFlag::Override,
        }
    }

    fn map_mutable_flag(&self, flag: current::MutableFlag) -> pt0::MutableFlag {
        match flag {
            current::MutableFlag::Immutable => pt0::MutableFlag::Immutable,
            current::MutableFlag::Mutable => pt0::MutableFlag::Mutable,
        }
    }

    fn map_private_flag(&self, flag: current::PrivateFlag) -> pt0::PrivateFlag {
        match flag {
            current::PrivateFlag::Private => pt0::PrivateFlag::Private,
            current::PrivateFlag::Public => pt0::PrivateFlag::Public,
        }
    }

    fn map_variance(&self, var: current::Variance) -> pt0::Variance {
        match var {
            current::Variance::Covariant => pt0::Variance::Covariant,
            current::Variance::Contravariant => pt0::Variance::Contravariant,
            current::Variance::Invariant => pt0::Variance::Invariant,
        }
    }

    fn map_arg_label(&self, label: &current::ArgLabel) -> pt0::ArgLabel {
        match label {
            current::ArgLabel::Nolabel => pt0::ArgLabel::Nolabel,
            // parsetree0 doesn't have location info, so extract just the string
            current::ArgLabel::Labelled(s) => pt0::ArgLabel::Labelled(s.txt.clone()),
            current::ArgLabel::Optional(s) => pt0::ArgLabel::Optional(s.txt.clone()),
        }
    }

    // ========== Constant ==========

    fn map_constant(&self, c: &current::Constant) -> pt0::Constant {
        match c {
            current::Constant::Integer(s, opt) => pt0::Constant::Integer(s.clone(), *opt),
            current::Constant::Char(i) => pt0::Constant::Char(*i),
            current::Constant::String(s, opt) => {
                // OCaml maps None (regular strings) to Some("js") in parsetree0.
                // The "*j" transformation happens later in Ast_config.process_str during compilation,
                // NOT during parsetree0 conversion.
                // For parsetree0 parity, we just need to convert None -> Some("js").
                let mapped_delim = match opt {
                    None => Some("js".to_string()),
                    other => other.clone(),
                };
                pt0::Constant::String(s.clone(), mapped_delim)
            }
            current::Constant::Float(s, opt) => pt0::Constant::Float(s.clone(), *opt),
        }
    }

    // ========== Attributes ==========

    fn map_attribute(&self, attr: &current::Attribute) -> pt0::Attribute {
        let (name, payload) = attr;
        (
            self.map_loc(name),
            self.map_payload(payload),
        )
    }

    fn map_attributes(&self, attrs: &current::Attributes) -> pt0::Attributes {
        attrs.iter().map(|a| self.map_attribute(a)).collect()
    }

    fn map_payload(&self, payload: &current::Payload) -> pt0::Payload {
        match payload {
            current::Payload::PStr(items) => pt0::Payload::PStr(items.iter().map(|i| self.map_structure_item(i)).collect()),
            current::Payload::PSig(items) => pt0::Payload::PSig(items.iter().map(|i| self.map_signature_item(i)).collect()),
            current::Payload::PTyp(ty) => pt0::Payload::PTyp(Box::new(self.map_core_type(ty))),
            current::Payload::PPat(pat, guard) => {
                pt0::Payload::PPat(
                    Box::new(self.map_pattern(pat)),
                    guard.as_ref().map(|e| Box::new(self.map_expression(e))),
                )
            }
        }
    }

    // ========== Core Types ==========

    pub fn map_core_type(&self, ty: &current::CoreType) -> pt0::CoreType {
        // Handle arrow types specially to support arity wrapping
        if let current::CoreTypeDesc::Ptyp_arrow { arg, ret, arity } = &ty.ptyp_desc {
            let loc = self.loc(ty.ptyp_loc);
            // Merge outer type's attributes with arg's attributes (OCaml does this)
            let arrow_attrs: Vec<pt0::Attribute> = self.map_attributes(&ty.ptyp_attributes)
                .into_iter()
                .chain(self.map_attributes(&arg.attrs))
                .collect();

            // Create the base arrow type
            let arrow_type = pt0::CoreType {
                ptyp_desc: pt0::CoreTypeDesc::Arrow(
                    self.map_arg_label(&arg.lbl),
                    Box::new(self.map_core_type(&arg.typ)),
                    Box::new(self.map_core_type(ret)),
                ),
                ptyp_loc: loc.clone(),
                ptyp_attributes: arrow_attrs,
            };

            match arity {
                current::Arity::Unknown => arrow_type,
                current::Arity::Full(n) => {
                    // Wrap in function$ type constructor with arity variant
                    // Creates: function$<arrow_type, [ `Has_arity{N} ]>
                    let arity_string = format!("Has_arity{}", n);
                    let arity_type = pt0::CoreType {
                        ptyp_desc: pt0::CoreTypeDesc::Variant(
                            vec![pt0::RowField::Rtag(
                                OldLocated::new(arity_string, loc.clone()),
                                vec![],   // no attributes
                                true,     // constant (no payload)
                                vec![],   // no types
                            )],
                            pt0::ClosedFlag::Closed,
                            None,
                        ),
                        ptyp_loc: loc.clone(),
                        ptyp_attributes: vec![],
                    };
                    pt0::CoreType {
                        ptyp_desc: pt0::CoreTypeDesc::Constr(
                            OldLocated::new(
                                crate::parser::longident::Longident::Lident("function$".to_string()),
                                loc.clone(),
                            ),
                            vec![arrow_type, arity_type],
                        ),
                        ptyp_loc: loc,
                        ptyp_attributes: vec![],
                    }
                }
            }
        } else {
            pt0::CoreType {
                ptyp_desc: self.map_core_type_desc(&ty.ptyp_desc),
                ptyp_loc: self.loc(ty.ptyp_loc),
                ptyp_attributes: self.map_attributes(&ty.ptyp_attributes),
            }
        }
    }

    fn map_core_type_desc(&self, desc: &current::CoreTypeDesc) -> pt0::CoreTypeDesc {
        match desc {
            current::CoreTypeDesc::Ptyp_any => pt0::CoreTypeDesc::Any,
            current::CoreTypeDesc::Ptyp_var(s) => pt0::CoreTypeDesc::Var(s.clone()),
            current::CoreTypeDesc::Ptyp_arrow { arg, ret, arity: _ } => {
                // This branch should not normally be reached since map_core_type handles arrows
                // But keep it for completeness - treat as no arity
                pt0::CoreTypeDesc::Arrow(
                    self.map_arg_label(&arg.lbl),
                    Box::new(self.map_core_type(&arg.typ)),
                    Box::new(self.map_core_type(ret)),
                )
            }
            current::CoreTypeDesc::Ptyp_tuple(types) => {
                pt0::CoreTypeDesc::Tuple(types.iter().map(|t| self.map_core_type(t)).collect())
            }
            current::CoreTypeDesc::Ptyp_constr(lid, types) => {
                pt0::CoreTypeDesc::Constr(
                    self.map_loc(lid),
                    types.iter().map(|t| self.map_core_type(t)).collect(),
                )
            }
            current::CoreTypeDesc::Ptyp_object(fields, flag) => {
                pt0::CoreTypeDesc::Object(
                    fields.iter().map(|f| self.map_object_field(f)).collect(),
                    self.map_closed_flag(*flag),
                )
            }
            current::CoreTypeDesc::Ptyp_alias(ty, s) => {
                pt0::CoreTypeDesc::Alias(Box::new(self.map_core_type(ty)), s.clone())
            }
            current::CoreTypeDesc::Ptyp_variant(fields, flag, labels) => {
                pt0::CoreTypeDesc::Variant(
                    fields.iter().map(|f| self.map_row_field(f)).collect(),
                    self.map_closed_flag(*flag),
                    labels.clone(),
                )
            }
            current::CoreTypeDesc::Ptyp_poly(vars, ty) => {
                pt0::CoreTypeDesc::Poly(
                    vars.iter()
                        .map(|v| self.map_loc(v))
                        .collect(),
                    Box::new(self.map_core_type(ty)),
                )
            }
            current::CoreTypeDesc::Ptyp_package(pkg) => {
                pt0::CoreTypeDesc::Package(self.map_package_type(pkg))
            }
            current::CoreTypeDesc::Ptyp_extension(ext) => {
                pt0::CoreTypeDesc::Extension(self.map_extension(ext))
            }
        }
    }

    fn map_package_type(&self, pkg: &current::PackageType) -> pt0::PackageType {
        let (lid, constraints) = pkg;
        (
            self.map_loc(lid),
            constraints
                .iter()
                .map(|(lid, ty)| (self.map_loc(lid), self.map_core_type(ty)))
                .collect(),
        )
    }

    fn map_row_field(&self, field: &current::RowField) -> pt0::RowField {
        match field {
            current::RowField::Rtag(label, attrs, b, types) => {
                pt0::RowField::Rtag(
                    self.map_loc(label),
                    self.map_attributes(attrs),
                    *b,
                    types.iter().map(|t| self.map_core_type(t)).collect(),
                )
            }
            current::RowField::Rinherit(ty) => pt0::RowField::Rinherit(self.map_core_type(ty)),
        }
    }

    fn map_object_field(&self, field: &current::ObjectField) -> pt0::ObjectField {
        match field {
            current::ObjectField::Otag(label, attrs, ty) => {
                pt0::ObjectField::Otag(
                    self.map_loc(label),
                    self.map_attributes(attrs),
                    self.map_core_type(ty),
                )
            }
            current::ObjectField::Oinherit(ty) => pt0::ObjectField::Oinherit(self.map_core_type(ty)),
        }
    }

    fn map_extension(&self, ext: &current::Extension) -> pt0::Extension {
        let (name, payload) = ext;
        (
            self.map_loc(name),
            self.map_payload(payload),
        )
    }

    // ========== Patterns ==========

    pub fn map_pattern(&self, pat: &current::Pattern) -> pt0::Pattern {
        pt0::Pattern {
            ppat_desc: self.map_pattern_desc(&pat.ppat_desc),
            ppat_loc: self.loc(pat.ppat_loc),
            ppat_attributes: self.map_attributes(&pat.ppat_attributes),
        }
    }

    fn map_pattern_desc(&self, desc: &current::PatternDesc) -> pt0::PatternDesc {
        match desc {
            current::PatternDesc::Ppat_any => pt0::PatternDesc::Any,
            current::PatternDesc::Ppat_var(s) => {
                pt0::PatternDesc::Var(self.map_loc(s))
            }
            current::PatternDesc::Ppat_alias(pat, s) => {
                pt0::PatternDesc::Alias(Box::new(self.map_pattern(pat)), self.map_loc(s))
            }
            current::PatternDesc::Ppat_constant(c) => pt0::PatternDesc::Constant(self.map_constant(c)),
            current::PatternDesc::Ppat_interval(c1, c2) => {
                pt0::PatternDesc::Interval(self.map_constant(c1), self.map_constant(c2))
            }
            current::PatternDesc::Ppat_tuple(pats) => {
                pt0::PatternDesc::Tuple(pats.iter().map(|p| self.map_pattern(p)).collect())
            }
            current::PatternDesc::Ppat_construct(lid, opt) => {
                pt0::PatternDesc::Construct(
                    self.map_loc(lid),
                    opt.as_ref().map(|p| Box::new(self.map_pattern(p))),
                )
            }
            current::PatternDesc::Ppat_variant(label, opt) => {
                pt0::PatternDesc::Variant(label.clone(), opt.as_ref().map(|p| Box::new(self.map_pattern(p))))
            }
            current::PatternDesc::Ppat_record(fields, flag) => {
                // Use RecordElement with 3 fields (lid, x, opt) for current parsetree format
                pt0::PatternDesc::Record(
                    fields
                        .iter()
                        .map(|f| pt0::RecordElement {
                            lid: self.map_loc(&f.lid),
                            x: self.map_pattern(&f.pat),
                            opt: f.opt,
                        })
                        .collect(),
                    self.map_closed_flag(*flag),
                )
            }
            current::PatternDesc::Ppat_array(pats) => {
                pt0::PatternDesc::Array(pats.iter().map(|p| self.map_pattern(p)).collect())
            }
            current::PatternDesc::Ppat_or(p1, p2) => {
                pt0::PatternDesc::Or(Box::new(self.map_pattern(p1)), Box::new(self.map_pattern(p2)))
            }
            current::PatternDesc::Ppat_constraint(pat, ty) => {
                pt0::PatternDesc::Constraint(Box::new(self.map_pattern(pat)), Box::new(self.map_core_type(ty)))
            }
            current::PatternDesc::Ppat_type(lid) => {
                pt0::PatternDesc::Type(self.map_loc(lid))
            }
            current::PatternDesc::Ppat_unpack(s) => {
                pt0::PatternDesc::Unpack(self.map_loc(s))
            }
            current::PatternDesc::Ppat_exception(pat) => {
                pt0::PatternDesc::Exception(Box::new(self.map_pattern(pat)))
            }
            current::PatternDesc::Ppat_extension(ext) => pt0::PatternDesc::Extension(self.map_extension(ext)),
            current::PatternDesc::Ppat_open(lid, pat) => {
                pt0::PatternDesc::Open(self.map_loc(lid), Box::new(self.map_pattern(pat)))
            }
        }
    }

    // ========== Expressions ==========

    pub fn map_expression(&self, expr: &current::Expression) -> pt0::Expression {
        // Check if this is a JSX element that needs the [@JSX] attribute
        let is_jsx = matches!(&expr.pexp_desc, current::ExpressionDesc::Pexp_jsx_element(_));
        let mut attrs = self.map_attributes(&expr.pexp_attributes);
        if is_jsx {
            attrs.insert(0, self.mk_jsx_attr(expr.pexp_loc));
        }
        pt0::Expression {
            pexp_desc: self.map_expression_desc(&expr.pexp_desc, expr.pexp_loc),
            pexp_loc: self.loc(expr.pexp_loc),
            pexp_attributes: attrs,
        }
    }

    fn map_expression_desc(&self, desc: &current::ExpressionDesc, loc: LocIdx) -> pt0::ExpressionDesc {
        match desc {
            current::ExpressionDesc::Pexp_ident(lid) => {
                pt0::ExpressionDesc::Ident(self.map_loc(lid))
            }
            current::ExpressionDesc::Pexp_constant(c) => {
                pt0::ExpressionDesc::Constant(self.map_constant(c))
            }
            current::ExpressionDesc::Pexp_let(flag, bindings, body) => {
                pt0::ExpressionDesc::Let(
                    self.map_rec_flag(*flag),
                    bindings.iter().map(|vb| self.map_value_binding(vb)).collect(),
                    Box::new(self.map_expression(body)),
                )
            }
            current::ExpressionDesc::Pexp_fun {
                arg_label,
                default,
                lhs,
                rhs,
                arity,
                is_async,
            } => {
                // Output current parsetree format with arity and is_async fields
                // Convert Arity enum to Option<i32>
                let arity_opt = match arity {
                    current::Arity::Full(n) => Some(*n as i32),
                    current::Arity::Unknown => None,
                };
                pt0::ExpressionDesc::Fun {
                    arg_label: self.map_arg_label(arg_label),
                    default: default.as_ref().map(|e| Box::new(self.map_expression(e))),
                    lhs: Box::new(self.map_pattern(lhs)),
                    rhs: Box::new(self.map_expression(rhs)),
                    arity: arity_opt,
                    is_async: *is_async,
                }
            }
            current::ExpressionDesc::Pexp_apply {
                funct,
                args,
                partial,
                transformed_jsx,
            } => {
                // Convert operators for parsetree0 compatibility
                let mapped_funct = match &funct.pexp_desc {
                    current::ExpressionDesc::Pexp_ident(lid)
                        if args.len() == 2
                            && matches!(&args[0].0, current::ArgLabel::Nolabel)
                            && matches!(&args[1].0, current::ArgLabel::Nolabel) =>
                    {
                        if let crate::parser::longident::Longident::Lident(op) = &lid.txt {
                            let new_op = match op.as_str() {
                                "->" => Some("|."),
                                "++" => Some("^"),
                                "!=" => Some("<>"),
                                "!==" => Some("!="),
                                "===" => Some("=="),
                                "==" => Some("="),
                                _ => None,
                            };
                            if let Some(new_name) = new_op {
                                let new_lid = Located {
                                    txt: crate::parser::longident::Longident::Lident(new_name.to_string()),
                                    loc: lid.loc,
                                };
                                current::Expression {
                                    pexp_desc: current::ExpressionDesc::Pexp_ident(new_lid),
                                    pexp_loc: funct.pexp_loc,
                                    pexp_attributes: funct.pexp_attributes.clone(),
                                }
                            } else {
                                funct.as_ref().clone()
                            }
                        } else {
                            funct.as_ref().clone()
                        }
                    }
                    _ => funct.as_ref().clone(),
                };
                pt0::ExpressionDesc::Apply {
                    funct: Box::new(self.map_expression(&mapped_funct)),
                    args: args
                        .iter()
                        .map(|(label, e)| (self.map_arg_label(label), self.map_expression(e)))
                        .collect(),
                    partial: *partial,
                    transformed_jsx: *transformed_jsx,
                }
            }
            current::ExpressionDesc::Pexp_match(e, cases) => {
                pt0::ExpressionDesc::Match(
                    Box::new(self.map_expression(e)),
                    cases.iter().map(|c| self.map_case(c)).collect(),
                )
            }
            current::ExpressionDesc::Pexp_try(e, cases) => {
                pt0::ExpressionDesc::Try(
                    Box::new(self.map_expression(e)),
                    cases.iter().map(|c| self.map_case(c)).collect(),
                )
            }
            current::ExpressionDesc::Pexp_tuple(exprs) => {
                pt0::ExpressionDesc::Tuple(exprs.iter().map(|e| self.map_expression(e)).collect())
            }
            current::ExpressionDesc::Pexp_construct(lid, opt) => {
                pt0::ExpressionDesc::Construct(
                    self.map_loc(lid),
                    opt.as_ref().map(|e| Box::new(self.map_expression(e))),
                )
            }
            current::ExpressionDesc::Pexp_variant(label, opt) => {
                pt0::ExpressionDesc::Variant(
                    label.clone(),
                    opt.as_ref().map(|e| Box::new(self.map_expression(e))),
                )
            }
            current::ExpressionDesc::Pexp_record(fields, spread) => {
                // Use RecordElement with 3 fields (lid, x, opt) for current parsetree format
                pt0::ExpressionDesc::Record(
                    fields
                        .iter()
                        .map(|f| pt0::RecordElement {
                            lid: self.map_loc(&f.lid),
                            x: self.map_expression(&f.expr),
                            opt: f.opt,
                        })
                        .collect(),
                    spread.as_ref().map(|e| Box::new(self.map_expression(e))),
                )
            }
            current::ExpressionDesc::Pexp_field(e, lid) => {
                pt0::ExpressionDesc::Field(
                    Box::new(self.map_expression(e)),
                    self.map_loc(lid),
                )
            }
            current::ExpressionDesc::Pexp_setfield(e1, lid, e2) => {
                pt0::ExpressionDesc::Setfield(
                    Box::new(self.map_expression(e1)),
                    self.map_loc(lid),
                    Box::new(self.map_expression(e2)),
                )
            }
            current::ExpressionDesc::Pexp_array(exprs) => {
                pt0::ExpressionDesc::Array(exprs.iter().map(|e| self.map_expression(e)).collect())
            }
            current::ExpressionDesc::Pexp_ifthenelse(e1, e2, e3) => {
                pt0::ExpressionDesc::Ifthenelse(
                    Box::new(self.map_expression(e1)),
                    Box::new(self.map_expression(e2)),
                    e3.as_ref().map(|e| Box::new(self.map_expression(e))),
                )
            }
            current::ExpressionDesc::Pexp_sequence(e1, e2) => {
                pt0::ExpressionDesc::Sequence(
                    Box::new(self.map_expression(e1)),
                    Box::new(self.map_expression(e2)),
                )
            }
            current::ExpressionDesc::Pexp_while(e1, e2) => {
                pt0::ExpressionDesc::While(Box::new(self.map_expression(e1)), Box::new(self.map_expression(e2)))
            }
            current::ExpressionDesc::Pexp_for(pat, e1, e2, flag, e3) => {
                pt0::ExpressionDesc::For(
                    Box::new(self.map_pattern(pat)),
                    Box::new(self.map_expression(e1)),
                    Box::new(self.map_expression(e2)),
                    self.map_direction_flag(*flag),
                    Box::new(self.map_expression(e3)),
                )
            }
            current::ExpressionDesc::Pexp_constraint(e, ty) => {
                pt0::ExpressionDesc::Constraint(
                    Box::new(self.map_expression(e)),
                    Box::new(self.map_core_type(ty)),
                )
            }
            current::ExpressionDesc::Pexp_coerce(e, _, ty) => {
                // parsetree0 has unit instead of Option<CoreType> for the middle arg
                pt0::ExpressionDesc::Coerce(Box::new(self.map_expression(e)), (), Box::new(self.map_core_type(ty)))
            }
            current::ExpressionDesc::Pexp_send(e, label) => {
                pt0::ExpressionDesc::Send(Box::new(self.map_expression(e)), self.map_loc(label))
            }
            current::ExpressionDesc::Pexp_letmodule(name, me, body) => {
                pt0::ExpressionDesc::Letmodule(
                    self.map_loc(name),
                    Box::new(self.map_module_expr(me)),
                    Box::new(self.map_expression(body)),
                )
            }
            current::ExpressionDesc::Pexp_letexception(ext, body) => {
                pt0::ExpressionDesc::Letexception(
                    self.map_extension_constructor(ext),
                    Box::new(self.map_expression(body)),
                )
            }
            current::ExpressionDesc::Pexp_assert(e) => {
                pt0::ExpressionDesc::Assert(Box::new(self.map_expression(e)))
            }
            current::ExpressionDesc::Pexp_newtype(name, body) => {
                pt0::ExpressionDesc::Newtype(self.map_loc(name), Box::new(self.map_expression(body)))
            }
            current::ExpressionDesc::Pexp_pack(me) => {
                pt0::ExpressionDesc::Pack(Box::new(self.map_module_expr(me)))
            }
            current::ExpressionDesc::Pexp_open(flag, lid, body) => {
                pt0::ExpressionDesc::Open(
                    self.map_override_flag(*flag),
                    self.map_loc(lid),
                    Box::new(self.map_expression(body)),
                )
            }
            current::ExpressionDesc::Pexp_extension(ext) => {
                pt0::ExpressionDesc::Extension(self.map_extension(ext))
            }
            current::ExpressionDesc::Pexp_await(e) => {
                // Convert await to inner expression with [@res.await] attribute
                let mut inner = self.map_expression(e);
                inner.pexp_attributes.insert(0, self.mk_await_attr(loc));
                inner.pexp_desc
            }
            current::ExpressionDesc::Pexp_jsx_element(jsx) => {
                // Convert JSX element to old-style function application with [@JSX] attribute
                let full_loc = self.loc(loc);
                match jsx {
                    current::JsxElement::Fragment(fragment) => {
                        // Fragment becomes a list with JSX attribute
                        let list_expr = self.map_jsx_children_to_list(&fragment.children, loc);
                        // The JSX attribute will be added by the caller
                        list_expr.pexp_desc
                    }
                    current::JsxElement::Unary(unary) => {
                        // Unary element: <Tag props /> -> Tag(~props, ~children=[], ())
                        let tag_ident = self.jsx_tag_to_longident(&unary.tag_name.txt);
                        let tag_loc = self.loc(unary.tag_name.loc);
                        let mapped_props = self.map_jsx_props(&unary.props, loc);
                        let children_expr = self.mk_nil_expr(loc);
                        let unit_expr = self.mk_unit_expr(loc);

                        let mut args = mapped_props;
                        args.push((pt0::ArgLabel::Labelled("children".to_string()), children_expr));
                        args.push((pt0::ArgLabel::Nolabel, unit_expr));

                        pt0::ExpressionDesc::Apply {
                            funct: Box::new(pt0::Expression {
                                pexp_desc: pt0::ExpressionDesc::Ident(OldLocated::new(
                                    tag_ident,
                                    tag_loc.clone(),
                                )),
                                pexp_loc: tag_loc,
                                pexp_attributes: vec![],
                            }),
                            args,
                            partial: false,
                            transformed_jsx: false,
                        }
                    }
                    current::JsxElement::Container(container) => {
                        // Container element: <Tag props>children</Tag> -> Tag(~props, ~children=list{...}, ())
                        let tag_ident = self.jsx_tag_to_longident(&container.tag_name_start.txt);
                        let tag_loc = self.loc(container.tag_name_start.loc);
                        let mapped_props = self.map_jsx_props(&container.props, loc);
                        let children_expr = self.map_jsx_children_to_list(&container.children, loc);
                        let unit_expr = self.mk_unit_expr(loc);

                        let mut args = mapped_props;
                        args.push((pt0::ArgLabel::Labelled("children".to_string()), children_expr));
                        args.push((pt0::ArgLabel::Nolabel, unit_expr));

                        pt0::ExpressionDesc::Apply {
                            funct: Box::new(pt0::Expression {
                                pexp_desc: pt0::ExpressionDesc::Ident(OldLocated::new(
                                    tag_ident,
                                    tag_loc.clone(),
                                )),
                                pexp_loc: tag_loc,
                                pexp_attributes: vec![],
                            }),
                            args,
                            partial: false,
                            transformed_jsx: false,
                        }
                    }
                }
            }
        }
    }

    fn map_case(&self, case: &current::Case) -> pt0::Case {
        pt0::Case {
            pc_lhs: self.map_pattern(&case.pc_lhs),
            pc_guard: case.pc_guard.as_ref().map(|e| self.map_expression(e)),
            pc_rhs: self.map_expression(&case.pc_rhs),
        }
    }

    fn map_value_binding(&self, vb: &current::ValueBinding) -> pt0::ValueBinding {
        pt0::ValueBinding {
            pvb_pat: self.map_pattern(&vb.pvb_pat),
            pvb_expr: self.map_expression(&vb.pvb_expr),
            pvb_attributes: self.map_attributes(&vb.pvb_attributes),
            pvb_loc: self.loc(vb.pvb_loc),
        }
    }

    // ========== Type declarations ==========

    fn map_type_declaration(&self, td: &current::TypeDeclaration) -> pt0::TypeDeclaration {
        pt0::TypeDeclaration {
            ptype_name: self.map_loc(&td.ptype_name),
            ptype_params: td
                .ptype_params
                .iter()
                .map(|(ty, var)| (self.map_core_type(ty), self.map_variance(*var)))
                .collect(),
            ptype_cstrs: td
                .ptype_cstrs
                .iter()
                .map(|(ty1, ty2, loc)| (self.map_core_type(ty1), self.map_core_type(ty2), self.loc(*loc)))
                .collect(),
            ptype_kind: self.map_type_kind(&td.ptype_kind),
            ptype_private: self.map_private_flag(td.ptype_private),
            ptype_manifest: td.ptype_manifest.as_ref().map(|t| self.map_core_type(t)),
            ptype_attributes: self.map_attributes(&td.ptype_attributes),
            ptype_loc: self.loc(td.ptype_loc),
        }
    }

    fn map_type_kind(&self, kind: &current::TypeKind) -> pt0::TypeKind {
        match kind {
            current::TypeKind::Ptype_abstract => pt0::TypeKind::Abstract,
            current::TypeKind::Ptype_variant(ctors) => {
                pt0::TypeKind::Variant(ctors.iter().map(|c| self.map_constructor_declaration(c)).collect())
            }
            current::TypeKind::Ptype_record(labels) => {
                pt0::TypeKind::Record(labels.iter().map(|l| self.map_label_declaration(l)).collect())
            }
            current::TypeKind::Ptype_open => pt0::TypeKind::Open,
        }
    }

    fn map_label_declaration(&self, ld: &current::LabelDeclaration) -> pt0::LabelDeclaration {
        pt0::LabelDeclaration {
            pld_name: self.map_loc(&ld.pld_name),
            pld_mutable: self.map_mutable_flag(ld.pld_mutable),
            pld_type: self.map_core_type(&ld.pld_type),
            pld_loc: self.loc(ld.pld_loc),
            pld_attributes: self.map_attributes(&ld.pld_attributes),
        }
    }

    fn map_constructor_declaration(&self, cd: &current::ConstructorDeclaration) -> pt0::ConstructorDeclaration {
        pt0::ConstructorDeclaration {
            pcd_name: self.map_loc(&cd.pcd_name),
            pcd_args: self.map_constructor_arguments(&cd.pcd_args),
            pcd_res: cd.pcd_res.as_ref().map(|t| self.map_core_type(t)),
            pcd_loc: self.loc(cd.pcd_loc),
            pcd_attributes: self.map_attributes(&cd.pcd_attributes),
        }
    }

    fn map_constructor_arguments(&self, args: &current::ConstructorArguments) -> pt0::ConstructorArguments {
        match args {
            current::ConstructorArguments::Pcstr_tuple(types) => {
                pt0::ConstructorArguments::Tuple(types.iter().map(|t| self.map_core_type(t)).collect())
            }
            current::ConstructorArguments::Pcstr_record(labels) => {
                pt0::ConstructorArguments::Record(labels.iter().map(|l| self.map_label_declaration(l)).collect())
            }
        }
    }

    fn map_extension_constructor(&self, ec: &current::ExtensionConstructor) -> pt0::ExtensionConstructor {
        pt0::ExtensionConstructor {
            pext_name: self.map_loc(&ec.pext_name),
            pext_kind: self.map_extension_constructor_kind(&ec.pext_kind),
            pext_loc: self.loc(ec.pext_loc),
            pext_attributes: self.map_attributes(&ec.pext_attributes),
        }
    }

    fn map_extension_constructor_kind(&self, kind: &current::ExtensionConstructorKind) -> pt0::ExtensionConstructorKind {
        match kind {
            current::ExtensionConstructorKind::Pext_decl(args, res) => {
                pt0::ExtensionConstructorKind::Decl(
                    self.map_constructor_arguments(args),
                    res.as_ref().map(|t| self.map_core_type(t)),
                )
            }
            current::ExtensionConstructorKind::Pext_rebind(lid) => {
                pt0::ExtensionConstructorKind::Rebind(self.map_loc(lid))
            }
        }
    }

    fn map_type_extension(&self, te: &current::TypeExtension) -> pt0::TypeExtension {
        pt0::TypeExtension {
            ptyext_path: self.map_loc(&te.ptyext_path),
            ptyext_params: te
                .ptyext_params
                .iter()
                .map(|(ty, var)| (self.map_core_type(ty), self.map_variance(*var)))
                .collect(),
            ptyext_constructors: te
                .ptyext_constructors
                .iter()
                .map(|ec| self.map_extension_constructor(ec))
                .collect(),
            ptyext_private: self.map_private_flag(te.ptyext_private),
            ptyext_attributes: self.map_attributes(&te.ptyext_attributes),
        }
    }

    // ========== Module types ==========

    fn map_module_type(&self, mt: &current::ModuleType) -> pt0::ModuleType {
        pt0::ModuleType {
            pmty_desc: self.map_module_type_desc(&mt.pmty_desc),
            pmty_loc: self.loc(mt.pmty_loc),
            pmty_attributes: self.map_attributes(&mt.pmty_attributes),
        }
    }

    fn map_module_type_desc(&self, desc: &current::ModuleTypeDesc) -> pt0::ModuleTypeDesc {
        match desc {
            current::ModuleTypeDesc::Pmty_ident(lid) => {
                pt0::ModuleTypeDesc::Ident(self.map_loc(lid))
            }
            current::ModuleTypeDesc::Pmty_signature(items) => {
                pt0::ModuleTypeDesc::Signature(items.iter().map(|i| self.map_signature_item(i)).collect())
            }
            current::ModuleTypeDesc::Pmty_functor(name, mty, body) => {
                pt0::ModuleTypeDesc::Functor(
                    self.map_loc(name),
                    mty.as_ref().map(|m| Box::new(self.map_module_type(m))),
                    Box::new(self.map_module_type(body)),
                )
            }
            current::ModuleTypeDesc::Pmty_with(mty, constraints) => {
                pt0::ModuleTypeDesc::With(
                    Box::new(self.map_module_type(mty)),
                    constraints.iter().map(|wc| self.map_with_constraint(wc)).collect(),
                )
            }
            current::ModuleTypeDesc::Pmty_typeof(me) => {
                pt0::ModuleTypeDesc::Typeof(Box::new(self.map_module_expr(me)))
            }
            current::ModuleTypeDesc::Pmty_extension(ext) => {
                pt0::ModuleTypeDesc::Extension(self.map_extension(ext))
            }
            current::ModuleTypeDesc::Pmty_alias(lid) => {
                pt0::ModuleTypeDesc::Alias(self.map_loc(lid))
            }
        }
    }

    fn map_with_constraint(&self, wc: &current::WithConstraint) -> pt0::WithConstraint {
        match wc {
            current::WithConstraint::Pwith_type(lid, td) => {
                pt0::WithConstraint::Type(self.map_loc(lid), self.map_type_declaration(td))
            }
            current::WithConstraint::Pwith_module(lid1, lid2) => {
                pt0::WithConstraint::Module(
                    self.map_loc(lid1),
                    self.map_loc(lid2),
                )
            }
            current::WithConstraint::Pwith_typesubst(lid, td) => {
                pt0::WithConstraint::TypeSubst(self.map_loc(lid), self.map_type_declaration(td))
            }
            current::WithConstraint::Pwith_modsubst(lid1, lid2) => {
                pt0::WithConstraint::ModSubst(
                    self.map_loc(lid1),
                    self.map_loc(lid2),
                )
            }
        }
    }

    // ========== Module expressions ==========

    fn map_module_expr(&self, me: &current::ModuleExpr) -> pt0::ModuleExpr {
        pt0::ModuleExpr {
            pmod_desc: self.map_module_expr_desc(&me.pmod_desc),
            pmod_loc: self.loc(me.pmod_loc),
            pmod_attributes: self.map_attributes(&me.pmod_attributes),
        }
    }

    fn map_module_expr_desc(&self, desc: &current::ModuleExprDesc) -> pt0::ModuleExprDesc {
        match desc {
            current::ModuleExprDesc::Pmod_ident(lid) => {
                pt0::ModuleExprDesc::Ident(self.map_loc(lid))
            }
            current::ModuleExprDesc::Pmod_structure(items) => {
                pt0::ModuleExprDesc::Structure(items.iter().map(|i| self.map_structure_item(i)).collect())
            }
            current::ModuleExprDesc::Pmod_functor(name, mty, body) => {
                pt0::ModuleExprDesc::Functor(
                    self.map_loc(name),
                    mty.as_ref().map(|m| Box::new(self.map_module_type(m))),
                    Box::new(self.map_module_expr(body)),
                )
            }
            current::ModuleExprDesc::Pmod_apply(me1, me2) => {
                pt0::ModuleExprDesc::Apply(
                    Box::new(self.map_module_expr(me1)),
                    Box::new(self.map_module_expr(me2)),
                )
            }
            current::ModuleExprDesc::Pmod_constraint(me, mty) => {
                pt0::ModuleExprDesc::Constraint(
                    Box::new(self.map_module_expr(me)),
                    Box::new(self.map_module_type(mty)),
                )
            }
            current::ModuleExprDesc::Pmod_unpack(e) => {
                pt0::ModuleExprDesc::Unpack(Box::new(self.map_expression(e)))
            }
            current::ModuleExprDesc::Pmod_extension(ext) => {
                pt0::ModuleExprDesc::Extension(self.map_extension(ext))
            }
        }
    }

    fn map_module_binding(&self, mb: &current::ModuleBinding) -> pt0::ModuleBinding {
        pt0::ModuleBinding {
            pmb_name: self.map_loc(&mb.pmb_name),
            pmb_expr: self.map_module_expr(&mb.pmb_expr),
            pmb_attributes: self.map_attributes(&mb.pmb_attributes),
            pmb_loc: self.loc(mb.pmb_loc),
        }
    }

    fn map_module_declaration(&self, md: &current::ModuleDeclaration) -> pt0::ModuleDeclaration {
        pt0::ModuleDeclaration {
            pmd_name: self.map_loc(&md.pmd_name),
            pmd_type: self.map_module_type(&md.pmd_type),
            pmd_attributes: self.map_attributes(&md.pmd_attributes),
            pmd_loc: self.loc(md.pmd_loc),
        }
    }

    fn map_module_type_declaration(&self, mtd: &current::ModuleTypeDeclaration) -> pt0::ModuleTypeDeclaration {
        pt0::ModuleTypeDeclaration {
            pmtd_name: self.map_loc(&mtd.pmtd_name),
            pmtd_type: mtd.pmtd_type.as_ref().map(|mt| self.map_module_type(mt)),
            pmtd_attributes: self.map_attributes(&mtd.pmtd_attributes),
            pmtd_loc: self.loc(mtd.pmtd_loc),
        }
    }

    fn map_open_description(&self, od: &current::OpenDescription) -> pt0::OpenDescription {
        pt0::OpenDescription {
            popen_lid: self.map_loc(&od.popen_lid),
            popen_override: self.map_override_flag(od.popen_override),
            popen_loc: self.loc(od.popen_loc),
            popen_attributes: self.map_attributes(&od.popen_attributes),
        }
    }

    fn map_include_declaration(&self, id: &current::IncludeDeclaration) -> pt0::IncludeDeclaration {
        pt0::IncludeInfos {
            pincl_mod: self.map_module_expr(&id.pincl_mod),
            pincl_loc: self.loc(id.pincl_loc),
            pincl_attributes: self.map_attributes(&id.pincl_attributes),
        }
    }

    fn map_include_description(&self, id: &current::IncludeDescription) -> pt0::IncludeDescription {
        pt0::IncludeInfos {
            pincl_mod: self.map_module_type(&id.pincl_mod),
            pincl_loc: self.loc(id.pincl_loc),
            pincl_attributes: self.map_attributes(&id.pincl_attributes),
        }
    }

    fn map_value_description(&self, vd: &current::ValueDescription) -> pt0::ValueDescription {
        pt0::ValueDescription {
            pval_name: self.map_loc(&vd.pval_name),
            pval_type: self.map_core_type(&vd.pval_type),
            pval_prim: vd.pval_prim.clone(),
            pval_attributes: self.map_attributes(&vd.pval_attributes),
            pval_loc: self.loc(vd.pval_loc),
        }
    }

    // ========== Structure items ==========

    fn map_structure_item(&self, item: &current::StructureItem) -> pt0::StructureItem {
        pt0::StructureItem {
            pstr_desc: self.map_structure_item_desc(&item.pstr_desc),
            pstr_loc: self.loc(item.pstr_loc),
        }
    }

    fn map_structure_item_desc(&self, desc: &current::StructureItemDesc) -> pt0::StructureItemDesc {
        match desc {
            current::StructureItemDesc::Pstr_eval(e, attrs) => {
                pt0::StructureItemDesc::Eval(self.map_expression(e), self.map_attributes(attrs))
            }
            current::StructureItemDesc::Pstr_value(flag, bindings) => {
                pt0::StructureItemDesc::Value(
                    self.map_rec_flag(*flag),
                    bindings.iter().map(|vb| self.map_value_binding(vb)).collect(),
                )
            }
            current::StructureItemDesc::Pstr_primitive(vd) => {
                pt0::StructureItemDesc::Primitive(self.map_value_description(vd))
            }
            current::StructureItemDesc::Pstr_type(flag, decls) => {
                pt0::StructureItemDesc::Type(
                    self.map_rec_flag(*flag),
                    decls.iter().map(|td| self.map_type_declaration(td)).collect(),
                )
            }
            current::StructureItemDesc::Pstr_typext(te) => {
                pt0::StructureItemDesc::Typext(self.map_type_extension(te))
            }
            current::StructureItemDesc::Pstr_exception(ec) => {
                pt0::StructureItemDesc::Exception(self.map_extension_constructor(ec))
            }
            current::StructureItemDesc::Pstr_module(mb) => {
                pt0::StructureItemDesc::Module(self.map_module_binding(mb))
            }
            current::StructureItemDesc::Pstr_recmodule(mbs) => {
                pt0::StructureItemDesc::Recmodule(mbs.iter().map(|mb| self.map_module_binding(mb)).collect())
            }
            current::StructureItemDesc::Pstr_modtype(mtd) => {
                pt0::StructureItemDesc::Modtype(self.map_module_type_declaration(mtd))
            }
            current::StructureItemDesc::Pstr_open(od) => {
                pt0::StructureItemDesc::Open(self.map_open_description(od))
            }
            current::StructureItemDesc::Pstr_include(ii) => {
                pt0::StructureItemDesc::Include(self.map_include_declaration(ii))
            }
            current::StructureItemDesc::Pstr_attribute(attr) => {
                pt0::StructureItemDesc::Attribute(self.map_attribute(attr))
            }
            current::StructureItemDesc::Pstr_extension(ext, attrs) => {
                pt0::StructureItemDesc::Extension(self.map_extension(ext), self.map_attributes(attrs))
            }
        }
    }

    // ========== Signature items ==========

    fn map_signature_item(&self, item: &current::SignatureItem) -> pt0::SignatureItem {
        pt0::SignatureItem {
            psig_desc: self.map_signature_item_desc(&item.psig_desc),
            psig_loc: self.loc(item.psig_loc),
        }
    }

    fn map_signature_item_desc(&self, desc: &current::SignatureItemDesc) -> pt0::SignatureItemDesc {
        match desc {
            current::SignatureItemDesc::Psig_value(vd) => {
                pt0::SignatureItemDesc::Value(self.map_value_description(vd))
            }
            current::SignatureItemDesc::Psig_type(flag, decls) => {
                pt0::SignatureItemDesc::Type(
                    self.map_rec_flag(*flag),
                    decls.iter().map(|td| self.map_type_declaration(td)).collect(),
                )
            }
            current::SignatureItemDesc::Psig_typext(te) => {
                pt0::SignatureItemDesc::Typext(self.map_type_extension(te))
            }
            current::SignatureItemDesc::Psig_exception(ec) => {
                pt0::SignatureItemDesc::Exception(self.map_extension_constructor(ec))
            }
            current::SignatureItemDesc::Psig_module(md) => {
                pt0::SignatureItemDesc::Module(self.map_module_declaration(md))
            }
            current::SignatureItemDesc::Psig_recmodule(mds) => {
                pt0::SignatureItemDesc::Recmodule(mds.iter().map(|md| self.map_module_declaration(md)).collect())
            }
            current::SignatureItemDesc::Psig_modtype(mtd) => {
                pt0::SignatureItemDesc::Modtype(self.map_module_type_declaration(mtd))
            }
            current::SignatureItemDesc::Psig_open(od) => {
                pt0::SignatureItemDesc::Open(self.map_open_description(od))
            }
            current::SignatureItemDesc::Psig_include(ii) => {
                pt0::SignatureItemDesc::Include(self.map_include_description(ii))
            }
            current::SignatureItemDesc::Psig_attribute(attr) => {
                pt0::SignatureItemDesc::Attribute(self.map_attribute(attr))
            }
            current::SignatureItemDesc::Psig_extension(ext, attrs) => {
                pt0::SignatureItemDesc::Extension(self.map_extension(ext), self.map_attributes(attrs))
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_map_rec_flag() {
        let arena = ParseArena::new();
        let mapper = Mapper::new(&arena);
        assert!(matches!(mapper.map_rec_flag(current::RecFlag::Nonrecursive), pt0::RecFlag::Nonrecursive));
        assert!(matches!(mapper.map_rec_flag(current::RecFlag::Recursive), pt0::RecFlag::Recursive));
    }

    #[test]
    fn test_map_arg_label() {
        let arena = ParseArena::new();
        let mapper = Mapper::new(&arena);

        assert!(matches!(mapper.map_arg_label(&current::ArgLabel::Nolabel), pt0::ArgLabel::Nolabel));

        let label = mapper.map_arg_label(&current::ArgLabel::Labelled(Located::mknoloc("x".to_string())));
        assert!(matches!(label, pt0::ArgLabel::Labelled(s) if s == "x"));

        let opt = mapper.map_arg_label(&current::ArgLabel::Optional(Located::mknoloc("y".to_string())));
        assert!(matches!(opt, pt0::ArgLabel::Optional(s) if s == "y"));
    }

    #[test]
    fn test_map_constant() {
        let arena = ParseArena::new();
        let mapper = Mapper::new(&arena);

        let c1 = mapper.map_constant(&current::Constant::Integer("42".to_string(), None));
        assert!(matches!(c1, pt0::Constant::Integer(s, None) if s == "42"));

        let c2 = mapper.map_constant(&current::Constant::Char(65));
        assert!(matches!(c2, pt0::Constant::Char(65)));

        // Note: None delimiter is mapped to Some("js") for OCaml parsetree0 parity
        let c3 = mapper.map_constant(&current::Constant::String("hello".to_string(), None));
        assert!(matches!(c3, pt0::Constant::String(s, Some(d)) if s == "hello" && d == "js"));
    }
}
