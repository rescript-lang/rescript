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

use crate::location::{Located, Location};
use crate::parser::ast::{self as current};

use super::parsetree0 as pt0;

/// Convert a structure from current parsetree to parsetree0
pub fn map_structure(str: &[current::StructureItem]) -> pt0::Structure {
    str.iter().map(map_structure_item).collect()
}

/// Convert a signature from current parsetree to parsetree0
pub fn map_signature(sig: &[current::SignatureItem]) -> pt0::Signature {
    sig.iter().map(map_signature_item).collect()
}

// ========== Helper functions ==========

fn mkloc<T: Clone>(txt: T, loc: &Location) -> Located<T> {
    Located {
        txt,
        loc: loc.clone(),
    }
}

fn mk_optional_attr(loc: &Location) -> pt0::Attribute {
    (
        Located::new("res.optional".to_string(), loc.clone()),
        pt0::Payload::PStr(vec![]),
    )
}

fn mk_async_attr(loc: &Location) -> pt0::Attribute {
    (
        Located::new("res.async".to_string(), loc.clone()),
        pt0::Payload::PStr(vec![]),
    )
}

fn mk_await_attr(loc: &Location) -> pt0::Attribute {
    (
        Located::new("res.await".to_string(), loc.clone()),
        pt0::Payload::PStr(vec![]),
    )
}

fn mk_partial_attr(loc: &Location) -> pt0::Attribute {
    (
        Located::new("res.partial".to_string(), loc.clone()),
        pt0::Payload::PStr(vec![]),
    )
}

#[allow(dead_code)]
fn mk_arity_attr(arity: usize, loc: &Location) -> pt0::Attribute {
    (
        Located::new("res.arity".to_string(), loc.clone()),
        pt0::Payload::PStr(vec![pt0::StructureItem {
            pstr_desc: pt0::StructureItemDesc::Eval(
                pt0::Expression {
                    pexp_desc: pt0::ExpressionDesc::Constant(pt0::Constant::Integer(
                        arity.to_string(),
                        None,
                    )),
                    pexp_loc: loc.clone(),
                    pexp_attributes: vec![],
                },
                vec![],
            ),
            pstr_loc: loc.clone(),
        }]),
    )
}

// ========== JSX helpers ==========

fn mk_jsx_attr(loc: &Location) -> pt0::Attribute {
    (
        Located::new("JSX".to_string(), loc.clone()),
        pt0::Payload::PStr(vec![]),
    )
}

fn mk_unit_expr(loc: &Location) -> pt0::Expression {
    pt0::Expression {
        pexp_desc: pt0::ExpressionDesc::Construct(
            Located::new(
                crate::parser::longident::Longident::Lident("()".to_string()),
                loc.clone(),
            ),
            None,
        ),
        pexp_loc: loc.clone(),
        pexp_attributes: vec![],
    }
}

fn mk_nil_expr(loc: &Location) -> pt0::Expression {
    pt0::Expression {
        pexp_desc: pt0::ExpressionDesc::Construct(
            Located::new(
                crate::parser::longident::Longident::Lident("[]".to_string()),
                loc.clone(),
            ),
            None,
        ),
        pexp_loc: loc.clone(),
        pexp_attributes: vec![],
    }
}

fn map_jsx_props(props: &[current::JsxProp], _loc: &Location) -> Vec<(pt0::ArgLabel, pt0::Expression)> {
    props
        .iter()
        .map(|prop| match prop {
            current::JsxProp::Punning { optional, name } => {
                let ident_expr = pt0::Expression {
                    pexp_desc: pt0::ExpressionDesc::Ident(Located::new(
                        crate::parser::longident::Longident::Lident(name.txt.clone()),
                        name.loc.clone(),
                    )),
                    pexp_loc: name.loc.clone(),
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
                (label, map_expression(value))
            }
            current::JsxProp::Spreading { expr, .. } => {
                (pt0::ArgLabel::Labelled("_spreadProps".to_string()), map_expression(expr))
            }
        })
        .collect()
}

fn jsx_tag_to_longident(tag: &current::JsxTagName) -> crate::parser::longident::Longident {
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

fn map_jsx_children_to_list(children: &[current::Expression], loc: &Location) -> pt0::Expression {
    // Convert children to a list expression (cons cells ending with [])
    let nil = mk_nil_expr(loc);
    children.iter().rev().fold(nil, |acc, child| {
        let child_expr = map_expression(child);
        pt0::Expression {
            pexp_desc: pt0::ExpressionDesc::Construct(
                Located::new(
                    crate::parser::longident::Longident::Lident("::".to_string()),
                    loc.clone(),
                ),
                Some(Box::new(pt0::Expression {
                    pexp_desc: pt0::ExpressionDesc::Tuple(vec![child_expr, acc]),
                    pexp_loc: loc.clone(),
                    pexp_attributes: vec![],
                })),
            ),
            pexp_loc: loc.clone(),
            pexp_attributes: vec![],
        }
    })
}

// ========== Flag conversions ==========

fn map_rec_flag(flag: current::RecFlag) -> pt0::RecFlag {
    match flag {
        current::RecFlag::Nonrecursive => pt0::RecFlag::Nonrecursive,
        current::RecFlag::Recursive => pt0::RecFlag::Recursive,
    }
}

fn map_direction_flag(flag: current::DirectionFlag) -> pt0::DirectionFlag {
    match flag {
        current::DirectionFlag::Upto => pt0::DirectionFlag::Upto,
        current::DirectionFlag::Downto => pt0::DirectionFlag::Downto,
    }
}

fn map_closed_flag(flag: current::ClosedFlag) -> pt0::ClosedFlag {
    match flag {
        current::ClosedFlag::Closed => pt0::ClosedFlag::Closed,
        current::ClosedFlag::Open => pt0::ClosedFlag::Open,
    }
}

fn map_override_flag(flag: current::OverrideFlag) -> pt0::OverrideFlag {
    match flag {
        current::OverrideFlag::Fresh => pt0::OverrideFlag::Fresh,
        current::OverrideFlag::Override => pt0::OverrideFlag::Override,
    }
}

fn map_mutable_flag(flag: current::MutableFlag) -> pt0::MutableFlag {
    match flag {
        current::MutableFlag::Immutable => pt0::MutableFlag::Immutable,
        current::MutableFlag::Mutable => pt0::MutableFlag::Mutable,
    }
}

fn map_private_flag(flag: current::PrivateFlag) -> pt0::PrivateFlag {
    match flag {
        current::PrivateFlag::Private => pt0::PrivateFlag::Private,
        current::PrivateFlag::Public => pt0::PrivateFlag::Public,
    }
}

fn map_variance(var: current::Variance) -> pt0::Variance {
    match var {
        current::Variance::Covariant => pt0::Variance::Covariant,
        current::Variance::Contravariant => pt0::Variance::Contravariant,
        current::Variance::Invariant => pt0::Variance::Invariant,
    }
}

fn map_arg_label(label: &current::ArgLabel) -> pt0::ArgLabel {
    match label {
        current::ArgLabel::Nolabel => pt0::ArgLabel::Nolabel,
        // parsetree0 doesn't have location info, so extract just the string
        current::ArgLabel::Labelled(s) => pt0::ArgLabel::Labelled(s.txt.clone()),
        current::ArgLabel::Optional(s) => pt0::ArgLabel::Optional(s.txt.clone()),
    }
}

// ========== Constant ==========

fn map_constant(c: &current::Constant) -> pt0::Constant {
    match c {
        current::Constant::Integer(s, opt) => pt0::Constant::Integer(s.clone(), *opt),
        current::Constant::Char(i) => pt0::Constant::Char(*i),
        current::Constant::String(s, opt) => {
            // OCaml transforms "js" delimiter to "*j" (escaped_j_delimiter) in Ast_config.process_str
            // For binary parity, we need to apply the same transformation:
            // - None or Some("js") -> Some("*j") for regular double-quoted strings
            // - Some("json") -> Some("json") (keep as is)
            // - Other delimiters -> keep as is
            let mapped_delim = match opt {
                None => Some("*j".to_string()),
                Some(d) if d == "js" => Some("*j".to_string()),
                other => other.clone(),
            };
            pt0::Constant::String(s.clone(), mapped_delim)
        }
        current::Constant::Float(s, opt) => pt0::Constant::Float(s.clone(), *opt),
    }
}

// ========== Attributes ==========

fn map_attribute(attr: &current::Attribute) -> pt0::Attribute {
    let (name, payload) = attr;
    (
        Located::new(name.txt.clone(), name.loc.clone()),
        map_payload(payload),
    )
}

fn map_attributes(attrs: &current::Attributes) -> pt0::Attributes {
    attrs.iter().map(map_attribute).collect()
}

fn map_payload(payload: &current::Payload) -> pt0::Payload {
    match payload {
        current::Payload::PStr(items) => pt0::Payload::PStr(items.iter().map(map_structure_item).collect()),
        current::Payload::PSig(items) => pt0::Payload::PSig(items.iter().map(map_signature_item).collect()),
        current::Payload::PTyp(ty) => pt0::Payload::PTyp(Box::new(map_core_type(ty))),
        current::Payload::PPat(pat, guard) => {
            pt0::Payload::PPat(
                Box::new(map_pattern(pat)),
                guard.as_ref().map(|e| Box::new(map_expression(e))),
            )
        }
    }
}

// ========== Core Types ==========

pub fn map_core_type(ty: &current::CoreType) -> pt0::CoreType {
    // Handle arrow types specially to support arity wrapping
    if let current::CoreTypeDesc::Ptyp_arrow { arg, ret, arity } = &ty.ptyp_desc {
        let loc = ty.ptyp_loc.clone();
        // Merge outer type's attributes with arg's attributes (OCaml does this)
        let arrow_attrs: Vec<pt0::Attribute> = map_attributes(&ty.ptyp_attributes)
            .into_iter()
            .chain(map_attributes(&arg.attrs))
            .collect();

        // Create the base arrow type
        let arrow_type = pt0::CoreType {
            ptyp_desc: pt0::CoreTypeDesc::Arrow(
                map_arg_label(&arg.lbl),
                Box::new(map_core_type(&arg.typ)),
                Box::new(map_core_type(ret)),
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
                            Located::new(arity_string, loc.clone()),
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
                        Located::new(
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
            ptyp_desc: map_core_type_desc(&ty.ptyp_desc),
            ptyp_loc: ty.ptyp_loc.clone(),
            ptyp_attributes: map_attributes(&ty.ptyp_attributes),
        }
    }
}

fn map_core_type_desc(desc: &current::CoreTypeDesc) -> pt0::CoreTypeDesc {
    match desc {
        current::CoreTypeDesc::Ptyp_any => pt0::CoreTypeDesc::Any,
        current::CoreTypeDesc::Ptyp_var(s) => pt0::CoreTypeDesc::Var(s.clone()),
        current::CoreTypeDesc::Ptyp_arrow { arg, ret, arity: _ } => {
            // This branch should not normally be reached since map_core_type handles arrows
            // But keep it for completeness - treat as no arity
            pt0::CoreTypeDesc::Arrow(
                map_arg_label(&arg.lbl),
                Box::new(map_core_type(&arg.typ)),
                Box::new(map_core_type(ret)),
            )
        }
        current::CoreTypeDesc::Ptyp_tuple(types) => {
            pt0::CoreTypeDesc::Tuple(types.iter().map(map_core_type).collect())
        }
        current::CoreTypeDesc::Ptyp_constr(lid, types) => {
            pt0::CoreTypeDesc::Constr(
                mkloc(lid.txt.clone(), &lid.loc),
                types.iter().map(map_core_type).collect(),
            )
        }
        current::CoreTypeDesc::Ptyp_object(fields, flag) => {
            pt0::CoreTypeDesc::Object(
                fields.iter().map(map_object_field).collect(),
                map_closed_flag(*flag),
            )
        }
        current::CoreTypeDesc::Ptyp_alias(ty, s) => {
            pt0::CoreTypeDesc::Alias(Box::new(map_core_type(ty)), s.clone())
        }
        current::CoreTypeDesc::Ptyp_variant(fields, flag, labels) => {
            pt0::CoreTypeDesc::Variant(
                fields.iter().map(map_row_field).collect(),
                map_closed_flag(*flag),
                labels.clone(),
            )
        }
        current::CoreTypeDesc::Ptyp_poly(vars, ty) => {
            pt0::CoreTypeDesc::Poly(
                vars.iter()
                    .map(|v| Located::new(v.txt.clone(), v.loc.clone()))
                    .collect(),
                Box::new(map_core_type(ty)),
            )
        }
        current::CoreTypeDesc::Ptyp_package(pkg) => {
            pt0::CoreTypeDesc::Package(map_package_type(pkg))
        }
        current::CoreTypeDesc::Ptyp_extension(ext) => {
            pt0::CoreTypeDesc::Extension(map_extension(ext))
        }
    }
}

fn map_package_type(pkg: &current::PackageType) -> pt0::PackageType {
    let (lid, constraints) = pkg;
    (
        mkloc(lid.txt.clone(), &lid.loc),
        constraints
            .iter()
            .map(|(lid, ty)| (mkloc(lid.txt.clone(), &lid.loc), map_core_type(ty)))
            .collect(),
    )
}

fn map_row_field(field: &current::RowField) -> pt0::RowField {
    match field {
        current::RowField::Rtag(label, attrs, b, types) => {
            pt0::RowField::Rtag(
                mkloc(label.txt.clone(), &label.loc),
                map_attributes(attrs),
                *b,
                types.iter().map(map_core_type).collect(),
            )
        }
        current::RowField::Rinherit(ty) => pt0::RowField::Rinherit(map_core_type(ty)),
    }
}

fn map_object_field(field: &current::ObjectField) -> pt0::ObjectField {
    match field {
        current::ObjectField::Otag(label, attrs, ty) => {
            pt0::ObjectField::Otag(
                mkloc(label.txt.clone(), &label.loc),
                map_attributes(attrs),
                map_core_type(ty),
            )
        }
        current::ObjectField::Oinherit(ty) => pt0::ObjectField::Oinherit(map_core_type(ty)),
    }
}

fn map_extension(ext: &current::Extension) -> pt0::Extension {
    let (name, payload) = ext;
    (
        Located::new(name.txt.clone(), name.loc.clone()),
        map_payload(payload),
    )
}

// ========== Patterns ==========

pub fn map_pattern(pat: &current::Pattern) -> pt0::Pattern {
    pt0::Pattern {
        ppat_desc: map_pattern_desc(&pat.ppat_desc),
        ppat_loc: pat.ppat_loc.clone(),
        ppat_attributes: map_attributes(&pat.ppat_attributes),
    }
}

fn map_pattern_desc(desc: &current::PatternDesc) -> pt0::PatternDesc {
    match desc {
        current::PatternDesc::Ppat_any => pt0::PatternDesc::Any,
        current::PatternDesc::Ppat_var(s) => {
            pt0::PatternDesc::Var(mkloc(s.txt.clone(), &s.loc))
        }
        current::PatternDesc::Ppat_alias(pat, s) => {
            pt0::PatternDesc::Alias(Box::new(map_pattern(pat)), mkloc(s.txt.clone(), &s.loc))
        }
        current::PatternDesc::Ppat_constant(c) => pt0::PatternDesc::Constant(map_constant(c)),
        current::PatternDesc::Ppat_interval(c1, c2) => {
            pt0::PatternDesc::Interval(map_constant(c1), map_constant(c2))
        }
        current::PatternDesc::Ppat_tuple(pats) => {
            pt0::PatternDesc::Tuple(pats.iter().map(map_pattern).collect())
        }
        current::PatternDesc::Ppat_construct(lid, opt) => {
            pt0::PatternDesc::Construct(
                mkloc(lid.txt.clone(), &lid.loc),
                opt.as_ref().map(|p| Box::new(map_pattern(p))),
            )
        }
        current::PatternDesc::Ppat_variant(label, opt) => {
            pt0::PatternDesc::Variant(label.clone(), opt.as_ref().map(|p| Box::new(map_pattern(p))))
        }
        current::PatternDesc::Ppat_record(fields, flag) => {
            // Use RecordElement with 3 fields (lid, x, opt) for current parsetree format
            pt0::PatternDesc::Record(
                fields
                    .iter()
                    .map(|f| pt0::RecordElement {
                        lid: mkloc(f.lid.txt.clone(), &f.lid.loc),
                        x: map_pattern(&f.pat),
                        opt: f.opt,
                    })
                    .collect(),
                map_closed_flag(*flag),
            )
        }
        current::PatternDesc::Ppat_array(pats) => {
            pt0::PatternDesc::Array(pats.iter().map(map_pattern).collect())
        }
        current::PatternDesc::Ppat_or(p1, p2) => {
            pt0::PatternDesc::Or(Box::new(map_pattern(p1)), Box::new(map_pattern(p2)))
        }
        current::PatternDesc::Ppat_constraint(pat, ty) => {
            pt0::PatternDesc::Constraint(Box::new(map_pattern(pat)), Box::new(map_core_type(ty)))
        }
        current::PatternDesc::Ppat_type(lid) => {
            pt0::PatternDesc::Type(mkloc(lid.txt.clone(), &lid.loc))
        }
        current::PatternDesc::Ppat_unpack(s) => {
            pt0::PatternDesc::Unpack(mkloc(s.txt.clone(), &s.loc))
        }
        current::PatternDesc::Ppat_exception(pat) => {
            pt0::PatternDesc::Exception(Box::new(map_pattern(pat)))
        }
        current::PatternDesc::Ppat_extension(ext) => pt0::PatternDesc::Extension(map_extension(ext)),
        current::PatternDesc::Ppat_open(lid, pat) => {
            pt0::PatternDesc::Open(mkloc(lid.txt.clone(), &lid.loc), Box::new(map_pattern(pat)))
        }
    }
}

// ========== Expressions ==========

pub fn map_expression(expr: &current::Expression) -> pt0::Expression {
    // Check if this is a JSX element that needs the [@JSX] attribute
    let is_jsx = matches!(&expr.pexp_desc, current::ExpressionDesc::Pexp_jsx_element(_));
    let mut attrs = map_attributes(&expr.pexp_attributes);
    if is_jsx {
        attrs.insert(0, mk_jsx_attr(&expr.pexp_loc));
    }
    pt0::Expression {
        pexp_desc: map_expression_desc(&expr.pexp_desc, &expr.pexp_loc),
        pexp_loc: expr.pexp_loc.clone(),
        pexp_attributes: attrs,
    }
}

fn map_expression_desc(desc: &current::ExpressionDesc, loc: &Location) -> pt0::ExpressionDesc {
    match desc {
        current::ExpressionDesc::Pexp_ident(lid) => {
            pt0::ExpressionDesc::Ident(mkloc(lid.txt.clone(), &lid.loc))
        }
        current::ExpressionDesc::Pexp_constant(c) => {
            pt0::ExpressionDesc::Constant(map_constant(c))
        }
        current::ExpressionDesc::Pexp_let(flag, bindings, body) => {
            pt0::ExpressionDesc::Let(
                map_rec_flag(*flag),
                bindings.iter().map(map_value_binding).collect(),
                Box::new(map_expression(body)),
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
                arg_label: map_arg_label(arg_label),
                default: default.as_ref().map(|e| Box::new(map_expression(e))),
                lhs: Box::new(map_pattern(lhs)),
                rhs: Box::new(map_expression(rhs)),
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
                                loc: lid.loc.clone(),
                            };
                            current::Expression {
                                pexp_desc: current::ExpressionDesc::Pexp_ident(new_lid),
                                pexp_loc: funct.pexp_loc.clone(),
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
                funct: Box::new(map_expression(&mapped_funct)),
                args: args
                    .iter()
                    .map(|(label, e)| (map_arg_label(label), map_expression(e)))
                    .collect(),
                partial: *partial,
                transformed_jsx: *transformed_jsx,
            }
        }
        current::ExpressionDesc::Pexp_match(e, cases) => {
            pt0::ExpressionDesc::Match(
                Box::new(map_expression(e)),
                cases.iter().map(map_case).collect(),
            )
        }
        current::ExpressionDesc::Pexp_try(e, cases) => {
            pt0::ExpressionDesc::Try(
                Box::new(map_expression(e)),
                cases.iter().map(map_case).collect(),
            )
        }
        current::ExpressionDesc::Pexp_tuple(exprs) => {
            pt0::ExpressionDesc::Tuple(exprs.iter().map(map_expression).collect())
        }
        current::ExpressionDesc::Pexp_construct(lid, opt) => {
            pt0::ExpressionDesc::Construct(
                mkloc(lid.txt.clone(), &lid.loc),
                opt.as_ref().map(|e| Box::new(map_expression(e))),
            )
        }
        current::ExpressionDesc::Pexp_variant(label, opt) => {
            pt0::ExpressionDesc::Variant(
                label.clone(),
                opt.as_ref().map(|e| Box::new(map_expression(e))),
            )
        }
        current::ExpressionDesc::Pexp_record(fields, spread) => {
            // Use RecordElement with 3 fields (lid, x, opt) for current parsetree format
            pt0::ExpressionDesc::Record(
                fields
                    .iter()
                    .map(|f| pt0::RecordElement {
                        lid: mkloc(f.lid.txt.clone(), &f.lid.loc),
                        x: map_expression(&f.expr),
                        opt: f.opt,
                    })
                    .collect(),
                spread.as_ref().map(|e| Box::new(map_expression(e))),
            )
        }
        current::ExpressionDesc::Pexp_field(e, lid) => {
            pt0::ExpressionDesc::Field(
                Box::new(map_expression(e)),
                mkloc(lid.txt.clone(), &lid.loc),
            )
        }
        current::ExpressionDesc::Pexp_setfield(e1, lid, e2) => {
            pt0::ExpressionDesc::Setfield(
                Box::new(map_expression(e1)),
                mkloc(lid.txt.clone(), &lid.loc),
                Box::new(map_expression(e2)),
            )
        }
        current::ExpressionDesc::Pexp_array(exprs) => {
            pt0::ExpressionDesc::Array(exprs.iter().map(map_expression).collect())
        }
        current::ExpressionDesc::Pexp_ifthenelse(e1, e2, e3) => {
            pt0::ExpressionDesc::Ifthenelse(
                Box::new(map_expression(e1)),
                Box::new(map_expression(e2)),
                e3.as_ref().map(|e| Box::new(map_expression(e))),
            )
        }
        current::ExpressionDesc::Pexp_sequence(e1, e2) => {
            pt0::ExpressionDesc::Sequence(
                Box::new(map_expression(e1)),
                Box::new(map_expression(e2)),
            )
        }
        current::ExpressionDesc::Pexp_while(e1, e2) => {
            pt0::ExpressionDesc::While(Box::new(map_expression(e1)), Box::new(map_expression(e2)))
        }
        current::ExpressionDesc::Pexp_for(pat, e1, e2, flag, e3) => {
            pt0::ExpressionDesc::For(
                Box::new(map_pattern(pat)),
                Box::new(map_expression(e1)),
                Box::new(map_expression(e2)),
                map_direction_flag(*flag),
                Box::new(map_expression(e3)),
            )
        }
        current::ExpressionDesc::Pexp_constraint(e, ty) => {
            pt0::ExpressionDesc::Constraint(
                Box::new(map_expression(e)),
                Box::new(map_core_type(ty)),
            )
        }
        current::ExpressionDesc::Pexp_coerce(e, _, ty) => {
            // parsetree0 has unit instead of Option<CoreType> for the middle arg
            pt0::ExpressionDesc::Coerce(Box::new(map_expression(e)), (), Box::new(map_core_type(ty)))
        }
        current::ExpressionDesc::Pexp_send(e, label) => {
            pt0::ExpressionDesc::Send(Box::new(map_expression(e)), mkloc(label.txt.clone(), &label.loc))
        }
        current::ExpressionDesc::Pexp_letmodule(name, me, body) => {
            pt0::ExpressionDesc::Letmodule(
                mkloc(name.txt.clone(), &name.loc),
                Box::new(map_module_expr(me)),
                Box::new(map_expression(body)),
            )
        }
        current::ExpressionDesc::Pexp_letexception(ext, body) => {
            pt0::ExpressionDesc::Letexception(
                map_extension_constructor(ext),
                Box::new(map_expression(body)),
            )
        }
        current::ExpressionDesc::Pexp_assert(e) => {
            pt0::ExpressionDesc::Assert(Box::new(map_expression(e)))
        }
        current::ExpressionDesc::Pexp_newtype(name, body) => {
            pt0::ExpressionDesc::Newtype(mkloc(name.txt.clone(), &name.loc), Box::new(map_expression(body)))
        }
        current::ExpressionDesc::Pexp_pack(me) => {
            pt0::ExpressionDesc::Pack(Box::new(map_module_expr(me)))
        }
        current::ExpressionDesc::Pexp_open(flag, lid, body) => {
            pt0::ExpressionDesc::Open(
                map_override_flag(*flag),
                mkloc(lid.txt.clone(), &lid.loc),
                Box::new(map_expression(body)),
            )
        }
        current::ExpressionDesc::Pexp_extension(ext) => {
            pt0::ExpressionDesc::Extension(map_extension(ext))
        }
        current::ExpressionDesc::Pexp_await(e) => {
            // Convert await to inner expression with [@res.await] attribute
            let mut inner = map_expression(e);
            inner.pexp_attributes.insert(0, mk_await_attr(loc));
            inner.pexp_desc
        }
        current::ExpressionDesc::Pexp_jsx_element(jsx) => {
            // Convert JSX element to old-style function application with [@JSX] attribute
            match jsx {
                current::JsxElement::Fragment(fragment) => {
                    // Fragment becomes a list with JSX attribute
                    let list_expr = map_jsx_children_to_list(&fragment.children, loc);
                    // The JSX attribute will be added by the caller
                    list_expr.pexp_desc
                }
                current::JsxElement::Unary(unary) => {
                    // Unary element: <Tag props /> -> Tag(~props, ~children=[], ())
                    let tag_ident = jsx_tag_to_longident(&unary.tag_name.txt);
                    let mapped_props = map_jsx_props(&unary.props, loc);
                    let children_expr = mk_nil_expr(loc);
                    let unit_expr = mk_unit_expr(loc);

                    let mut args = mapped_props;
                    args.push((pt0::ArgLabel::Labelled("children".to_string()), children_expr));
                    args.push((pt0::ArgLabel::Nolabel, unit_expr));

                    pt0::ExpressionDesc::Apply {
                        funct: Box::new(pt0::Expression {
                            pexp_desc: pt0::ExpressionDesc::Ident(Located::new(
                                tag_ident,
                                unary.tag_name.loc.clone(),
                            )),
                            pexp_loc: unary.tag_name.loc.clone(),
                            pexp_attributes: vec![],
                        }),
                        args,
                        partial: false,
                        transformed_jsx: false,
                    }
                }
                current::JsxElement::Container(container) => {
                    // Container element: <Tag props>children</Tag> -> Tag(~props, ~children=list{...}, ())
                    let tag_ident = jsx_tag_to_longident(&container.tag_name_start.txt);
                    let mapped_props = map_jsx_props(&container.props, loc);
                    let children_expr = map_jsx_children_to_list(&container.children, loc);
                    let unit_expr = mk_unit_expr(loc);

                    let mut args = mapped_props;
                    args.push((pt0::ArgLabel::Labelled("children".to_string()), children_expr));
                    args.push((pt0::ArgLabel::Nolabel, unit_expr));

                    pt0::ExpressionDesc::Apply {
                        funct: Box::new(pt0::Expression {
                            pexp_desc: pt0::ExpressionDesc::Ident(Located::new(
                                tag_ident,
                                container.tag_name_start.loc.clone(),
                            )),
                            pexp_loc: container.tag_name_start.loc.clone(),
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

fn map_case(case: &current::Case) -> pt0::Case {
    pt0::Case {
        pc_lhs: map_pattern(&case.pc_lhs),
        pc_guard: case.pc_guard.as_ref().map(map_expression),
        pc_rhs: map_expression(&case.pc_rhs),
    }
}

fn map_value_binding(vb: &current::ValueBinding) -> pt0::ValueBinding {
    pt0::ValueBinding {
        pvb_pat: map_pattern(&vb.pvb_pat),
        pvb_expr: map_expression(&vb.pvb_expr),
        pvb_attributes: map_attributes(&vb.pvb_attributes),
        pvb_loc: vb.pvb_loc.clone(),
    }
}

// ========== Type declarations ==========

fn map_type_declaration(td: &current::TypeDeclaration) -> pt0::TypeDeclaration {
    pt0::TypeDeclaration {
        ptype_name: mkloc(td.ptype_name.txt.clone(), &td.ptype_name.loc),
        ptype_params: td
            .ptype_params
            .iter()
            .map(|(ty, var)| (map_core_type(ty), map_variance(*var)))
            .collect(),
        ptype_cstrs: td
            .ptype_cstrs
            .iter()
            .map(|(ty1, ty2, loc)| (map_core_type(ty1), map_core_type(ty2), loc.clone()))
            .collect(),
        ptype_kind: map_type_kind(&td.ptype_kind),
        ptype_private: map_private_flag(td.ptype_private),
        ptype_manifest: td.ptype_manifest.as_ref().map(map_core_type),
        ptype_attributes: map_attributes(&td.ptype_attributes),
        ptype_loc: td.ptype_loc.clone(),
    }
}

fn map_type_kind(kind: &current::TypeKind) -> pt0::TypeKind {
    match kind {
        current::TypeKind::Ptype_abstract => pt0::TypeKind::Abstract,
        current::TypeKind::Ptype_variant(ctors) => {
            pt0::TypeKind::Variant(ctors.iter().map(map_constructor_declaration).collect())
        }
        current::TypeKind::Ptype_record(labels) => {
            pt0::TypeKind::Record(labels.iter().map(map_label_declaration).collect())
        }
        current::TypeKind::Ptype_open => pt0::TypeKind::Open,
    }
}

fn map_label_declaration(ld: &current::LabelDeclaration) -> pt0::LabelDeclaration {
    pt0::LabelDeclaration {
        pld_name: mkloc(ld.pld_name.txt.clone(), &ld.pld_name.loc),
        pld_mutable: map_mutable_flag(ld.pld_mutable),
        pld_type: map_core_type(&ld.pld_type),
        pld_loc: ld.pld_loc.clone(),
        pld_attributes: map_attributes(&ld.pld_attributes),
    }
}

fn map_constructor_declaration(cd: &current::ConstructorDeclaration) -> pt0::ConstructorDeclaration {
    pt0::ConstructorDeclaration {
        pcd_name: mkloc(cd.pcd_name.txt.clone(), &cd.pcd_name.loc),
        pcd_args: map_constructor_arguments(&cd.pcd_args),
        pcd_res: cd.pcd_res.as_ref().map(map_core_type),
        pcd_loc: cd.pcd_loc.clone(),
        pcd_attributes: map_attributes(&cd.pcd_attributes),
    }
}

fn map_constructor_arguments(args: &current::ConstructorArguments) -> pt0::ConstructorArguments {
    match args {
        current::ConstructorArguments::Pcstr_tuple(types) => {
            pt0::ConstructorArguments::Tuple(types.iter().map(map_core_type).collect())
        }
        current::ConstructorArguments::Pcstr_record(labels) => {
            pt0::ConstructorArguments::Record(labels.iter().map(map_label_declaration).collect())
        }
    }
}

fn map_extension_constructor(ec: &current::ExtensionConstructor) -> pt0::ExtensionConstructor {
    pt0::ExtensionConstructor {
        pext_name: mkloc(ec.pext_name.txt.clone(), &ec.pext_name.loc),
        pext_kind: map_extension_constructor_kind(&ec.pext_kind),
        pext_loc: ec.pext_loc.clone(),
        pext_attributes: map_attributes(&ec.pext_attributes),
    }
}

fn map_extension_constructor_kind(kind: &current::ExtensionConstructorKind) -> pt0::ExtensionConstructorKind {
    match kind {
        current::ExtensionConstructorKind::Pext_decl(args, res) => {
            pt0::ExtensionConstructorKind::Decl(
                map_constructor_arguments(args),
                res.as_ref().map(map_core_type),
            )
        }
        current::ExtensionConstructorKind::Pext_rebind(lid) => {
            pt0::ExtensionConstructorKind::Rebind(mkloc(lid.txt.clone(), &lid.loc))
        }
    }
}

fn map_type_extension(te: &current::TypeExtension) -> pt0::TypeExtension {
    pt0::TypeExtension {
        ptyext_path: mkloc(te.ptyext_path.txt.clone(), &te.ptyext_path.loc),
        ptyext_params: te
            .ptyext_params
            .iter()
            .map(|(ty, var)| (map_core_type(ty), map_variance(*var)))
            .collect(),
        ptyext_constructors: te
            .ptyext_constructors
            .iter()
            .map(map_extension_constructor)
            .collect(),
        ptyext_private: map_private_flag(te.ptyext_private),
        ptyext_attributes: map_attributes(&te.ptyext_attributes),
    }
}

// ========== Module types ==========

fn map_module_type(mt: &current::ModuleType) -> pt0::ModuleType {
    pt0::ModuleType {
        pmty_desc: map_module_type_desc(&mt.pmty_desc),
        pmty_loc: mt.pmty_loc.clone(),
        pmty_attributes: map_attributes(&mt.pmty_attributes),
    }
}

fn map_module_type_desc(desc: &current::ModuleTypeDesc) -> pt0::ModuleTypeDesc {
    match desc {
        current::ModuleTypeDesc::Pmty_ident(lid) => {
            pt0::ModuleTypeDesc::Ident(mkloc(lid.txt.clone(), &lid.loc))
        }
        current::ModuleTypeDesc::Pmty_signature(items) => {
            pt0::ModuleTypeDesc::Signature(items.iter().map(map_signature_item).collect())
        }
        current::ModuleTypeDesc::Pmty_functor(name, mty, body) => {
            pt0::ModuleTypeDesc::Functor(
                mkloc(name.txt.clone(), &name.loc),
                mty.as_ref().map(|m| Box::new(map_module_type(m))),
                Box::new(map_module_type(body)),
            )
        }
        current::ModuleTypeDesc::Pmty_with(mty, constraints) => {
            pt0::ModuleTypeDesc::With(
                Box::new(map_module_type(mty)),
                constraints.iter().map(map_with_constraint).collect(),
            )
        }
        current::ModuleTypeDesc::Pmty_typeof(me) => {
            pt0::ModuleTypeDesc::Typeof(Box::new(map_module_expr(me)))
        }
        current::ModuleTypeDesc::Pmty_extension(ext) => {
            pt0::ModuleTypeDesc::Extension(map_extension(ext))
        }
        current::ModuleTypeDesc::Pmty_alias(lid) => {
            pt0::ModuleTypeDesc::Alias(mkloc(lid.txt.clone(), &lid.loc))
        }
    }
}

fn map_with_constraint(wc: &current::WithConstraint) -> pt0::WithConstraint {
    match wc {
        current::WithConstraint::Pwith_type(lid, td) => {
            pt0::WithConstraint::Type(mkloc(lid.txt.clone(), &lid.loc), map_type_declaration(td))
        }
        current::WithConstraint::Pwith_module(lid1, lid2) => {
            pt0::WithConstraint::Module(
                mkloc(lid1.txt.clone(), &lid1.loc),
                mkloc(lid2.txt.clone(), &lid2.loc),
            )
        }
        current::WithConstraint::Pwith_typesubst(lid, td) => {
            pt0::WithConstraint::TypeSubst(mkloc(lid.txt.clone(), &lid.loc), map_type_declaration(td))
        }
        current::WithConstraint::Pwith_modsubst(lid1, lid2) => {
            pt0::WithConstraint::ModSubst(
                mkloc(lid1.txt.clone(), &lid1.loc),
                mkloc(lid2.txt.clone(), &lid2.loc),
            )
        }
    }
}

// ========== Module expressions ==========

fn map_module_expr(me: &current::ModuleExpr) -> pt0::ModuleExpr {
    pt0::ModuleExpr {
        pmod_desc: map_module_expr_desc(&me.pmod_desc),
        pmod_loc: me.pmod_loc.clone(),
        pmod_attributes: map_attributes(&me.pmod_attributes),
    }
}

fn map_module_expr_desc(desc: &current::ModuleExprDesc) -> pt0::ModuleExprDesc {
    match desc {
        current::ModuleExprDesc::Pmod_ident(lid) => {
            pt0::ModuleExprDesc::Ident(mkloc(lid.txt.clone(), &lid.loc))
        }
        current::ModuleExprDesc::Pmod_structure(items) => {
            pt0::ModuleExprDesc::Structure(items.iter().map(map_structure_item).collect())
        }
        current::ModuleExprDesc::Pmod_functor(name, mty, body) => {
            pt0::ModuleExprDesc::Functor(
                mkloc(name.txt.clone(), &name.loc),
                mty.as_ref().map(|m| Box::new(map_module_type(m))),
                Box::new(map_module_expr(body)),
            )
        }
        current::ModuleExprDesc::Pmod_apply(me1, me2) => {
            pt0::ModuleExprDesc::Apply(
                Box::new(map_module_expr(me1)),
                Box::new(map_module_expr(me2)),
            )
        }
        current::ModuleExprDesc::Pmod_constraint(me, mty) => {
            pt0::ModuleExprDesc::Constraint(
                Box::new(map_module_expr(me)),
                Box::new(map_module_type(mty)),
            )
        }
        current::ModuleExprDesc::Pmod_unpack(e) => {
            pt0::ModuleExprDesc::Unpack(Box::new(map_expression(e)))
        }
        current::ModuleExprDesc::Pmod_extension(ext) => {
            pt0::ModuleExprDesc::Extension(map_extension(ext))
        }
    }
}

fn map_module_binding(mb: &current::ModuleBinding) -> pt0::ModuleBinding {
    pt0::ModuleBinding {
        pmb_name: mkloc(mb.pmb_name.txt.clone(), &mb.pmb_name.loc),
        pmb_expr: map_module_expr(&mb.pmb_expr),
        pmb_attributes: map_attributes(&mb.pmb_attributes),
        pmb_loc: mb.pmb_loc.clone(),
    }
}

fn map_module_declaration(md: &current::ModuleDeclaration) -> pt0::ModuleDeclaration {
    pt0::ModuleDeclaration {
        pmd_name: mkloc(md.pmd_name.txt.clone(), &md.pmd_name.loc),
        pmd_type: map_module_type(&md.pmd_type),
        pmd_attributes: map_attributes(&md.pmd_attributes),
        pmd_loc: md.pmd_loc.clone(),
    }
}

fn map_module_type_declaration(mtd: &current::ModuleTypeDeclaration) -> pt0::ModuleTypeDeclaration {
    pt0::ModuleTypeDeclaration {
        pmtd_name: mkloc(mtd.pmtd_name.txt.clone(), &mtd.pmtd_name.loc),
        pmtd_type: mtd.pmtd_type.as_ref().map(map_module_type),
        pmtd_attributes: map_attributes(&mtd.pmtd_attributes),
        pmtd_loc: mtd.pmtd_loc.clone(),
    }
}

fn map_open_description(od: &current::OpenDescription) -> pt0::OpenDescription {
    pt0::OpenDescription {
        popen_lid: mkloc(od.popen_lid.txt.clone(), &od.popen_lid.loc),
        popen_override: map_override_flag(od.popen_override),
        popen_loc: od.popen_loc.clone(),
        popen_attributes: map_attributes(&od.popen_attributes),
    }
}

fn map_include_declaration(id: &current::IncludeDeclaration) -> pt0::IncludeDeclaration {
    pt0::IncludeInfos {
        pincl_mod: map_module_expr(&id.pincl_mod),
        pincl_loc: id.pincl_loc.clone(),
        pincl_attributes: map_attributes(&id.pincl_attributes),
    }
}

fn map_include_description(id: &current::IncludeDescription) -> pt0::IncludeDescription {
    pt0::IncludeInfos {
        pincl_mod: map_module_type(&id.pincl_mod),
        pincl_loc: id.pincl_loc.clone(),
        pincl_attributes: map_attributes(&id.pincl_attributes),
    }
}

fn map_value_description(vd: &current::ValueDescription) -> pt0::ValueDescription {
    pt0::ValueDescription {
        pval_name: mkloc(vd.pval_name.txt.clone(), &vd.pval_name.loc),
        pval_type: map_core_type(&vd.pval_type),
        pval_prim: vd.pval_prim.clone(),
        pval_attributes: map_attributes(&vd.pval_attributes),
        pval_loc: vd.pval_loc.clone(),
    }
}

// ========== Structure items ==========

fn map_structure_item(item: &current::StructureItem) -> pt0::StructureItem {
    pt0::StructureItem {
        pstr_desc: map_structure_item_desc(&item.pstr_desc),
        pstr_loc: item.pstr_loc.clone(),
    }
}

fn map_structure_item_desc(desc: &current::StructureItemDesc) -> pt0::StructureItemDesc {
    match desc {
        current::StructureItemDesc::Pstr_eval(e, attrs) => {
            pt0::StructureItemDesc::Eval(map_expression(e), map_attributes(attrs))
        }
        current::StructureItemDesc::Pstr_value(flag, bindings) => {
            pt0::StructureItemDesc::Value(
                map_rec_flag(*flag),
                bindings.iter().map(map_value_binding).collect(),
            )
        }
        current::StructureItemDesc::Pstr_primitive(vd) => {
            pt0::StructureItemDesc::Primitive(map_value_description(vd))
        }
        current::StructureItemDesc::Pstr_type(flag, decls) => {
            pt0::StructureItemDesc::Type(
                map_rec_flag(*flag),
                decls.iter().map(map_type_declaration).collect(),
            )
        }
        current::StructureItemDesc::Pstr_typext(te) => {
            pt0::StructureItemDesc::Typext(map_type_extension(te))
        }
        current::StructureItemDesc::Pstr_exception(ec) => {
            pt0::StructureItemDesc::Exception(map_extension_constructor(ec))
        }
        current::StructureItemDesc::Pstr_module(mb) => {
            pt0::StructureItemDesc::Module(map_module_binding(mb))
        }
        current::StructureItemDesc::Pstr_recmodule(mbs) => {
            pt0::StructureItemDesc::Recmodule(mbs.iter().map(map_module_binding).collect())
        }
        current::StructureItemDesc::Pstr_modtype(mtd) => {
            pt0::StructureItemDesc::Modtype(map_module_type_declaration(mtd))
        }
        current::StructureItemDesc::Pstr_open(od) => {
            pt0::StructureItemDesc::Open(map_open_description(od))
        }
        current::StructureItemDesc::Pstr_include(ii) => {
            pt0::StructureItemDesc::Include(map_include_declaration(ii))
        }
        current::StructureItemDesc::Pstr_attribute(attr) => {
            pt0::StructureItemDesc::Attribute(map_attribute(attr))
        }
        current::StructureItemDesc::Pstr_extension(ext, attrs) => {
            pt0::StructureItemDesc::Extension(map_extension(ext), map_attributes(attrs))
        }
    }
}

// ========== Signature items ==========

fn map_signature_item(item: &current::SignatureItem) -> pt0::SignatureItem {
    pt0::SignatureItem {
        psig_desc: map_signature_item_desc(&item.psig_desc),
        psig_loc: item.psig_loc.clone(),
    }
}

fn map_signature_item_desc(desc: &current::SignatureItemDesc) -> pt0::SignatureItemDesc {
    match desc {
        current::SignatureItemDesc::Psig_value(vd) => {
            pt0::SignatureItemDesc::Value(map_value_description(vd))
        }
        current::SignatureItemDesc::Psig_type(flag, decls) => {
            pt0::SignatureItemDesc::Type(
                map_rec_flag(*flag),
                decls.iter().map(map_type_declaration).collect(),
            )
        }
        current::SignatureItemDesc::Psig_typext(te) => {
            pt0::SignatureItemDesc::Typext(map_type_extension(te))
        }
        current::SignatureItemDesc::Psig_exception(ec) => {
            pt0::SignatureItemDesc::Exception(map_extension_constructor(ec))
        }
        current::SignatureItemDesc::Psig_module(md) => {
            pt0::SignatureItemDesc::Module(map_module_declaration(md))
        }
        current::SignatureItemDesc::Psig_recmodule(mds) => {
            pt0::SignatureItemDesc::Recmodule(mds.iter().map(map_module_declaration).collect())
        }
        current::SignatureItemDesc::Psig_modtype(mtd) => {
            pt0::SignatureItemDesc::Modtype(map_module_type_declaration(mtd))
        }
        current::SignatureItemDesc::Psig_open(od) => {
            pt0::SignatureItemDesc::Open(map_open_description(od))
        }
        current::SignatureItemDesc::Psig_include(ii) => {
            pt0::SignatureItemDesc::Include(map_include_description(ii))
        }
        current::SignatureItemDesc::Psig_attribute(attr) => {
            pt0::SignatureItemDesc::Attribute(map_attribute(attr))
        }
        current::SignatureItemDesc::Psig_extension(ext, attrs) => {
            pt0::SignatureItemDesc::Extension(map_extension(ext), map_attributes(attrs))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_map_rec_flag() {
        assert!(matches!(map_rec_flag(current::RecFlag::Nonrecursive), pt0::RecFlag::Nonrecursive));
        assert!(matches!(map_rec_flag(current::RecFlag::Recursive), pt0::RecFlag::Recursive));
    }

    #[test]
    fn test_map_arg_label() {
        use crate::location::Located;

        assert!(matches!(map_arg_label(&current::ArgLabel::Nolabel), pt0::ArgLabel::Nolabel));

        let label = map_arg_label(&current::ArgLabel::Labelled(Located::mknoloc("x".to_string())));
        assert!(matches!(label, pt0::ArgLabel::Labelled(s) if s == "x"));

        let opt = map_arg_label(&current::ArgLabel::Optional(Located::mknoloc("y".to_string())));
        assert!(matches!(opt, pt0::ArgLabel::Optional(s) if s == "y"));
    }

    #[test]
    fn test_map_constant() {
        let c1 = map_constant(&current::Constant::Integer("42".to_string(), None));
        assert!(matches!(c1, pt0::Constant::Integer(s, None) if s == "42"));

        let c2 = map_constant(&current::Constant::Char(65));
        assert!(matches!(c2, pt0::Constant::Char(65)));

        // Note: None delimiter is mapped to Some("*j") for OCaml binary parity
        let c3 = map_constant(&current::Constant::String("hello".to_string(), None));
        assert!(matches!(c3, pt0::Constant::String(s, Some(d)) if s == "hello" && d == "*j"));
    }
}
