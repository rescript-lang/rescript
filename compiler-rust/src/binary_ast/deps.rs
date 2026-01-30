//! Module dependency extraction from AST
//!
//! This module provides functionality to extract module dependencies from
//! ReScript AST, matching the OCaml implementation in `compiler/frontend/ast_extract.ml`.
//!
//! Dependencies are extracted by walking the AST and collecting all module references
//! from Longidents. Only uppercase module names are collected (lowercase names are
//! local bindings, not module references).

use std::collections::BTreeSet;

use crate::parser::ast::{
    Case, ConstructorArguments, CoreType, CoreTypeDesc, Expression, ExpressionDesc,
    ExtensionConstructor, ExtensionConstructorKind, IncludeDeclaration, IncludeDescription,
    LabelDeclaration, ModuleBinding, ModuleDeclaration, ModuleExpr, ModuleExprDesc, ModuleType,
    ModuleTypeDeclaration, ModuleTypeDesc, ObjectField, OpenDescription, Pattern, PatternDesc,
    Payload, RowField, SignatureItem, SignatureItemDesc, StructureItem, StructureItemDesc,
    TypeDeclaration, TypeExtension, TypeKind, ValueBinding, ValueDescription, WithConstraint,
};
use crate::parse_arena::{LidentIdx, ParseArena};
use crate::parser::longident::Longident;

/// A set of module dependencies, sorted for deterministic output.
pub type Dependencies = BTreeSet<String>;

/// Extract module dependencies from a structure (implementation).
pub fn extract_structure_deps(arena: &ParseArena, structure: &[StructureItem]) -> Dependencies {
    let mut collector = DependencyCollector::new(arena);
    collector.add_structure(structure);
    collector.finish()
}

/// Extract module dependencies from a signature (interface).
pub fn extract_signature_deps(arena: &ParseArena, signature: &[SignatureItem]) -> Dependencies {
    let mut collector = DependencyCollector::new(arena);
    collector.add_signature(signature);
    collector.finish()
}

/// Collector for module dependencies.
struct DependencyCollector<'a> {
    arena: &'a ParseArena,
    deps: BTreeSet<String>,
}

impl<'a> DependencyCollector<'a> {
    fn new(arena: &'a ParseArena) -> Self {
        Self {
            arena,
            deps: BTreeSet::new(),
        }
    }

    /// Finish collecting and return the filtered dependencies.
    fn finish(self) -> Dependencies {
        // Filter out:
        // - Empty strings
        // - Strings starting with '*' (like *predef*)
        self.deps
            .into_iter()
            .filter(|s| !s.is_empty() && !s.starts_with('*'))
            .collect()
    }

    /// Add a module reference from a Longident.
    fn add_longident(&mut self, lid: &Longident) {
        // Get the root module name
        let parts = lid.flatten_idx();
        if let Some(first) = parts.first() {
            // Look up the string content
            let first_str = self.arena.get_string(*first);
            // Only add if it starts with uppercase (module reference)
            // Lowercase identifiers are local bindings
            if first_str.chars().next().is_some_and(|c| c.is_ascii_uppercase()) {
                self.deps.insert(first_str.to_string());
            }
        }

        // Also collect from functor applications
        if let Longident::Lapply(func, arg) = lid {
            self.add_longident(func);
            self.add_longident(arg);
        }
    }

    /// Add a module reference from a LidentIdx (arena-allocated longident).
    fn add_lident_idx(&mut self, lid_idx: LidentIdx) {
        let lid = self.arena.get_longident(lid_idx);
        self.add_longident(lid);
    }

    // ========== Structure ==========

    fn add_structure(&mut self, structure: &[StructureItem]) {
        for item in structure {
            self.add_structure_item(item);
        }
    }

    fn add_structure_item(&mut self, item: &StructureItem) {
        match &item.pstr_desc {
            StructureItemDesc::Pstr_eval(expr, _attrs) => {
                self.add_expression(expr);
            }
            StructureItemDesc::Pstr_value(_rec_flag, bindings) => {
                for binding in bindings {
                    self.add_value_binding(binding);
                }
            }
            StructureItemDesc::Pstr_primitive(vd) => {
                self.add_value_description(vd);
            }
            StructureItemDesc::Pstr_type(_rec_flag, decls) => {
                for decl in decls {
                    self.add_type_declaration(decl);
                }
            }
            StructureItemDesc::Pstr_typext(ext) => {
                self.add_type_extension(ext);
            }
            StructureItemDesc::Pstr_exception(ext_ctor) => {
                self.add_extension_constructor(ext_ctor);
            }
            StructureItemDesc::Pstr_module(mb) => {
                self.add_module_binding(mb);
            }
            StructureItemDesc::Pstr_recmodule(mbs) => {
                for mb in mbs {
                    self.add_module_binding(mb);
                }
            }
            StructureItemDesc::Pstr_modtype(mtd) => {
                self.add_module_type_declaration(mtd);
            }
            StructureItemDesc::Pstr_open(od) => {
                self.add_open_description(od);
            }
            StructureItemDesc::Pstr_include(incl) => {
                self.add_include_declaration(incl);
            }
            StructureItemDesc::Pstr_attribute(_attr) => {
                // Attributes don't contribute dependencies
            }
            StructureItemDesc::Pstr_extension((_id, payload), _attrs) => {
                self.add_payload(payload);
            }
        }
    }

    // ========== Signature ==========

    fn add_signature(&mut self, signature: &[SignatureItem]) {
        for item in signature {
            self.add_signature_item(item);
        }
    }

    fn add_signature_item(&mut self, item: &SignatureItem) {
        match &item.psig_desc {
            SignatureItemDesc::Psig_value(vd) => {
                self.add_value_description(vd);
            }
            SignatureItemDesc::Psig_type(_rec_flag, decls) => {
                for decl in decls {
                    self.add_type_declaration(decl);
                }
            }
            SignatureItemDesc::Psig_typext(ext) => {
                self.add_type_extension(ext);
            }
            SignatureItemDesc::Psig_exception(ext_ctor) => {
                self.add_extension_constructor(ext_ctor);
            }
            SignatureItemDesc::Psig_module(md) => {
                self.add_module_declaration(md);
            }
            SignatureItemDesc::Psig_recmodule(mds) => {
                for md in mds {
                    self.add_module_declaration(md);
                }
            }
            SignatureItemDesc::Psig_modtype(mtd) => {
                self.add_module_type_declaration(mtd);
            }
            SignatureItemDesc::Psig_open(od) => {
                self.add_open_description(od);
            }
            SignatureItemDesc::Psig_include(incl) => {
                self.add_include_description(incl);
            }
            SignatureItemDesc::Psig_attribute(_attr) => {
                // Attributes don't contribute dependencies
            }
            SignatureItemDesc::Psig_extension((_id, payload), _attrs) => {
                self.add_payload(payload);
            }
        }
    }

    // ========== Expressions ==========

    fn add_expression(&mut self, expr: &Expression) {
        match &expr.pexp_desc {
            ExpressionDesc::Pexp_ident(lid) => {
                self.add_lident_idx(lid.txt);
            }
            ExpressionDesc::Pexp_constant(_) => {}
            ExpressionDesc::Pexp_let(_rec_flag, bindings, body) => {
                for binding in bindings {
                    self.add_value_binding(binding);
                }
                self.add_expression(body);
            }
            ExpressionDesc::Pexp_fun {
                default,
                lhs,
                rhs,
                ..
            } => {
                if let Some(default) = default {
                    self.add_expression(default);
                }
                self.add_pattern(lhs);
                self.add_expression(rhs);
            }
            ExpressionDesc::Pexp_apply { funct, args, .. } => {
                self.add_expression(funct);
                for (_label, arg) in args {
                    self.add_expression(arg);
                }
            }
            ExpressionDesc::Pexp_match(expr, cases) | ExpressionDesc::Pexp_try(expr, cases) => {
                self.add_expression(expr);
                for case in cases {
                    self.add_case(case);
                }
            }
            ExpressionDesc::Pexp_tuple(exprs) | ExpressionDesc::Pexp_array(exprs) => {
                for expr in exprs {
                    self.add_expression(expr);
                }
            }
            ExpressionDesc::Pexp_construct(lid, arg) => {
                self.add_lident_idx(lid.txt);
                if let Some(arg) = arg {
                    self.add_expression(arg);
                }
            }
            ExpressionDesc::Pexp_variant(_label, arg) => {
                if let Some(arg) = arg {
                    self.add_expression(arg);
                }
            }
            ExpressionDesc::Pexp_record(fields, spread) => {
                for field in fields {
                    self.add_lident_idx(field.lid.txt);
                    self.add_expression(&field.expr);
                }
                if let Some(spread) = spread {
                    self.add_expression(spread);
                }
            }
            ExpressionDesc::Pexp_field(expr, lid) => {
                self.add_expression(expr);
                self.add_lident_idx(lid.txt);
            }
            ExpressionDesc::Pexp_setfield(expr1, lid, expr2) => {
                self.add_expression(expr1);
                self.add_lident_idx(lid.txt);
                self.add_expression(expr2);
            }
            ExpressionDesc::Pexp_ifthenelse(cond, then_expr, else_expr) => {
                self.add_expression(cond);
                self.add_expression(then_expr);
                if let Some(else_expr) = else_expr {
                    self.add_expression(else_expr);
                }
            }
            ExpressionDesc::Pexp_sequence(e1, e2) => {
                self.add_expression(e1);
                self.add_expression(e2);
            }
            ExpressionDesc::Pexp_while(cond, body) => {
                self.add_expression(cond);
                self.add_expression(body);
            }
            ExpressionDesc::Pexp_for(pat, start, end, _dir, body) => {
                self.add_pattern(pat);
                self.add_expression(start);
                self.add_expression(end);
                self.add_expression(body);
            }
            ExpressionDesc::Pexp_constraint(expr, ty) => {
                self.add_expression(expr);
                self.add_core_type(ty);
            }
            ExpressionDesc::Pexp_coerce(expr, ty1, ty2) => {
                self.add_expression(expr);
                if let Some(ty1) = ty1 {
                    self.add_core_type(ty1);
                }
                self.add_core_type(ty2);
            }
            ExpressionDesc::Pexp_send(expr, _label) => {
                self.add_expression(expr);
            }
            ExpressionDesc::Pexp_letmodule(_name, me, body) => {
                self.add_module_expr(me);
                self.add_expression(body);
            }
            ExpressionDesc::Pexp_letexception(ext_ctor, body) => {
                self.add_extension_constructor(ext_ctor);
                self.add_expression(body);
            }
            ExpressionDesc::Pexp_assert(expr) => {
                self.add_expression(expr);
            }
            ExpressionDesc::Pexp_newtype(_name, body) => {
                self.add_expression(body);
            }
            ExpressionDesc::Pexp_pack(me) => {
                self.add_module_expr(me);
            }
            ExpressionDesc::Pexp_open(_override, lid, body) => {
                self.add_lident_idx(lid.txt);
                self.add_expression(body);
            }
            ExpressionDesc::Pexp_extension((_id, payload)) => {
                self.add_payload(payload);
            }
            ExpressionDesc::Pexp_await(expr) => {
                self.add_expression(expr);
            }
            ExpressionDesc::Pexp_jsx_element(jsx) => {
                self.add_jsx_element(jsx);
            }
        }
    }

    fn add_jsx_element(&mut self, jsx: &crate::parser::ast::JsxElement) {
        use crate::parser::ast::{JsxElement, JsxTagName};

        match jsx {
            JsxElement::Fragment(frag) => {
                for child in &frag.children {
                    self.add_expression(child);
                }
            }
            JsxElement::Unary(elem) => {
                // Add dependency from tag name if it's a component
                match &elem.tag_name.txt {
                    JsxTagName::Upper(lid) => {
                        self.add_lident_idx(*lid);
                    }
                    JsxTagName::QualifiedLower { path, .. } => {
                        self.add_lident_idx(*path);
                    }
                    JsxTagName::Lower(_) | JsxTagName::Invalid(_) => {}
                }
                for prop in &elem.props {
                    self.add_jsx_prop(prop);
                }
            }
            JsxElement::Container(elem) => {
                match &elem.tag_name_start.txt {
                    JsxTagName::Upper(lid) => {
                        self.add_lident_idx(*lid);
                    }
                    JsxTagName::QualifiedLower { path, .. } => {
                        self.add_lident_idx(*path);
                    }
                    JsxTagName::Lower(_) | JsxTagName::Invalid(_) => {}
                }
                for prop in &elem.props {
                    self.add_jsx_prop(prop);
                }
                for child in &elem.children {
                    self.add_expression(child);
                }
            }
        }
    }

    fn add_jsx_prop(&mut self, prop: &crate::parser::ast::JsxProp) {
        use crate::parser::ast::JsxProp;
        match prop {
            JsxProp::Punning { .. } => {}
            JsxProp::Value { value, .. } => {
                self.add_expression(value);
            }
            JsxProp::Spreading { expr, .. } => {
                self.add_expression(expr);
            }
        }
    }

    // ========== Patterns ==========

    fn add_pattern(&mut self, pat: &Pattern) {
        match &pat.ppat_desc {
            PatternDesc::Ppat_any | PatternDesc::Ppat_var(_) | PatternDesc::Ppat_constant(_) => {}
            PatternDesc::Ppat_alias(pat, _name) => {
                self.add_pattern(pat);
            }
            PatternDesc::Ppat_interval(_, _) => {}
            PatternDesc::Ppat_tuple(pats) | PatternDesc::Ppat_array(pats) => {
                for pat in pats {
                    self.add_pattern(pat);
                }
            }
            PatternDesc::Ppat_construct(lid, arg) => {
                self.add_lident_idx(lid.txt);
                if let Some(arg) = arg {
                    self.add_pattern(arg);
                }
            }
            PatternDesc::Ppat_variant(_label, arg) => {
                if let Some(arg) = arg {
                    self.add_pattern(arg);
                }
            }
            PatternDesc::Ppat_record(fields, _closed) => {
                for field in fields {
                    self.add_lident_idx(field.lid.txt);
                    self.add_pattern(&field.pat);
                }
            }
            PatternDesc::Ppat_or(p1, p2) => {
                self.add_pattern(p1);
                self.add_pattern(p2);
            }
            PatternDesc::Ppat_constraint(pat, ty) => {
                self.add_pattern(pat);
                self.add_core_type(ty);
            }
            PatternDesc::Ppat_type(lid) => {
                self.add_lident_idx(lid.txt);
            }
            PatternDesc::Ppat_unpack(_name) => {}
            PatternDesc::Ppat_exception(pat) => {
                self.add_pattern(pat);
            }
            PatternDesc::Ppat_extension((_id, payload)) => {
                self.add_payload(payload);
            }
            PatternDesc::Ppat_open(lid, pat) => {
                self.add_lident_idx(lid.txt);
                self.add_pattern(pat);
            }
        }
    }

    // ========== Core Types ==========

    fn add_core_type(&mut self, ty: &CoreType) {
        match &ty.ptyp_desc {
            CoreTypeDesc::Ptyp_any | CoreTypeDesc::Ptyp_var(_) => {}
            CoreTypeDesc::Ptyp_arrow { arg, ret, .. } => {
                self.add_core_type(&arg.typ);
                self.add_core_type(ret);
            }
            CoreTypeDesc::Ptyp_tuple(tys) => {
                for ty in tys {
                    self.add_core_type(ty);
                }
            }
            CoreTypeDesc::Ptyp_constr(lid, args) => {
                self.add_lident_idx(lid.txt);
                for arg in args {
                    self.add_core_type(arg);
                }
            }
            CoreTypeDesc::Ptyp_object(fields, _closed) => {
                for field in fields {
                    match field {
                        ObjectField::Otag(_label, _attrs, ty) => {
                            self.add_core_type(ty);
                        }
                        ObjectField::Oinherit(ty) => {
                            self.add_core_type(ty);
                        }
                    }
                }
            }
            CoreTypeDesc::Ptyp_alias(ty, _name) => {
                self.add_core_type(ty);
            }
            CoreTypeDesc::Ptyp_variant(rows, _closed, _labels) => {
                for row in rows {
                    match row {
                        RowField::Rtag(_label, _attrs, _bool, tys) => {
                            for ty in tys {
                                self.add_core_type(ty);
                            }
                        }
                        RowField::Rinherit(ty) => {
                            self.add_core_type(ty);
                        }
                    }
                }
            }
            CoreTypeDesc::Ptyp_poly(_names, ty) => {
                self.add_core_type(ty);
            }
            CoreTypeDesc::Ptyp_package((lid, constraints)) => {
                self.add_lident_idx(lid.txt);
                for (lid, ty) in constraints {
                    self.add_lident_idx(lid.txt);
                    self.add_core_type(ty);
                }
            }
            CoreTypeDesc::Ptyp_extension((_id, payload)) => {
                self.add_payload(payload);
            }
        }
    }

    // ========== Modules ==========

    fn add_module_expr(&mut self, me: &ModuleExpr) {
        match &me.pmod_desc {
            ModuleExprDesc::Pmod_ident(lid) => {
                self.add_lident_idx(lid.txt);
            }
            ModuleExprDesc::Pmod_structure(str) => {
                self.add_structure(str);
            }
            ModuleExprDesc::Pmod_functor(_name, mt, body) => {
                if let Some(mt) = mt {
                    self.add_module_type(mt);
                }
                self.add_module_expr(body);
            }
            ModuleExprDesc::Pmod_apply(me1, me2) => {
                self.add_module_expr(me1);
                self.add_module_expr(me2);
            }
            ModuleExprDesc::Pmod_constraint(me, mt) => {
                self.add_module_expr(me);
                self.add_module_type(mt);
            }
            ModuleExprDesc::Pmod_unpack(expr) => {
                self.add_expression(expr);
            }
            ModuleExprDesc::Pmod_extension((_id, payload)) => {
                self.add_payload(payload);
            }
        }
    }

    fn add_module_type(&mut self, mt: &ModuleType) {
        match &mt.pmty_desc {
            ModuleTypeDesc::Pmty_ident(lid) | ModuleTypeDesc::Pmty_alias(lid) => {
                self.add_lident_idx(lid.txt);
            }
            ModuleTypeDesc::Pmty_signature(sig) => {
                self.add_signature(sig);
            }
            ModuleTypeDesc::Pmty_functor(_name, mt1, mt2) => {
                if let Some(mt1) = mt1 {
                    self.add_module_type(mt1);
                }
                self.add_module_type(mt2);
            }
            ModuleTypeDesc::Pmty_with(mt, constraints) => {
                self.add_module_type(mt);
                for constraint in constraints {
                    self.add_with_constraint(constraint);
                }
            }
            ModuleTypeDesc::Pmty_typeof(me) => {
                self.add_module_expr(me);
            }
            ModuleTypeDesc::Pmty_extension((_id, payload)) => {
                self.add_payload(payload);
            }
        }
    }

    fn add_with_constraint(&mut self, constraint: &WithConstraint) {
        match constraint {
            WithConstraint::Pwith_type(lid, td) | WithConstraint::Pwith_typesubst(lid, td) => {
                self.add_lident_idx(lid.txt);
                self.add_type_declaration(td);
            }
            WithConstraint::Pwith_module(lid1, lid2)
            | WithConstraint::Pwith_modsubst(lid1, lid2) => {
                self.add_lident_idx(lid1.txt);
                self.add_lident_idx(lid2.txt);
            }
        }
    }

    // ========== Declarations ==========

    fn add_value_binding(&mut self, vb: &ValueBinding) {
        self.add_pattern(&vb.pvb_pat);
        self.add_expression(&vb.pvb_expr);
    }

    fn add_value_description(&mut self, vd: &ValueDescription) {
        self.add_core_type(&vd.pval_type);
    }

    fn add_type_declaration(&mut self, td: &TypeDeclaration) {
        for (ty, _variance) in &td.ptype_params {
            self.add_core_type(ty);
        }
        for (ty1, ty2, _loc) in &td.ptype_cstrs {
            self.add_core_type(ty1);
            self.add_core_type(ty2);
        }
        match &td.ptype_kind {
            TypeKind::Ptype_abstract | TypeKind::Ptype_open => {}
            TypeKind::Ptype_variant(ctors) => {
                for ctor in ctors {
                    self.add_constructor_arguments(&ctor.pcd_args);
                    if let Some(res) = &ctor.pcd_res {
                        self.add_core_type(res);
                    }
                }
            }
            TypeKind::Ptype_record(fields) => {
                for field in fields {
                    self.add_label_declaration(field);
                }
            }
        }
        if let Some(manifest) = &td.ptype_manifest {
            self.add_core_type(manifest);
        }
    }

    fn add_constructor_arguments(&mut self, args: &ConstructorArguments) {
        match args {
            ConstructorArguments::Pcstr_tuple(tys) => {
                for ty in tys {
                    self.add_core_type(ty);
                }
            }
            ConstructorArguments::Pcstr_record(fields) => {
                for field in fields {
                    self.add_label_declaration(field);
                }
            }
        }
    }

    fn add_label_declaration(&mut self, ld: &LabelDeclaration) {
        self.add_core_type(&ld.pld_type);
    }

    fn add_type_extension(&mut self, te: &TypeExtension) {
        self.add_lident_idx(te.ptyext_path.txt);
        for (ty, _variance) in &te.ptyext_params {
            self.add_core_type(ty);
        }
        for ctor in &te.ptyext_constructors {
            self.add_extension_constructor(ctor);
        }
    }

    fn add_extension_constructor(&mut self, ec: &ExtensionConstructor) {
        match &ec.pext_kind {
            ExtensionConstructorKind::Pext_decl(args, res) => {
                self.add_constructor_arguments(args);
                if let Some(res) = res {
                    self.add_core_type(res);
                }
            }
            ExtensionConstructorKind::Pext_rebind(lid) => {
                self.add_lident_idx(lid.txt);
            }
        }
    }

    fn add_module_binding(&mut self, mb: &ModuleBinding) {
        self.add_module_expr(&mb.pmb_expr);
    }

    fn add_module_declaration(&mut self, md: &ModuleDeclaration) {
        self.add_module_type(&md.pmd_type);
    }

    fn add_module_type_declaration(&mut self, mtd: &ModuleTypeDeclaration) {
        if let Some(mt) = &mtd.pmtd_type {
            self.add_module_type(mt);
        }
    }

    fn add_open_description(&mut self, od: &OpenDescription) {
        self.add_lident_idx(od.popen_lid.txt);
    }

    fn add_include_declaration(&mut self, id: &IncludeDeclaration) {
        self.add_module_expr(&id.pincl_mod);
    }

    fn add_include_description(&mut self, id: &IncludeDescription) {
        self.add_module_type(&id.pincl_mod);
    }

    fn add_case(&mut self, case: &Case) {
        self.add_pattern(&case.pc_lhs);
        if let Some(guard) = &case.pc_guard {
            self.add_expression(guard);
        }
        self.add_expression(&case.pc_rhs);
    }

    fn add_payload(&mut self, payload: &Payload) {
        match payload {
            Payload::PStr(str) => {
                self.add_structure(str);
            }
            Payload::PSig(sig) => {
                self.add_signature(sig);
            }
            Payload::PTyp(ty) => {
                self.add_core_type(ty);
            }
            Payload::PPat(pat, guard) => {
                self.add_pattern(pat);
                if let Some(guard) = guard {
                    self.add_expression(guard);
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parse_arena::{LocIdx, Located};

    /// Parse a dotted string like "Foo.Bar.baz" into a Longident using the arena.
    fn parse_longident(arena: &mut ParseArena, s: &str) -> Longident {
        let parts: Vec<&str> = s.split('.').collect();
        let mut lid = Longident::Lident(arena.intern_string(parts[0]));
        for part in &parts[1..] {
            lid = Longident::Ldot(Box::new(lid), arena.intern_string(part));
        }
        lid
    }

    fn make_ident_expr(arena: &mut ParseArena, lid: Longident) -> Expression {
        let lid_idx = arena.push_longident(lid);
        Expression {
            pexp_desc: ExpressionDesc::Pexp_ident(Located {
                txt: lid_idx,
                loc: LocIdx::none(),
            }),
            pexp_loc: LocIdx::none(),
            pexp_attributes: vec![],
        }
    }

    fn make_eval_item(expr: Expression) -> StructureItem {
        StructureItem {
            pstr_desc: StructureItemDesc::Pstr_eval(expr, vec![]),
            pstr_loc: LocIdx::none(),
        }
    }

    #[test]
    fn test_simple_module_reference() {
        // Test: Array.map extracts "Array"
        let mut arena = ParseArena::default();
        let lid = parse_longident(&mut arena, "Array.map");
        let expr = make_ident_expr(&mut arena, lid);
        let structure = vec![make_eval_item(expr)];

        let deps = extract_structure_deps(&arena, &structure);
        assert_eq!(deps.len(), 1);
        assert!(deps.contains("Array"));
    }

    #[test]
    fn test_nested_module_reference() {
        // Test: Belt.Array.map extracts "Belt"
        let mut arena = ParseArena::default();
        let lid = parse_longident(&mut arena, "Belt.Array.map");
        let expr = make_ident_expr(&mut arena, lid);
        let structure = vec![make_eval_item(expr)];

        let deps = extract_structure_deps(&arena, &structure);
        assert_eq!(deps.len(), 1);
        assert!(deps.contains("Belt"));
    }

    #[test]
    fn test_lowercase_identifier_ignored() {
        // Test: lowercase identifiers are not module references
        let mut arena = ParseArena::default();
        let foo_idx = arena.intern_string("foo");
        let lid = Longident::lident(foo_idx);
        let expr = make_ident_expr(&mut arena, lid);
        let structure = vec![make_eval_item(expr)];

        let deps = extract_structure_deps(&arena, &structure);
        assert!(deps.is_empty());
    }

    #[test]
    fn test_multiple_modules() {
        // Test: multiple module references are collected
        let mut arena = ParseArena::default();
        let lid1 = parse_longident(&mut arena, "Array.map");
        let lid2 = parse_longident(&mut arena, "Belt.Array.get");
        let lid3 = parse_longident(&mut arena, "Js.log");
        let structure = vec![
            make_eval_item(make_ident_expr(&mut arena, lid1)),
            make_eval_item(make_ident_expr(&mut arena, lid2)),
            make_eval_item(make_ident_expr(&mut arena, lid3)),
        ];

        let deps = extract_structure_deps(&arena, &structure);
        assert_eq!(deps.len(), 3);
        assert!(deps.contains("Array"));
        assert!(deps.contains("Belt"));
        assert!(deps.contains("Js"));
    }

    #[test]
    fn test_sorted_output() {
        // Test: output is sorted alphabetically
        let mut arena = ParseArena::default();
        let lid1 = parse_longident(&mut arena, "Zebra.x");
        let lid2 = parse_longident(&mut arena, "Alpha.y");
        let lid3 = parse_longident(&mut arena, "Middle.z");
        let structure = vec![
            make_eval_item(make_ident_expr(&mut arena, lid1)),
            make_eval_item(make_ident_expr(&mut arena, lid2)),
            make_eval_item(make_ident_expr(&mut arena, lid3)),
        ];

        let deps = extract_structure_deps(&arena, &structure);
        let deps_vec: Vec<_> = deps.into_iter().collect();
        assert_eq!(deps_vec, vec!["Alpha", "Middle", "Zebra"]);
    }

    #[test]
    fn test_predef_filtered() {
        // Test: *predef* is filtered out
        let arena = ParseArena::default();
        let mut collector = DependencyCollector::new(&arena);
        collector.deps.insert("*predef*".to_string());
        collector.deps.insert("Array".to_string());

        let deps = collector.finish();
        assert_eq!(deps.len(), 1);
        assert!(deps.contains("Array"));
        assert!(!deps.contains("*predef*"));
    }

    #[test]
    fn test_empty_string_filtered() {
        // Test: empty strings are filtered out
        let arena = ParseArena::default();
        let mut collector = DependencyCollector::new(&arena);
        collector.deps.insert(String::new());
        collector.deps.insert("Array".to_string());

        let deps = collector.finish();
        assert_eq!(deps.len(), 1);
        assert!(deps.contains("Array"));
    }

    #[test]
    fn test_deduplicated() {
        // Test: duplicate modules are deduplicated
        let mut arena = ParseArena::default();
        let lid1 = parse_longident(&mut arena, "Array.map");
        let lid2 = parse_longident(&mut arena, "Array.length");
        let lid3 = parse_longident(&mut arena, "Array.get");
        let structure = vec![
            make_eval_item(make_ident_expr(&mut arena, lid1)),
            make_eval_item(make_ident_expr(&mut arena, lid2)),
            make_eval_item(make_ident_expr(&mut arena, lid3)),
        ];

        let deps = extract_structure_deps(&arena, &structure);
        assert_eq!(deps.len(), 1);
        assert!(deps.contains("Array"));
    }

    #[test]
    fn test_empty_structure() {
        let arena = ParseArena::default();
        let deps = extract_structure_deps(&arena, &[]);
        assert!(deps.is_empty());
    }

    #[test]
    fn test_empty_signature() {
        let arena = ParseArena::default();
        let deps = extract_signature_deps(&arena, &[]);
        assert!(deps.is_empty());
    }
}
