//! Marshal implementations for parsetree0 types
//!
//! This module provides Marshal implementations for all parsetree0 AST types.
//! Tag assignments follow OCaml's variant encoding rules:
//! - Constant constructors (no data) are encoded as integers starting from 0
//! - Non-constant constructors (with data) are encoded as blocks with tags starting from 0

use super::marshal::MarshalWriter;
use super::parsetree0::*;
use super::serialize::Marshal;

// ========== Flag types (all constant constructors = integers) ==========

impl Marshal for RecFlag {
    fn marshal(&self, w: &mut MarshalWriter) {
        w.write_int(*self as i64);
    }
}

impl Marshal for DirectionFlag {
    fn marshal(&self, w: &mut MarshalWriter) {
        w.write_int(*self as i64);
    }
}

impl Marshal for PrivateFlag {
    fn marshal(&self, w: &mut MarshalWriter) {
        w.write_int(*self as i64);
    }
}

impl Marshal for MutableFlag {
    fn marshal(&self, w: &mut MarshalWriter) {
        w.write_int(*self as i64);
    }
}

impl Marshal for VirtualFlag {
    fn marshal(&self, w: &mut MarshalWriter) {
        w.write_int(*self as i64);
    }
}

impl Marshal for OverrideFlag {
    fn marshal(&self, w: &mut MarshalWriter) {
        w.write_int(*self as i64);
    }
}

impl Marshal for ClosedFlag {
    fn marshal(&self, w: &mut MarshalWriter) {
        w.write_int(*self as i64);
    }
}

impl Marshal for Variance {
    fn marshal(&self, w: &mut MarshalWriter) {
        w.write_int(*self as i64);
    }
}

// ========== ArgLabel (mixed: 1 constant, 2 non-constant) ==========

impl Marshal for ArgLabel {
    /// ArgLabel encoding:
    /// - Nolabel: int 0 (constant constructor)
    /// - Labelled(s): Block(tag=0, [s])
    /// - Optional(s): Block(tag=1, [s])
    fn marshal(&self, w: &mut MarshalWriter) {
        match self {
            ArgLabel::Nolabel => w.write_int(0),
            ArgLabel::Labelled(s) => {
                w.write_block_header(0, 1);
                w.write_str(s);
            }
            ArgLabel::Optional(s) => {
                w.write_block_header(1, 1);
                w.write_str(s);
            }
        }
    }
}

// ========== RecordElement<T> (for record patterns and expressions) ==========

impl<T: Marshal> Marshal for RecordElement<T> {
    /// RecordElement encoding: block with 3 fields (lid, x, opt)
    fn marshal(&self, w: &mut MarshalWriter) {
        w.write_block_header(0, 3);
        self.lid.marshal(w);
        self.x.marshal(w);
        w.write_int(if self.opt { 1 } else { 0 });
    }
}

// ========== Constant (all non-constant constructors) ==========

impl Marshal for Constant {
    /// Constant encoding:
    /// - Integer(s, opt): Block(tag=0, [s, opt])
    /// - Char(i): Block(tag=1, [i])
    /// - String(s, opt): Block(tag=2, [s, opt])
    /// - Float(s, opt): Block(tag=3, [s, opt])
    fn marshal(&self, w: &mut MarshalWriter) {
        match self {
            Constant::Integer(s, opt) => {
                w.write_block_header(0, 2);
                w.write_str(s);
                opt.marshal(w);
            }
            Constant::Char(i) => {
                w.write_block_header(1, 1);
                w.write_int(*i as i64);
            }
            Constant::String(s, opt) => {
                w.write_block_header(2, 2);
                w.write_str(s);
                opt.marshal(w);
            }
            Constant::Float(s, opt) => {
                w.write_block_header(3, 2);
                w.write_str(s);
                opt.marshal(w);
            }
        }
    }
}

// ========== Payload (all non-constant constructors) ==========

impl Marshal for Payload {
    /// Payload encoding:
    /// - PStr(s): Block(tag=0, [s])
    /// - PSig(s): Block(tag=1, [s])
    /// - PTyp(t): Block(tag=2, [t])
    /// - PPat(p, e): Block(tag=3, [p, e])
    fn marshal(&self, w: &mut MarshalWriter) {
        match self {
            Payload::PStr(s) => {
                w.write_block_header(0, 1);
                s.marshal(w);
            }
            Payload::PSig(s) => {
                w.write_block_header(1, 1);
                s.marshal(w);
            }
            Payload::PTyp(t) => {
                w.write_block_header(2, 1);
                t.marshal(w);
            }
            Payload::PPat(p, e) => {
                w.write_block_header(3, 2);
                p.marshal(w);
                e.marshal(w);
            }
        }
    }
}

// ========== CoreType ==========

impl Marshal for CoreType {
    fn marshal(&self, w: &mut MarshalWriter) {
        w.write_block_header(0, 3);
        self.ptyp_desc.marshal(w);
        self.ptyp_loc.marshal(w);
        self.ptyp_attributes.marshal(w);
    }
}

impl Marshal for CoreTypeDesc {
    /// CoreTypeDesc encoding:
    /// Constant: Any (int 0)
    /// Non-constant (tags 0-10):
    /// - Var(s): Block(tag=0)
    /// - Arrow(...): Block(tag=1)
    /// - Tuple(...): Block(tag=2)
    /// - Constr(...): Block(tag=3)
    /// - Object(...): Block(tag=4)
    /// - Class: Block(tag=5)
    /// - Alias(...): Block(tag=6)
    /// - Variant(...): Block(tag=7)
    /// - Poly(...): Block(tag=8)
    /// - Package(...): Block(tag=9)
    /// - Extension(...): Block(tag=10)
    fn marshal(&self, w: &mut MarshalWriter) {
        match self {
            CoreTypeDesc::Any => w.write_int(0),
            CoreTypeDesc::Var(s) => {
                w.write_block_header(0, 1);
                w.write_str(s);
            }
            CoreTypeDesc::Arrow(label, t1, t2) => {
                w.write_block_header(1, 3);
                label.marshal(w);
                t1.marshal(w);
                t2.marshal(w);
            }
            CoreTypeDesc::Tuple(types) => {
                w.write_block_header(2, 1);
                types.marshal(w);
            }
            CoreTypeDesc::Constr(lid, types) => {
                w.write_block_header(3, 2);
                lid.marshal(w);
                types.marshal(w);
            }
            CoreTypeDesc::Object(fields, flag) => {
                w.write_block_header(4, 2);
                fields.marshal(w);
                flag.marshal(w);
            }
            CoreTypeDesc::Class => {
                w.write_block_header(5, 1);
                ().marshal(w);
            }
            CoreTypeDesc::Alias(t, s) => {
                w.write_block_header(6, 2);
                t.marshal(w);
                w.write_str(s);
            }
            CoreTypeDesc::Variant(fields, flag, labels) => {
                w.write_block_header(7, 3);
                fields.marshal(w);
                flag.marshal(w);
                labels.marshal(w);
            }
            CoreTypeDesc::Poly(vars, t) => {
                w.write_block_header(8, 2);
                vars.marshal(w);
                t.marshal(w);
            }
            CoreTypeDesc::Package(pkg) => {
                w.write_block_header(9, 1);
                pkg.marshal(w);
            }
            CoreTypeDesc::Extension(ext) => {
                w.write_block_header(10, 1);
                ext.marshal(w);
            }
        }
    }
}

impl Marshal for RowField {
    /// RowField encoding:
    /// - Rtag(...): Block(tag=0, [label, attrs, bool, types])
    /// - Rinherit(t): Block(tag=1, [t])
    fn marshal(&self, w: &mut MarshalWriter) {
        match self {
            RowField::Rtag(label, attrs, b, types) => {
                w.write_block_header(0, 4);
                label.marshal(w);
                attrs.marshal(w);
                b.marshal(w);
                types.marshal(w);
            }
            RowField::Rinherit(t) => {
                w.write_block_header(1, 1);
                t.marshal(w);
            }
        }
    }
}

impl Marshal for ObjectField {
    /// ObjectField encoding:
    /// - Otag(...): Block(tag=0, [label, attrs, type])
    /// - Oinherit(t): Block(tag=1, [t])
    fn marshal(&self, w: &mut MarshalWriter) {
        match self {
            ObjectField::Otag(label, attrs, t) => {
                w.write_block_header(0, 3);
                label.marshal(w);
                attrs.marshal(w);
                t.marshal(w);
            }
            ObjectField::Oinherit(t) => {
                w.write_block_header(1, 1);
                t.marshal(w);
            }
        }
    }
}

// ========== Pattern ==========

impl Marshal for Pattern {
    fn marshal(&self, w: &mut MarshalWriter) {
        w.write_block_header(0, 3);
        self.ppat_desc.marshal(w);
        self.ppat_loc.marshal(w);
        self.ppat_attributes.marshal(w);
    }
}

impl Marshal for PatternDesc {
    /// PatternDesc encoding:
    /// Constant: Any (int 0)
    /// Non-constant (tags 0-16):
    /// - Var: tag 0
    /// - Alias: tag 1
    /// - Constant: tag 2
    /// - Interval: tag 3
    /// - Tuple: tag 4
    /// - Construct: tag 5
    /// - Variant: tag 6
    /// - Record: tag 7
    /// - Array: tag 8
    /// - Or: tag 9
    /// - Constraint: tag 10
    /// - Type: tag 11
    /// - Lazy: tag 12
    /// - Unpack: tag 13
    /// - Exception: tag 14
    /// - Extension: tag 15
    /// - Open: tag 16
    fn marshal(&self, w: &mut MarshalWriter) {
        match self {
            PatternDesc::Any => w.write_int(0),
            PatternDesc::Var(s) => {
                w.write_block_header(0, 1);
                s.marshal(w);
            }
            PatternDesc::Alias(p, s) => {
                w.write_block_header(1, 2);
                p.marshal(w);
                s.marshal(w);
            }
            PatternDesc::Constant(c) => {
                w.write_block_header(2, 1);
                c.marshal(w);
            }
            PatternDesc::Interval(c1, c2) => {
                w.write_block_header(3, 2);
                c1.marshal(w);
                c2.marshal(w);
            }
            PatternDesc::Tuple(pats) => {
                w.write_block_header(4, 1);
                pats.marshal(w);
            }
            PatternDesc::Construct(lid, opt) => {
                w.write_block_header(5, 2);
                lid.marshal(w);
                opt.marshal(w);
            }
            PatternDesc::Variant(label, opt) => {
                w.write_block_header(6, 2);
                w.write_str(label);
                opt.marshal(w);
            }
            PatternDesc::Record(fields, flag) => {
                w.write_block_header(7, 2);
                fields.marshal(w);
                flag.marshal(w);
            }
            PatternDesc::Array(pats) => {
                w.write_block_header(8, 1);
                pats.marshal(w);
            }
            PatternDesc::Or(p1, p2) => {
                w.write_block_header(9, 2);
                p1.marshal(w);
                p2.marshal(w);
            }
            PatternDesc::Constraint(p, t) => {
                w.write_block_header(10, 2);
                p.marshal(w);
                t.marshal(w);
            }
            PatternDesc::Type(lid) => {
                w.write_block_header(11, 1);
                lid.marshal(w);
            }
            PatternDesc::Lazy(p) => {
                w.write_block_header(12, 1);
                p.marshal(w);
            }
            PatternDesc::Unpack(s) => {
                w.write_block_header(13, 1);
                s.marshal(w);
            }
            PatternDesc::Exception(p) => {
                w.write_block_header(14, 1);
                p.marshal(w);
            }
            PatternDesc::Extension(ext) => {
                w.write_block_header(15, 1);
                ext.marshal(w);
            }
            PatternDesc::Open(lid, p) => {
                w.write_block_header(16, 2);
                lid.marshal(w);
                p.marshal(w);
            }
        }
    }
}

// ========== Expression ==========

impl Marshal for Expression {
    fn marshal(&self, w: &mut MarshalWriter) {
        w.write_block_header(0, 3);
        self.pexp_desc.marshal(w);
        self.pexp_loc.marshal(w);
        self.pexp_attributes.marshal(w);
    }
}

impl Marshal for ExpressionDesc {
    /// ExpressionDesc encoding:
    /// Constant: Unreachable (int 0)
    /// Non-constant (tags 0-34):
    fn marshal(&self, w: &mut MarshalWriter) {
        match self {
            ExpressionDesc::Ident(lid) => {
                w.write_block_header(0, 1);
                lid.marshal(w);
            }
            ExpressionDesc::Constant(c) => {
                w.write_block_header(1, 1);
                c.marshal(w);
            }
            ExpressionDesc::Let(flag, bindings, body) => {
                w.write_block_header(2, 3);
                flag.marshal(w);
                bindings.marshal(w);
                body.marshal(w);
            }
            // Pexp_fun is now tag 3 with 6 fields in current parsetree
            // (Pexp_function was removed)
            ExpressionDesc::Fun {
                arg_label,
                default,
                lhs,
                rhs,
                arity,
                is_async,
            } => {
                w.write_block_header(3, 6);
                arg_label.marshal(w);
                default.marshal(w);
                lhs.marshal(w);
                rhs.marshal(w);
                arity.marshal(w);
                w.write_int(if *is_async { 1 } else { 0 });
            }
            // Pexp_apply has 4 fields in current parsetree
            ExpressionDesc::Apply {
                funct,
                args,
                partial,
                transformed_jsx,
            } => {
                w.write_block_header(4, 4);
                funct.marshal(w);
                args.marshal(w);
                w.write_int(if *partial { 1 } else { 0 });
                w.write_int(if *transformed_jsx { 1 } else { 0 });
            }
            ExpressionDesc::Match(e, cases) => {
                w.write_block_header(5, 2);
                e.marshal(w);
                cases.marshal(w);
            }
            ExpressionDesc::Try(e, cases) => {
                w.write_block_header(6, 2);
                e.marshal(w);
                cases.marshal(w);
            }
            ExpressionDesc::Tuple(exprs) => {
                w.write_block_header(7, 1);
                exprs.marshal(w);
            }
            ExpressionDesc::Construct(lid, opt) => {
                w.write_block_header(8, 2);
                lid.marshal(w);
                opt.marshal(w);
            }
            ExpressionDesc::Variant(label, opt) => {
                w.write_block_header(9, 2);
                w.write_str(label);
                opt.marshal(w);
            }
            ExpressionDesc::Record(fields, opt) => {
                w.write_block_header(10, 2);
                fields.marshal(w);
                opt.marshal(w);
            }
            ExpressionDesc::Field(e, lid) => {
                w.write_block_header(11, 2);
                e.marshal(w);
                lid.marshal(w);
            }
            ExpressionDesc::Setfield(e1, lid, e2) => {
                w.write_block_header(12, 3);
                e1.marshal(w);
                lid.marshal(w);
                e2.marshal(w);
            }
            ExpressionDesc::Array(exprs) => {
                w.write_block_header(13, 1);
                exprs.marshal(w);
            }
            ExpressionDesc::Ifthenelse(e1, e2, e3) => {
                w.write_block_header(14, 3);
                e1.marshal(w);
                e2.marshal(w);
                e3.marshal(w);
            }
            ExpressionDesc::Sequence(e1, e2) => {
                w.write_block_header(15, 2);
                e1.marshal(w);
                e2.marshal(w);
            }
            ExpressionDesc::While(e1, e2) => {
                w.write_block_header(16, 2);
                e1.marshal(w);
                e2.marshal(w);
            }
            ExpressionDesc::For(pat, e1, e2, flag, e3) => {
                w.write_block_header(18, 5);
                pat.marshal(w);
                e1.marshal(w);
                e2.marshal(w);
                flag.marshal(w);
                e3.marshal(w);
            }
            ExpressionDesc::Constraint(e, t) => {
                w.write_block_header(19, 2);
                e.marshal(w);
                t.marshal(w);
            }
            ExpressionDesc::Coerce(e, (), t) => {
                w.write_block_header(20, 3);
                e.marshal(w);
                ().marshal(w); // unit placeholder
                t.marshal(w);
            }
            ExpressionDesc::Send(e, label) => {
                w.write_block_header(21, 2);
                e.marshal(w);
                label.marshal(w);
            }
            ExpressionDesc::New(lid) => {
                w.write_block_header(22, 1);
                lid.marshal(w);
            }
            ExpressionDesc::Setinstvar(label, e) => {
                w.write_block_header(23, 2);
                label.marshal(w);
                e.marshal(w);
            }
            ExpressionDesc::Override(fields) => {
                w.write_block_header(24, 1);
                fields.marshal(w);
            }
            ExpressionDesc::Letmodule(name, me, body) => {
                w.write_block_header(25, 3);
                name.marshal(w);
                me.marshal(w);
                body.marshal(w);
            }
            ExpressionDesc::Letexception(ext, body) => {
                w.write_block_header(26, 2);
                ext.marshal(w);
                body.marshal(w);
            }
            ExpressionDesc::Assert(e) => {
                w.write_block_header(27, 1);
                e.marshal(w);
            }
            ExpressionDesc::Lazy(e) => {
                w.write_block_header(28, 1);
                e.marshal(w);
            }
            ExpressionDesc::Poly(e, t) => {
                w.write_block_header(29, 2);
                e.marshal(w);
                t.marshal(w);
            }
            ExpressionDesc::Object => {
                w.write_block_header(30, 1);
                ().marshal(w);
            }
            ExpressionDesc::Newtype(name, body) => {
                w.write_block_header(31, 2);
                name.marshal(w);
                body.marshal(w);
            }
            ExpressionDesc::Pack(me) => {
                w.write_block_header(32, 1);
                me.marshal(w);
            }
            ExpressionDesc::Open(flag, lid, body) => {
                w.write_block_header(33, 3);
                flag.marshal(w);
                lid.marshal(w);
                body.marshal(w);
            }
            ExpressionDesc::Extension(ext) => {
                w.write_block_header(34, 1);
                ext.marshal(w);
            }
            ExpressionDesc::Unreachable => w.write_int(0),
        }
    }
}

impl Marshal for Case {
    fn marshal(&self, w: &mut MarshalWriter) {
        w.write_block_header(0, 3);
        self.pc_lhs.marshal(w);
        self.pc_guard.marshal(w);
        self.pc_rhs.marshal(w);
    }
}

// ========== Value binding and description ==========

impl Marshal for ValueBinding {
    fn marshal(&self, w: &mut MarshalWriter) {
        w.write_block_header(0, 4);
        self.pvb_pat.marshal(w);
        self.pvb_expr.marshal(w);
        self.pvb_attributes.marshal(w);
        self.pvb_loc.marshal(w);
    }
}

impl Marshal for ValueDescription {
    fn marshal(&self, w: &mut MarshalWriter) {
        w.write_block_header(0, 5);
        self.pval_name.marshal(w);
        self.pval_type.marshal(w);
        self.pval_prim.marshal(w);
        self.pval_attributes.marshal(w);
        self.pval_loc.marshal(w);
    }
}

// ========== Type declarations ==========

impl Marshal for TypeDeclaration {
    fn marshal(&self, w: &mut MarshalWriter) {
        w.write_block_header(0, 8);
        self.ptype_name.marshal(w);
        self.ptype_params.marshal(w);
        self.ptype_cstrs.marshal(w);
        self.ptype_kind.marshal(w);
        self.ptype_private.marshal(w);
        self.ptype_manifest.marshal(w);
        self.ptype_attributes.marshal(w);
        self.ptype_loc.marshal(w);
    }
}

impl Marshal for TypeKind {
    /// TypeKind encoding:
    /// Constant: Abstract (int 0), Open (int 1)
    /// Non-constant: Variant (tag 0), Record (tag 1)
    fn marshal(&self, w: &mut MarshalWriter) {
        match self {
            TypeKind::Abstract => w.write_int(0),
            TypeKind::Variant(ctors) => {
                w.write_block_header(0, 1);
                ctors.marshal(w);
            }
            TypeKind::Record(labels) => {
                w.write_block_header(1, 1);
                labels.marshal(w);
            }
            TypeKind::Open => w.write_int(1),
        }
    }
}

impl Marshal for LabelDeclaration {
    fn marshal(&self, w: &mut MarshalWriter) {
        w.write_block_header(0, 5);
        self.pld_name.marshal(w);
        self.pld_mutable.marshal(w);
        self.pld_type.marshal(w);
        self.pld_loc.marshal(w);
        self.pld_attributes.marshal(w);
    }
}

impl Marshal for ConstructorDeclaration {
    fn marshal(&self, w: &mut MarshalWriter) {
        w.write_block_header(0, 5);
        self.pcd_name.marshal(w);
        self.pcd_args.marshal(w);
        self.pcd_res.marshal(w);
        self.pcd_loc.marshal(w);
        self.pcd_attributes.marshal(w);
    }
}

impl Marshal for ConstructorArguments {
    /// ConstructorArguments encoding:
    /// - Tuple: Block(tag=0, [types])
    /// - Record: Block(tag=1, [labels])
    fn marshal(&self, w: &mut MarshalWriter) {
        match self {
            ConstructorArguments::Tuple(types) => {
                w.write_block_header(0, 1);
                types.marshal(w);
            }
            ConstructorArguments::Record(labels) => {
                w.write_block_header(1, 1);
                labels.marshal(w);
            }
        }
    }
}

// ========== Type extensions ==========

impl Marshal for TypeExtension {
    fn marshal(&self, w: &mut MarshalWriter) {
        w.write_block_header(0, 5);
        self.ptyext_path.marshal(w);
        self.ptyext_params.marshal(w);
        self.ptyext_constructors.marshal(w);
        self.ptyext_private.marshal(w);
        self.ptyext_attributes.marshal(w);
    }
}

impl Marshal for ExtensionConstructor {
    fn marshal(&self, w: &mut MarshalWriter) {
        w.write_block_header(0, 4);
        self.pext_name.marshal(w);
        self.pext_kind.marshal(w);
        self.pext_loc.marshal(w);
        self.pext_attributes.marshal(w);
    }
}

impl Marshal for ExtensionConstructorKind {
    /// ExtensionConstructorKind encoding:
    /// - Decl: Block(tag=0, [args, res])
    /// - Rebind: Block(tag=1, [lid])
    fn marshal(&self, w: &mut MarshalWriter) {
        match self {
            ExtensionConstructorKind::Decl(args, res) => {
                w.write_block_header(0, 2);
                args.marshal(w);
                res.marshal(w);
            }
            ExtensionConstructorKind::Rebind(lid) => {
                w.write_block_header(1, 1);
                lid.marshal(w);
            }
        }
    }
}

// ========== Module types ==========

impl Marshal for ModuleType {
    fn marshal(&self, w: &mut MarshalWriter) {
        w.write_block_header(0, 3);
        self.pmty_desc.marshal(w);
        self.pmty_loc.marshal(w);
        self.pmty_attributes.marshal(w);
    }
}

impl Marshal for ModuleTypeDesc {
    /// ModuleTypeDesc encoding (all non-constant):
    /// - Ident: tag 0
    /// - Signature: tag 1
    /// - Functor: tag 2
    /// - With: tag 3
    /// - Typeof: tag 4
    /// - Extension: tag 5
    /// - Alias: tag 6
    fn marshal(&self, w: &mut MarshalWriter) {
        match self {
            ModuleTypeDesc::Ident(lid) => {
                w.write_block_header(0, 1);
                lid.marshal(w);
            }
            ModuleTypeDesc::Signature(sig) => {
                w.write_block_header(1, 1);
                sig.marshal(w);
            }
            ModuleTypeDesc::Functor(name, mty, body) => {
                w.write_block_header(2, 3);
                name.marshal(w);
                mty.marshal(w);
                body.marshal(w);
            }
            ModuleTypeDesc::With(mty, constraints) => {
                w.write_block_header(3, 2);
                mty.marshal(w);
                constraints.marshal(w);
            }
            ModuleTypeDesc::Typeof(me) => {
                w.write_block_header(4, 1);
                me.marshal(w);
            }
            ModuleTypeDesc::Extension(ext) => {
                w.write_block_header(5, 1);
                ext.marshal(w);
            }
            ModuleTypeDesc::Alias(lid) => {
                w.write_block_header(6, 1);
                lid.marshal(w);
            }
        }
    }
}

impl Marshal for WithConstraint {
    /// WithConstraint encoding:
    /// - Type: Block(tag=0, [lid, decl])
    /// - Module: Block(tag=1, [lid1, lid2])
    /// - TypeSubst: Block(tag=2, [lid, decl])
    /// - ModSubst: Block(tag=3, [lid1, lid2])
    fn marshal(&self, w: &mut MarshalWriter) {
        match self {
            WithConstraint::Type(lid, decl) => {
                w.write_block_header(0, 2);
                lid.marshal(w);
                decl.marshal(w);
            }
            WithConstraint::Module(lid1, lid2) => {
                w.write_block_header(1, 2);
                lid1.marshal(w);
                lid2.marshal(w);
            }
            WithConstraint::TypeSubst(lid, decl) => {
                w.write_block_header(2, 2);
                lid.marshal(w);
                decl.marshal(w);
            }
            WithConstraint::ModSubst(lid1, lid2) => {
                w.write_block_header(3, 2);
                lid1.marshal(w);
                lid2.marshal(w);
            }
        }
    }
}

// ========== Signatures ==========

impl Marshal for SignatureItem {
    fn marshal(&self, w: &mut MarshalWriter) {
        w.write_block_header(0, 2);
        self.psig_desc.marshal(w);
        self.psig_loc.marshal(w);
    }
}

impl Marshal for SignatureItemDesc {
    /// SignatureItemDesc encoding:
    /// Non-constant: Value(0), Type(1), Typext(2), Exception(3), Module(4),
    /// Recmodule(5), Modtype(6), Open(7), Include(8), Class(9), ClassType(10),
    /// Attribute(11), Extension(12)
    fn marshal(&self, w: &mut MarshalWriter) {
        match self {
            SignatureItemDesc::Value(vd) => {
                w.write_block_header(0, 1);
                vd.marshal(w);
            }
            SignatureItemDesc::Type(flag, decls) => {
                w.write_block_header(1, 2);
                flag.marshal(w);
                decls.marshal(w);
            }
            SignatureItemDesc::Typext(te) => {
                w.write_block_header(2, 1);
                te.marshal(w);
            }
            SignatureItemDesc::Exception(ext) => {
                w.write_block_header(3, 1);
                ext.marshal(w);
            }
            SignatureItemDesc::Module(md) => {
                w.write_block_header(4, 1);
                md.marshal(w);
            }
            SignatureItemDesc::Recmodule(mds) => {
                w.write_block_header(5, 1);
                mds.marshal(w);
            }
            SignatureItemDesc::Modtype(mtd) => {
                w.write_block_header(6, 1);
                mtd.marshal(w);
            }
            SignatureItemDesc::Open(od) => {
                w.write_block_header(7, 1);
                od.marshal(w);
            }
            SignatureItemDesc::Include(id) => {
                w.write_block_header(8, 1);
                id.marshal(w);
            }
            SignatureItemDesc::Class => {
                w.write_block_header(9, 1);
                ().marshal(w);
            }
            SignatureItemDesc::ClassType => {
                w.write_block_header(10, 1);
                ().marshal(w);
            }
            SignatureItemDesc::Attribute(attr) => {
                w.write_block_header(11, 1);
                attr.marshal(w);
            }
            SignatureItemDesc::Extension(ext, attrs) => {
                w.write_block_header(12, 2);
                ext.marshal(w);
                attrs.marshal(w);
            }
        }
    }
}

impl Marshal for ModuleDeclaration {
    fn marshal(&self, w: &mut MarshalWriter) {
        w.write_block_header(0, 4);
        self.pmd_name.marshal(w);
        self.pmd_type.marshal(w);
        self.pmd_attributes.marshal(w);
        self.pmd_loc.marshal(w);
    }
}

impl Marshal for ModuleTypeDeclaration {
    fn marshal(&self, w: &mut MarshalWriter) {
        w.write_block_header(0, 4);
        self.pmtd_name.marshal(w);
        self.pmtd_type.marshal(w);
        self.pmtd_attributes.marshal(w);
        self.pmtd_loc.marshal(w);
    }
}

impl Marshal for OpenDescription {
    fn marshal(&self, w: &mut MarshalWriter) {
        w.write_block_header(0, 4);
        self.popen_lid.marshal(w);
        self.popen_override.marshal(w);
        self.popen_loc.marshal(w);
        self.popen_attributes.marshal(w);
    }
}

impl<A: Marshal> Marshal for IncludeInfos<A> {
    fn marshal(&self, w: &mut MarshalWriter) {
        w.write_block_header(0, 3);
        self.pincl_mod.marshal(w);
        self.pincl_loc.marshal(w);
        self.pincl_attributes.marshal(w);
    }
}

// ========== Module expressions ==========

impl Marshal for ModuleExpr {
    fn marshal(&self, w: &mut MarshalWriter) {
        w.write_block_header(0, 3);
        self.pmod_desc.marshal(w);
        self.pmod_loc.marshal(w);
        self.pmod_attributes.marshal(w);
    }
}

impl Marshal for ModuleExprDesc {
    /// ModuleExprDesc encoding (all non-constant):
    /// - Ident: tag 0
    /// - Structure: tag 1
    /// - Functor: tag 2
    /// - Apply: tag 3
    /// - Constraint: tag 4
    /// - Unpack: tag 5
    /// - Extension: tag 6
    fn marshal(&self, w: &mut MarshalWriter) {
        match self {
            ModuleExprDesc::Ident(lid) => {
                w.write_block_header(0, 1);
                lid.marshal(w);
            }
            ModuleExprDesc::Structure(str) => {
                w.write_block_header(1, 1);
                str.marshal(w);
            }
            ModuleExprDesc::Functor(name, mty, body) => {
                w.write_block_header(2, 3);
                name.marshal(w);
                mty.marshal(w);
                body.marshal(w);
            }
            ModuleExprDesc::Apply(me1, me2) => {
                w.write_block_header(3, 2);
                me1.marshal(w);
                me2.marshal(w);
            }
            ModuleExprDesc::Constraint(me, mty) => {
                w.write_block_header(4, 2);
                me.marshal(w);
                mty.marshal(w);
            }
            ModuleExprDesc::Unpack(e) => {
                w.write_block_header(5, 1);
                e.marshal(w);
            }
            ModuleExprDesc::Extension(ext) => {
                w.write_block_header(6, 1);
                ext.marshal(w);
            }
        }
    }
}

// ========== Structures ==========

impl Marshal for StructureItem {
    fn marshal(&self, w: &mut MarshalWriter) {
        w.write_block_header(0, 2);
        self.pstr_desc.marshal(w);
        self.pstr_loc.marshal(w);
    }
}

impl Marshal for StructureItemDesc {
    /// StructureItemDesc encoding (all non-constant):
    /// - Eval: tag 0
    /// - Value: tag 1
    /// - Primitive: tag 2
    /// - Type: tag 3
    /// - Typext: tag 4
    /// - Exception: tag 5
    /// - Module: tag 6
    /// - Recmodule: tag 7
    /// - Modtype: tag 8
    /// - Open: tag 9
    /// - Class: tag 10
    /// - ClassType: tag 11
    /// - Include: tag 12
    /// - Attribute: tag 13
    /// - Extension: tag 14
    fn marshal(&self, w: &mut MarshalWriter) {
        match self {
            StructureItemDesc::Eval(e, attrs) => {
                w.write_block_header(0, 2);
                e.marshal(w);
                attrs.marshal(w);
            }
            StructureItemDesc::Value(flag, bindings) => {
                w.write_block_header(1, 2);
                flag.marshal(w);
                bindings.marshal(w);
            }
            StructureItemDesc::Primitive(vd) => {
                w.write_block_header(2, 1);
                vd.marshal(w);
            }
            StructureItemDesc::Type(flag, decls) => {
                w.write_block_header(3, 2);
                flag.marshal(w);
                decls.marshal(w);
            }
            StructureItemDesc::Typext(te) => {
                w.write_block_header(4, 1);
                te.marshal(w);
            }
            StructureItemDesc::Exception(ext) => {
                w.write_block_header(5, 1);
                ext.marshal(w);
            }
            StructureItemDesc::Module(mb) => {
                w.write_block_header(6, 1);
                mb.marshal(w);
            }
            StructureItemDesc::Recmodule(mbs) => {
                w.write_block_header(7, 1);
                mbs.marshal(w);
            }
            StructureItemDesc::Modtype(mtd) => {
                w.write_block_header(8, 1);
                mtd.marshal(w);
            }
            StructureItemDesc::Open(od) => {
                w.write_block_header(9, 1);
                od.marshal(w);
            }
            StructureItemDesc::Class => {
                w.write_block_header(10, 1);
                ().marshal(w);
            }
            StructureItemDesc::ClassType => {
                w.write_block_header(11, 1);
                ().marshal(w);
            }
            StructureItemDesc::Include(id) => {
                w.write_block_header(12, 1);
                id.marshal(w);
            }
            StructureItemDesc::Attribute(attr) => {
                w.write_block_header(13, 1);
                attr.marshal(w);
            }
            StructureItemDesc::Extension(ext, attrs) => {
                w.write_block_header(14, 2);
                ext.marshal(w);
                attrs.marshal(w);
            }
        }
    }
}

impl Marshal for ModuleBinding {
    fn marshal(&self, w: &mut MarshalWriter) {
        w.write_block_header(0, 4);
        self.pmb_name.marshal(w);
        self.pmb_expr.marshal(w);
        self.pmb_attributes.marshal(w);
        self.pmb_loc.marshal(w);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_rec_flag() {
        let mut w = MarshalWriter::new();
        RecFlag::Nonrecursive.marshal(&mut w);
        assert_eq!(w.payload(), &[0x40]); // int 0

        w.reset();
        RecFlag::Recursive.marshal(&mut w);
        assert_eq!(w.payload(), &[0x41]); // int 1
    }

    #[test]
    fn test_arg_label() {
        let mut w = MarshalWriter::new();
        ArgLabel::Nolabel.marshal(&mut w);
        assert_eq!(w.payload(), &[0x40]); // int 0

        w.reset();
        ArgLabel::Labelled("x".to_string()).marshal(&mut w);
        // Block(tag=0, size=1), "x"
        assert_eq!(w.payload(), &[0x90, 0x21, b'x']);

        w.reset();
        ArgLabel::Optional("y".to_string()).marshal(&mut w);
        // Block(tag=1, size=1), "y"
        assert_eq!(w.payload(), &[0x91, 0x21, b'y']);
    }

    #[test]
    fn test_constant_integer() {
        let mut w = MarshalWriter::new();
        Constant::Integer("42".to_string(), None).marshal(&mut w);
        // Block(tag=0, size=2), "42", None
        let payload = w.payload();
        assert_eq!(payload[0], 0xA0); // block tag=0, size=2
        assert_eq!(payload[1], 0x22); // string len=2
        assert_eq!(&payload[2..4], b"42");
        assert_eq!(payload[4], 0x40); // None = int 0
    }

    #[test]
    fn test_core_type_any() {
        let mut w = MarshalWriter::new();
        let ty = CoreType {
            ptyp_desc: CoreTypeDesc::Any,
            ptyp_loc: crate::location::Location::none(),
            ptyp_attributes: vec![],
        };
        ty.marshal(&mut w);

        let payload = w.payload();
        assert_eq!(payload[0], 0xB0); // block tag=0, size=3
        assert_eq!(payload[1], 0x40); // Ptyp_any = int 0
    }

    #[test]
    fn test_pattern_any() {
        let mut w = MarshalWriter::new();
        let pat = Pattern {
            ppat_desc: PatternDesc::Any,
            ppat_loc: crate::location::Location::none(),
            ppat_attributes: vec![],
        };
        pat.marshal(&mut w);

        let payload = w.payload();
        assert_eq!(payload[0], 0xB0); // block tag=0, size=3
        assert_eq!(payload[1], 0x40); // Ppat_any = int 0
    }

    #[test]
    fn test_expression_ident() {
        use crate::parser::longident::Longident;

        let mut w = MarshalWriter::new();
        let expr = Expression {
            pexp_desc: ExpressionDesc::Ident(crate::location::Located::new(
                Longident::Lident("x".to_string()),
                crate::location::Location::none(),
            )),
            pexp_loc: crate::location::Location::none(),
            pexp_attributes: vec![],
        };
        expr.marshal(&mut w);

        let payload = w.payload();
        assert_eq!(payload[0], 0xB0); // block tag=0, size=3 (expression)
        assert_eq!(payload[1], 0x90); // Pexp_ident: block tag=0, size=1
    }

    #[test]
    fn test_type_kind() {
        let mut w = MarshalWriter::new();
        TypeKind::Abstract.marshal(&mut w);
        assert_eq!(w.payload(), &[0x40]); // int 0

        w.reset();
        TypeKind::Open.marshal(&mut w);
        assert_eq!(w.payload(), &[0x41]); // int 1

        w.reset();
        TypeKind::Variant(vec![]).marshal(&mut w);
        // Block(tag=0, size=1), empty list
        assert_eq!(w.payload(), &[0x90, 0x40]);

        w.reset();
        TypeKind::Record(vec![]).marshal(&mut w);
        // Block(tag=1, size=1), empty list
        assert_eq!(w.payload(), &[0x91, 0x40]);
    }
}
