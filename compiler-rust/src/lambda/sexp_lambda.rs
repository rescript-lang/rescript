//! Lambda IR sexp printing for parity testing.
//!
//! This module provides S-expression output for Lambda IR that matches
//! the OCaml compiler's output, enabling parity testing between implementations.

use std::io::Write;

use crate::lambda::constant::{Constant, PointerInfo, StringDelim};
use crate::lambda::compat::{Comparison, FieldDbgInfo, LetKind, SetFieldDbgInfo};
use crate::lambda::primitive::{Mutable, Primitive};
use crate::lambda::tag_info::TagInfo;
use crate::parser::ast::MutableFlag;
use crate::lambda::{
    ApInfo, Apply, ApplyStatus, DirectionFlag, FunctionAttribute, InlineAttribute, LFunction,
    Lambda, LambdaSwitch, PrimInfo,
};

/// Simple S-expression type for output.
pub enum Sexp {
    Atom(String),
    List(Vec<Sexp>),
}

impl Sexp {
    pub fn atom(s: impl Into<String>) -> Self {
        Sexp::Atom(s.into())
    }

    pub fn list(items: Vec<Sexp>) -> Self {
        Sexp::List(items)
    }

    fn to_string_indent(&self, indent: usize) -> String {
        let spaces = " ".repeat(indent * 2);
        match self {
            Sexp::Atom(s) => s.clone(),
            Sexp::List(items) if items.is_empty() => "()".to_string(),
            Sexp::List(items) if items.len() == 1 => {
                format!("({})", items[0].to_string_indent(indent))
            }
            Sexp::List(items) => {
                let inner: Vec<String> = items
                    .iter()
                    .map(|item| item.to_string_indent(indent + 1))
                    .collect();
                format!("({})", inner.join(&format!("\n{}  ", spaces)))
            }
        }
    }

    pub fn to_string(&self) -> String {
        self.to_string_indent(0)
    }
}

// Helper functions

fn string(s: &str) -> Sexp {
    Sexp::atom(format!("\"{}\"", s.escape_default()))
}

fn opt_string(s: &Option<String>) -> Sexp {
    match s {
        None => Sexp::atom("None"),
        Some(s) => Sexp::list(vec![Sexp::atom("Some"), string(s)]),
    }
}

fn opt<T>(f: impl Fn(&T) -> Sexp, v: &Option<T>) -> Sexp {
    match v {
        None => Sexp::atom("None"),
        Some(x) => Sexp::list(vec![Sexp::atom("Some"), f(x)]),
    }
}

fn bool_sexp(b: bool) -> Sexp {
    Sexp::atom(if b { "true" } else { "false" })
}

fn int(i: i32) -> Sexp {
    Sexp::atom(i.to_string())
}

// Ident - name only for determinism
fn ident(id: &crate::ident::Ident) -> Sexp {
    string(id.name())
}

// Direction flag
fn direction_flag(d: &DirectionFlag) -> Sexp {
    match d {
        DirectionFlag::Upto => Sexp::atom("Upto"),
        DirectionFlag::Downto => Sexp::atom("Downto"),
    }
}

// Comparison
fn comparison(cmp: &Comparison) -> Sexp {
    match cmp {
        Comparison::Eq => Sexp::atom("Ceq"),
        Comparison::Neq => Sexp::atom("Cneq"),
        Comparison::Lt => Sexp::atom("Clt"),
        Comparison::Le => Sexp::atom("Cle"),
        Comparison::Gt => Sexp::atom("Cgt"),
        Comparison::Ge => Sexp::atom("Cge"),
    }
}

// Let kind
fn let_kind(k: &LetKind) -> Sexp {
    match k {
        LetKind::Alias => Sexp::atom("Alias"),
        LetKind::Strict => Sexp::atom("Strict"),
        LetKind::StrictOpt => Sexp::atom("StrictOpt"),
        LetKind::Variable => Sexp::atom("Variable"),
    }
}

// Apply status
fn apply_status(s: &ApplyStatus) -> Sexp {
    match s {
        ApplyStatus::Na => Sexp::atom("App_na"),
        ApplyStatus::InferFull => Sexp::atom("App_infer_full"),
        ApplyStatus::Uncurry => Sexp::atom("App_uncurry"),
    }
}

// Inline attribute
fn inline_attribute(attr: &InlineAttribute) -> Sexp {
    match attr {
        InlineAttribute::Always => Sexp::atom("Always_inline"),
        InlineAttribute::Never => Sexp::atom("Never_inline"),
        InlineAttribute::Default => Sexp::atom("Default_inline"),
    }
}

// Mutable flag (from parser::ast::MutableFlag)
fn mutable_flag_sexp(m: &MutableFlag) -> Sexp {
    match m {
        MutableFlag::Mutable => Sexp::atom("Mutable"),
        MutableFlag::Immutable => Sexp::atom("Immutable"),
    }
}

// Mutable (from lambda::primitive::Mutable)
fn mutable_sexp(m: &Mutable) -> Sexp {
    match m {
        Mutable::Mutable => Sexp::atom("Mutable"),
        Mutable::Immutable => Sexp::atom("Immutable"),
    }
}

// Tag info
fn tag_info(info: &TagInfo) -> Sexp {
    match info {
        TagInfo::Constructor {
            name,
            num_nonconst,
            tag,
            ..
        } => Sexp::list(vec![
            Sexp::atom("Blk_constructor"),
            string(name),
            int(*num_nonconst),
            int(*tag as i32),
        ]),
        TagInfo::Tuple => Sexp::atom("Blk_tuple"),
        TagInfo::PolyVar(s) => Sexp::list(vec![Sexp::atom("Blk_poly_var"), string(s)]),
        TagInfo::Record {
            fields,
            mutable_flag,
        } => Sexp::list(vec![
            Sexp::atom("Blk_record"),
            Sexp::list(
                fields
                    .iter()
                    .map(|(name, _)| string(name))
                    .collect(),
            ),
            mutable_flag_sexp(mutable_flag),
        ]),
        TagInfo::RecordInlined {
            name,
            num_nonconst,
            tag,
            fields,
            mutable_flag,
            ..
        } => Sexp::list(vec![
            Sexp::atom("Blk_record_inlined"),
            string(name),
            int(*num_nonconst),
            int(*tag as i32),
            Sexp::list(
                fields
                    .iter()
                    .map(|(name, _)| string(name))
                    .collect(),
            ),
            mutable_flag_sexp(mutable_flag),
        ]),
        TagInfo::Module(fields) => {
            let mut items = vec![Sexp::atom("Blk_module")];
            items.extend(fields.iter().map(|s| string(s)));
            Sexp::list(items)
        }
        TagInfo::ModuleExport(_) => Sexp::atom("Blk_module_export"),
        TagInfo::Extension => Sexp::atom("Blk_extension"),
        TagInfo::Some => Sexp::atom("Blk_some"),
        TagInfo::SomeNotNested => Sexp::atom("Blk_some_not_nested"),
        TagInfo::RecordExt {
            fields,
            mutable_flag,
        } => Sexp::list(vec![
            Sexp::atom("Blk_record_ext"),
            Sexp::list(fields.iter().map(|s| string(s)).collect()),
            mutable_flag_sexp(mutable_flag),
        ]),
    }
}

// Field debug info
fn field_dbg_info(info: &FieldDbgInfo) -> Sexp {
    match info {
        FieldDbgInfo::Record { name, mutable_flag } => Sexp::list(vec![
            Sexp::atom("Fld_record"),
            string(name),
            mutable_flag_sexp(mutable_flag),
        ]),
        FieldDbgInfo::Module { name } => {
            Sexp::list(vec![Sexp::atom("Fld_module"), string(name)])
        }
        FieldDbgInfo::RecordInline { name } => {
            Sexp::list(vec![Sexp::atom("Fld_record_inline"), string(name)])
        }
        FieldDbgInfo::RecordExtension { name } => {
            Sexp::list(vec![Sexp::atom("Fld_record_extension"), string(name)])
        }
        FieldDbgInfo::Tuple => Sexp::atom("Fld_tuple"),
        FieldDbgInfo::PolyVarTag => Sexp::atom("Fld_poly_var_tag"),
        FieldDbgInfo::PolyVarContent => Sexp::atom("Fld_poly_var_content"),
        FieldDbgInfo::Extension => Sexp::atom("Fld_extension"),
        FieldDbgInfo::Variant => Sexp::atom("Fld_variant"),
        FieldDbgInfo::VariantTag => Sexp::atom("Fld_variant_tag"),
        FieldDbgInfo::Cons => Sexp::atom("Fld_cons"),
    }
}

// Set field debug info
fn set_field_dbg_info(info: &SetFieldDbgInfo) -> Sexp {
    match info {
        SetFieldDbgInfo::RecordSet(s) => Sexp::list(vec![Sexp::atom("Fld_record_set"), string(s)]),
        SetFieldDbgInfo::RecordInlineSet(s) => {
            Sexp::list(vec![Sexp::atom("Fld_record_inline_set"), string(s)])
        }
        SetFieldDbgInfo::RecordExtensionSet(s) => {
            Sexp::list(vec![Sexp::atom("Fld_record_extension_set"), string(s)])
        }
    }
}

// Pointer info
fn pointer_info(info: &Option<PointerInfo>) -> Sexp {
    match info {
        None => Sexp::atom("None"),
        Some(PointerInfo::None) => Sexp::atom("None"),
        Some(PointerInfo::Constructor(tag)) => Sexp::list(vec![
            Sexp::atom("Pt_constructor"),
            string(&tag.name),
            int(tag.const_),
            int(tag.non_const),
        ]),
        Some(PointerInfo::AssertFalse) => Sexp::atom("Pt_assertfalse"),
        Some(PointerInfo::Some(s)) => Sexp::list(vec![Sexp::atom("Some"), string(s)]),
    }
}

// String delimiter
fn string_delim(delim: &Option<StringDelim>) -> Sexp {
    match delim {
        None => Sexp::atom("None"),
        Some(StringDelim::NoQuotes) => {
            Sexp::list(vec![Sexp::atom("Some"), Sexp::atom("DNoQuotes")])
        }
        Some(StringDelim::Backtick) => {
            Sexp::list(vec![Sexp::atom("Some"), Sexp::atom("DBackQuotes")])
        }
    }
}

// Constant
fn constant(c: &Constant) -> Sexp {
    match c {
        Constant::JsNull => Sexp::atom("Const_js_null"),
        Constant::JsUndefined { is_unit } => {
            Sexp::list(vec![Sexp::atom("Const_js_undefined"), bool_sexp(*is_unit)])
        }
        Constant::JsTrue => Sexp::atom("Const_js_true"),
        Constant::JsFalse => Sexp::atom("Const_js_false"),
        Constant::Int { i, comment } => Sexp::list(vec![
            Sexp::atom("Const_int"),
            Sexp::atom(i.to_string()),
            pointer_info(comment),
        ]),
        Constant::Char(c) => Sexp::list(vec![Sexp::atom("Const_char"), int(*c)]),
        Constant::String { s, delim } => {
            Sexp::list(vec![Sexp::atom("Const_string"), string(s), string_delim(delim)])
        }
        Constant::Float(f) => Sexp::list(vec![Sexp::atom("Const_float"), string(f)]),
        Constant::BigInt { negative, value } => {
            Sexp::list(vec![Sexp::atom("Const_bigint"), bool_sexp(*negative), string(value)])
        }
        Constant::Pointer(s) => Sexp::list(vec![Sexp::atom("Const_pointer"), string(s)]),
        Constant::Block {
            tag,
            tag_info: info,
            elements,
        } => Sexp::list(vec![
            Sexp::atom("Const_block"),
            int(*tag),
            tag_info(info),
            Sexp::list(elements.iter().map(constant).collect()),
        ]),
        Constant::Some(c) => Sexp::list(vec![Sexp::atom("Const_some"), constant(c)]),
        Constant::ModuleAlias => Sexp::atom("Const_module_alias"),
    }
}

// Primitive
fn primitive(prim: &Primitive, _with_locs: bool) -> Sexp {
    match prim {
        Primitive::PcreateExtension(s) => {
            Sexp::list(vec![Sexp::atom("Pcreate_extension"), string(s)])
        }
        Primitive::PwrapExn => Sexp::atom("Pwrap_exn"),
        Primitive::PinitMod => Sexp::atom("Pinit_mod"),
        Primitive::PupdateMod => Sexp::atom("Pupdate_mod"),
        Primitive::PjsApply => Sexp::atom("Pjs_apply"),
        Primitive::PjsRuntimeApply => Sexp::atom("Pjs_runtime_apply"),
        Primitive::PjsUnsafeDowngrade { name, setter } => Sexp::list(vec![
            Sexp::atom("Pjs_unsafe_downgrade"),
            string(name),
            bool_sexp(*setter),
        ]),
        Primitive::PfnArity => Sexp::atom("Pfn_arity"),
        Primitive::PjsFnMake(i) => Sexp::list(vec![Sexp::atom("Pjs_fn_make"), int(*i)]),
        Primitive::PjsFnMakeUnit => Sexp::atom("Pjs_fn_make_unit"),
        Primitive::PjsFnMethod => Sexp::atom("Pjs_fn_method"),
        Primitive::Pdebugger => Sexp::atom("Pdebugger"),
        Primitive::PrawJsCode(info) => {
            if info.is_stmt {
                Sexp::list(vec![Sexp::atom("Praw_js_code_stmt"), string(&info.code)])
            } else {
                Sexp::list(vec![Sexp::atom("Praw_js_code_exp"), string(&info.code)])
            }
        }
        Primitive::Ptypeof => Sexp::atom("Ptypeof"),
        Primitive::PnullToOpt => Sexp::atom("Pnull_to_opt"),
        Primitive::PnullUndefinedToOpt => Sexp::atom("Pnull_undefined_to_opt"),
        Primitive::PisNull => Sexp::atom("Pis_null"),
        Primitive::PisNotNone => Sexp::atom("Pis_not_none"),
        Primitive::Psome => Sexp::atom("Psome"),
        Primitive::PsomeNotNest => Sexp::atom("Psome_not_nest"),
        Primitive::PvalFromOption => Sexp::atom("Pval_from_option"),
        Primitive::PvalFromOptionNotNest => Sexp::atom("Pval_from_option_not_nest"),
        Primitive::PisUndefined => Sexp::atom("Pis_undefined"),
        Primitive::PisNullUndefined => Sexp::atom("Pis_null_undefined"),
        Primitive::Pimport => Sexp::atom("Pimport"),
        Primitive::Pmakeblock(tag, info, m) => Sexp::list(vec![
            Sexp::atom("Pmakeblock"),
            int(*tag),
            tag_info(info),
            mutable_sexp(m),
        ]),
        Primitive::Pfield(n, info) => {
            Sexp::list(vec![Sexp::atom("Pfield"), int(*n), field_dbg_info(info)])
        }
        Primitive::Psetfield(n, info) => Sexp::list(vec![
            Sexp::atom("Psetfield"),
            int(*n),
            set_field_dbg_info(info),
        ]),
        Primitive::Pduprecord => Sexp::atom("Pduprecord"),
        Primitive::PjsCall {
            prim_name,
            dynamic_import,
            transformed_jsx,
            ..
        } => Sexp::list(vec![
            Sexp::atom("Pjs_call"),
            string(prim_name),
            bool_sexp(*dynamic_import),
            bool_sexp(*transformed_jsx),
        ]),
        Primitive::PjsObjectCreate(_) => Sexp::atom("Pjs_object_create"),
        Primitive::Praise => Sexp::atom("Praise"),
        Primitive::Pobjcomp(cmp) => Sexp::list(vec![Sexp::atom("Pobjcomp"), comparison(cmp)]),
        Primitive::Pobjorder => Sexp::atom("Pobjorder"),
        Primitive::Pobjmin => Sexp::atom("Pobjmin"),
        Primitive::Pobjmax => Sexp::atom("Pobjmax"),
        Primitive::Pobjtag => Sexp::atom("Pobjtag"),
        Primitive::Pobjsize => Sexp::atom("Pobjsize"),
        Primitive::Psequand => Sexp::atom("Psequand"),
        Primitive::Psequor => Sexp::atom("Psequor"),
        Primitive::Pnot => Sexp::atom("Pnot"),
        Primitive::Pboolcomp(cmp) => Sexp::list(vec![Sexp::atom("Pboolcomp"), comparison(cmp)]),
        Primitive::Pboolorder => Sexp::atom("Pboolorder"),
        Primitive::Pboolmin => Sexp::atom("Pboolmin"),
        Primitive::Pboolmax => Sexp::atom("Pboolmax"),
        Primitive::Pnegint => Sexp::atom("Pnegint"),
        Primitive::Paddint => Sexp::atom("Paddint"),
        Primitive::Pstringadd => Sexp::atom("Pstringadd"),
        Primitive::Psubint => Sexp::atom("Psubint"),
        Primitive::Pmulint => Sexp::atom("Pmulint"),
        Primitive::Pdivint => Sexp::atom("Pdivint"),
        Primitive::Pmodint => Sexp::atom("Pmodint"),
        Primitive::Ppowint => Sexp::atom("Ppowint"),
        Primitive::Pandint => Sexp::atom("Pandint"),
        Primitive::Porint => Sexp::atom("Porint"),
        Primitive::Pxorint => Sexp::atom("Pxorint"),
        Primitive::Pnotint => Sexp::atom("Pnotint"),
        Primitive::Plslint => Sexp::atom("Plslint"),
        Primitive::Plsrint => Sexp::atom("Plsrint"),
        Primitive::Pasrint => Sexp::atom("Pasrint"),
        Primitive::Pintcomp(cmp) => Sexp::list(vec![Sexp::atom("Pintcomp"), comparison(cmp)]),
        Primitive::Pintorder => Sexp::atom("Pintorder"),
        Primitive::Pintmin => Sexp::atom("Pintmin"),
        Primitive::Pintmax => Sexp::atom("Pintmax"),
        Primitive::Poffsetint(n) => Sexp::list(vec![Sexp::atom("Poffsetint"), int(*n)]),
        Primitive::Poffsetref(n) => Sexp::list(vec![Sexp::atom("Poffsetref"), int(*n)]),
        Primitive::Pintoffloat => Sexp::atom("Pintoffloat"),
        Primitive::Pfloatofint => Sexp::atom("Pfloatofint"),
        Primitive::Pnegfloat => Sexp::atom("Pnegfloat"),
        Primitive::Paddfloat => Sexp::atom("Paddfloat"),
        Primitive::Psubfloat => Sexp::atom("Psubfloat"),
        Primitive::Pmulfloat => Sexp::atom("Pmulfloat"),
        Primitive::Pdivfloat => Sexp::atom("Pdivfloat"),
        Primitive::Pmodfloat => Sexp::atom("Pmodfloat"),
        Primitive::Ppowfloat => Sexp::atom("Ppowfloat"),
        Primitive::Pfloatcomp(cmp) => Sexp::list(vec![Sexp::atom("Pfloatcomp"), comparison(cmp)]),
        Primitive::Pfloatorder => Sexp::atom("Pfloatorder"),
        Primitive::Pfloatmin => Sexp::atom("Pfloatmin"),
        Primitive::Pfloatmax => Sexp::atom("Pfloatmax"),
        Primitive::Pnegbigint => Sexp::atom("Pnegbigint"),
        Primitive::Paddbigint => Sexp::atom("Paddbigint"),
        Primitive::Psubbigint => Sexp::atom("Psubbigint"),
        Primitive::Pmulbigint => Sexp::atom("Pmulbigint"),
        Primitive::Pdivbigint => Sexp::atom("Pdivbigint"),
        Primitive::Pmodbigint => Sexp::atom("Pmodbigint"),
        Primitive::Ppowbigint => Sexp::atom("Ppowbigint"),
        Primitive::Pandbigint => Sexp::atom("Pandbigint"),
        Primitive::Porbigint => Sexp::atom("Porbigint"),
        Primitive::Pxorbigint => Sexp::atom("Pxorbigint"),
        Primitive::Pnotbigint => Sexp::atom("Pnotbigint"),
        Primitive::Plslbigint => Sexp::atom("Plslbigint"),
        Primitive::Pasrbigint => Sexp::atom("Pasrbigint"),
        Primitive::Pbigintcomp(cmp) => {
            Sexp::list(vec![Sexp::atom("Pbigintcomp"), comparison(cmp)])
        }
        Primitive::Pbigintorder => Sexp::atom("Pbigintorder"),
        Primitive::Pbigintmin => Sexp::atom("Pbigintmin"),
        Primitive::Pbigintmax => Sexp::atom("Pbigintmax"),
        Primitive::Pjscomp(cmp) => Sexp::list(vec![Sexp::atom("Pjscomp"), comparison(cmp)]),
        Primitive::Pstringlength => Sexp::atom("Pstringlength"),
        Primitive::Pstringrefu => Sexp::atom("Pstringrefu"),
        Primitive::Pstringrefs => Sexp::atom("Pstringrefs"),
        Primitive::Pstringcomp(cmp) => Sexp::list(vec![Sexp::atom("Pstringcomp"), comparison(cmp)]),
        Primitive::Pstringorder => Sexp::atom("Pstringorder"),
        Primitive::Pstringmin => Sexp::atom("Pstringmin"),
        Primitive::Pstringmax => Sexp::atom("Pstringmax"),
        Primitive::Parraylength => Sexp::atom("Parraylength"),
        Primitive::Pmakearray => Sexp::atom("Pmakearray"),
        Primitive::Pmakelist => Sexp::atom("Pmakelist"),
        Primitive::Pmakedict => Sexp::atom("Pmakedict"),
        Primitive::PdictHas => Sexp::atom("Pdict_has"),
        Primitive::Parrayrefu => Sexp::atom("Parrayrefu"),
        Primitive::Parraysetu => Sexp::atom("Parraysetu"),
        Primitive::Parrayrefs => Sexp::atom("Parrayrefs"),
        Primitive::Parraysets => Sexp::atom("Parraysets"),
        Primitive::Pisint => Sexp::atom("Pisint"),
        Primitive::PisPolyVarBlock => Sexp::atom("Pis_poly_var_block"),
        Primitive::Pisout(n) => Sexp::list(vec![Sexp::atom("Pisout"), int(*n)]),
        Primitive::Pawait => Sexp::atom("Pawait"),
        Primitive::Phash => Sexp::atom("Phash"),
        Primitive::PhashMixint => Sexp::atom("Phash_mixint"),
        Primitive::PhashMixstring => Sexp::atom("Phash_mixstring"),
        Primitive::PhashFinalmix => Sexp::atom("Phash_finalmix"),
    }
}

// Function attributes
fn function_attribute(attr: &FunctionAttribute) -> Sexp {
    Sexp::list(vec![
        Sexp::atom("function_attribute"),
        Sexp::list(vec![Sexp::atom("inline"), inline_attribute(&attr.inline)]),
        Sexp::list(vec![
            Sexp::atom("is_a_functor"),
            bool_sexp(attr.is_a_functor),
        ]),
        Sexp::list(vec![Sexp::atom("return_unit"), bool_sexp(attr.return_unit)]),
        Sexp::list(vec![Sexp::atom("async"), bool_sexp(attr.async_)]),
        Sexp::list(vec![
            Sexp::atom("directive"),
            opt_string(&attr.directive),
        ]),
        Sexp::list(vec![
            Sexp::atom("one_unit_arg"),
            bool_sexp(attr.one_unit_arg),
        ]),
    ])
}

// Lambda
fn lambda(lam: &Lambda, with_locs: bool) -> Sexp {
    match lam {
        Lambda::Lvar(id) => Sexp::list(vec![Sexp::atom("Lvar"), ident(id)]),
        Lambda::LglobalModule(id, dynamic_import) => Sexp::list(vec![
            Sexp::atom("Lglobal_module"),
            ident(id),
            bool_sexp(*dynamic_import),
        ]),
        Lambda::Lconst(c) => Sexp::list(vec![Sexp::atom("Lconst"), constant(c)]),
        Lambda::Lapply(Apply {
            ap_func,
            ap_args,
            ap_info,
            ap_transformed_jsx,
        }) => Sexp::list(vec![
            Sexp::atom("Lapply"),
            lambda(ap_func, with_locs),
            Sexp::list(ap_args.iter().map(|a| lambda(a, with_locs)).collect()),
            ap_info_sexp(ap_info, with_locs),
            bool_sexp(*ap_transformed_jsx),
        ]),
        Lambda::Lfunction(LFunction {
            arity,
            params,
            body,
            attr,
        }) => Sexp::list(vec![
            Sexp::atom("Lfunction"),
            int(*arity),
            Sexp::list(params.iter().map(ident).collect()),
            lambda(body, with_locs),
            function_attribute(attr),
        ]),
        Lambda::Llet(kind, id, arg, body) => Sexp::list(vec![
            Sexp::atom("Llet"),
            let_kind(kind),
            ident(id),
            lambda(arg, with_locs),
            lambda(body, with_locs),
        ]),
        Lambda::Lletrec(bindings, body) => Sexp::list(vec![
            Sexp::atom("Lletrec"),
            Sexp::list(
                bindings
                    .iter()
                    .map(|(id, lam)| Sexp::list(vec![ident(id), lambda(lam, with_locs)]))
                    .collect(),
            ),
            lambda(body, with_locs),
        ]),
        Lambda::Lprim(PrimInfo {
            primitive: prim,
            args,
            loc: _,
        }) => {
            let items = vec![
                Sexp::atom("Lprim"),
                primitive(prim, with_locs),
                Sexp::list(args.iter().map(|a| lambda(a, with_locs)).collect()),
            ];
            // TODO: add location if with_locs
            Sexp::list(items)
        }
        Lambda::Lswitch(arg, sw) => Sexp::list(vec![
            Sexp::atom("Lswitch"),
            lambda(arg, with_locs),
            lambda_switch(sw, with_locs),
        ]),
        Lambda::LstringSwitch(arg, cases, default) => Sexp::list(vec![
            Sexp::atom("Lstringswitch"),
            lambda(arg, with_locs),
            Sexp::list(
                cases
                    .iter()
                    .map(|(s, lam)| Sexp::list(vec![string(s), lambda(lam, with_locs)]))
                    .collect(),
            ),
            opt(|l| lambda(l, with_locs), default),
        ]),
        Lambda::LstaticRaise(id, args) => Sexp::list(vec![
            Sexp::atom("Lstaticraise"),
            int(*id),
            Sexp::list(args.iter().map(|a| lambda(a, with_locs)).collect()),
        ]),
        Lambda::LstaticCatch(body, (id, params), handler) => Sexp::list(vec![
            Sexp::atom("Lstaticcatch"),
            lambda(body, with_locs),
            int(*id),
            Sexp::list(params.iter().map(ident).collect()),
            lambda(handler, with_locs),
        ]),
        Lambda::LtryWith(body, exn, handler) => Sexp::list(vec![
            Sexp::atom("Ltrywith"),
            lambda(body, with_locs),
            ident(exn),
            lambda(handler, with_locs),
        ]),
        Lambda::LifThenElse(cond, then_, else_) => Sexp::list(vec![
            Sexp::atom("Lifthenelse"),
            lambda(cond, with_locs),
            lambda(then_, with_locs),
            lambda(else_, with_locs),
        ]),
        Lambda::Lsequence(first, second) => Sexp::list(vec![
            Sexp::atom("Lsequence"),
            lambda(first, with_locs),
            lambda(second, with_locs),
        ]),
        Lambda::Lwhile(cond, body) => Sexp::list(vec![
            Sexp::atom("Lwhile"),
            lambda(cond, with_locs),
            lambda(body, with_locs),
        ]),
        Lambda::Lfor(var, start, end, dir, body) => Sexp::list(vec![
            Sexp::atom("Lfor"),
            ident(var),
            lambda(start, with_locs),
            lambda(end, with_locs),
            direction_flag(dir),
            lambda(body, with_locs),
        ]),
        Lambda::Lassign(var, value) => Sexp::list(vec![
            Sexp::atom("Lassign"),
            ident(var),
            lambda(value, with_locs),
        ]),
    }
}

fn ap_info_sexp(info: &ApInfo, _with_locs: bool) -> Sexp {
    Sexp::list(vec![
        Sexp::atom("ap_info"),
        inline_attribute(&info.inlined),
        apply_status(&info.status),
        // TODO: add location if with_locs
    ])
}

fn lambda_switch(sw: &LambdaSwitch, with_locs: bool) -> Sexp {
    Sexp::list(vec![
        Sexp::atom("lambda_switch"),
        Sexp::list(vec![
            Sexp::atom("sw_consts_full"),
            bool_sexp(sw.sw_consts_full),
        ]),
        Sexp::list(vec![
            Sexp::atom("sw_consts"),
            Sexp::list(
                sw.sw_consts
                    .iter()
                    .map(|(tag, lam)| Sexp::list(vec![int(*tag), lambda(lam, with_locs)]))
                    .collect(),
            ),
        ]),
        Sexp::list(vec![
            Sexp::atom("sw_blocks_full"),
            bool_sexp(sw.sw_blocks_full),
        ]),
        Sexp::list(vec![
            Sexp::atom("sw_blocks"),
            Sexp::list(
                sw.sw_blocks
                    .iter()
                    .map(|(tag, lam)| Sexp::list(vec![int(*tag), lambda(lam, with_locs)]))
                    .collect(),
            ),
        ]),
        Sexp::list(vec![
            Sexp::atom("sw_failaction"),
            opt(|l| lambda(l, with_locs), &sw.sw_failaction.as_ref().map(|b| b.as_ref())),
        ]),
        Sexp::list(vec![
            Sexp::atom("sw_names"),
            switch_names_sexp(&sw.sw_names),
        ]),
    ])
}

fn switch_names_sexp(names: &Option<crate::lambda::SwitchNames>) -> Sexp {
    match names {
        None => Sexp::atom("None"),
        Some(n) => Sexp::list(vec![
            Sexp::atom("Some"),
            Sexp::list({
                let mut items = vec![Sexp::atom("consts")];
                items.extend(n.names.iter().map(|s| string(s)));
                items
            }),
            Sexp::list(vec![Sexp::atom("blocks")]), // TODO: populate if needed
        ]),
    }
}

// Public API

/// Print lambda as sexp (without locations)
pub fn print_lambda(lam: &Lambda, out: &mut impl Write) -> std::io::Result<()> {
    let sexp = lambda(lam, false);
    writeln!(out, "{}", sexp.to_string())
}

/// Print lambda as sexp (with locations)
pub fn print_lambda_with_locs(lam: &Lambda, out: &mut impl Write) -> std::io::Result<()> {
    let sexp = lambda(lam, true);
    writeln!(out, "{}", sexp.to_string())
}
