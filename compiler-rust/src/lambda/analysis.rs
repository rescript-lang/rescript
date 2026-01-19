//! Lambda IR analysis - Side effect and size analysis.
//!
//! This module provides analysis functions for Lambda expressions:
//! - Side effect analysis: determines if an expression has side effects
//! - Size analysis: estimates the size of an expression for inlining decisions
//! - Inlining hints: determines if a function is safe/profitable to inline

use crate::ident::Ident;
use crate::lambda::constant::Constant;
use crate::lambda::primitive::Primitive;
use crate::lambda::{Lambda, LFunction};
use crate::lambda::compat::FieldDbgInfo;

/// Check if a constant is definitely not zero.
/// Used in side effect analysis to determine safe division.
fn not_zero_constant(c: &Constant) -> bool {
    match c {
        Constant::Int { i, .. } => *i != 0,
        Constant::BigInt { value, .. } => value != "0",
        _ => false,
    }
}

/// Check if a Lambda expression has no side effects.
///
/// This is a conservative analysis - it returns `true` only if we can
/// prove the expression is side-effect free. Unknown cases return `false`.
///
/// Side effects include:
/// - Mutable state modifications (assignments, setfield)
/// - Exceptions (raise, division by zero)
/// - I/O operations (debugger, raw JS, etc.)
/// - Non-termination (while loops)
pub fn no_side_effects(lam: &Lambda) -> bool {
    match lam {
        // Variables, constants, and functions are pure
        Lambda::Lvar(_) | Lambda::Lconst(_) | Lambda::Lfunction(_) => true,

        // Global module references are pure (side effects tracked at module level)
        Lambda::LglobalModule(_, _) => true,

        // Primitive operations - check if the primitive is pure
        Lambda::Lprim(prim_info) => {
            // First check all arguments are pure
            if !prim_info.args.iter().all(no_side_effects) {
                return false;
            }

            // Then check the primitive itself
            is_pure_primitive(&prim_info.primitive, &prim_info.args)
        }

        // Let bindings are pure if both parts are pure
        Lambda::Llet(_, _, arg, body) => no_side_effects(arg) && no_side_effects(body),

        // Switch is not pure (can have pattern matching side effects)
        Lambda::Lswitch(_, _) => false,

        // String switch is not pure
        Lambda::LstringSwitch(_, _, _) => false,

        // Static raise is not pure (control flow)
        Lambda::LstaticRaise(_, _) => false,

        // Static catch is not pure
        Lambda::LstaticCatch(_, _, _) => false,

        // Try-with is pure if both body and handler are pure
        Lambda::LtryWith(body, _, handler) => no_side_effects(body) && no_side_effects(handler),

        // If-then-else is pure if all branches are pure
        Lambda::LifThenElse(a, b, c) => no_side_effects(a) && no_side_effects(b) && no_side_effects(c),

        // Sequence is pure if both parts are pure
        Lambda::Lsequence(a, b) => no_side_effects(a) && no_side_effects(b),

        // Letrec is pure if all bindings and body are pure
        Lambda::Lletrec(bindings, body) => {
            bindings.iter().all(|(_, expr)| no_side_effects(expr)) && no_side_effects(body)
        }

        // While loops are not pure (may not terminate)
        Lambda::Lwhile(_, _) => false,

        // For loops are not pure
        Lambda::Lfor(_, _, _, _, _) => false,

        // Assignments are not pure
        Lambda::Lassign(_, _) => false,

        // Function application - special case for known pure functions
        Lambda::Lapply(apply) => {
            // Special case: Lazy.from_fun application
            if let Lambda::Lprim(prim_info) = &*apply.ap_func {
                if let Primitive::Pfield(_, FieldDbgInfo::Module { name }) = &prim_info.primitive {
                    if name == "from_fun" && apply.ap_args.len() == 1 {
                        return no_side_effects(&apply.ap_args[0]);
                    }
                }
            }
            // General function application is not pure
            false
        }
    }
}

/// Check if a primitive operation is pure (no side effects).
fn is_pure_primitive(prim: &Primitive, args: &[Lambda]) -> bool {
    match prim {
        // Division can raise division by zero
        Primitive::Pmodint | Primitive::Pdivint | Primitive::Pdivbigint | Primitive::Pmodbigint => {
            if args.len() >= 2 {
                if let Lambda::Lconst(cst) = &args[1] {
                    return not_zero_constant(cst);
                }
            }
            false
        }

        // Creation and type checking primitives
        Primitive::PcreateExtension(_)
        | Primitive::Ptypeof
        | Primitive::PisNull
        | Primitive::PisNotNone
        | Primitive::Psome
        | Primitive::PsomeNotNest
        | Primitive::PisUndefined
        | Primitive::PisNullUndefined
        | Primitive::PnullToOpt
        | Primitive::PnullUndefinedToOpt
        | Primitive::PjsFnMake(_)
        | Primitive::PjsFnMakeUnit
        | Primitive::PjsObjectCreate(_)
        | Primitive::Pimport => true,

        // Block operations
        Primitive::Pmakeblock(_, _, _)
        | Primitive::Pfield(_, _)
        | Primitive::PvalFromOption
        | Primitive::PvalFromOptionNotNest
        | Primitive::Pduprecord => true,

        // Object primitives
        Primitive::Pobjcomp(_)
        | Primitive::Pobjorder
        | Primitive::Pobjmin
        | Primitive::Pobjmax
        | Primitive::Pobjtag
        | Primitive::Pobjsize => true,

        // Boolean primitives
        Primitive::Psequand
        | Primitive::Psequor
        | Primitive::Pnot
        | Primitive::Pboolcomp(_)
        | Primitive::Pboolorder
        | Primitive::Pboolmin
        | Primitive::Pboolmax => true,

        // Integer primitives
        Primitive::Pnegint
        | Primitive::Paddint
        | Primitive::Psubint
        | Primitive::Pmulint
        | Primitive::Ppowint
        | Primitive::Pnotint
        | Primitive::Pandint
        | Primitive::Porint
        | Primitive::Pxorint
        | Primitive::Plslint
        | Primitive::Plsrint
        | Primitive::Pasrint
        | Primitive::Pintcomp(_)
        | Primitive::Pintorder
        | Primitive::Pintmin
        | Primitive::Pintmax => true,

        // Float primitives
        Primitive::Pintoffloat
        | Primitive::Pfloatofint
        | Primitive::Pnegfloat
        | Primitive::Paddfloat
        | Primitive::Psubfloat
        | Primitive::Pmulfloat
        | Primitive::Ppowfloat
        | Primitive::Pdivfloat
        | Primitive::Pmodfloat
        | Primitive::Pfloatcomp(_)
        | Primitive::Pjscomp(_)
        | Primitive::Pfloatorder
        | Primitive::Pfloatmin
        | Primitive::Pfloatmax => true,

        // BigInt primitives
        Primitive::Pnegbigint
        | Primitive::Paddbigint
        | Primitive::Psubbigint
        | Primitive::Pmulbigint
        | Primitive::Ppowbigint
        | Primitive::Pnotbigint
        | Primitive::Pandbigint
        | Primitive::Porbigint
        | Primitive::Pxorbigint
        | Primitive::Plslbigint
        | Primitive::Pasrbigint
        | Primitive::Pbigintcomp(_)
        | Primitive::Pbigintorder
        | Primitive::Pbigintmin
        | Primitive::Pbigintmax => true,

        // String primitives
        Primitive::Pstringlength
        | Primitive::Pstringrefu
        | Primitive::Pstringrefs
        | Primitive::Pstringcomp(_)
        | Primitive::Pstringorder
        | Primitive::Pstringmin
        | Primitive::Pstringmax => true,

        // Array primitives (read-only)
        Primitive::Pmakearray
        | Primitive::Parraylength
        | Primitive::Parrayrefu
        | Primitive::Parrayrefs => true,

        // List and dict primitives
        Primitive::Pmakelist | Primitive::Pmakedict | Primitive::PdictHas => true,

        // Type testing primitives
        Primitive::Pisint
        | Primitive::PisPolyVarBlock
        | Primitive::Pisout(_) => true,

        // Offset and misc
        Primitive::Poffsetint(_)
        | Primitive::Pstringadd
        | Primitive::PfnArity
        | Primitive::PwrapExn
        | Primitive::Phash
        | Primitive::PhashMixstring
        | Primitive::PhashMixint
        | Primitive::PhashFinalmix => true,

        // Raw JS code - check if it's a pure expression
        Primitive::PrawJsCode(info) => {
            // Only certain JS code patterns are pure
            // TODO: Check code_info for Js_function, Js_literal, Js_stmt_comment
            !info.is_stmt
        }

        // Impure primitives
        Primitive::PjsApply
        | Primitive::PjsRuntimeApply
        | Primitive::PjsCall { .. }
        | Primitive::PinitMod
        | Primitive::PupdateMod
        | Primitive::PjsUnsafeDowngrade { .. }
        | Primitive::Pdebugger
        | Primitive::PjsFnMethod
        | Primitive::Pawait
        | Primitive::Parraysets
        | Primitive::Parraysetu
        | Primitive::Poffsetref(_)
        | Primitive::Praise
        | Primitive::Psetfield(_, _) => false,
    }
}

/// Maximum size before we consider something too big to inline.
const TOO_BIG_TO_INLINE: i32 = 1000;

/// Small inline size threshold.
pub const SMALL_INLINE_SIZE: i32 = 5;

/// Exit inline size threshold (for static raise handlers).
pub const EXIT_INLINE_SIZE: i32 = 7;

/// Estimate the size of a Lambda expression for inlining decisions.
///
/// Returns a size estimate where smaller values indicate smaller code.
/// Returns `TOO_BIG_TO_INLINE` for expressions that shouldn't be inlined.
pub fn size(lam: &Lambda) -> i32 {
    size_inner(lam).unwrap_or(TOO_BIG_TO_INLINE)
}

/// Internal size computation that can fail for large expressions.
fn size_inner(lam: &Lambda) -> Option<i32> {
    match lam {
        Lambda::Lvar(_) => Some(1),

        Lambda::Lconst(c) => Some(size_constant(c)),

        Lambda::Llet(_, _, l1, l2) => {
            let s1 = size_inner(l1)?;
            let s2 = size_inner(l2)?;
            Some(1 + s1 + s2)
        }

        // Letrec is too big
        Lambda::Lletrec(_, _) => None,

        // Module field access is small
        Lambda::Lprim(prim_info) => {
            match &prim_info.primitive {
                Primitive::Pfield(_, FieldDbgInfo::Module { .. }) => {
                    if prim_info.args.len() == 1 {
                        match &prim_info.args[0] {
                            Lambda::LglobalModule(_, _) | Lambda::Lvar(_) => return Some(1),
                            _ => {}
                        }
                    }
                }
                Primitive::Praise | Primitive::PisNotNone => {
                    if prim_info.args.len() == 1 {
                        return size_inner(&prim_info.args[0]);
                    }
                }
                Primitive::PrawJsCode(_) => return None,
                _ => {}
            }
            size_lams(1, &prim_info.args)
        }

        Lambda::LglobalModule(_, _) => Some(1),

        Lambda::Lapply(apply) => {
            let func_size = size_inner(&apply.ap_func)?;
            size_lams(func_size, &apply.ap_args)
        }

        Lambda::Lfunction(func) => size_inner(&func.body),

        // Complex control flow is too big
        Lambda::Lswitch(_, _) => None,
        Lambda::LstringSwitch(_, _, _) => None,
        Lambda::LstaticCatch(_, _, _) => None,
        Lambda::LtryWith(_, _, _) => None,
        Lambda::Lwhile(_, _) => None,
        Lambda::Lfor(_, _, _, _, _) => None,

        Lambda::LstaticRaise(_, args) => {
            let mut acc = 1;
            for arg in args {
                acc += size_inner(arg)?;
            }
            Some(acc)
        }

        Lambda::LifThenElse(l1, l2, l3) => {
            let s1 = size_inner(l1)?;
            let s2 = size_inner(l2)?;
            let s3 = size_inner(l3)?;
            Some(1 + s1 + s2 + s3)
        }

        Lambda::Lsequence(l1, l2) => {
            let s1 = size_inner(l1)?;
            let s2 = size_inner(l2)?;
            Some(s1 + s2)
        }

        Lambda::Lassign(_, v) => {
            let s = size_inner(v)?;
            Some(1 + s)
        }
    }
}

/// Compute the size of a constant.
fn size_constant(c: &Constant) -> i32 {
    match c {
        Constant::Int { .. }
        | Constant::Char(_)
        | Constant::Float(_)
        | Constant::BigInt { .. }
        | Constant::Pointer(_)
        | Constant::JsNull
        | Constant::JsUndefined { .. }
        | Constant::ModuleAlias
        | Constant::JsTrue
        | Constant::JsFalse => 1,

        Constant::String { .. } => 1,

        Constant::Some(inner) => size_constant(inner),

        Constant::Block { elements, .. } => {
            elements.iter().map(size_constant).sum()
        }
    }
}

/// Compute the size of a list of Lambda expressions.
fn size_lams(acc: i32, lams: &[Lambda]) -> Option<i32> {
    let mut total = acc;
    for lam in lams {
        total += size_inner(lam)?;
    }
    Some(total)
}

/// Check if all arguments are constants.
pub fn args_all_const(args: &[Lambda]) -> bool {
    args.iter().all(|x| matches!(x, Lambda::Lconst(_)))
}

/// Check if a function can be inlined (not async, no directive).
pub fn lfunction_can_be_inlined(func: &LFunction) -> bool {
    !func.attr.async_ && func.attr.directive.is_none()
}

/// Determine if it's profitable to inline a function when applied.
pub fn ok_to_inline_fun_when_app(func: &LFunction, args: &[Lambda]) -> bool {
    match func.attr.inline {
        crate::lambda::InlineAttribute::Always => true,
        crate::lambda::InlineAttribute::Never => false,
        crate::lambda::InlineAttribute::Default => {
            let s = size(&func.body);
            s < SMALL_INLINE_SIZE
                || destruct_pattern(&func.body, &func.params, args)
                || (args_all_const(args) && s < 10 && no_side_effects(&func.body))
        }
    }
}

/// Check if inlining would enable pattern matching optimization.
///
/// This returns true when the function body is a switch/if on one of its
/// parameters and the corresponding argument is a constant.
fn destruct_pattern(body: &Lambda, params: &[Ident], args: &[Lambda]) -> bool {
    use crate::ident::Ident;

    fn find_arg<'a>(v: &Ident, params: &[Ident], args: &'a [Lambda]) -> Option<&'a Lambda> {
        for (i, param) in params.iter().enumerate() {
            if param == v {
                return args.get(i);
            }
        }
        None
    }

    match body {
        Lambda::Lswitch(scrutinee, switch) => {
            if let Lambda::Lvar(v) = &**scrutinee {
                if let Some(Lambda::Lconst(_)) = find_arg(v, params, args) {
                    // Would need to evaluate the switch to check size
                    return size(&Lambda::Lswitch(scrutinee.clone(), switch.clone())) < SMALL_INLINE_SIZE;
                }
            }
            false
        }
        Lambda::LifThenElse(cond, then_, else_) => {
            if let Lambda::Lvar(v) = &**cond {
                if let Some(Lambda::Lconst(_)) = find_arg(v, params, args) {
                    // Would enable constant folding
                    return size(&Lambda::if_(
                        (**cond).clone(),
                        (**then_).clone(),
                        (**else_).clone(),
                    )) < SMALL_INLINE_SIZE;
                }
            }
            false
        }
        _ => false,
    }
}

/// Check if a Lambda expression is safe to inline (won't duplicate work).
pub fn safe_to_inline(lam: &Lambda) -> bool {
    match lam {
        Lambda::Lfunction(_) => true,
        Lambda::Lconst(c) => matches!(
            c,
            Constant::Pointer(_)
                | Constant::Int {
                    comment: Some(crate::lambda::constant::PointerInfo::Constructor(_)),
                    ..
                }
                | Constant::JsTrue
                | Constant::JsFalse
                | Constant::JsUndefined { .. }
        ),
        _ => false,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lambda::{FunctionAttribute, LetKind, Primitive};
    use crate::ident::Ident;
    use crate::location::Location;

    #[test]
    fn test_no_side_effects_simple() {
        let x = Ident::create_local("x");

        // Variable is pure
        assert!(no_side_effects(&Lambda::var(x)));

        // Constant is pure
        assert!(no_side_effects(&Lambda::const_(Constant::int(42))));

        // Function is pure
        let func = Lambda::function_(
            1,
            vec![Ident::create_local("x")],
            Lambda::const_(Constant::int(1)),
            FunctionAttribute::default(),
        );
        assert!(no_side_effects(&func));
    }

    #[test]
    fn test_no_side_effects_let() {
        let x = Ident::create_local("x");

        // Pure let binding
        let pure_let = Lambda::let_(
            LetKind::Strict,
            x.clone(),
            Lambda::const_(Constant::int(1)),
            Lambda::var(x.clone()),
        );
        assert!(no_side_effects(&pure_let));
    }

    #[test]
    fn test_no_side_effects_impure() {
        let x = Ident::create_local("x");

        // Assignment is impure
        let assign = Lambda::assign(x.clone(), Lambda::const_(Constant::int(1)));
        assert!(!no_side_effects(&assign));

        // While loop is impure
        let while_loop = Lambda::while_(Lambda::true_(), Lambda::unit());
        assert!(!no_side_effects(&while_loop));
    }

    #[test]
    fn test_no_side_effects_primitive() {
        // Addition is pure
        let add = Lambda::prim(
            Primitive::Paddint,
            vec![
                Lambda::const_(Constant::int(1)),
                Lambda::const_(Constant::int(2)),
            ],
            Location::none(),
        );
        assert!(no_side_effects(&add));

        // Division by non-zero constant is pure
        let div = Lambda::prim(
            Primitive::Pdivint,
            vec![
                Lambda::const_(Constant::int(10)),
                Lambda::const_(Constant::int(2)),
            ],
            Location::none(),
        );
        assert!(no_side_effects(&div));

        // Division by zero is impure
        let div_zero = Lambda::prim(
            Primitive::Pdivint,
            vec![
                Lambda::const_(Constant::int(10)),
                Lambda::const_(Constant::int(0)),
            ],
            Location::none(),
        );
        assert!(!no_side_effects(&div_zero));

        // Raise is impure
        let raise = Lambda::prim(
            Primitive::Praise,
            vec![Lambda::const_(Constant::int(1))],
            Location::none(),
        );
        assert!(!no_side_effects(&raise));
    }

    #[test]
    fn test_size_simple() {
        // Variable has size 1
        let var = Lambda::var(Ident::create_local("x"));
        assert_eq!(size(&var), 1);

        // Constant has size 1
        let const_ = Lambda::const_(Constant::int(42));
        assert_eq!(size(&const_), 1);
    }

    #[test]
    fn test_size_let() {
        let x = Ident::create_local("x");

        // let x = 1 in x has size 3 (1 for let + 1 for arg + 1 for body)
        let let_expr = Lambda::let_(
            LetKind::Strict,
            x.clone(),
            Lambda::const_(Constant::int(1)),
            Lambda::var(x),
        );
        assert_eq!(size(&let_expr), 3);
    }

    #[test]
    fn test_size_big() {
        // Letrec is too big
        let x = Ident::create_local("x");
        let letrec = Lambda::letrec(
            vec![(x.clone(), Lambda::const_(Constant::int(1)))],
            Lambda::var(x),
        );
        assert_eq!(size(&letrec), TOO_BIG_TO_INLINE);

        // Switch is too big
        let switch = Lambda::Lswitch(
            Box::new(Lambda::const_(Constant::int(0))),
            crate::lambda::LambdaSwitch {
                sw_consts_full: true,
                sw_consts: vec![],
                sw_blocks_full: true,
                sw_blocks: vec![],
                sw_failaction: None,
                sw_names: None,
            },
        );
        assert_eq!(size(&switch), TOO_BIG_TO_INLINE);
    }

    #[test]
    fn test_args_all_const() {
        assert!(args_all_const(&[
            Lambda::const_(Constant::int(1)),
            Lambda::const_(Constant::int(2)),
        ]));

        assert!(!args_all_const(&[
            Lambda::const_(Constant::int(1)),
            Lambda::var(Ident::create_local("x")),
        ]));
    }

    #[test]
    fn test_lfunction_can_be_inlined() {
        // Normal function can be inlined
        let func = LFunction {
            arity: 1,
            params: vec![Ident::create_local("x")],
            body: Box::new(Lambda::const_(Constant::int(1))),
            attr: FunctionAttribute::default(),
        };
        assert!(lfunction_can_be_inlined(&func));

        // Async function cannot be inlined
        let async_func = LFunction {
            arity: 1,
            params: vec![Ident::create_local("x")],
            body: Box::new(Lambda::const_(Constant::int(1))),
            attr: FunctionAttribute {
                async_: true,
                ..FunctionAttribute::default()
            },
        };
        assert!(!lfunction_can_be_inlined(&async_func));

        // Function with directive cannot be inlined
        let directive_func = LFunction {
            arity: 1,
            params: vec![Ident::create_local("x")],
            body: Box::new(Lambda::const_(Constant::int(1))),
            attr: FunctionAttribute {
                directive: Some("use server".to_string()),
                ..FunctionAttribute::default()
            },
        };
        assert!(!lfunction_can_be_inlined(&directive_func));
    }

    #[test]
    fn test_safe_to_inline() {
        // Function is safe
        let func = Lambda::function_(
            1,
            vec![Ident::create_local("x")],
            Lambda::const_(Constant::int(1)),
            FunctionAttribute::default(),
        );
        assert!(safe_to_inline(&func));

        // JS true/false/undefined are safe
        assert!(safe_to_inline(&Lambda::true_()));
        assert!(safe_to_inline(&Lambda::false_()));
        assert!(safe_to_inline(&Lambda::unit()));

        // Variable is not safe (would duplicate evaluation)
        assert!(!safe_to_inline(&Lambda::var(Ident::create_local("x"))));
    }

    #[test]
    fn test_size_constant() {
        assert_eq!(size_constant(&Constant::int(42)), 1);
        assert_eq!(size_constant(&Constant::string("hello")), 1);
        assert_eq!(size_constant(&Constant::JsTrue), 1);

        // Some wraps another constant
        assert_eq!(size_constant(&Constant::Some(Box::new(Constant::int(1)))), 1);

        // Block sums up fields
        assert_eq!(
            size_constant(&Constant::Block {
                tag: 0,
                tag_info: crate::lambda::tag_info::TagInfo::Tuple,
                elements: vec![Constant::int(1), Constant::int(2), Constant::int(3)],
            }),
            3
        );
    }
}
