//! Lambda substitution.
//!
//! This module provides substitution of variables in Lambda expressions.
//!
//! # Assumptions
//! - Bound variables of the lambda-term do not belong to the domain of the substitution
//! - The image of the substitution is out of reach of the bound variables (no capture)
//!
//! These assumptions are typically ensured by alpha-renaming before substitution.

use std::collections::HashMap;

use crate::ident::Ident;
use crate::lambda::{
    Apply, Lambda, LFunction, LambdaSwitch, PrimInfo,
};

/// A substitution mapping identifiers to Lambda expressions.
pub type Subst = HashMap<Ident, Lambda>;

/// Apply a substitution to a Lambda expression.
///
/// This replaces free variables that appear in the substitution with their
/// corresponding Lambda expressions. Bound variables are not affected.
///
/// # Arguments
/// * `s` - The substitution mapping
/// * `lam` - The Lambda expression to substitute into
///
/// # Returns
/// A new Lambda expression with substitutions applied.
pub fn subst(s: &Subst, lam: &Lambda) -> Lambda {
    if s.is_empty() {
        return lam.clone();
    }
    subst_aux(s, lam)
}

/// Internal substitution helper.
fn subst_aux(s: &Subst, lam: &Lambda) -> Lambda {
    match lam {
        Lambda::Lvar(id) => {
            // Look up in substitution, return original if not found
            s.get(id).cloned().unwrap_or_else(|| lam.clone())
        }

        Lambda::Lconst(_) => lam.clone(),

        Lambda::Lapply(apply) => {
            Lambda::Lapply(Apply {
                ap_func: Box::new(subst_aux(s, &apply.ap_func)),
                ap_args: apply.ap_args.iter().map(|arg| subst_aux(s, arg)).collect(),
                ap_info: apply.ap_info.clone(),
                ap_transformed_jsx: apply.ap_transformed_jsx,
            })
        }

        Lambda::Lfunction(func) => {
            Lambda::Lfunction(LFunction {
                arity: func.arity,
                params: func.params.clone(),
                body: Box::new(subst_aux(s, &func.body)),
                attr: func.attr.clone(),
            })
        }

        Lambda::Llet(kind, id, arg, body) => {
            Lambda::Llet(
                *kind,
                id.clone(),
                Box::new(subst_aux(s, arg)),
                Box::new(subst_aux(s, body)),
            )
        }

        Lambda::Lletrec(bindings, body) => {
            let new_bindings: Vec<_> = bindings
                .iter()
                .map(|(id, expr)| (id.clone(), subst_aux(s, expr)))
                .collect();
            Lambda::Lletrec(new_bindings, Box::new(subst_aux(s, body)))
        }

        Lambda::Lprim(prim_info) => {
            Lambda::Lprim(PrimInfo {
                primitive: prim_info.primitive.clone(),
                args: prim_info.args.iter().map(|arg| subst_aux(s, arg)).collect(),
                loc: prim_info.loc.clone(),
            })
        }

        Lambda::LglobalModule(_, _) => lam.clone(),

        Lambda::Lswitch(arg, switch) => {
            Lambda::Lswitch(
                Box::new(subst_aux(s, arg)),
                LambdaSwitch {
                    sw_consts_full: switch.sw_consts_full,
                    sw_consts: switch
                        .sw_consts
                        .iter()
                        .map(|(k, v)| (*k, subst_aux(s, v)))
                        .collect(),
                    sw_blocks_full: switch.sw_blocks_full,
                    sw_blocks: switch
                        .sw_blocks
                        .iter()
                        .map(|(k, v)| (*k, subst_aux(s, v)))
                        .collect(),
                    sw_failaction: switch.sw_failaction.as_ref().map(|f| Box::new(subst_aux(s, f))),
                    sw_names: switch.sw_names.clone(),
                },
            )
        }

        Lambda::LstringSwitch(arg, cases, default) => {
            Lambda::LstringSwitch(
                Box::new(subst_aux(s, arg)),
                cases
                    .iter()
                    .map(|(k, v)| (k.clone(), subst_aux(s, v)))
                    .collect(),
                default.as_ref().map(|d| Box::new(subst_aux(s, d))),
            )
        }

        Lambda::LstaticRaise(i, args) => {
            Lambda::LstaticRaise(*i, args.iter().map(|arg| subst_aux(s, arg)).collect())
        }

        Lambda::LstaticCatch(body, (i, vars), handler) => {
            Lambda::LstaticCatch(
                Box::new(subst_aux(s, body)),
                (*i, vars.clone()),
                Box::new(subst_aux(s, handler)),
            )
        }

        Lambda::LtryWith(body, exn, handler) => {
            Lambda::LtryWith(
                Box::new(subst_aux(s, body)),
                exn.clone(),
                Box::new(subst_aux(s, handler)),
            )
        }

        Lambda::LifThenElse(cond, then_, else_) => {
            Lambda::LifThenElse(
                Box::new(subst_aux(s, cond)),
                Box::new(subst_aux(s, then_)),
                Box::new(subst_aux(s, else_)),
            )
        }

        Lambda::Lsequence(e1, e2) => {
            Lambda::Lsequence(Box::new(subst_aux(s, e1)), Box::new(subst_aux(s, e2)))
        }

        Lambda::Lwhile(cond, body) => {
            Lambda::Lwhile(Box::new(subst_aux(s, cond)), Box::new(subst_aux(s, body)))
        }

        Lambda::Lfor(v, e1, e2, dir, e3) => {
            Lambda::Lfor(
                v.clone(),
                Box::new(subst_aux(s, e1)),
                Box::new(subst_aux(s, e2)),
                *dir,
                Box::new(subst_aux(s, e3)),
            )
        }

        Lambda::Lassign(id, e) => {
            Lambda::Lassign(id.clone(), Box::new(subst_aux(s, e)))
        }
    }
}

/// Create a substitution from parallel lists of identifiers and expressions.
pub fn make_subst(ids: &[Ident], exprs: &[Lambda]) -> Subst {
    ids.iter().cloned().zip(exprs.iter().cloned()).collect()
}

/// Create a single-binding substitution.
pub fn singleton(id: Ident, expr: Lambda) -> Subst {
    let mut s = HashMap::new();
    s.insert(id, expr);
    s
}

/// Extend a substitution with a new binding.
pub fn extend(mut s: Subst, id: Ident, expr: Lambda) -> Subst {
    s.insert(id, expr);
    s
}

/// Apply a single substitution [id -> expr] to a Lambda.
pub fn subst_single(id: &Ident, expr: &Lambda, lam: &Lambda) -> Lambda {
    let s = singleton(id.clone(), expr.clone());
    subst(&s, lam)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lambda::{Constant, FunctionAttribute, LetKind, Primitive};
    use crate::location::Location;

    #[test]
    fn test_subst_var() {
        let x = Ident::create_local("x");
        let y = Ident::create_local("y");

        let var = Lambda::var(x.clone());
        let replacement = Lambda::const_(Constant::int(42));

        let s = singleton(x, replacement.clone());
        let result = subst(&s, &var);

        assert!(matches!(result, Lambda::Lconst(Constant::Int { i: 42, .. })));

        // y should not be substituted
        let var_y = Lambda::var(y.clone());
        let result_y = subst(&s, &var_y);
        assert!(matches!(result_y, Lambda::Lvar(ref id) if id.name() == "y"));
    }

    #[test]
    fn test_subst_constant() {
        let x = Ident::create_local("x");

        let c = Lambda::const_(Constant::int(1));
        let s = singleton(x, Lambda::const_(Constant::int(99)));

        let result = subst(&s, &c);

        // Constants are not affected by substitution
        assert!(matches!(result, Lambda::Lconst(Constant::Int { i: 1, .. })));
    }

    #[test]
    fn test_subst_let() {
        let x = Ident::create_local("x");
        let y = Ident::create_local("y");
        let z = Ident::create_local("z");

        // let y = x in z  [x -> 42]
        // should become: let y = 42 in z
        let expr = Lambda::let_(
            LetKind::Strict,
            y.clone(),
            Lambda::var(x.clone()),
            Lambda::var(z.clone()),
        );

        let s = singleton(x, Lambda::const_(Constant::int(42)));
        let result = subst(&s, &expr);

        match result {
            Lambda::Llet(_, _, ref arg, _) => {
                assert!(matches!(**arg, Lambda::Lconst(Constant::Int { i: 42, .. })));
            }
            _ => panic!("Expected Llet"),
        }
    }

    #[test]
    fn test_subst_function() {
        let x = Ident::create_local("x");
        let y = Ident::create_local("y");

        // fun x -> y  [y -> 42]
        // should become: fun x -> 42
        let func = Lambda::function_(
            1,
            vec![x.clone()],
            Lambda::var(y.clone()),
            FunctionAttribute::default(),
        );

        let s = singleton(y, Lambda::const_(Constant::int(42)));
        let result = subst(&s, &func);

        match result {
            Lambda::Lfunction(ref f) => {
                assert!(matches!(*f.body, Lambda::Lconst(Constant::Int { i: 42, .. })));
            }
            _ => panic!("Expected Lfunction"),
        }
    }

    #[test]
    fn test_subst_prim() {
        let x = Ident::create_local("x");

        // x + x  [x -> 5]
        // should become: 5 + 5
        let prim = Lambda::prim(
            Primitive::Paddint,
            vec![Lambda::var(x.clone()), Lambda::var(x.clone())],
            Location::none(),
        );

        let s = singleton(x, Lambda::const_(Constant::int(5)));
        let result = subst(&s, &prim);

        match result {
            Lambda::Lprim(ref info) => {
                assert_eq!(info.args.len(), 2);
                assert!(matches!(info.args[0], Lambda::Lconst(Constant::Int { i: 5, .. })));
                assert!(matches!(info.args[1], Lambda::Lconst(Constant::Int { i: 5, .. })));
            }
            _ => panic!("Expected Lprim"),
        }
    }

    #[test]
    fn test_subst_if_then_else() {
        let x = Ident::create_local("x");

        // if x then x else x  [x -> true]
        let expr = Lambda::if_(
            Lambda::var(x.clone()),
            Lambda::var(x.clone()),
            Lambda::var(x.clone()),
        );

        let s = singleton(x, Lambda::true_());
        let result = subst(&s, &expr);

        match result {
            Lambda::LifThenElse(ref cond, ref then_, ref else_) => {
                assert!(matches!(**cond, Lambda::Lconst(Constant::JsTrue)));
                assert!(matches!(**then_, Lambda::Lconst(Constant::JsTrue)));
                assert!(matches!(**else_, Lambda::Lconst(Constant::JsTrue)));
            }
            _ => panic!("Expected LifThenElse"),
        }
    }

    #[test]
    fn test_subst_empty() {
        let x = Ident::create_local("x");
        let var = Lambda::var(x.clone());

        // Empty substitution should return the same expression
        let s = Subst::new();
        let result = subst(&s, &var);

        assert!(matches!(result, Lambda::Lvar(ref id) if id.name() == "x"));
    }

    #[test]
    fn test_make_subst() {
        let x = Ident::create_local("x");
        let y = Ident::create_local("y");

        let ids = vec![x.clone(), y.clone()];
        let exprs = vec![
            Lambda::const_(Constant::int(1)),
            Lambda::const_(Constant::int(2)),
        ];

        let s = make_subst(&ids, &exprs);

        assert_eq!(s.len(), 2);
        assert!(s.contains_key(&x));
        assert!(s.contains_key(&y));
    }

    #[test]
    fn test_subst_single() {
        let x = Ident::create_local("x");
        let var = Lambda::var(x.clone());
        let replacement = Lambda::const_(Constant::int(42));

        let result = subst_single(&x, &replacement, &var);

        assert!(matches!(result, Lambda::Lconst(Constant::Int { i: 42, .. })));
    }

    #[test]
    fn test_subst_letrec() {
        let f = Ident::create_local("f");
        let x = Ident::create_local("x");
        let y = Ident::create_local("y");

        // let rec f = fun x -> y in f  [y -> 42]
        let func = Lambda::function_(
            1,
            vec![x.clone()],
            Lambda::var(y.clone()),
            FunctionAttribute::default(),
        );
        let letrec = Lambda::letrec(vec![(f.clone(), func)], Lambda::var(f.clone()));

        let s = singleton(y, Lambda::const_(Constant::int(42)));
        let result = subst(&s, &letrec);

        match result {
            Lambda::Lletrec(ref bindings, _) => {
                let (_, func_expr) = &bindings[0];
                match func_expr {
                    Lambda::Lfunction(func) => {
                        assert!(matches!(*func.body, Lambda::Lconst(Constant::Int { i: 42, .. })));
                    }
                    _ => panic!("Expected Lfunction in letrec binding"),
                }
            }
            _ => panic!("Expected Lletrec"),
        }
    }
}
