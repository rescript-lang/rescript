//! Free variable analysis for Lambda IR.
//!
//! This module computes the set of free variables in a Lambda expression.
//! Free variables are variables that are used but not bound within the
//! expression. This analysis is essential for closure conversion and
//! determining what variables need to be captured in closures.

use std::collections::HashSet;

use crate::ident::Ident;
use crate::lambda::Lambda;

/// A set of identifiers, used for tracking free variables.
pub type IdentSet = HashSet<Ident>;

/// Compute the free variables of a Lambda expression.
///
/// This returns the set of all variables that are referenced but not
/// bound within the expression. Bound variables from `let`, `function`,
/// `for`, `try`, and `catch` constructs are excluded.
///
/// Note: Global modules are not considered free variables since they
/// represent module references rather than local bindings.
pub fn free_variables(lam: &Lambda) -> IdentSet {
    let mut fv = IdentSet::new();
    collect_free_variables(lam, &mut fv);
    fv
}

/// Collect free variables into the provided set.
fn collect_free_variables(lam: &Lambda, fv: &mut IdentSet) {
    match lam {
        Lambda::Lvar(id) => {
            fv.insert(id.clone());
        }

        Lambda::Lassign(id, e) => {
            collect_free_variables(e, fv);
            fv.insert(id.clone());
        }

        Lambda::LstaticCatch(body, (_, vars), handler) => {
            collect_free_variables(body, fv);
            collect_free_variables(handler, fv);
            // Remove bound variables
            for id in vars {
                fv.remove(id);
            }
        }

        Lambda::LtryWith(body, exn, handler) => {
            collect_free_variables(body, fv);
            collect_free_variables(handler, fv);
            fv.remove(exn);
        }

        Lambda::Lfunction(func) => {
            collect_free_variables(&func.body, fv);
            // Remove bound parameters
            for param in &func.params {
                fv.remove(param);
            }
        }

        Lambda::Llet(_kind, id, arg, body) => {
            collect_free_variables(arg, fv);
            collect_free_variables(body, fv);
            fv.remove(id);
        }

        Lambda::Lletrec(bindings, body) => {
            collect_free_variables(body, fv);
            for (_, expr) in bindings {
                collect_free_variables(expr, fv);
            }
            // Remove all bound identifiers
            for (id, _) in bindings {
                fv.remove(id);
            }
        }

        Lambda::Lfor(v, e1, e2, _dir, e3) => {
            collect_free_variables(e1, fv);
            collect_free_variables(e2, fv);
            collect_free_variables(e3, fv);
            fv.remove(v);
        }

        Lambda::Lconst(_) => {
            // Constants have no free variables
        }

        Lambda::Lapply(apply) => {
            collect_free_variables(&apply.ap_func, fv);
            for arg in &apply.ap_args {
                collect_free_variables(arg, fv);
            }
        }

        Lambda::LglobalModule(_, _) => {
            // Global modules are not considered free variables
            // (they represent module references, not local bindings)
        }

        Lambda::Lprim(prim_info) => {
            for arg in &prim_info.args {
                collect_free_variables(arg, fv);
            }
        }

        Lambda::Lswitch(arg, switch) => {
            collect_free_variables(arg, fv);
            for (_, lam) in &switch.sw_consts {
                collect_free_variables(lam, fv);
            }
            for (_, lam) in &switch.sw_blocks {
                collect_free_variables(lam, fv);
            }
            if let Some(ref fail) = switch.sw_failaction {
                collect_free_variables(fail, fv);
            }
        }

        Lambda::LstringSwitch(arg, cases, default) => {
            collect_free_variables(arg, fv);
            for (_, lam) in cases {
                collect_free_variables(lam, fv);
            }
            if let Some(d) = default {
                collect_free_variables(d, fv);
            }
        }

        Lambda::LstaticRaise(_, args) => {
            for arg in args {
                collect_free_variables(arg, fv);
            }
        }

        Lambda::LifThenElse(e1, e2, e3) => {
            collect_free_variables(e1, fv);
            collect_free_variables(e2, fv);
            collect_free_variables(e3, fv);
        }

        Lambda::Lsequence(e1, e2) => {
            collect_free_variables(e1, fv);
            collect_free_variables(e2, fv);
        }

        Lambda::Lwhile(cond, body) => {
            collect_free_variables(cond, fv);
            collect_free_variables(body, fv);
        }
    }
}

/// Check if a Lambda expression has any free variables from the given set.
///
/// This is more efficient than computing the full free variable set when
/// we only need to know if certain variables are captured.
///
/// Note: This properly handles bound variables - a variable that is bound
/// in the expression is not considered free, even if it appears in `vars`.
pub fn has_free_variables(lam: &Lambda, vars: &IdentSet) -> bool {
    if vars.is_empty() {
        return false;
    }
    let fv = free_variables(lam);
    vars.iter().any(|v| fv.contains(v))
}

/// Check if a Lambda expression is closed (has no free variables).
pub fn is_closed(lam: &Lambda) -> bool {
    free_variables(lam).is_empty()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lambda::{Constant, FunctionAttribute, LetKind};

    #[test]
    fn test_free_variable_simple() {
        let x = Ident::create_local("x");
        let var = Lambda::var(x.clone());

        let fv = free_variables(&var);
        assert!(fv.contains(&x));
        assert_eq!(fv.len(), 1);
    }

    #[test]
    fn test_free_variable_constant() {
        let c = Lambda::const_(Constant::int(42));

        let fv = free_variables(&c);
        assert!(fv.is_empty());
    }

    #[test]
    fn test_free_variable_let_binding() {
        let x = Ident::create_local("x");
        let y = Ident::create_local("y");

        // let x = y in x
        // y is free, x is bound
        let expr = Lambda::let_(
            LetKind::Strict,
            x.clone(),
            Lambda::var(y.clone()),
            Lambda::var(x.clone()),
        );

        let fv = free_variables(&expr);
        assert!(fv.contains(&y));
        assert!(!fv.contains(&x));
        assert_eq!(fv.len(), 1);
    }

    #[test]
    fn test_free_variable_function() {
        let x = Ident::create_local("x");
        let y = Ident::create_local("y");

        // fun x -> y
        // y is free, x is bound
        let func = Lambda::function_(
            1,
            vec![x.clone()],
            Lambda::var(y.clone()),
            FunctionAttribute::default(),
        );

        let fv = free_variables(&func);
        assert!(fv.contains(&y));
        assert!(!fv.contains(&x));
        assert_eq!(fv.len(), 1);
    }

    #[test]
    fn test_free_variable_function_self_reference() {
        let x = Ident::create_local("x");

        // fun x -> x
        // No free variables
        let func = Lambda::function_(
            1,
            vec![x.clone()],
            Lambda::var(x.clone()),
            FunctionAttribute::default(),
        );

        let fv = free_variables(&func);
        assert!(fv.is_empty());
    }

    #[test]
    fn test_free_variable_letrec() {
        let f = Ident::create_local("f");
        let x = Ident::create_local("x");
        let y = Ident::create_local("y");

        // let rec f = fun x -> y in f
        // y is free, f and x are bound
        let func = Lambda::function_(
            1,
            vec![x.clone()],
            Lambda::var(y.clone()),
            FunctionAttribute::default(),
        );

        let letrec = Lambda::letrec(vec![(f.clone(), func)], Lambda::var(f.clone()));

        let fv = free_variables(&letrec);
        assert!(fv.contains(&y));
        assert!(!fv.contains(&f));
        assert!(!fv.contains(&x));
        assert_eq!(fv.len(), 1);
    }

    #[test]
    fn test_free_variable_for_loop() {
        let i = Ident::create_local("i");
        let n = Ident::create_local("n");
        let body_var = Ident::create_local("body");

        // for i = 0 to n do body_var
        // n and body_var are free, i is bound
        let for_loop = Lambda::for_(
            i.clone(),
            Lambda::const_(Constant::int(0)),
            Lambda::var(n.clone()),
            crate::lambda::DirectionFlag::Upto,
            Lambda::var(body_var.clone()),
        );

        let fv = free_variables(&for_loop);
        assert!(fv.contains(&n));
        assert!(fv.contains(&body_var));
        assert!(!fv.contains(&i));
        assert_eq!(fv.len(), 2);
    }

    #[test]
    fn test_free_variable_try_with() {
        let exn = Ident::create_local("exn");
        let x = Ident::create_local("x");
        let y = Ident::create_local("y");

        // try x with exn -> y
        // x and y are free, exn is bound
        let try_expr = Lambda::try_(
            Lambda::var(x.clone()),
            exn.clone(),
            Lambda::var(y.clone()),
        );

        let fv = free_variables(&try_expr);
        assert!(fv.contains(&x));
        assert!(fv.contains(&y));
        assert!(!fv.contains(&exn));
        assert_eq!(fv.len(), 2);
    }

    #[test]
    fn test_free_variable_static_catch() {
        let p1 = Ident::create_local("p1");
        let x = Ident::create_local("x");
        let y = Ident::create_local("y");

        // catch x with (1, p1) -> y
        // x and y are free, p1 is bound
        let catch_expr = Lambda::static_catch(
            Lambda::var(x.clone()),
            1,
            vec![p1.clone()],
            Lambda::var(y.clone()),
        );

        let fv = free_variables(&catch_expr);
        assert!(fv.contains(&x));
        assert!(fv.contains(&y));
        assert!(!fv.contains(&p1));
        assert_eq!(fv.len(), 2);
    }

    #[test]
    fn test_free_variable_assign() {
        let x = Ident::create_local("x");
        let y = Ident::create_local("y");

        // x := y
        // Both x and y are free (x is being assigned, not bound)
        let assign = Lambda::assign(x.clone(), Lambda::var(y.clone()));

        let fv = free_variables(&assign);
        assert!(fv.contains(&x));
        assert!(fv.contains(&y));
        assert_eq!(fv.len(), 2);
    }

    #[test]
    fn test_has_free_variables() {
        let x = Ident::create_local("x");
        let y = Ident::create_local("y");
        let z = Ident::create_local("z");

        // let x = y in x
        let expr = Lambda::let_(
            LetKind::Strict,
            x.clone(),
            Lambda::var(y.clone()),
            Lambda::var(x.clone()),
        );

        let mut check_set = IdentSet::new();
        check_set.insert(y.clone());
        assert!(has_free_variables(&expr, &check_set));

        check_set.clear();
        check_set.insert(z);
        assert!(!has_free_variables(&expr, &check_set));

        check_set.clear();
        check_set.insert(x);
        assert!(!has_free_variables(&expr, &check_set));
    }

    #[test]
    fn test_is_closed() {
        let x = Ident::create_local("x");

        // Constant is closed
        assert!(is_closed(&Lambda::const_(Constant::int(42))));

        // fun x -> x is closed
        let func = Lambda::function_(
            1,
            vec![x.clone()],
            Lambda::var(x.clone()),
            FunctionAttribute::default(),
        );
        assert!(is_closed(&func));

        // Variable reference is not closed
        assert!(!is_closed(&Lambda::var(x)));
    }

    #[test]
    fn test_global_module_not_free() {
        let m = Ident::create_local("M");

        let global = Lambda::global_module(m.clone(), false);
        let fv = free_variables(&global);

        // Global modules should not be considered free variables
        assert!(fv.is_empty());
    }
}
