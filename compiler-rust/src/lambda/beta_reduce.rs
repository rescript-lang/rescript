//! Lambda beta reduction.
//!
//! This module provides beta reduction (function inlining) for Lambda expressions.
//!
//! Beta reduction transforms `(fun x y -> body) arg1 arg2` into
//! `let x = arg1 in let y = arg2 in body`, but with optimizations:
//!
//! - Simple cases: direct substitution when arguments are values
//! - Complex cases: create let bindings with proper evaluation order
//! - Side effect handling: preserve evaluation semantics
//!
//! # Evaluation Order
//!
//! OCaml/ReScript has unspecified evaluation order for function arguments.
//! We ensure that side effects happen in a consistent order by binding
//! arguments to variables when needed.

use std::collections::HashMap;

use crate::context::IdGenerator;
use crate::ident::Ident;
use crate::lambda::analysis::no_side_effects;
use crate::lambda::closure::StatsMap;
use crate::lambda::subst::{Subst, subst};
use crate::lambda::compat::LetKind;
use crate::lambda::primitive::Primitive;
use crate::lambda::{Apply, Lambda, PrimInfo};

/// Tracking state for simple beta reduction.
struct ArgUsage {
    /// The argument expression
    lambda: Lambda,
    /// Whether this argument has been used
    used: bool,
}

/// Attempt simple beta reduction.
///
/// This handles the common case where the function body is a primitive
/// or simple application, and all arguments can be directly substituted.
///
/// # Conditions for simple reduction:
/// - Function body is a primitive or application
/// - Each parameter is used at most once
/// - Arguments in the body are variables or constants
///
/// Returns `Some(reduced)` if reduction is possible, `None` otherwise.
pub fn simple_beta_reduce(
    params: &[Ident],
    body: &Lambda,
    args: &[Lambda],
) -> Option<Lambda> {
    // Build parameter -> argument mapping
    let mut param_map: HashMap<Ident, ArgUsage> = HashMap::new();
    for (param, arg) in params.iter().zip(args.iter()) {
        param_map.insert(
            param.clone(),
            ArgUsage {
                lambda: arg.clone(),
                used: false,
            },
        );
    }

    // Try to substitute arguments in the body
    fn substitute_args(
        lams: &[Lambda],
        param_map: &mut HashMap<Ident, ArgUsage>,
    ) -> Option<Vec<Lambda>> {
        let mut result = Vec::new();
        for lam in lams {
            match lam {
                Lambda::Lvar(x) => {
                    if let Some(usage) = param_map.get_mut(x) {
                        if usage.used {
                            return None;
                        }
                        usage.used = true;
                        result.push(usage.lambda.clone());
                    } else {
                        result.push(lam.clone());
                    }
                }
                Lambda::Lconst(_) => {
                    result.push(lam.clone());
                }
                _ => {
                    // Complex argument - can't do simple reduction
                    return None;
                }
            }
        }
        Some(result)
    }

    match body {
        // Primitive application: (fun x y -> prim(x, y)) a b => prim(a, b)
        Lambda::Lprim(prim_info) => {
            let new_args = substitute_args(&prim_info.args, &mut param_map)?;

            // Build the new primitive
            let mut result = Lambda::Lprim(PrimInfo {
                primitive: prim_info.primitive.clone(),
                args: new_args,
                loc: prim_info.loc.clone(),
            });

            // Add sequences for unused arguments (preserve side effects)
            for (_param, usage) in param_map.iter() {
                if !usage.used {
                    result = Lambda::seq(usage.lambda.clone(), result);
                }
            }

            Some(result)
        }

        // Function application: (fun x y -> f(x, y)) a b => f(a, b)
        Lambda::Lapply(apply) => {
            // Check if the function is simple (variable or module access)
            let is_simple_func = match &*apply.ap_func {
                Lambda::Lvar(_) => true,
                Lambda::Lprim(PrimInfo {
                    primitive: Primitive::Pfield(_, _),
                    args,
                    ..
                }) => {
                    matches!(args.as_slice(), [Lambda::LglobalModule(_, _)])
                }
                _ => false,
            };

            if !is_simple_func {
                return None;
            }

            let new_args = substitute_args(&apply.ap_args, &mut param_map)?;

            // Handle function substitution if it's a variable
            let new_func = match &*apply.ap_func {
                Lambda::Lvar(fn_name) => {
                    if let Some(usage) = param_map.get_mut(fn_name) {
                        if usage.used {
                            return None;
                        }
                        usage.used = true;
                        usage.lambda.clone()
                    } else {
                        (*apply.ap_func).clone()
                    }
                }
                _ => (*apply.ap_func).clone(),
            };

            // Build the new application
            let mut result = Lambda::Lapply(Apply {
                ap_func: Box::new(new_func),
                ap_args: new_args,
                ap_info: apply.ap_info.clone(),
                ap_transformed_jsx: apply.ap_transformed_jsx,
            });

            // Add sequences for unused arguments
            for (_param, usage) in param_map.iter() {
                if !usage.used {
                    result = Lambda::seq(usage.lambda.clone(), result);
                }
            }

            Some(result)
        }

        _ => None,
    }
}

/// Perform beta reduction without variable usage statistics.
///
/// This is the simplest form of beta reduction that creates let bindings
/// for all arguments.
pub fn no_names_beta_reduce(params: &[Ident], body: &Lambda, args: &[Lambda]) -> Lambda {
    // Try simple beta reduce first
    if let Some(result) = simple_beta_reduce(params, body, args) {
        return result;
    }

    // Fall back to creating let bindings
    let mut result = body.clone();
    for (param, arg) in params.iter().zip(args.iter()).rev() {
        result = refine_let(LetKind::Strict, param.clone(), arg.clone(), result);
    }
    result
}

/// Perform beta reduction with smart inlining decisions.
///
/// This uses closure analysis to determine which arguments can be
/// directly substituted vs. bound to let variables.
///
/// Arguments that are:
/// - Constants or variables: always substituted
/// - Pure and used 0-1 times: substituted
/// - Otherwise: bound to let variables
pub fn propagate_beta_reduce_with_map(
    id_gen: &IdGenerator,
    stats: &StatsMap,
    params: &[Ident],
    body: &Lambda,
    args: &[Lambda],
) -> Lambda {
    // Try simple beta reduce first
    if let Some(result) = simple_beta_reduce(params, body, args) {
        return result;
    }

    // Categorize arguments
    let mut rest_bindings: Vec<(Ident, Lambda)> = Vec::new();
    let mut substitution: Subst = HashMap::new();

    for (param, arg) in params.iter().zip(args.iter()) {
        match arg {
            // Constants and variables can always be substituted
            Lambda::Lconst(_) | Lambda::Lvar(_) => {
                substitution.insert(param.clone(), arg.clone());
            }
            // Global modules need to be bound (for proper module loading)
            Lambda::LglobalModule(_, _) => {
                let new_param = param.rename(id_gen);
                rest_bindings.push((new_param.clone(), arg.clone()));
                substitution.insert(param.clone(), Lambda::var(new_param));
            }
            _ => {
                // Check if we can substitute based on usage stats
                if no_side_effects(arg) {
                    if let Some(stat) = stats.get(param) {
                        if stat.top_and_used_zero_or_one() {
                            substitution.insert(param.clone(), arg.clone());
                            continue;
                        }
                    }
                }
                // Bind to a new variable
                let new_param = param.rename(id_gen);
                rest_bindings.push((new_param.clone(), arg.clone()));
                substitution.insert(param.clone(), Lambda::var(new_param));
            }
        }
    }

    // Apply substitution to body
    let new_body = subst(&substitution, body);

    // Wrap with let bindings (in reverse order for proper scoping)
    let mut result = new_body;
    for (param, arg) in rest_bindings.into_iter().rev() {
        result = refine_let(LetKind::Strict, param, arg, result);
    }
    result
}

/// Create an optimized let binding.
///
/// This may optimize away the binding in simple cases:
/// - If the body is just the variable, return the value
/// - If the value is a simple constant, substitute directly
fn refine_let(kind: LetKind, id: Ident, value: Lambda, body: Lambda) -> Lambda {
    // Check if body is just the variable being bound
    if let Lambda::Lvar(ref body_id) = body {
        if body_id == &id {
            return value;
        }
    }

    // For now, just create the let binding
    // More optimizations can be added here
    Lambda::let_(kind, id, value, body)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lambda::closure::VarStats;
    use crate::lambda::{Constant, Primitive};
    use crate::location::Location;

    #[test]
    fn test_simple_beta_reduce_prim() {
        let x = Ident::create_local("x");
        let y = Ident::create_local("y");

        // (fun x y -> x + y) 1 2 => 1 + 2
        let params = vec![x.clone(), y.clone()];
        let body = Lambda::prim(
            Primitive::Paddint,
            vec![Lambda::var(x.clone()), Lambda::var(y.clone())],
            Location::none(),
        );
        let args = vec![
            Lambda::const_(Constant::int(1)),
            Lambda::const_(Constant::int(2)),
        ];

        let result = simple_beta_reduce(&params, &body, &args);
        assert!(result.is_some());

        let reduced = result.unwrap();
        match reduced {
            Lambda::Lprim(ref info) => {
                assert_eq!(info.args.len(), 2);
                assert!(matches!(info.args[0], Lambda::Lconst(Constant::Int { i: 1, .. })));
                assert!(matches!(info.args[1], Lambda::Lconst(Constant::Int { i: 2, .. })));
            }
            _ => panic!("Expected Lprim"),
        }
    }

    #[test]
    fn test_simple_beta_reduce_unused_arg() {
        let x = Ident::create_local("x");
        let y = Ident::create_local("y");

        // (fun x y -> x) 1 (side_effect) => side_effect; 1
        // Note: y is unused, but we need to evaluate the argument
        let params = vec![x.clone(), y.clone()];
        let body = Lambda::var(x.clone());  // Only uses x

        // For simple_beta_reduce, complex unused args prevent optimization
        let args = vec![
            Lambda::const_(Constant::int(1)),
            Lambda::var(Ident::create_local("z")),  // Variable, so it's okay
        ];

        // This should NOT work because body is not a prim or apply
        let result = simple_beta_reduce(&params, &body, &args);
        assert!(result.is_none());
    }

    #[test]
    fn test_simple_beta_reduce_double_use() {
        let x = Ident::create_local("x");

        // (fun x -> x + x) 1 => should NOT simple reduce (x used twice)
        let params = vec![x.clone()];
        let body = Lambda::prim(
            Primitive::Paddint,
            vec![Lambda::var(x.clone()), Lambda::var(x.clone())],
            Location::none(),
        );
        let args = vec![Lambda::const_(Constant::int(1))];

        let result = simple_beta_reduce(&params, &body, &args);
        assert!(result.is_none());
    }

    #[test]
    fn test_no_names_beta_reduce() {
        let x = Ident::create_local("x");
        let y = Ident::create_local("y");

        // (fun x y -> x + x + y) (f) (g)
        // x is used twice, so simple_beta_reduce won't work
        // => let x = f in let y = g in x + x + y
        let params = vec![x.clone(), y.clone()];
        // Body uses x twice, which prevents simple_beta_reduce
        let body = Lambda::prim(
            Primitive::Paddint,
            vec![
                Lambda::prim(
                    Primitive::Paddint,
                    vec![Lambda::var(x.clone()), Lambda::var(x.clone())],
                    Location::none(),
                ),
                Lambda::var(y.clone()),
            ],
            Location::none(),
        );
        let f = Ident::create_local("f");
        let g = Ident::create_local("g");
        let args = vec![
            Lambda::var(f),
            Lambda::var(g),
        ];

        let result = no_names_beta_reduce(&params, &body, &args);

        // Should create let bindings because x is used twice
        match result {
            Lambda::Llet(_, _, _, _) => {}
            _ => panic!("Expected Llet, got {result:?}"),
        }
    }

    #[test]
    fn test_propagate_beta_reduce_with_map_const() {
        let id_gen = IdGenerator::new();
        let x = Ident::create_local("x");

        // (fun x -> x + 1) 5
        // With stats showing x is used once in top position
        // => 5 + 1 (direct substitution)
        let params = vec![x.clone()];
        let body = Lambda::prim(
            Primitive::Paddint,
            vec![
                Lambda::var(x.clone()),
                Lambda::const_(Constant::int(1)),
            ],
            Location::none(),
        );
        let args = vec![Lambda::const_(Constant::int(5))];

        let mut stats = StatsMap::new();
        stats.insert(x.clone(), VarStats { top: true, times: 1 });

        let result = propagate_beta_reduce_with_map(&id_gen, &stats, &params, &body, &args);

        // Should get direct primitive application
        match result {
            Lambda::Lprim(ref info) => {
                assert!(matches!(info.args[0], Lambda::Lconst(Constant::Int { i: 5, .. })));
            }
            _ => panic!("Expected Lprim, got {result:?}"),
        }
    }

    #[test]
    fn test_propagate_beta_reduce_with_map_pure_once() {
        let id_gen = IdGenerator::new();
        let x = Ident::create_local("x");
        let y = Ident::create_local("y");

        // (fun x -> x + 1) y where y is pure
        // With stats showing x is used once in top position
        // => y + 1 (substitute because pure and used once)
        let params = vec![x.clone()];
        let body = Lambda::prim(
            Primitive::Paddint,
            vec![
                Lambda::var(x.clone()),
                Lambda::const_(Constant::int(1)),
            ],
            Location::none(),
        );
        // Lambda::var is pure
        let args = vec![Lambda::var(y.clone())];

        let mut stats = StatsMap::new();
        stats.insert(x.clone(), VarStats { top: true, times: 1 });

        let result = propagate_beta_reduce_with_map(&id_gen, &stats, &params, &body, &args);

        // Should substitute y directly
        match result {
            Lambda::Lprim(ref info) => {
                match &info.args[0] {
                    Lambda::Lvar(id) => assert_eq!(id.name(), "y"),
                    _ => panic!("Expected Lvar"),
                }
            }
            _ => panic!("Expected Lprim, got {result:?}"),
        }
    }

    #[test]
    fn test_refine_let_identity() {
        let x = Ident::create_local("x");

        // let x = v in x => v
        let value = Lambda::const_(Constant::int(42));
        let body = Lambda::var(x.clone());

        let result = refine_let(LetKind::Strict, x, value.clone(), body);

        assert!(matches!(result, Lambda::Lconst(Constant::Int { i: 42, .. })));
    }

    #[test]
    fn test_refine_let_normal() {
        let x = Ident::create_local("x");
        let y = Ident::create_local("y");

        // let x = 42 in y => let x = 42 in y (can't optimize)
        let value = Lambda::const_(Constant::int(42));
        let body = Lambda::var(y);

        let result = refine_let(LetKind::Strict, x, value, body);

        assert!(matches!(result, Lambda::Llet(_, _, _, _)));
    }
}
