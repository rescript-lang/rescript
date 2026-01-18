//! Lambda closure analysis.
//!
//! This module provides closure analysis for Lambda expressions, which
//! determines:
//! - Whether a Lambda expression is closed (has no free variables)
//! - Variable usage statistics for inlining and optimization decisions
//! - Position tracking for safe substitution

use std::collections::{HashMap, HashSet};

use crate::ident::Ident;
use crate::lambda::analysis::no_side_effects;
use crate::lambda::Lambda;

/// High penalty for loop usage (variables used in loops can't be safely inlined).
const LOOP_USE: i32 = 100;

/// Variable usage statistics.
///
/// Tracks how a variable is used within an expression, which helps
/// determine if it's safe to inline.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VarStats {
    /// Whether all usages are in "top" position (safe for substitution).
    ///
    /// A variable is in top position if it appears before any side effects.
    /// For example, in `x + (print("hi"); y)`, `x` is in top position but
    /// `y` is not (because `print` has side effects).
    pub top: bool,

    /// Number of times the variable is used.
    pub times: i32,
}

impl VarStats {
    /// Create fresh statistics (unused, top position).
    pub fn fresh() -> Self {
        VarStats { top: true, times: 0 }
    }

    /// Create "sink" statistics for variables in loops.
    pub fn sink() -> Self {
        VarStats {
            top: false,
            times: LOOP_USE,
        }
    }

    /// Check if variable is used zero or one time in top position.
    ///
    /// Variables matching this pattern are safe to inline regardless
    /// of whether they're pure.
    pub fn top_and_used_zero_or_one(&self) -> bool {
        self.top && (self.times == 0 || self.times == 1)
    }

    /// Update statistics based on position.
    pub fn update(&mut self, pos: Position) {
        match pos {
            Position::Begin => {
                self.times += 1;
            }
            Position::NotBegin => {
                self.top = false;
                self.times += 1;
            }
            Position::Sink => {
                self.top = false;
                self.times = LOOP_USE;
            }
        }
    }
}

/// Position within an expression for tracking side effect boundaries.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Position {
    /// At the beginning (before any side effects).
    Begin,
    /// Not at the beginning (after some side effects).
    NotBegin,
    /// In a loop or other sink position (unsafe for substitution).
    Sink,
}

impl Position {
    /// Compute new position after evaluating a Lambda expression.
    ///
    /// If we were at `Begin` and the expression has side effects,
    /// we move to `NotBegin`. Otherwise, we stay in the same position.
    pub fn after_lam(self, lam: &Lambda) -> Position {
        if self != Position::Begin || no_side_effects(lam) {
            self
        } else {
            Position::NotBegin
        }
    }
}

/// Type alias for variable statistics map.
pub type StatsMap = HashMap<Ident, VarStats>;

/// Compute free variables with usage statistics.
///
/// This is an enriched version of free variable analysis that also tracks
/// how each variable is used (top position, usage count).
///
/// # Arguments
/// * `exports` - Set of identifiers that are considered bound (exports)
/// * `params` - Initial statistics for parameters
/// * `lam` - The Lambda expression to analyze
///
/// # Returns
/// A map from free variable identifiers to their usage statistics.
pub fn free_variables_with_stats(
    exports: &HashSet<Ident>,
    params: StatsMap,
    lam: &Lambda,
) -> StatsMap {
    let mut fv = params;
    let mut local_set: HashSet<Ident> = exports.clone();

    fn iter(
        lam: &Lambda,
        pos: Position,
        fv: &mut StatsMap,
        local_set: &mut HashSet<Ident>,
    ) {
        match lam {
            Lambda::Lvar(v) => {
                if !local_set.contains(v) {
                    fv.entry(v.clone())
                        .or_insert_with(VarStats::fresh)
                        .update(pos);
                }
            }

            Lambda::Lconst(_) => {}

            Lambda::Lapply(apply) => {
                iter(&apply.ap_func, pos, fv, local_set);
                let new_pos = pos.after_lam(&apply.ap_func);
                for arg in &apply.ap_args {
                    iter(arg, new_pos, fv, local_set);
                }
            }

            Lambda::Lprim(prim_info) => {
                for arg in &prim_info.args {
                    iter(arg, pos, fv, local_set);
                }
            }

            Lambda::LglobalModule(_, _) => {}

            Lambda::Lfunction(func) => {
                for param in &func.params {
                    local_set.insert(param.clone());
                }
                iter(&func.body, Position::Sink, fv, local_set);
            }

            Lambda::Llet(_, id, arg, body) => {
                iter(arg, pos, fv, local_set);
                local_set.insert(id.clone());
                iter(body, Position::Sink, fv, local_set);
            }

            Lambda::Lletrec(bindings, body) => {
                for (id, _) in bindings {
                    local_set.insert(id.clone());
                }
                for (_, expr) in bindings {
                    iter(expr, Position::Sink, fv, local_set);
                }
                iter(body, Position::Sink, fv, local_set);
            }

            Lambda::Lswitch(arg, switch) => {
                iter(arg, pos, fv, local_set);
                let new_pos = pos.after_lam(arg);

                for (_, case) in &switch.sw_consts {
                    iter(case, new_pos, fv, local_set);
                }
                for (_, case) in &switch.sw_blocks {
                    iter(case, new_pos, fv, local_set);
                }
                if let Some(ref fail) = switch.sw_failaction {
                    // If switch is complete, failaction is in normal position
                    // Otherwise it's in sink position
                    let fail_pos = if switch.sw_consts_full || switch.sw_blocks_full {
                        new_pos
                    } else {
                        Position::Sink
                    };
                    iter(fail, fail_pos, fv, local_set);
                }
            }

            Lambda::LstringSwitch(arg, cases, default) => {
                iter(arg, pos, fv, local_set);
                let new_pos = pos.after_lam(arg);

                for (_, case) in cases {
                    iter(case, new_pos, fv, local_set);
                }
                if let Some(d) = default {
                    iter(d, new_pos, fv, local_set);
                }
            }

            Lambda::LstaticRaise(_, args) => {
                for arg in args {
                    iter(arg, Position::Sink, fv, local_set);
                }
            }

            Lambda::LstaticCatch(body, (_, vars), handler) => {
                iter(body, Position::Sink, fv, local_set);
                for v in vars {
                    local_set.insert(v.clone());
                }
                iter(handler, Position::Sink, fv, local_set);
            }

            Lambda::LtryWith(body, _exn, handler) => {
                iter(body, pos, fv, local_set);
                iter(handler, Position::Sink, fv, local_set);
            }

            Lambda::LifThenElse(cond, then_, else_) => {
                iter(cond, pos, fv, local_set);
                let new_pos = pos.after_lam(cond);
                iter(then_, new_pos, fv, local_set);
                iter(else_, new_pos, fv, local_set);
            }

            Lambda::Lsequence(e1, e2) => {
                iter(e1, pos, fv, local_set);
                iter(e2, Position::Sink, fv, local_set);
            }

            Lambda::Lwhile(cond, body) => {
                iter(cond, Position::Sink, fv, local_set);
                iter(body, Position::Sink, fv, local_set);
            }

            Lambda::Lfor(v, e1, e2, _, e3) => {
                local_set.insert(v.clone());
                iter(e1, Position::Sink, fv, local_set);
                iter(e2, Position::Sink, fv, local_set);
                iter(e3, Position::Sink, fv, local_set);
            }

            Lambda::Lassign(id, e) => {
                if !local_set.contains(id) {
                    fv.entry(id.clone())
                        .or_insert_with(VarStats::fresh)
                        .update(pos);
                }
                iter(e, pos, fv, local_set);
            }
        }
    }

    iter(lam, Position::Begin, &mut fv, &mut local_set);
    fv
}

/// Check if a Lambda expression is closed.
///
/// An expression is closed if all its free variables are global identifiers.
pub fn is_closed(lam: &Lambda) -> bool {
    let fv = free_variables_with_stats(&HashSet::new(), HashMap::new(), lam);
    fv.keys().all(|k| k.is_global())
}

/// Check if a function body is closed with respect to its parameters.
///
/// Returns a tuple of:
/// - Whether the body only references its parameters (is closed)
/// - The statistics map for parameter usage
///
/// # Arguments
/// * `exports` - Set of exported identifiers
/// * `params` - List of function parameters
/// * `body` - Function body to analyze
pub fn is_closed_with_map(
    exports: &HashSet<Ident>,
    params: &[Ident],
    body: &Lambda,
) -> (bool, StatsMap) {
    // Create initial stats for each parameter
    let param_map: StatsMap = params
        .iter()
        .map(|p| (p.clone(), VarStats::fresh()))
        .collect();

    let result_map = free_variables_with_stats(exports, param_map, body);

    // Check if the result only contains the original parameters
    let is_closed = result_map.len() == params.len();

    (is_closed, result_map)
}

/// Create a parameter statistics map from a list of identifiers.
pub fn param_map_of_list(params: &[Ident]) -> StatsMap {
    params
        .iter()
        .map(|p| (p.clone(), VarStats::fresh()))
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lambda::{Constant, FunctionAttribute, LetKind};

    #[test]
    fn test_var_stats_fresh() {
        let stats = VarStats::fresh();
        assert!(stats.top);
        assert_eq!(stats.times, 0);
    }

    #[test]
    fn test_var_stats_sink() {
        let stats = VarStats::sink();
        assert!(!stats.top);
        assert_eq!(stats.times, LOOP_USE);
    }

    #[test]
    fn test_var_stats_top_and_used_zero_or_one() {
        assert!(VarStats { top: true, times: 0 }.top_and_used_zero_or_one());
        assert!(VarStats { top: true, times: 1 }.top_and_used_zero_or_one());
        assert!(!VarStats { top: true, times: 2 }.top_and_used_zero_or_one());
        assert!(!VarStats { top: false, times: 1 }.top_and_used_zero_or_one());
    }

    #[test]
    fn test_var_stats_update() {
        let mut stats = VarStats::fresh();

        stats.update(Position::Begin);
        assert!(stats.top);
        assert_eq!(stats.times, 1);

        stats.update(Position::NotBegin);
        assert!(!stats.top);
        assert_eq!(stats.times, 2);

        let mut stats2 = VarStats::fresh();
        stats2.update(Position::Sink);
        assert!(!stats2.top);
        assert_eq!(stats2.times, LOOP_USE);
    }

    #[test]
    fn test_position_after_lam() {
        // Pure expression doesn't change position
        let pure = Lambda::const_(Constant::int(42));
        assert_eq!(Position::Begin.after_lam(&pure), Position::Begin);

        // NotBegin stays NotBegin
        assert_eq!(Position::NotBegin.after_lam(&pure), Position::NotBegin);

        // Sink stays Sink
        assert_eq!(Position::Sink.after_lam(&pure), Position::Sink);
    }

    #[test]
    fn test_free_variables_simple() {
        let x = Ident::create_local("x");
        let var = Lambda::var(x.clone());

        let fv = free_variables_with_stats(&HashSet::new(), HashMap::new(), &var);
        assert!(fv.contains_key(&x));
        assert_eq!(fv.len(), 1);

        let stats = fv.get(&x).unwrap();
        assert!(stats.top);
        assert_eq!(stats.times, 1);
    }

    #[test]
    fn test_free_variables_bound() {
        let x = Ident::create_local("x");
        let y = Ident::create_local("y");

        // let x = y in x
        let expr = Lambda::let_(
            LetKind::Strict,
            x.clone(),
            Lambda::var(y.clone()),
            Lambda::var(x.clone()),
        );

        let fv = free_variables_with_stats(&HashSet::new(), HashMap::new(), &expr);

        // y is free, x is bound
        assert!(fv.contains_key(&y));
        assert!(!fv.contains_key(&x));
    }

    #[test]
    fn test_free_variables_function() {
        let x = Ident::create_local("x");
        let y = Ident::create_local("y");

        // fun x -> y (y is free)
        let func = Lambda::function_(
            1,
            vec![x.clone()],
            Lambda::var(y.clone()),
            FunctionAttribute::default(),
        );

        let fv = free_variables_with_stats(&HashSet::new(), HashMap::new(), &func);

        assert!(fv.contains_key(&y));
        assert!(!fv.contains_key(&x));

        // y is used in sink position (inside function body)
        let stats = fv.get(&y).unwrap();
        assert!(!stats.top);
    }

    #[test]
    fn test_free_variables_loop() {
        let x = Ident::create_local("x");

        // while true do x
        let while_loop = Lambda::while_(
            Lambda::true_(),
            Lambda::var(x.clone()),
        );

        let fv = free_variables_with_stats(&HashSet::new(), HashMap::new(), &while_loop);

        // x is used in loop (sink position)
        let stats = fv.get(&x).unwrap();
        assert!(!stats.top);
        assert_eq!(stats.times, LOOP_USE);
    }

    #[test]
    fn test_is_closed() {
        // Constant is closed
        assert!(is_closed(&Lambda::const_(Constant::int(42))));

        // Variable is not closed
        let x = Ident::create_local("x");
        assert!(!is_closed(&Lambda::var(x)));

        // Function with self-reference is closed
        let param = Ident::create_local("param");
        let func = Lambda::function_(
            1,
            vec![param.clone()],
            Lambda::var(param.clone()),
            FunctionAttribute::default(),
        );
        assert!(is_closed(&func));
    }

    #[test]
    fn test_is_closed_with_map() {
        let x = Ident::create_local("x");
        let y = Ident::create_local("y");

        // fun x -> x is closed
        let body1 = Lambda::var(x.clone());
        let (closed1, _) = is_closed_with_map(&HashSet::new(), &[x.clone()], &body1);
        assert!(closed1);

        // fun x -> y is not closed (y is free)
        let body2 = Lambda::var(y.clone());
        let (closed2, _) = is_closed_with_map(&HashSet::new(), &[x.clone()], &body2);
        assert!(!closed2);
    }

    #[test]
    fn test_param_map_of_list() {
        let x = Ident::create_local("x");
        let y = Ident::create_local("y");

        let map = param_map_of_list(&[x.clone(), y.clone()]);

        assert!(map.contains_key(&x));
        assert!(map.contains_key(&y));
        assert_eq!(map.len(), 2);

        assert!(map.get(&x).unwrap().top);
        assert_eq!(map.get(&x).unwrap().times, 0);
    }

    #[test]
    fn test_free_variables_with_exports() {
        let x = Ident::create_local("x");
        let exported = Ident::create_local("exported");

        // x + exported (but exported is in exports set)
        let expr = Lambda::seq(Lambda::var(x.clone()), Lambda::var(exported.clone()));

        let mut exports = HashSet::new();
        exports.insert(exported.clone());

        let fv = free_variables_with_stats(&exports, HashMap::new(), &expr);

        // x is free, exported is not (it's in exports)
        assert!(fv.contains_key(&x));
        assert!(!fv.contains_key(&exported));
    }
}
