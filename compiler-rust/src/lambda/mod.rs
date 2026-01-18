//! Lambda IR - Intermediate representation for ReScript compilation.
//!
//! This module provides the Lambda IR, which sits between the typed AST
//! and the JavaScript IR in the compilation pipeline. The Lambda IR is
//! a simplified, imperative representation that makes code generation
//! and optimization easier.
//!
//! # Architecture
//!
//! The Lambda IR is organized into several submodules:
//!
//! - [`constant`] - Lambda constants (literals, blocks, etc.)
//! - [`primitive`] - Primitive operations (arithmetic, comparisons, etc.)
//! - [`compat`] - Compatibility types (comparison, let_kind, etc.)
//! - [`tag_info`] - Block tag information for variants and records
//! - [`arity`] - Arity tracking for functions
//! - [`free_variables`] - Free variable analysis
//! - [`analysis`] - Side effect and size analysis
//! - [`closure`] - Closure analysis
//! - [`subst`] - Variable substitution
//! - [`beta_reduce`] - Beta reduction (function inlining)
//! - [`print`] - Pretty printing for debugging
//! - [`compile_context`] - Compilation context for Lambda to JS IR

pub mod analysis;
pub mod arity;
pub mod beta_reduce;
pub mod closure;
pub mod compat;
pub mod compile_context;
pub mod constant;
pub mod free_variables;
pub mod primitive;
pub mod print;
pub mod subst;
pub mod tag_info;

use crate::ident::Ident;
use crate::location::Location;
pub use compat::{Comparison, FieldDbgInfo, LetKind, SetFieldDbgInfo};
pub use constant::Constant;
pub use primitive::Primitive;
pub use tag_info::TagInfo;

/// Application status for function calls
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ApplyStatus {
    /// No special handling
    Na,
    /// Infer full application
    InferFull,
    /// Uncurried application
    Uncurry,
}

/// Inline attribute for functions
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum InlineAttribute {
    /// Always inline this function
    Always,
    /// Never inline this function
    Never,
    /// Default inlining behavior
    #[default]
    Default,
}

/// Application info for function calls
#[derive(Debug, Clone)]
pub struct ApInfo {
    /// Source location
    pub loc: Location,
    /// Inline hint
    pub inlined: InlineAttribute,
    /// Application status
    pub status: ApplyStatus,
}

/// Function attributes
#[derive(Debug, Clone)]
pub struct FunctionAttribute {
    /// Inline hint
    pub inline: InlineAttribute,
    /// Is this a functor?
    pub is_a_functor: bool,
    /// Does this function return unit?
    pub return_unit: bool,
    /// Is this an async function?
    pub async_: bool,
    /// Directive (e.g., "use server")
    pub directive: Option<String>,
    /// Does this function take a single unit argument?
    pub one_unit_arg: bool,
}

impl Default for FunctionAttribute {
    fn default() -> Self {
        Self {
            inline: InlineAttribute::Default,
            is_a_functor: false,
            return_unit: false,
            async_: false,
            directive: None,
            one_unit_arg: false,
        }
    }
}

/// Switch construct for pattern matching
#[derive(Debug, Clone)]
pub struct LambdaSwitch {
    /// Are all constant cases covered?
    pub sw_consts_full: bool,
    /// Constant case branches (tag -> lambda)
    pub sw_consts: Vec<(i32, Lambda)>,
    /// Are all block cases covered?
    pub sw_blocks_full: bool,
    /// Block case branches (tag -> lambda)
    pub sw_blocks: Vec<(i32, Lambda)>,
    /// Fallback action if no case matches
    pub sw_failaction: Option<Box<Lambda>>,
    /// Names for switch cases (for untagged variants)
    pub sw_names: Option<SwitchNames>,
}

/// Switch names for untagged variants
#[derive(Debug, Clone)]
pub struct SwitchNames {
    // Placeholder - actual structure from Ast_untagged_variants.switch_names
    pub names: Vec<String>,
}

/// Lambda function definition
#[derive(Debug, Clone)]
pub struct LFunction {
    /// Number of arguments (arity)
    pub arity: i32,
    /// Parameter identifiers
    pub params: Vec<Ident>,
    /// Function body
    pub body: Box<Lambda>,
    /// Function attributes
    pub attr: FunctionAttribute,
}

/// Primitive operation info
#[derive(Debug, Clone)]
pub struct PrimInfo {
    /// The primitive operation
    pub primitive: Primitive,
    /// Arguments to the primitive
    pub args: Vec<Lambda>,
    /// Source location
    pub loc: Location,
}

/// Function application
#[derive(Debug, Clone)]
pub struct Apply {
    /// Function being applied
    pub ap_func: Box<Lambda>,
    /// Arguments
    pub ap_args: Vec<Lambda>,
    /// Application info
    pub ap_info: ApInfo,
    /// Was this transformed from JSX?
    pub ap_transformed_jsx: bool,
}

/// Lambda IR expression
///
/// This is the core representation used for code generation and optimization.
#[derive(Debug, Clone)]
pub enum Lambda {
    /// Variable reference
    Lvar(Ident),

    /// Global module reference (with dynamic import flag)
    LglobalModule(Ident, bool),

    /// Constant value
    Lconst(Constant),

    /// Function application
    Lapply(Apply),

    /// Function definition
    Lfunction(LFunction),

    /// Let binding: let kind, identifier, value, body
    Llet(LetKind, Ident, Box<Lambda>, Box<Lambda>),

    /// Recursive let bindings
    Lletrec(Vec<(Ident, Lambda)>, Box<Lambda>),

    /// Primitive operation
    Lprim(PrimInfo),

    /// Switch on integer/block tag
    Lswitch(Box<Lambda>, LambdaSwitch),

    /// Switch on string value
    LstringSwitch(Box<Lambda>, Vec<(String, Lambda)>, Option<Box<Lambda>>),

    /// Raise a static exception
    LstaticRaise(i32, Vec<Lambda>),

    /// Catch a static exception: body, (exception id, params), handler
    LstaticCatch(Box<Lambda>, (i32, Vec<Ident>), Box<Lambda>),

    /// Try-with exception handling
    LtryWith(Box<Lambda>, Ident, Box<Lambda>),

    /// If-then-else
    LifThenElse(Box<Lambda>, Box<Lambda>, Box<Lambda>),

    /// Sequence of expressions
    Lsequence(Box<Lambda>, Box<Lambda>),

    /// While loop
    Lwhile(Box<Lambda>, Box<Lambda>),

    /// For loop: variable, start, end, direction, body
    Lfor(Ident, Box<Lambda>, Box<Lambda>, DirectionFlag, Box<Lambda>),

    /// Assignment to a mutable variable
    Lassign(Ident, Box<Lambda>),
}

/// Direction flag for for loops
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DirectionFlag {
    /// Ascending (to)
    Upto,
    /// Descending (downto)
    Downto,
}

impl Lambda {
    /// Create a variable reference
    pub fn var(id: Ident) -> Self {
        Lambda::Lvar(id)
    }

    /// Create a global module reference
    pub fn global_module(id: Ident, dynamic_import: bool) -> Self {
        Lambda::LglobalModule(id, dynamic_import)
    }

    /// Create a constant
    pub fn const_(c: Constant) -> Self {
        Lambda::Lconst(c)
    }

    /// Create a function
    pub fn function_(
        arity: i32,
        params: Vec<Ident>,
        body: Lambda,
        attr: FunctionAttribute,
    ) -> Self {
        Lambda::Lfunction(LFunction {
            arity,
            params,
            body: Box::new(body),
            attr,
        })
    }

    /// Create a let binding
    pub fn let_(kind: LetKind, id: Ident, value: Lambda, body: Lambda) -> Self {
        Lambda::Llet(kind, id, Box::new(value), Box::new(body))
    }

    /// Create recursive let bindings
    pub fn letrec(bindings: Vec<(Ident, Lambda)>, body: Lambda) -> Self {
        Lambda::Lletrec(bindings, Box::new(body))
    }

    /// Create a primitive operation
    pub fn prim(primitive: Primitive, args: Vec<Lambda>, loc: Location) -> Self {
        Lambda::Lprim(PrimInfo {
            primitive,
            args,
            loc,
        })
    }

    /// Create a sequence
    pub fn seq(first: Lambda, second: Lambda) -> Self {
        Lambda::Lsequence(Box::new(first), Box::new(second))
    }

    /// Create an if-then-else
    pub fn if_(cond: Lambda, then_: Lambda, else_: Lambda) -> Self {
        Lambda::LifThenElse(Box::new(cond), Box::new(then_), Box::new(else_))
    }

    /// Create a while loop
    pub fn while_(cond: Lambda, body: Lambda) -> Self {
        Lambda::Lwhile(Box::new(cond), Box::new(body))
    }

    /// Create a for loop
    pub fn for_(var: Ident, start: Lambda, end: Lambda, dir: DirectionFlag, body: Lambda) -> Self {
        Lambda::Lfor(var, Box::new(start), Box::new(end), dir, Box::new(body))
    }

    /// Create a try-with block
    pub fn try_(body: Lambda, exn: Ident, handler: Lambda) -> Self {
        Lambda::LtryWith(Box::new(body), exn, Box::new(handler))
    }

    /// Create an assignment
    pub fn assign(var: Ident, value: Lambda) -> Self {
        Lambda::Lassign(var, Box::new(value))
    }

    /// Create a static raise
    pub fn static_raise(id: i32, args: Vec<Lambda>) -> Self {
        Lambda::LstaticRaise(id, args)
    }

    /// Create a static catch
    pub fn static_catch(
        body: Lambda,
        handler_id: i32,
        params: Vec<Ident>,
        handler: Lambda,
    ) -> Self {
        Lambda::LstaticCatch(Box::new(body), (handler_id, params), Box::new(handler))
    }

    /// JS true constant
    pub fn true_() -> Self {
        Lambda::Lconst(Constant::JsTrue)
    }

    /// JS false constant
    pub fn false_() -> Self {
        Lambda::Lconst(Constant::JsFalse)
    }

    /// Unit constant
    pub fn unit() -> Self {
        Lambda::Lconst(Constant::JsUndefined { is_unit: true })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lambda_creation() {
        let id = Ident::create_local("x");
        let var = Lambda::var(id.clone());

        match var {
            Lambda::Lvar(ref i) => assert_eq!(i.name(), "x"),
            _ => panic!("Expected Lvar"),
        }
    }

    #[test]
    fn test_let_binding() {
        let x = Ident::create_local("x");
        let _y = Ident::create_local("y");

        let binding = Lambda::let_(
            LetKind::Strict,
            x.clone(),
            Lambda::const_(Constant::Int {
                i: 42,
                comment: None,
            }),
            Lambda::var(x.clone()),
        );

        match binding {
            Lambda::Llet(LetKind::Strict, ref id, _, _) => {
                assert_eq!(id.name(), "x");
            }
            _ => panic!("Expected Llet"),
        }
    }

    #[test]
    fn test_function() {
        let x = Ident::create_local("x");
        let func = Lambda::function_(
            1,
            vec![x.clone()],
            Lambda::var(x.clone()),
            FunctionAttribute::default(),
        );

        match func {
            Lambda::Lfunction(ref f) => {
                assert_eq!(f.arity, 1);
                assert_eq!(f.params.len(), 1);
            }
            _ => panic!("Expected Lfunction"),
        }
    }

    #[test]
    fn test_if_then_else() {
        let cond = Lambda::true_();
        let then_ = Lambda::const_(Constant::Int {
            i: 1,
            comment: None,
        });
        let else_ = Lambda::const_(Constant::Int {
            i: 0,
            comment: None,
        });

        let if_expr = Lambda::if_(cond, then_, else_);

        match if_expr {
            Lambda::LifThenElse(_, _, _) => {}
            _ => panic!("Expected LifThenElse"),
        }
    }

    #[test]
    fn test_while_loop() {
        let cond = Lambda::true_();
        let body = Lambda::unit();

        let while_loop = Lambda::while_(cond, body);

        match while_loop {
            Lambda::Lwhile(_, _) => {}
            _ => panic!("Expected Lwhile"),
        }
    }
}
