//! Lambda compilation context.
//!
//! This module provides the compilation context used when transforming
//! Lambda IR to JavaScript IR. It tracks continuation, exception handlers,
//! and compilation metadata.

use std::collections::{HashMap, HashSet};

use crate::ident::Ident;
use crate::lambda::compat::LetKind;
use crate::lambda::Lambda;
use crate::lambda::arity::Arity;
use crate::lambda::constant::Constant;

/// Label for jump tables (exception handler labels).
pub type JblLabel = i32;

/// Recursion flag for functions.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RecFlag {
    /// Recursive function
    Rec,
    /// Non-recursive function
    NonRec,
    /// Self-recursive function (special case)
    SelfRec,
}

/// Element in a block.
#[derive(Debug, Clone)]
pub enum Element {
    /// No information available
    Na,
    /// Simple form - can be substituted
    SimpleForm(Lambda),
}

/// Nullable kind for optional values.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BoxedNullable {
    Undefined,
    Null,
    NullUndefined,
}

/// Information about an identifier's kind.
///
/// This tracks what kind of value an identifier is bound to,
/// which helps with optimization and code generation.
#[derive(Debug, Clone)]
pub enum IdKind {
    /// Optional value with normal null handling
    NormalOptional(Lambda),
    /// Optional block with nullable kind
    OptionalBlock(Lambda, BoxedNullable),
    /// Immutable block (tuple, record)
    ImmutableBlock(Vec<Element>),
    /// Mutable block
    MutableBlock(Vec<Element>),
    /// Constant value
    Constant(Constant),
    /// Module reference
    Module(Ident),
    /// Function with arity information
    FunctionId {
        arity: Arity,
        lambda: Option<(Lambda, RecFlag)>,
    },
    /// Exception identifier
    Exception,
    /// Function parameter
    Parameter,
    /// No specific information (default)
    Na,
}

impl Default for IdKind {
    fn default() -> Self {
        IdKind::Na
    }
}

/// Identifier table for tracking identifier kinds.
pub type IdentTable = HashMap<Ident, IdKind>;

/// Lambda compilation statistics and metadata.
#[derive(Debug, Clone)]
pub struct LamStats {
    /// Set of export identifiers
    pub export_idents: HashSet<Ident>,
    /// List of exports (in order)
    pub exports: Vec<Ident>,
    /// Identifier kind table
    pub ident_tbl: IdentTable,
}

impl LamStats {
    /// Create a new LamStats with the given exports.
    pub fn new(exports: Vec<Ident>) -> Self {
        let export_idents: HashSet<Ident> = exports.iter().cloned().collect();
        Self {
            export_idents,
            exports,
            ident_tbl: HashMap::new(),
        }
    }

    /// Check if an identifier is exported.
    pub fn is_export(&self, id: &Ident) -> bool {
        self.export_idents.contains(id)
    }

    /// Get the kind of an identifier.
    pub fn get_kind(&self, id: &Ident) -> Option<&IdKind> {
        self.ident_tbl.get(id)
    }

    /// Set the kind of an identifier.
    pub fn set_kind(&mut self, id: Ident, kind: IdKind) {
        self.ident_tbl.insert(id, kind);
    }
}

/// Return label for tail call optimization.
#[derive(Debug, Clone)]
pub struct ReturnLabel {
    /// Identifier for the return label
    pub id: Ident,
    /// Parameters for the return
    pub params: Vec<Ident>,
    /// Mask indicating which parameters are immutable
    pub immutable_mask: Vec<bool>,
    /// New parameter mappings (for renaming)
    pub new_params: HashMap<Ident, Ident>,
    /// Whether this label has been triggered
    pub triggered: bool,
}

impl ReturnLabel {
    /// Create a new return label.
    pub fn new(id: Ident, params: Vec<Ident>, immutable_mask: Vec<bool>) -> Self {
        Self {
            id,
            params,
            immutable_mask,
            new_params: HashMap::new(),
            triggered: false,
        }
    }
}

/// Tail position information.
#[derive(Debug, Clone)]
pub struct Tail {
    /// Optional return label for tail calls
    pub label: Option<ReturnLabel>,
    /// Whether we're inside a staticcatch
    pub in_staticcatch: bool,
}

impl Tail {
    /// Create a new tail position info.
    pub fn new(label: Option<ReturnLabel>, in_staticcatch: bool) -> Self {
        Self {
            label,
            in_staticcatch,
        }
    }

    /// Create tail info with no label.
    pub fn none() -> Self {
        Self {
            label: None,
            in_staticcatch: false,
        }
    }
}

/// Maybe tail - for exception handling context.
#[derive(Debug, Clone)]
pub enum MaybeTail {
    /// In a try block (not real tail position)
    TailInTry,
    /// With tail information
    TailWithName(Tail),
}

/// Tail type - whether we're in tail position.
#[derive(Debug, Clone)]
pub enum TailType {
    /// Not in tail position
    NotTail,
    /// Maybe in tail position (with context)
    MaybeTailIsReturn(MaybeTail),
}

impl TailType {
    /// Check if this might be a return position.
    pub fn is_maybe_return(&self) -> bool {
        matches!(self, TailType::MaybeTailIsReturn(_))
    }
}

/// Continuation - what to do with the result of an expression.
#[derive(Debug, Clone)]
pub enum Continuation {
    /// Expression is called for effect only
    EffectCall(TailType),
    /// Expression result is needed as a value
    NeedValue(TailType),
    /// Declare a new variable with the result
    Declare(LetKind, Ident),
    /// Assign the result to an existing variable
    Assign(Ident),
}

impl Continuation {
    /// Check if this continuation is a return.
    pub fn is_return(&self) -> bool {
        match self {
            Continuation::EffectCall(tt) | Continuation::NeedValue(tt) => tt.is_maybe_return(),
            Continuation::Declare(_, _) | Continuation::Assign(_) => false,
        }
    }
}

/// Value for jump table entries.
#[derive(Debug, Clone)]
pub struct JmpValue {
    /// Exit identifier
    pub exit_id: Ident,
    /// Bindings for this handler
    pub bindings: Vec<Ident>,
    /// Order ID for sorting
    pub order_id: i32,
}

/// Jump table - maps exception labels to handler info.
pub type JmpTable = HashMap<JblLabel, JmpValue>;

/// Exception handler definition.
#[derive(Debug, Clone)]
pub struct Handler {
    /// Handler label
    pub label: JblLabel,
    /// Handler body
    pub handler: Lambda,
    /// Bindings for this handler
    pub bindings: Vec<Ident>,
}

/// Lambda compilation context.
///
/// This is the main context used during Lambda to JS IR compilation.
/// It tracks the current continuation, exception handlers, and metadata.
#[derive(Debug, Clone)]
pub struct CompileContext {
    /// Current continuation
    pub continuation: Continuation,
    /// Jump table for exception handlers
    pub jmp_table: JmpTable,
    /// Compilation metadata
    pub meta: LamStats,
}

impl CompileContext {
    /// Create a new compilation context.
    pub fn new(continuation: Continuation, meta: LamStats) -> Self {
        Self {
            continuation,
            jmp_table: HashMap::new(),
            meta,
        }
    }

    /// Create a context for effect-only compilation.
    pub fn effect_call(meta: LamStats) -> Self {
        Self::new(Continuation::EffectCall(TailType::NotTail), meta)
    }

    /// Create a context for value-needed compilation.
    pub fn need_value(meta: LamStats) -> Self {
        Self::new(Continuation::NeedValue(TailType::NotTail), meta)
    }

    /// Find a handler by label.
    pub fn find_handler(&self, label: JblLabel) -> Option<&JmpValue> {
        self.jmp_table.get(&label)
    }

    /// Add handlers to the jump table.
    ///
    /// Returns the updated jump table and a list of (order_id, handler) pairs.
    pub fn add_handlers(
        &mut self,
        exit_id: Ident,
        handlers: Vec<Handler>,
    ) -> Vec<(i32, Lambda)> {
        let base_order = self.jmp_table.len() as i32 + 1;
        let mut result = Vec::new();

        for (i, h) in handlers.into_iter().enumerate() {
            let order_id = base_order + i as i32;
            self.jmp_table.insert(
                h.label,
                JmpValue {
                    exit_id: exit_id.clone(),
                    bindings: h.bindings,
                    order_id,
                },
            );
            result.push((order_id, h.handler));
        }

        result
    }

    /// Add a pseudo handler (for simplified cases).
    pub fn add_pseudo_handler(&mut self, exit_id: Ident, handler: Handler) -> Lambda {
        self.jmp_table.insert(
            handler.label,
            JmpValue {
                exit_id,
                bindings: handler.bindings,
                order_id: -1,
            },
        );
        handler.handler
    }

    /// Check if the continuation is a return.
    pub fn is_return(&self) -> bool {
        self.continuation.is_return()
    }

    /// Create a new context with a different continuation.
    pub fn with_continuation(&self, continuation: Continuation) -> Self {
        Self {
            continuation,
            jmp_table: self.jmp_table.clone(),
            meta: self.meta.clone(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lam_stats_creation() {
        let x = Ident::create_local("x");
        let y = Ident::create_local("y");
        let stats = LamStats::new(vec![x.clone(), y.clone()]);

        assert!(stats.is_export(&x));
        assert!(stats.is_export(&y));
        assert!(!stats.is_export(&Ident::create_local("z")));
    }

    #[test]
    fn test_id_kind() {
        let x = Ident::create_local("x");
        let mut stats = LamStats::new(vec![]);

        stats.set_kind(x.clone(), IdKind::Parameter);
        assert!(matches!(stats.get_kind(&x), Some(IdKind::Parameter)));

        stats.set_kind(
            x.clone(),
            IdKind::FunctionId {
                arity: Arity::info(vec![2], false),
                lambda: None,
            },
        );
        assert!(matches!(
            stats.get_kind(&x),
            Some(IdKind::FunctionId { .. })
        ));
    }

    #[test]
    fn test_continuation_is_return() {
        let effect_not_tail = Continuation::EffectCall(TailType::NotTail);
        assert!(!effect_not_tail.is_return());

        let effect_maybe = Continuation::EffectCall(TailType::MaybeTailIsReturn(
            MaybeTail::TailWithName(Tail::none()),
        ));
        assert!(effect_maybe.is_return());

        let declare = Continuation::Declare(LetKind::Strict, Ident::create_local("x"));
        assert!(!declare.is_return());
    }

    #[test]
    fn test_compile_context_creation() {
        let stats = LamStats::new(vec![]);
        let ctx = CompileContext::effect_call(stats.clone());

        assert!(matches!(
            ctx.continuation,
            Continuation::EffectCall(TailType::NotTail)
        ));
        assert!(ctx.jmp_table.is_empty());

        let ctx = CompileContext::need_value(stats);
        assert!(matches!(
            ctx.continuation,
            Continuation::NeedValue(TailType::NotTail)
        ));
    }

    #[test]
    fn test_add_handlers() {
        let stats = LamStats::new(vec![]);
        let mut ctx = CompileContext::effect_call(stats);

        let exit_id = Ident::create_local("exit");
        let x = Ident::create_local("x");

        let handlers = vec![
            Handler {
                label: 1,
                handler: Lambda::unit(),
                bindings: vec![x.clone()],
            },
            Handler {
                label: 2,
                handler: Lambda::true_(),
                bindings: vec![],
            },
        ];

        let result = ctx.add_handlers(exit_id.clone(), handlers);

        assert_eq!(result.len(), 2);
        assert_eq!(ctx.jmp_table.len(), 2);

        let h1 = ctx.find_handler(1).unwrap();
        assert_eq!(h1.exit_id, exit_id);
        assert_eq!(h1.bindings.len(), 1);
        assert!(h1.order_id > 0);
    }

    #[test]
    fn test_with_continuation() {
        let stats = LamStats::new(vec![]);
        let ctx = CompileContext::effect_call(stats);

        let new_ctx = ctx.with_continuation(Continuation::Assign(Ident::create_local("x")));

        assert!(matches!(new_ctx.continuation, Continuation::Assign(_)));
    }

    #[test]
    fn test_return_label() {
        let id = Ident::create_local("return");
        let params = vec![Ident::create_local("a"), Ident::create_local("b")];
        let mask = vec![true, false];

        let mut label = ReturnLabel::new(id.clone(), params.clone(), mask.clone());

        assert_eq!(label.id, id);
        assert_eq!(label.params.len(), 2);
        assert!(!label.triggered);

        label.triggered = true;
        assert!(label.triggered);
    }

    #[test]
    fn test_tail_type() {
        let not_tail = TailType::NotTail;
        assert!(!not_tail.is_maybe_return());

        let maybe_tail = TailType::MaybeTailIsReturn(MaybeTail::TailInTry);
        assert!(maybe_tail.is_maybe_return());
    }
}
