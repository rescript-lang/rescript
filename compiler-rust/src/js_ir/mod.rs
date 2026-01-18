//! JavaScript IR - Intermediate representation for JavaScript output.
//!
//! This module provides the JavaScript IR, which is the final intermediate
//! representation before generating JavaScript source code. It's a subset
//! of JavaScript AST specialized for the ReScript compilation target.
//!
//! # Architecture
//!
//! The JS IR is organized into:
//!
//! - [`op`] - Operators, properties, and other JS-specific types
//! - Expressions - Values that produce results
//! - Statements - Instructions that have effects
//! - Programs - Complete compilation units

pub mod op;
pub mod printer;

use crate::ident::Ident;
use crate::lambda::tag_info::TagInfo;
pub use op::{BinOp, IdentInfo, Kind, LengthObject, MutableFlag, Number, Property, PropertyName};
pub use printer::{JsPrinter, print_block, print_expression, print_program};

/// Module identifier for imports/requires
#[derive(Debug, Clone, PartialEq)]
pub struct ModuleId {
    /// The module identifier
    pub id: Ident,
    /// Module kind (Ml, Runtime, External)
    pub kind: Kind,
    /// Is this a dynamic import?
    pub dynamic_import: bool,
}

/// Variable identifier (local or qualified)
#[derive(Debug, Clone, PartialEq)]
pub enum VIdent {
    /// Local identifier
    Id(Ident),
    /// Qualified identifier (module.member)
    Qualified(ModuleId, Option<String>),
}

/// String delimiter type
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Delim {
    /// No special delimiter
    None,
    /// StarJ format
    StarJ,
    /// No quotes (raw)
    NoQuotes,
    /// Backtick template literal
    BackQuotes,
}

/// For loop direction
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ForDirection {
    /// i++
    Upto,
    /// i--
    Downto,
    /// for...of style
    Up,
}

/// Call information for function calls
#[derive(Debug, Clone, Default)]
pub struct CallInfo {
    /// Is this a method call?
    pub is_method: bool,
    /// Additional metadata
    pub comment: Option<String>,
}

/// Function environment (for closures)
#[derive(Debug, Clone, Default)]
pub struct FunEnv {
    /// Captured variables
    pub captured: Vec<Ident>,
}

/// Raw JavaScript code info
#[derive(Debug, Clone, PartialEq)]
pub struct JsRawInfo {
    /// The JavaScript code
    pub code: String,
    /// Is this a statement?
    pub is_stmt: bool,
}

/// JavaScript expression description
#[derive(Debug, Clone)]
pub enum ExpressionDesc {
    /// Length property access (arr.length, str.length)
    Length(Box<Expression>, LengthObject),

    /// Null/undefined check (== null)
    IsNullOrUndefined(Box<Expression>),

    /// String concatenation
    StringAppend(Box<Expression>, Box<Expression>),

    /// Boolean literal
    Bool(bool),

    /// typeof operator
    Typeof(Box<Expression>),

    /// in operator (prop in obj)
    In(Box<Expression>, Box<Expression>),

    /// Logical not (!)
    JsNot(Box<Expression>),

    /// Bitwise not (~)
    JsBnot(Box<Expression>),

    /// Sequence expression (a, b)
    Seq(Box<Expression>, Box<Expression>),

    /// Conditional expression (a ? b : c)
    Cond(Box<Expression>, Box<Expression>, Box<Expression>),

    /// Binary operation
    Bin(BinOp, Box<Expression>, Box<Expression>),

    /// Function.prototype.apply call
    FlatCall(Box<Expression>, Box<Expression>),

    /// Function call
    Call(Box<Expression>, Vec<Expression>, CallInfo),

    /// String index access (str[i])
    StringIndex(Box<Expression>, Box<Expression>),

    /// Array index access (arr[i])
    ArrayIndex(Box<Expression>, Box<Expression>),

    /// Tagged template literal
    TaggedTemplate(Box<Expression>, Vec<Expression>, Vec<Expression>),

    /// Static property access (obj.prop or obj["prop"])
    StaticIndex(Box<Expression>, String, Option<i32>),

    /// new expression
    New(Box<Expression>, Option<Vec<Expression>>),

    /// Variable reference
    Var(VIdent),

    /// Function expression
    Fun {
        is_method: bool,
        params: Vec<Ident>,
        body: Block,
        env: FunEnv,
        return_unit: bool,
        async_: bool,
        directive: Option<String>,
    },

    /// String literal
    Str { delim: Delim, txt: String },

    /// Raw JavaScript code
    RawJsCode(JsRawInfo),

    /// Array literal
    Array(Vec<Expression>, MutableFlag),

    /// Optional block (Some value)
    OptionalBlock(Box<Expression>, bool),

    /// Caml block (variant, record, tuple)
    CamlBlock(Vec<Expression>, MutableFlag, Box<Expression>, TagInfo),

    /// Block tag access
    CamlBlockTag(Box<Expression>, String),

    /// Number literal
    Number(Number),

    /// Object literal
    Object(Option<Box<Expression>>, PropertyMap),

    /// undefined literal
    Undefined { is_unit: bool },

    /// null literal
    Null,

    /// await expression
    Await(Box<Expression>),

    /// spread operator
    Spread(Box<Expression>),
}

/// Property map for object literals
pub type PropertyMap = Vec<(PropertyName, Expression)>;

/// JavaScript expression
#[derive(Debug, Clone)]
pub struct Expression {
    /// Expression description
    pub desc: ExpressionDesc,
    /// Optional comment
    pub comment: Option<String>,
}

impl Expression {
    /// Create a new expression
    pub fn new(desc: ExpressionDesc) -> Self {
        Self {
            desc,
            comment: None,
        }
    }

    /// Create a new expression with a comment
    pub fn with_comment(desc: ExpressionDesc, comment: impl Into<String>) -> Self {
        Self {
            desc,
            comment: Some(comment.into()),
        }
    }

    /// Create a boolean expression
    pub fn bool(value: bool) -> Self {
        Self::new(ExpressionDesc::Bool(value))
    }

    /// Create a variable expression
    pub fn var(id: Ident) -> Self {
        Self::new(ExpressionDesc::Var(VIdent::Id(id)))
    }

    /// Create a qualified variable expression
    pub fn qualified(module: ModuleId, member: Option<String>) -> Self {
        Self::new(ExpressionDesc::Var(VIdent::Qualified(module, member)))
    }

    /// Create a string expression
    pub fn string(txt: impl Into<String>) -> Self {
        Self::new(ExpressionDesc::Str {
            delim: Delim::None,
            txt: txt.into(),
        })
    }

    /// Create an undefined expression
    pub fn undefined(is_unit: bool) -> Self {
        Self::new(ExpressionDesc::Undefined { is_unit })
    }

    /// Create a null expression
    pub fn null() -> Self {
        Self::new(ExpressionDesc::Null)
    }

    /// Create a number expression
    pub fn number(n: Number) -> Self {
        Self::new(ExpressionDesc::Number(n))
    }

    /// Create an array expression
    pub fn array(elements: Vec<Expression>, mutable_flag: MutableFlag) -> Self {
        Self::new(ExpressionDesc::Array(elements, mutable_flag))
    }

    /// Create a binary operation
    pub fn bin(op: BinOp, left: Expression, right: Expression) -> Self {
        Self::new(ExpressionDesc::Bin(op, Box::new(left), Box::new(right)))
    }

    /// Create a function call
    pub fn call(func: Expression, args: Vec<Expression>, info: CallInfo) -> Self {
        Self::new(ExpressionDesc::Call(Box::new(func), args, info))
    }

    /// Create a conditional expression
    pub fn cond(test: Expression, consequent: Expression, alternate: Expression) -> Self {
        Self::new(ExpressionDesc::Cond(
            Box::new(test),
            Box::new(consequent),
            Box::new(alternate),
        ))
    }
}

/// Switch case clause
#[derive(Debug, Clone)]
pub struct CaseClause {
    /// Case body
    pub body: Block,
    /// Should break after this case?
    pub should_break: bool,
    /// Optional comment
    pub comment: Option<String>,
}

/// Integer switch clause
pub type IntClause = (i32, CaseClause);

/// String switch clause (tag type, case clause)
#[derive(Debug, Clone)]
pub struct StringClause {
    /// Tag type (from Ast_untagged_variants)
    pub tag_type: String,
    /// Case clause
    pub clause: CaseClause,
}

/// Variable declaration
#[derive(Debug, Clone)]
pub struct VariableDeclaration {
    /// Variable identifier
    pub ident: Ident,
    /// Initial value
    pub value: Option<Expression>,
    /// Declaration property (let, const, var semantics)
    pub property: Property,
    /// Usage info
    pub ident_info: IdentInfo,
}

/// JavaScript statement description
#[derive(Debug, Clone)]
pub enum StatementDesc {
    /// Block of statements
    Block(Block),

    /// Variable declaration
    Variable(VariableDeclaration),

    /// Expression statement
    Exp(Expression),

    /// If statement
    If(Expression, Block, Block),

    /// While loop
    While(Expression, Block),

    /// For loop (init, end, ident, direction, body)
    ForRange(Option<Expression>, Expression, Ident, ForDirection, Block),

    /// Continue statement
    Continue,

    /// Break statement
    Break,

    /// Return statement
    Return(Expression),

    /// Integer switch
    IntSwitch(Expression, Vec<IntClause>, Option<Block>),

    /// String switch
    StringSwitch(Expression, Vec<StringClause>, Option<Block>),

    /// Throw statement
    Throw(Expression),

    /// Try statement (body, catch, finally)
    Try(Block, Option<(Ident, Block)>, Option<Block>),

    /// Debugger statement
    Debugger,
}

/// JavaScript statement
#[derive(Debug, Clone)]
pub struct Statement {
    /// Statement description
    pub desc: StatementDesc,
    /// Optional comment
    pub comment: Option<String>,
}

impl Statement {
    /// Create a new statement
    pub fn new(desc: StatementDesc) -> Self {
        Self {
            desc,
            comment: None,
        }
    }

    /// Create a new statement with a comment
    pub fn with_comment(desc: StatementDesc, comment: impl Into<String>) -> Self {
        Self {
            desc,
            comment: Some(comment.into()),
        }
    }

    /// Create an expression statement
    pub fn exp(expr: Expression) -> Self {
        Self::new(StatementDesc::Exp(expr))
    }

    /// Create an if statement
    pub fn if_(cond: Expression, then_: Block, else_: Block) -> Self {
        Self::new(StatementDesc::If(cond, then_, else_))
    }

    /// Create a while loop
    pub fn while_(cond: Expression, body: Block) -> Self {
        Self::new(StatementDesc::While(cond, body))
    }

    /// Create a return statement
    pub fn return_(expr: Expression) -> Self {
        Self::new(StatementDesc::Return(expr))
    }

    /// Create a throw statement
    pub fn throw(expr: Expression) -> Self {
        Self::new(StatementDesc::Throw(expr))
    }

    /// Create a break statement
    pub fn break_() -> Self {
        Self::new(StatementDesc::Break)
    }

    /// Create a continue statement
    pub fn continue_() -> Self {
        Self::new(StatementDesc::Continue)
    }

    /// Create a debugger statement
    pub fn debugger() -> Self {
        Self::new(StatementDesc::Debugger)
    }

    /// Create a variable declaration
    pub fn variable(decl: VariableDeclaration) -> Self {
        Self::new(StatementDesc::Variable(decl))
    }
}

/// Block of statements
pub type Block = Vec<Statement>;

/// Exports from a module
pub type Exports = Vec<Ident>;

/// Required (imported) modules
pub type RequiredModules = Vec<ModuleId>;

/// JavaScript program
#[derive(Debug, Clone)]
pub struct Program {
    /// Program statements
    pub block: Block,
    /// Exported identifiers
    pub exports: Exports,
    /// Set of exported identifiers
    pub export_set: std::collections::HashSet<Ident>,
}

impl Program {
    /// Create a new program
    pub fn new(block: Block, exports: Exports) -> Self {
        let export_set = exports.iter().cloned().collect();
        Self {
            block,
            exports,
            export_set,
        }
    }

    /// Create an empty program
    pub fn empty() -> Self {
        Self {
            block: vec![],
            exports: vec![],
            export_set: std::collections::HashSet::new(),
        }
    }
}

/// Program with dependencies
#[derive(Debug, Clone)]
pub struct DepsProgram {
    /// The program
    pub program: Program,
    /// Required modules
    pub modules: RequiredModules,
    /// Side effect reason (None = no side effects)
    pub side_effect: Option<String>,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_expression_creation() {
        let expr = Expression::bool(true);
        match expr.desc {
            ExpressionDesc::Bool(true) => {}
            _ => panic!("Expected Bool(true)"),
        }
    }

    #[test]
    fn test_variable_expression() {
        let id = Ident::create_local("x");
        let expr = Expression::var(id.clone());
        match &expr.desc {
            ExpressionDesc::Var(VIdent::Id(i)) => {
                assert_eq!(i.name(), "x");
            }
            _ => panic!("Expected Var(Id)"),
        }
    }

    #[test]
    fn test_string_expression() {
        let expr = Expression::string("hello");
        match &expr.desc {
            ExpressionDesc::Str {
                delim: Delim::None,
                txt,
            } => {
                assert_eq!(txt, "hello");
            }
            _ => panic!("Expected Str"),
        }
    }

    #[test]
    fn test_binary_expression() {
        let left = Expression::number(Number::int(1));
        let right = Expression::number(Number::int(2));
        let expr = Expression::bin(BinOp::Plus, left, right);

        match &expr.desc {
            ExpressionDesc::Bin(BinOp::Plus, _, _) => {}
            _ => panic!("Expected Bin(Plus)"),
        }
    }

    #[test]
    fn test_statement_creation() {
        let stmt = Statement::break_();
        match stmt.desc {
            StatementDesc::Break => {}
            _ => panic!("Expected Break"),
        }
    }

    #[test]
    fn test_if_statement() {
        let cond = Expression::bool(true);
        let then_ = vec![Statement::break_()];
        let else_ = vec![];
        let stmt = Statement::if_(cond, then_, else_);

        match &stmt.desc {
            StatementDesc::If(_, then_block, else_block) => {
                assert_eq!(then_block.len(), 1);
                assert_eq!(else_block.len(), 0);
            }
            _ => panic!("Expected If"),
        }
    }

    #[test]
    fn test_program_creation() {
        let stmt = Statement::return_(Expression::undefined(true));
        let program = Program::new(vec![stmt], vec![]);

        assert_eq!(program.block.len(), 1);
        assert!(program.exports.is_empty());
    }
}
