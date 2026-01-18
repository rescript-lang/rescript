//! Grammar rules for the ReScript parser.
//!
//! This module defines grammar rules that specify what tokens can start
//! and terminate various syntactic constructs. It mirrors `res_grammar.ml`.

use super::token::Token;

/// Grammar rules representing different parsing contexts.
///
/// These are used by the parser to determine what tokens are valid
/// in a given context, enabling error recovery and better diagnostics.
#[derive(Debug, Clone, PartialEq)]
pub enum Grammar {
    /// Open description: `open Belt`
    OpenDescription,
    /// Module long identifier: `Foo` or `Foo.Bar`
    ModuleLongIdent,
    /// Ternary expression: `condExpr ? trueExpr : falseExpr`
    Ternary,
    /// ES6 arrow function expression
    Es6ArrowExpr,
    /// JSX expression
    Jsx,
    /// JSX attribute
    JsxAttribute,
    /// JSX child
    JsxChild,
    /// Basic expression operand
    ExprOperand,
    /// Unary expression
    ExprUnary,
    /// Record field set expression
    ExprSetField,
    /// Expression after a binary operator
    ExprBinaryAfterOp(Token),
    /// Block with expressions
    ExprBlock,
    /// Function call expression
    ExprCall,
    /// Multiple expressions
    ExprList,
    /// Array access expression
    ExprArrayAccess,
    /// Array mutation expression
    ExprArrayMutation,
    /// If expression
    ExprIf,
    /// For expression
    ExprFor,
    /// Condition of an if expression
    IfCondition,
    /// True branch of an if expression
    IfBranch,
    /// Else branch of an if expression
    ElseBranch,
    /// Type expression
    TypeExpression,
    /// External declaration
    External,
    /// Pattern matching cases
    PatternMatching,
    /// Pattern match case
    PatternMatchCase,
    /// Let binding
    LetBinding,
    /// Multiple patterns
    PatternList,
    /// OCaml-style list pattern
    PatternOcamlList,
    /// Record pattern
    PatternRecord,
    /// Type definition
    TypeDef,
    /// Type constructor name
    TypeConstrName,
    /// Type parameters
    TypeParams,
    /// Single type parameter
    TypeParam,
    /// Package constraint
    PackageConstraint,
    /// Type representation
    TypeRepresentation,
    /// Record declaration
    RecordDecl,
    /// Constructor declaration
    ConstructorDeclaration,
    /// Parameter list
    ParameterList,
    /// String field declarations
    StringFieldDeclarations,
    /// Field declarations
    FieldDeclarations,
    /// List of type expressions
    TypExprList,
    /// Functor arguments
    FunctorArgs,
    /// List of module expressions
    ModExprList,
    /// List of type parameters
    TypeParameters,
    /// Rows of a record
    RecordRows,
    /// Rows of a record with string keys
    RecordRowsStringKey,
    /// Argument list
    ArgumentList,
    /// Signature
    Signature,
    /// Specification
    Specification,
    /// Structure
    Structure,
    /// Implementation
    Implementation,
    /// Attribute
    Attribute,
    /// Type constraint
    TypeConstraint,
    /// Atomic type expression
    AtomicTypExpr,
    /// OCaml-style list expression
    ListExpr,
    /// Pattern
    Pattern,
    /// Attribute payload
    AttributePayload,
    /// Tag names
    TagNames,
    /// Dict rows
    DictRows,
}

impl std::fmt::Display for Grammar {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Grammar::OpenDescription => "an open description",
            Grammar::ModuleLongIdent => "a module path",
            Grammar::Ternary => "a ternary expression",
            Grammar::Es6ArrowExpr => "an es6 arrow function",
            Grammar::Jsx => "a jsx expression",
            Grammar::JsxAttribute => "a jsx attribute",
            Grammar::ExprOperand => "a basic expression",
            Grammar::ExprUnary => "a unary expression",
            Grammar::ExprBinaryAfterOp(op) => {
                return write!(f, "an expression after the operator \"{}\"", op);
            }
            Grammar::ExprIf => "an if expression",
            Grammar::IfCondition => "the condition of an if expression",
            Grammar::IfBranch => "the true-branch of an if expression",
            Grammar::ElseBranch => "the else-branch of an if expression",
            Grammar::TypeExpression => "a type",
            Grammar::External => "an external",
            Grammar::PatternMatching => "the cases of a pattern match",
            Grammar::ExprBlock => "a block with expressions",
            Grammar::ExprSetField => "a record field mutation",
            Grammar::ExprCall => "a function application",
            Grammar::ExprArrayAccess => "an array access expression",
            Grammar::ExprArrayMutation => "an array mutation",
            Grammar::LetBinding => "a let binding",
            Grammar::TypeDef => "a type definition",
            Grammar::TypeParams => "type parameters",
            Grammar::TypeParam => "a type parameter",
            Grammar::TypeConstrName => "a type-constructor name",
            Grammar::TypeRepresentation => "a type representation",
            Grammar::RecordDecl => "a record declaration",
            Grammar::PatternMatchCase => "a pattern match case",
            Grammar::ConstructorDeclaration => "a constructor declaration",
            Grammar::ExprList => "multiple expressions",
            Grammar::PatternList => "multiple patterns",
            Grammar::PatternOcamlList => "a list pattern",
            Grammar::PatternRecord => "a record pattern",
            Grammar::ParameterList => "parameters",
            Grammar::StringFieldDeclarations => "string field declarations",
            Grammar::FieldDeclarations => "field declarations",
            Grammar::TypExprList => "list of types",
            Grammar::FunctorArgs => "functor arguments",
            Grammar::ModExprList => "list of module expressions",
            Grammar::TypeParameters => "list of type parameters",
            Grammar::RecordRows => "rows of a record",
            Grammar::RecordRowsStringKey => "rows of a record with string keys",
            Grammar::ArgumentList => "arguments",
            Grammar::Signature => "signature",
            Grammar::Specification => "specification",
            Grammar::Structure => "structure",
            Grammar::Implementation => "implementation",
            Grammar::Attribute => "an attribute",
            Grammar::TypeConstraint => "constraints on a type",
            Grammar::AtomicTypExpr => "a type",
            Grammar::ListExpr => "an ocaml list expr",
            Grammar::PackageConstraint => "a package constraint",
            Grammar::JsxChild => "jsx child",
            Grammar::Pattern => "pattern",
            Grammar::ExprFor => "a for expression",
            Grammar::AttributePayload => "an attribute payload",
            Grammar::TagNames => "tag names",
            Grammar::DictRows => "rows of a dict",
        };
        write!(f, "{}", s)
    }
}

/// Check if a token can start a signature item.
pub fn is_signature_item_start(token: &Token) -> bool {
    matches!(
        token,
        Token::At
            | Token::Let { .. }
            | Token::Typ
            | Token::External
            | Token::Exception
            | Token::Open
            | Token::Include
            | Token::Module
            | Token::AtAt
            | Token::PercentPercent
    )
}

/// Check if a token can start an atomic pattern.
pub fn is_atomic_pattern_start(token: &Token) -> bool {
    matches!(
        token,
        Token::Int { .. }
            | Token::String(_)
            | Token::Codepoint { .. }
            | Token::Backtick
            | Token::Lparen
            | Token::Lbracket
            | Token::Lbrace
            | Token::Underscore
            | Token::Lident(_)
            | Token::Uident(_)
            | Token::List
            | Token::Dict
            | Token::Exception
            | Token::Percent
    )
}

/// Check if a token can start an atomic expression.
pub fn is_atomic_expr_start(token: &Token) -> bool {
    matches!(
        token,
        Token::True
            | Token::False
            | Token::Int { .. }
            | Token::String(_)
            | Token::Float { .. }
            | Token::Codepoint { .. }
            | Token::Backtick
            | Token::Uident(_)
            | Token::Lident(_)
            | Token::Hash
            | Token::Lparen
            | Token::List
            | Token::Lbracket
            | Token::Lbrace
            | Token::LessThan
            | Token::Module
            | Token::Percent
            | Token::Forwardslash
            | Token::ForwardslashDot
            | Token::Dict
    )
}

/// Check if a token can start an atomic type expression.
pub fn is_atomic_typ_expr_start(token: &Token) -> bool {
    matches!(
        token,
        Token::SingleQuote
            | Token::Underscore
            | Token::Lparen
            | Token::Lbrace
            | Token::Uident(_)
            | Token::Lident(_)
            | Token::Percent
    )
}

/// Check if a token can start an expression.
pub fn is_expr_start(token: &Token) -> bool {
    matches!(
        token,
        Token::Assert
            | Token::At
            | Token::Await
            | Token::Backtick
            | Token::Bang
            | Token::Codepoint { .. }
            | Token::False
            | Token::Float { .. }
            | Token::For
            | Token::Hash
            | Token::If
            | Token::Int { .. }
            | Token::Lbrace
            | Token::Lbracket
            | Token::LessThan
            | Token::Lident(_)
            | Token::List
            | Token::Lparen
            | Token::Minus
            | Token::MinusDot
            | Token::Module
            | Token::Percent
            | Token::Plus
            | Token::PlusDot
            | Token::Bnot
            | Token::Bor
            | Token::Bxor
            | Token::Band
            | Token::String(_)
            | Token::Switch
            | Token::True
            | Token::Try
            | Token::Uident(_)
            | Token::Underscore // _ => doThings()
            | Token::While
            | Token::Forwardslash
            | Token::ForwardslashDot
            | Token::Dict
    )
}

/// Check if a token can start a block expression.
/// This determines whether consecutive expressions in a block can continue.
pub fn is_block_expr_start(token: &Token) -> bool {
    matches!(
        token,
        Token::Assert
            | Token::At
            | Token::Await
            | Token::Backtick
            | Token::Bang
            | Token::Codepoint { .. }
            | Token::Exception
            | Token::False
            | Token::Float { .. }
            | Token::For
            | Token::Forwardslash
            | Token::ForwardslashDot
            | Token::Hash
            | Token::If
            | Token::Int { .. }
            | Token::Lbrace
            | Token::Lbracket
            | Token::LessThan
            | Token::Let { .. }
            | Token::Lident(_)
            | Token::List
            | Token::Lparen
            | Token::Minus
            | Token::MinusDot
            | Token::Module
            | Token::Open
            | Token::Percent
            | Token::Plus
            | Token::PlusDot
            | Token::String(_)
            | Token::Switch
            | Token::True
            | Token::Try
            | Token::Uident(_)
            | Token::Underscore
            | Token::While
            | Token::Dict
    )
}

/// Check if a token can start a JSX attribute.
pub fn is_jsx_attribute_start(token: &Token) -> bool {
    matches!(token, Token::Lident(_) | Token::Question | Token::Lbrace)
}

/// Check if a token can start a structure item.
pub fn is_structure_item_start(token: &Token) -> bool {
    matches!(
        token,
        Token::Open
            | Token::Let { .. }
            | Token::Typ
            | Token::External
            | Token::Exception
            | Token::Include
            | Token::Module
            | Token::AtAt
            | Token::PercentPercent
            | Token::At
    ) || is_expr_start(token)
}

/// Check if a token can start a pattern.
pub fn is_pattern_start(token: &Token) -> bool {
    matches!(
        token,
        Token::Int { .. }
            | Token::Float { .. }
            | Token::String(_)
            | Token::Codepoint { .. }
            | Token::Backtick
            | Token::True
            | Token::False
            | Token::Minus
            | Token::Plus
            | Token::Lparen
            | Token::Lbracket
            | Token::Lbrace
            | Token::List
            | Token::Dict
            | Token::Underscore
            | Token::DotDotDot
            | Token::Lident(_)
            | Token::Uident(_)
            | Token::Hash
            | Token::Exception
            | Token::Percent
            | Token::Module
            | Token::At
    )
}

/// Check if a token can start a parameter.
pub fn is_parameter_start(token: &Token) -> bool {
    matches!(token, Token::Typ | Token::Tilde | Token::Dot) || is_pattern_start(token)
}

/// Check if a token can start a string field declaration.
pub fn is_string_field_decl_start(token: &Token) -> bool {
    matches!(
        token,
        Token::String(_) | Token::Lident(_) | Token::At | Token::DotDotDot
    )
}

/// Check if a token can start a field declaration.
pub fn is_field_decl_start(token: &Token) -> bool {
    matches!(
        token,
        Token::At | Token::Mutable | Token::Lident(_) | Token::Uident(_)
    ) || token.is_keyword()
}

/// Check if a token can start a record declaration.
pub fn is_record_decl_start(token: &Token) -> bool {
    matches!(
        token,
        Token::At | Token::Mutable | Token::Lident(_) | Token::DotDotDot
    )
}

/// Check if a token can start a type expression.
pub fn is_typ_expr_start(token: &Token) -> bool {
    matches!(
        token,
        Token::At
            | Token::SingleQuote
            | Token::Underscore
            | Token::Lparen
            | Token::Lbracket
            | Token::Uident(_)
            | Token::Lident(_)
            | Token::Module
            | Token::Percent
            | Token::Lbrace
    )
}

/// Check if a token can start a type parameter.
pub fn is_type_parameter_start(token: &Token) -> bool {
    matches!(token, Token::Tilde | Token::Dot) || is_typ_expr_start(token)
}

/// Check if a token can start a type param (in type definition).
pub fn is_type_param_start(token: &Token) -> bool {
    matches!(
        token,
        Token::Plus | Token::Minus | Token::SingleQuote | Token::Underscore
    )
}

/// Check if a token can start a functor argument.
pub fn is_functor_arg_start(token: &Token) -> bool {
    matches!(
        token,
        Token::At
            | Token::Uident(_)
            | Token::Underscore
            | Token::Percent
            | Token::Lbrace
            | Token::Lparen
    )
}

/// Check if a token can start a module expression.
pub fn is_mod_expr_start(token: &Token) -> bool {
    matches!(
        token,
        Token::At
            | Token::Percent
            | Token::Uident(_)
            | Token::Lbrace
            | Token::Lparen
            | Token::Await
    ) || matches!(token, Token::Lident(s) if s == "unpack")
}

/// Check if a token can start a dict row.
pub fn is_dict_row_start(token: &Token) -> bool {
    matches!(token, Token::String(_))
}

/// Check if a token can start a record row.
pub fn is_record_row_start(token: &Token) -> bool {
    matches!(
        token,
        Token::DotDotDot | Token::Uident(_) | Token::Lident(_)
    ) || token.is_keyword()
}

/// Check if a token can start a record row with string key.
pub fn is_record_row_string_key_start(token: &Token) -> bool {
    matches!(token, Token::String(_))
}

/// Check if a token can start an argument.
pub fn is_argument_start(token: &Token) -> bool {
    matches!(token, Token::Tilde | Token::Dot | Token::Underscore) || is_expr_start(token)
}

/// Check if a token can start a pattern match.
pub fn is_pattern_match_start(token: &Token) -> bool {
    matches!(token, Token::Bar) || is_pattern_start(token)
}

/// Check if a token can start an OCaml list pattern.
pub fn is_pattern_ocaml_list_start(token: &Token) -> bool {
    matches!(token, Token::DotDotDot) || is_pattern_start(token)
}

/// Check if a token can start a pattern record item.
pub fn is_pattern_record_item_start(token: &Token) -> bool {
    matches!(
        token,
        Token::DotDotDot | Token::Uident(_) | Token::Lident(_) | Token::Underscore
    )
}

/// Check if a token can start an attribute.
pub fn is_attribute_start(token: &Token) -> bool {
    matches!(token, Token::At)
}

/// Check if a token can start a JSX child.
pub fn is_jsx_child_start(token: &Token) -> bool {
    is_atomic_expr_start(token)
}

/// Check if a token is a valid list element in the given grammar context.
pub fn is_list_element(grammar: &Grammar, token: &Token) -> bool {
    match grammar {
        Grammar::ExprList => *token == Token::DotDotDot || is_expr_start(token),
        Grammar::ListExpr => *token == Token::DotDotDot || is_expr_start(token),
        Grammar::PatternList => *token == Token::DotDotDot || is_pattern_start(token),
        Grammar::ParameterList => is_parameter_start(token),
        Grammar::StringFieldDeclarations => is_string_field_decl_start(token),
        Grammar::FieldDeclarations => is_field_decl_start(token),
        Grammar::RecordDecl => is_record_decl_start(token),
        Grammar::TypExprList => is_typ_expr_start(token) || *token == Token::LessThan,
        Grammar::TypeParams => is_type_param_start(token),
        Grammar::FunctorArgs => is_functor_arg_start(token),
        Grammar::ModExprList => is_mod_expr_start(token),
        Grammar::TypeParameters => is_type_parameter_start(token),
        Grammar::DictRows => is_dict_row_start(token),
        Grammar::RecordRows => is_record_row_start(token),
        Grammar::RecordRowsStringKey => is_record_row_string_key_start(token),
        Grammar::ArgumentList => is_argument_start(token),
        Grammar::Signature | Grammar::Specification => is_signature_item_start(token),
        Grammar::Structure | Grammar::Implementation => is_structure_item_start(token),
        Grammar::PatternMatching => is_pattern_match_start(token),
        Grammar::PatternOcamlList => is_pattern_ocaml_list_start(token),
        Grammar::PatternRecord => is_pattern_record_item_start(token),
        Grammar::Attribute => is_attribute_start(token),
        Grammar::TypeConstraint => *token == Token::Constraint,
        Grammar::PackageConstraint => *token == Token::And,
        Grammar::ConstructorDeclaration => *token == Token::Bar,
        Grammar::JsxAttribute => is_jsx_attribute_start(token),
        Grammar::AttributePayload => *token == Token::Lparen,
        Grammar::TagNames => *token == Token::Hash,
        _ => false,
    }
}

/// Check if a token terminates a list in the given grammar context.
pub fn is_list_terminator(grammar: &Grammar, token: &Token) -> bool {
    match (grammar, token) {
        (_, Token::Eof) => true,
        (Grammar::ExprList, Token::Rparen | Token::Forwardslash | Token::Rbracket) => true,
        (Grammar::ListExpr, Token::Rparen) => true,
        (Grammar::ArgumentList, Token::Rparen | Token::DotDotDot) => true,
        (
            Grammar::TypExprList,
            Token::Rparen | Token::Forwardslash | Token::GreaterThan | Token::Equal,
        ) => true,
        (Grammar::ModExprList, Token::Rparen) => true,
        (
            Grammar::PatternList | Grammar::PatternOcamlList | Grammar::PatternRecord,
            Token::Forwardslash
            | Token::Rbracket
            | Token::Rparen
            | Token::EqualGreater // pattern matching =>
            | Token::In           // for expressions
            | Token::Equal,       // let {x} = foo
        ) => true,
        (Grammar::ExprBlock, Token::Rbrace) => true,
        (Grammar::Structure | Grammar::Signature, Token::Rbrace) => true,
        (Grammar::TypeParams, Token::Rparen) => true,
        (Grammar::ParameterList, Token::EqualGreater | Token::Lbrace) => true,
        (Grammar::JsxAttribute, Token::Forwardslash | Token::GreaterThan) => true,
        (Grammar::StringFieldDeclarations, Token::Rbrace) => true,
        (Grammar::Attribute, t) if *t != Token::At => true,
        (Grammar::TypeConstraint, t) if *t != Token::Constraint => true,
        (Grammar::PackageConstraint, t) if *t != Token::And => true,
        (Grammar::ConstructorDeclaration, t) if *t != Token::Bar => true,
        (Grammar::AttributePayload, Token::Rparen) => true,
        (Grammar::TagNames, Token::Rbracket) => true,
        _ => false,
    }
}

/// Check if a token is part of a list (either an element or terminator).
pub fn is_part_of_list(grammar: &Grammar, token: &Token) -> bool {
    is_list_element(grammar, token) || is_list_terminator(grammar, token)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_grammar_to_string() {
        assert_eq!(Grammar::OpenDescription.to_string(), "an open description");
        assert_eq!(Grammar::Jsx.to_string(), "a jsx expression");
        assert_eq!(
            Grammar::ExprBinaryAfterOp(Token::Plus).to_string(),
            "an expression after the operator \"+\""
        );
    }

    #[test]
    fn test_is_expr_start() {
        assert!(is_expr_start(&Token::If));
        assert!(is_expr_start(&Token::True));
        assert!(is_expr_start(&Token::Int {
            i: "42".to_string(),
            suffix: None
        }));
        assert!(is_expr_start(&Token::Lident("foo".to_string())));
        assert!(!is_expr_start(&Token::Rparen));
        assert!(!is_expr_start(&Token::Comma));
    }

    #[test]
    fn test_is_pattern_start() {
        assert!(is_pattern_start(&Token::Underscore));
        assert!(is_pattern_start(&Token::True));
        assert!(is_pattern_start(&Token::Lbrace));
        assert!(is_pattern_start(&Token::DotDotDot));
        assert!(!is_pattern_start(&Token::Rparen));
    }

    #[test]
    fn test_is_signature_item_start() {
        assert!(is_signature_item_start(&Token::Let { unwrap: false }));
        assert!(is_signature_item_start(&Token::Module));
        assert!(is_signature_item_start(&Token::External));
        assert!(!is_signature_item_start(&Token::Plus));
    }

    #[test]
    fn test_is_list_element() {
        assert!(is_list_element(&Grammar::ExprList, &Token::If));
        assert!(is_list_element(&Grammar::ExprList, &Token::DotDotDot));
        assert!(!is_list_element(&Grammar::ExprList, &Token::Rparen));

        assert!(is_list_element(&Grammar::PatternList, &Token::Underscore));
        assert!(is_list_element(&Grammar::TypeParams, &Token::SingleQuote));
    }

    #[test]
    fn test_is_list_terminator() {
        assert!(is_list_terminator(&Grammar::ExprList, &Token::Rparen));
        assert!(is_list_terminator(&Grammar::ExprList, &Token::Eof));
        assert!(!is_list_terminator(&Grammar::ExprList, &Token::Plus));

        assert!(is_list_terminator(&Grammar::ExprBlock, &Token::Rbrace));
        assert!(is_list_terminator(
            &Grammar::PatternList,
            &Token::EqualGreater
        ));
    }

    #[test]
    fn test_is_part_of_list() {
        // Element
        assert!(is_part_of_list(&Grammar::ExprList, &Token::If));
        // Terminator
        assert!(is_part_of_list(&Grammar::ExprList, &Token::Rparen));
        // Neither
        assert!(!is_part_of_list(
            &Grammar::FieldDeclarations,
            &Token::Rparen
        ));
    }

    #[test]
    fn test_grammar_is_send_sync() {
        fn assert_send_sync<T: Send + Sync>() {}
        assert_send_sync::<Grammar>();
    }
}
