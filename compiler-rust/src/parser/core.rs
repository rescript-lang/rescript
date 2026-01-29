//! Core parsing logic for ReScript.
//!
//! This module contains the main parsing functions that convert a token
//! stream into an AST. It mirrors `res_core.ml` from the OCaml implementation.

use crate::location::Position;
use crate::parse_arena::{Located, LocIdx};

use super::ast::*;
use super::diagnostics::DiagnosticCategory;
use super::grammar;
use super::longident::Longident;
use super::state::Parser;
use super::token::Token;

// ============================================================================
// Location Helpers
// ============================================================================

/// Create a located value with the given location.
pub fn with_loc<T>(txt: T, loc: LocIdx) -> Loc<T> {
    Loc { txt, loc }
}

/// Create a located value with no location (ghost).
/// Uses the pre-allocated "none" location at index 0.
pub fn mknoloc<T>(txt: T) -> Loc<T> {
    Loc {
        txt,
        loc: LocIdx::none(),
    }
}

// ============================================================================
// Error Recovery
// ============================================================================

/// Error recovery module - provides default AST nodes for error recovery.
pub mod recover {
    use super::*;

    /// Create a default expression for error recovery.
    pub fn default_expr() -> Expression {
        let id = mknoloc("rescript.exprhole".to_string());
        Expression {
            pexp_desc: ExpressionDesc::Pexp_extension((id, Payload::PStr(vec![]))),
            pexp_loc: LocIdx::none(),
            pexp_attributes: vec![],
        }
    }

    /// Create a default type for error recovery.
    pub fn default_type() -> CoreType {
        let id = mknoloc("rescript.typehole".to_string());
        CoreType {
            ptyp_desc: CoreTypeDesc::Ptyp_extension((id, Payload::PStr(vec![]))),
            ptyp_loc: LocIdx::none(),
            ptyp_attributes: vec![],
        }
    }

    /// Create a default pattern for error recovery.
    pub fn default_pattern() -> Pattern {
        let id = mknoloc("rescript.patternhole".to_string());
        Pattern {
            ppat_desc: PatternDesc::Ppat_extension((id, Payload::PStr(vec![]))),
            ppat_loc: LocIdx::none(),
            ppat_attributes: vec![],
        }
    }

    /// Create a default module expression for error recovery.
    pub fn default_module_expr() -> ModuleExpr {
        ModuleExpr {
            pmod_desc: ModuleExprDesc::Pmod_structure(vec![]),
            pmod_loc: LocIdx::none(),
            pmod_attributes: vec![],
        }
    }

    /// Create a default module type for error recovery.
    pub fn default_module_type() -> ModuleType {
        ModuleType {
            pmty_desc: ModuleTypeDesc::Pmty_signature(vec![]),
            pmty_loc: LocIdx::none(),
            pmty_attributes: vec![],
        }
    }

    /// Recover from missing `=>` by also accepting `->`.
    pub fn recover_equal_greater(p: &mut Parser<'_>) {
        p.expect(Token::EqualGreater);
        if p.token == Token::MinusGreater {
            p.next();
        }
    }

    /// Check if list parsing should abort based on breadcrumbs.
    pub fn should_abort_list_parse(p: &Parser<'_>) -> bool {
        for (g, _) in p.breadcrumbs() {
            if grammar::is_part_of_list(g, &p.token) {
                return true;
            }
        }
        false
    }
}

// ============================================================================
// Error Messages
// ============================================================================

/// Common error messages for parsing.
pub mod error_messages {
    /// Error message for list pattern spread.
    pub const LIST_PATTERN_SPREAD: &str = "List pattern matches only supports one `...` spread, at the end.\n\
         Explanation: a list spread at the tail is efficient, but a spread in the \
         middle would create new lists; out of performance concern, our pattern \
         matching currently guarantees to never create new intermediate data.";

    /// Error message for record pattern spread.
    pub const RECORD_PATTERN_SPREAD: &str = "Record spread (`...`) is not supported in pattern matches.\n\
         Explanation: you can't collect a subset of a record's field into its own \
         record, since a record needs an explicit declaration and that subset \
         wouldn't have one.\n\
         Solution: you need to pull out each field you want explicitly.";

    /// Error message for array pattern spread.
    pub const ARRAY_PATTERN_SPREAD: &str = "Array spread (`...`) is not supported in pattern matches.\n\n\
         Explanation: Allowing `...` here would require creating a new subarray at \
         match time, but for performance reasons pattern matching is guaranteed to \
         never create intermediate data.\n\n\
         Possible solutions:\n\
         - To validate specific elements: Use `if` with length checks and `Array.get`\n\
         - To extract a subarray: Use `Array.slice`";

    /// Error message for record expression spread.
    pub const RECORD_EXPR_SPREAD: &str = "Records can only have one `...` spread, at the beginning.\n\
         Explanation: since records have a known, fixed shape, a spread like `{a, \
         ...b}` wouldn't make sense, as `b` would override every field of `a` \
         anyway.";

    /// Error message for dict expression spread.
    pub const DICT_EXPR_SPREAD: &str = "Dict literals do not support spread (`...`) yet.";

    /// Error message for single element tuple.
    pub const TUPLE_SINGLE_ELEMENT: &str = "A tuple needs at least two elements";

    /// Error message for string interpolation in pattern.
    pub const STRING_INTERPOLATION_IN_PATTERN: &str =
        "String interpolation is not supported in pattern matching.";

    /// Error message for type definition in function.
    pub const TYPE_DEFINITION_IN_FUNCTION: &str = "Type definitions are not allowed inside functions.\n\
         Move this `type` declaration to the top level or into a module.";

    /// Error message for type param.
    pub const TYPE_PARAM: &str =
        "A type param consists of a singlequote followed by a name like `'a` or `'A`";

    /// Error message for type var.
    pub const TYPE_VAR: &str =
        "A type variable consists of a singlequote followed by a name like `'a` or `'A`";

    /// Error message for record field missing colon.
    pub const RECORD_FIELD_MISSING_COLON: &str =
        "Records use `:` when assigning fields. Example: `{field: value}`";

    /// Error message for labelled argument missing equal.
    pub const LABELLED_ARGUMENT_MISSING_EQUAL: &str =
        "Use `=` to pass a labelled argument. Example: `~label=value`";

    /// Generate error message for missing tilde on labeled parameter.
    pub fn missing_tilde_labeled_parameter(name: &str) -> String {
        if name.is_empty() {
            "A labeled parameter starts with a `~`.".to_string()
        } else {
            format!(
                "A labeled parameter starts with a `~`. Did you mean: `~{}`?",
                name
            )
        }
    }

    /// Generate error message for attribute without node.
    pub fn attribute_without_node(attr_name: &str) -> String {
        format!(
            "Did you forget to attach `{}` to an item?\n  \
             Standalone attributes start with `@@` like: `@@{}`",
            attr_name, attr_name
        )
    }
}

// ============================================================================
// Special Attributes
// ============================================================================

/// Create the ternary attribute `@res.ternary`.
pub fn ternary_attr() -> Attribute {
    (mknoloc("res.ternary".to_string()), Payload::PStr(vec![]))
}

/// Create the if-let attribute `@res.iflet`.
pub fn if_let_attr() -> Attribute {
    (mknoloc("res.iflet".to_string()), Payload::PStr(vec![]))
}

/// Create the await attribute `@res.await` with location.
pub fn make_await_attr(loc: Location) -> Attribute {
    (
        with_loc("res.await".to_string(), loc),
        Payload::PStr(vec![]),
    )
}

/// Create the braces attribute `@res.braces` with location.
pub fn make_braces_attr(loc: Location) -> Attribute {
    (
        with_loc("res.braces".to_string(), loc),
        Payload::PStr(vec![]),
    )
}

/// Create the template literal attribute `@res.template`.
pub fn template_literal_attr() -> Attribute {
    (mknoloc("res.template".to_string()), Payload::PStr(vec![]))
}

/// Create the spread attribute `@res.spread`.
pub fn spread_attr() -> Attribute {
    (mknoloc("res.spread".to_string()), Payload::PStr(vec![]))
}

/// Create the pattern variant spread attribute.
pub fn make_pat_variant_spread_attr() -> Attribute {
    (
        mknoloc("res.patVariantSpread".to_string()),
        Payload::PStr(vec![]),
    )
}

/// Create the tagged template literal attribute.
pub fn tagged_template_literal_attr() -> Attribute {
    (
        mknoloc("res.taggedTemplate".to_string()),
        Payload::PStr(vec![]),
    )
}

// ============================================================================
// Parsing Context
// ============================================================================

/// Expression parsing context.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum ExprContext {
    /// Ordinary expression.
    #[default]
    Ordinary,
    /// True branch of ternary.
    TernaryTrueBranch,
    /// When expression in pattern match.
    When,
    /// Switch case right-hand side (stop at `|`).
    SwitchCaseRhs,
}

// ============================================================================
// Token Utilities
// ============================================================================

/// Get the closing token for an opening token.
pub fn get_closing_token(token: &Token) -> Token {
    match token {
        Token::Lparen => Token::Rparen,
        Token::Lbrace => Token::Rbrace,
        Token::Lbracket => Token::Rbracket,
        Token::List => Token::Rbrace,
        Token::Dict => Token::Rbrace,
        Token::LessThan => Token::GreaterThan,
        _ => panic!("get_closing_token: unexpected token {:?}", token),
    }
}

/// Skip to the closing token, handling nested brackets.
pub fn go_to_closing(closing_token: &Token, p: &mut Parser<'_>) {
    loop {
        match (&p.token, closing_token) {
            (Token::Rparen, Token::Rparen)
            | (Token::Rbrace, Token::Rbrace)
            | (Token::Rbracket, Token::Rbracket)
            | (Token::GreaterThan, Token::GreaterThan) => {
                p.next();
                return;
            }
            (
                Token::Lbracket
                | Token::Lparen
                | Token::Lbrace
                | Token::List
                | Token::Dict
                | Token::LessThan,
                _,
            ) => {
                let opening = p.token.clone();
                p.next();
                go_to_closing(&get_closing_token(&opening), p);
            }
            (Token::Rparen | Token::Rbrace | Token::Rbracket | Token::Eof, _) => {
                return;
            }
            _ => {
                p.next();
            }
        }
    }
}

/// Skip doc comments at current position.
pub fn skip_doc_comments(p: &mut Parser<'_>) {
    while matches!(p.token, Token::DocComment { .. }) {
        p.next();
    }
}

/// Convert a doc comment to a res.doc attribute.
pub fn doc_comment_to_attribute(loc: Location, content: String) -> Attribute {
    let name = with_loc("res.doc".to_string(), loc.clone());
    let payload = Payload::PStr(vec![StructureItem {
        pstr_desc: StructureItemDesc::Pstr_eval(
            Expression {
                pexp_desc: ExpressionDesc::Pexp_constant(Constant::String(
                    content,
                    None,
                )),
                pexp_loc: loc.clone(),
                pexp_attributes: vec![],
            },
            vec![],
        ),
        pstr_loc: loc,
    }]);
    (name, payload)
}

// ============================================================================
// Longident Building
// ============================================================================

/// Build a Longident from a list of strings (in order).
pub fn build_longident(words: &[String]) -> Longident {
    let mut iter = words.iter();
    let first = iter.next().expect("build_longident: empty words");
    let mut result = Longident::Lident(first.clone());
    for word in iter {
        result = Longident::Ldot(Box::new(result), word.clone());
    }
    result
}

/// Get the last component of a longident.
pub fn lident_of_path(longident: &Longident) -> String {
    longident.last().to_string()
}

// ============================================================================
// AST Helpers
// ============================================================================

/// Helper module for constructing AST nodes.
pub mod ast_helper {
    use super::*;

    /// Create an expression with the given description and location.
    pub fn make_expr(desc: ExpressionDesc, loc: Location) -> Expression {
        Expression {
            pexp_desc: desc,
            pexp_loc: loc,
            pexp_attributes: vec![],
        }
    }

    /// Create a pattern with the given description and location.
    pub fn make_pat(desc: PatternDesc, loc: Location) -> Pattern {
        Pattern {
            ppat_desc: desc,
            ppat_loc: loc,
            ppat_attributes: vec![],
        }
    }

    /// Create a type with the given description and location.
    pub fn make_typ(desc: CoreTypeDesc, loc: Location) -> CoreType {
        CoreType {
            ptyp_desc: desc,
            ptyp_loc: loc,
            ptyp_attributes: vec![],
        }
    }

    /// Create an identifier expression.
    pub fn make_ident(lid: Longident, loc: Location) -> Expression {
        make_expr(ExpressionDesc::Pexp_ident(with_loc(lid, loc.clone())), loc)
    }

    /// Create a constant expression.
    pub fn make_constant(constant: Constant, loc: Location) -> Expression {
        make_expr(ExpressionDesc::Pexp_constant(constant), loc)
    }

    /// Create a unit expression: ()
    pub fn make_unit(loc: Location) -> Expression {
        make_expr(
            ExpressionDesc::Pexp_construct(
                with_loc(Longident::Lident("()".to_string()), loc.clone()),
                None,
            ),
            loc,
        )
    }

    /// Create an apply expression.
    pub fn make_apply(
        func: Expression,
        args: Vec<(ArgLabel, Expression)>,
        loc: Location,
    ) -> Expression {
        make_expr(
            ExpressionDesc::Pexp_apply {
                funct: Box::new(func),
                args,
                partial: false,
                transformed_jsx: false,
            },
            loc,
        )
    }

    /// Create a partial apply expression.
    pub fn make_apply_partial(
        func: Expression,
        args: Vec<(ArgLabel, Expression)>,
        loc: Location,
        partial: bool,
    ) -> Expression {
        make_expr(
            ExpressionDesc::Pexp_apply {
                funct: Box::new(func),
                args,
                partial,
                transformed_jsx: false,
            },
            loc,
        )
    }

    /// Create a let expression.
    pub fn make_let(
        rec_flag: RecFlag,
        bindings: Vec<ValueBinding>,
        body: Expression,
        loc: Location,
    ) -> Expression {
        make_expr(
            ExpressionDesc::Pexp_let(rec_flag, bindings, Box::new(body)),
            loc,
        )
    }

    /// Create a variable pattern.
    pub fn make_var_pat(name: String, loc: Location) -> Pattern {
        make_pat(PatternDesc::Ppat_var(with_loc(name, loc.clone())), loc)
    }

    /// Create a constructor pattern.
    ///
    /// Note: We use separate locations for lid_loc and ppat_loc:
    /// - lid_loc: Just the constructor name (e.g., `Some` at columns 28-32)
    /// - ppat_loc: Full pattern extent (e.g., `Some(y)` at columns 28-35)
    pub fn make_construct_pat(
        lid: Longident,
        arg: Option<Pattern>,
        lid_loc: Location,
        ppat_loc: Location,
    ) -> Pattern {
        make_pat(
            PatternDesc::Ppat_construct(with_loc(lid, lid_loc), arg.map(Box::new)),
            ppat_loc,
        )
    }

    /// Create a list pattern `[a, b, ...rest]`.
    pub fn make_list_pattern(
        p: &mut Parser<'_>,
        loc: Location,
        items: Vec<Pattern>,
        spread: Option<Pattern>,
    ) -> Pattern {
        // Build from the end: start with spread or []
        let mut result = match spread {
            Some(pat) => pat,
            None => {
                // Use ghost location for the empty list constructor
                let nil_loc = LocIdx::none();
                let nil = with_loc(Longident::Lident("[]".to_string()), nil_loc);
                make_pat(PatternDesc::Ppat_construct(nil, None), nil_loc)
            }
        };

        // Build :: chain from the end
        for item in items.into_iter().rev() {
            let tuple_loc = p.mk_loc(&p.loc_start(item.ppat_loc), &p.loc_end(result.ppat_loc));
            let cons_loc = tuple_loc;
            let tuple = make_pat(PatternDesc::Ppat_tuple(vec![item, result]), tuple_loc);
            let cons = with_loc(Longident::Lident("::".to_string()), cons_loc);
            result = make_pat(
                PatternDesc::Ppat_construct(cons, Some(Box::new(tuple))),
                cons_loc,
            );
        }

        // The outermost pattern uses the full list{...} location
        result.ppat_loc = loc;
        result
    }

    /// Create a type constructor.
    ///
    /// Note: We use separate locations for lid and ptyp_loc:
    /// - lid_loc: just the type constructor identifier
    /// - ptyp_loc: full extent including type arguments
    /// This matches OCaml's behavior.
    pub fn make_type_constr(
        lid: Longident,
        args: Vec<CoreType>,
        lid_loc: Location,
        ptyp_loc: Location,
    ) -> CoreType {
        make_typ(CoreTypeDesc::Ptyp_constr(with_loc(lid, lid_loc), args), ptyp_loc)
    }
}

// ============================================================================
// Unary Expression Helpers
// ============================================================================

/// Negate a string number.
fn negate_string(s: &str) -> String {
    if let Some(stripped) = s.strip_prefix('-') {
        stripped.to_string()
    } else {
        format!("-{}", s)
    }
}

/// Create a unary expression from an operator token and operand.
pub fn make_unary_expr(
    p: &mut Parser<'_>,
    start_pos: Position,
    token_end: Position,
    token: Token,
    operand: Expression,
) -> Expression {
    // Handle numeric literal negation specially
    match (&token, &operand.pexp_desc) {
        (Token::Plus | Token::PlusDot, ExpressionDesc::Pexp_constant(Constant::Integer(..)))
        | (Token::Plus | Token::PlusDot, ExpressionDesc::Pexp_constant(Constant::Float(..))) => {
            // +n is just n
            return operand;
        }
        (Token::Minus, ExpressionDesc::Pexp_constant(Constant::Integer(n, suffix))) => {
            return Expression {
                pexp_desc: ExpressionDesc::Pexp_constant(Constant::Integer(
                    negate_string(n),
                    *suffix,
                )),
                ..operand
            };
        }
        (
            Token::Minus | Token::MinusDot,
            ExpressionDesc::Pexp_constant(Constant::Float(n, suffix)),
        ) => {
            return Expression {
                pexp_desc: ExpressionDesc::Pexp_constant(Constant::Float(
                    negate_string(n),
                    *suffix,
                )),
                ..operand
            };
        }
        _ => {}
    }

    // General unary operator application
    let token_loc = p.mk_loc(&start_pos, &token_end);
    let token_string = token.to_string();
    let operator = if matches!(token, Token::Question) {
        "?".to_string()
    } else if token_string.starts_with('~') {
        token_string
    } else if matches!(token, Token::Bang) {
        "not".to_string()
    } else {
        format!("~{}", token_string)
    };

    let op_expr = ast_helper::make_ident(Longident::Lident(operator), token_loc);
    let loc = p.mk_loc(&start_pos, &p.loc_end(operand.pexp_loc));

    ast_helper::make_apply(op_expr, vec![(ArgLabel::Nolabel, operand)], loc)
}

/// Create an infix operator expression.
pub fn make_infix_operator(
    p: &mut Parser<'_>,
    token: Token,
    start_pos: Position,
    end_pos: Position,
) -> Expression {
    let stringified_token = token.to_string();

    let loc = p.mk_loc(&start_pos, &end_pos);
    let operator = with_loc(Longident::Lident(stringified_token), loc.clone());

    ast_helper::make_ident(operator.txt, loc)
}

// ============================================================================
// Lookahead Helpers
// ============================================================================

/// Check if the current position indicates an ES6 arrow expression.
pub fn is_es6_arrow_expression(p: &mut Parser<'_>, in_ternary: bool) -> bool {
    p.lookahead(|state| {
        // Check for async prefix
        if let Token::Lident(ref s) = state.token {
            if s == "async" {
                state.next();
            }
        }

        match &state.token {
            Token::Lident(_) | Token::Underscore => {
                state.next();
                matches!(state.token, Token::EqualGreater)
            }
            Token::Lparen => {
                let prev_end_pos = state.prev_end_pos.clone();
                state.next();
                match &state.token {
                    Token::Rparen => {
                        state.next();
                        match &state.token {
                            Token::Colon if !in_ternary => {
                                state.next();
                                // Check for `() :typ =>`
                                super::typ::parse_typ_expr_no_arrow(state);
                                matches!(state.token, Token::EqualGreater)
                            }
                            Token::EqualGreater => true,
                            _ => false,
                        }
                    }
                    Token::Dot => true, // uncurried
                    Token::Backtick => false,
                    _ => {
                        go_to_closing(&Token::Rparen, state);
                        match &state.token {
                            Token::EqualGreater => true,
                            Token::Colon if !in_ternary => {
                                state.next();
                                super::typ::parse_typ_expr_no_arrow(state);
                                matches!(state.token, Token::EqualGreater)
                            }
                            Token::Rparen => false,
                            _ => {
                                state.next_unsafe();
                                matches!(&state.token, Token::EqualGreater)
                                    && state.start_pos.line == prev_end_pos.line
                            }
                        }
                    }
                }
            }
            _ => false,
        }
    })
}

/// Check if the current position indicates an ES6 arrow functor.
pub fn is_es6_arrow_functor(p: &mut Parser<'_>) -> bool {
    p.lookahead(|state| {
        if !matches!(state.token, Token::Lparen) {
            return false;
        }

        state.next();
        match &state.token {
            Token::Rparen => {
                state.next();
                matches!(state.token, Token::Colon | Token::EqualGreater)
            }
            _ => {
                go_to_closing(&Token::Rparen, state);
                matches!(
                    state.token,
                    Token::EqualGreater | Token::Lbrace | Token::Colon
                )
            }
        }
    })
}

/// Check if the current position indicates an ES6 arrow type.
pub fn is_es6_arrow_type(p: &mut Parser<'_>) -> bool {
    p.lookahead(|state| match &state.token {
        Token::Lparen => {
            state.next();
            match &state.token {
                Token::Rparen => {
                    state.next();
                    matches!(state.token, Token::EqualGreater)
                }
                Token::Tilde | Token::Dot => true,
                _ => {
                    go_to_closing(&Token::Rparen, state);
                    matches!(state.token, Token::EqualGreater)
                }
            }
        }
        Token::Tilde => true,
        _ => false,
    })
}

// ============================================================================
// Parse Helpers
// ============================================================================

/// A type parameter parsed from the source.
#[derive(Debug, Clone)]
pub struct TypeParameter {
    /// Attributes on the parameter.
    pub attrs: Attributes,
    /// Argument label.
    pub label: ArgLabel,
    /// The type.
    pub typ: CoreType,
    /// Start position.
    pub start_pos: Position,
}

/// A term parameter in a function definition.
#[derive(Debug, Clone)]
pub struct FundefTermParam {
    /// Attributes.
    pub attrs: Attributes,
    /// Parameter label.
    pub label: ArgLabel,
    /// Default expression.
    pub expr: Option<Expression>,
    /// Pattern.
    pub pat: Pattern,
    /// Start position.
    pub start_pos: Position,
}

/// A type parameter in a function definition (for newtypes).
#[derive(Debug, Clone)]
pub struct FundefTypeParam {
    /// Attributes.
    pub attrs: Attributes,
    /// Located strings for type names.
    pub locs: Vec<Located<String>>,
    /// Start position (usually the `type` keyword).
    pub start_pos: Position,
    /// Position of the opening paren (if parenthesized).
    /// Used for the implicit unit pattern location in type-only arrows.
    pub paren_start_pos: Option<Position>,
    /// Position after the closing paren (if parenthesized).
    /// Used for the implicit unit pattern location in type-only arrows.
    pub paren_end_pos: Option<Position>,
}

/// A function definition parameter.
#[derive(Debug, Clone)]
#[allow(clippy::large_enum_variant)]
pub enum FundefParameter {
    /// Term parameter (value).
    Term(FundefTermParam),
    /// Type parameter (newtype).
    Type(FundefTypeParam),
}

/// Extract type and term parameters from a list of function definition parameters.
pub fn extract_fundef_params(
    params: Vec<FundefParameter>,
) -> (Option<FundefTypeParam>, Vec<FundefTermParam>) {
    let mut type_acc: Option<FundefTypeParam> = None;
    let mut term_acc: Vec<FundefTermParam> = Vec::new();

    for param in params {
        match param {
            FundefParameter::Term(tp) => {
                term_acc.push(tp);
            }
            FundefParameter::Type(tp) => {
                type_acc = match type_acc {
                    Some(mut tpa) => {
                        tpa.attrs.extend(tp.attrs);
                        tpa.locs.extend(tp.locs);
                        Some(tpa)
                    }
                    None => Some(tp),
                };
            }
        }
    }

    (type_acc, term_acc)
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_mk_loc() {
        use crate::parse_arena::ParseArena;
        let mut arena = ParseArena::new();
        let start = Position::new("test.res", 1, 0, 0);
        let end = Position::new("test.res", 1, 0, 10);
        let loc_idx = arena.mk_loc_from_positions(&start, &end);
        // Verify the location can be retrieved from the arena
        let full_loc = arena.to_location(loc_idx);
        assert_eq!(full_loc.loc_start, start);
        assert_eq!(full_loc.loc_end, end);
    }

    #[test]
    fn test_build_longident() {
        let words = vec!["Foo".to_string(), "Bar".to_string(), "baz".to_string()];
        let lid = build_longident(&words);
        assert_eq!(lid.to_string(), "Foo.Bar.baz");
    }

    #[test]
    fn test_negate_string() {
        assert_eq!(negate_string("42"), "-42");
        assert_eq!(negate_string("-42"), "42");
        assert_eq!(negate_string("3.14"), "-3.14");
    }

    #[test]
    fn test_get_closing_token() {
        assert_eq!(get_closing_token(&Token::Lparen), Token::Rparen);
        assert_eq!(get_closing_token(&Token::Lbrace), Token::Rbrace);
        assert_eq!(get_closing_token(&Token::Lbracket), Token::Rbracket);
        assert_eq!(get_closing_token(&Token::LessThan), Token::GreaterThan);
    }

    #[test]
    fn test_recover_default_expr() {
        let expr = recover::default_expr();
        assert!(matches!(expr.pexp_desc, ExpressionDesc::Pexp_extension(_)));
    }

    #[test]
    fn test_recover_default_pattern() {
        let pat = recover::default_pattern();
        assert!(matches!(pat.ppat_desc, PatternDesc::Ppat_extension(_)));
    }

    #[test]
    fn test_recover_default_type() {
        let typ = recover::default_type();
        assert!(matches!(typ.ptyp_desc, CoreTypeDesc::Ptyp_extension(_)));
    }

    #[test]
    fn test_expr_context_default() {
        let ctx = ExprContext::default();
        assert_eq!(ctx, ExprContext::Ordinary);
    }

    #[test]
    fn test_ternary_attr() {
        let attr = ternary_attr();
        assert_eq!(attr.0.txt, "res.ternary");
    }

    #[test]
    fn test_template_literal_attr() {
        let attr = template_literal_attr();
        assert_eq!(attr.0.txt, "res.template");
    }

    #[test]
    fn test_is_es6_arrow_expression_simple() {
        let mut parser = Parser::new("test.res", "x => x");
        assert!(is_es6_arrow_expression(&mut parser, false));
    }

    #[test]
    fn test_is_es6_arrow_expression_parens() {
        let mut parser = Parser::new("test.res", "() => x");
        assert!(is_es6_arrow_expression(&mut parser, false));
    }

    #[test]
    fn test_is_es6_arrow_expression_not() {
        let mut parser = Parser::new("test.res", "x + y");
        assert!(!is_es6_arrow_expression(&mut parser, false));
    }

    #[test]
    fn test_is_es6_arrow_expression_return_type() {
        let mut parser = Parser::new("test.res", "(a): int => a");
        assert!(is_es6_arrow_expression(&mut parser, false));
    }

    #[test]
    fn test_is_es6_arrow_expression_object_return_type() {
        let mut parser = Parser::new("test.res", "(a): {\"x\": int} => a");
        assert!(is_es6_arrow_expression(&mut parser, false));
    }

    #[test]
    fn test_ast_helper_make_expr() {
        let loc = LocIdx::none();
        let expr = ast_helper::make_expr(
            ExpressionDesc::Pexp_constant(Constant::Integer("42".to_string(), None)),
            loc.clone(),
        );
        assert!(matches!(
            expr.pexp_desc,
            ExpressionDesc::Pexp_constant(Constant::Integer(..))
        ));
    }

    #[test]
    fn test_ast_helper_make_var_pat() {
        let loc = LocIdx::none();
        let pat = ast_helper::make_var_pat("x".to_string(), loc);
        assert!(matches!(pat.ppat_desc, PatternDesc::Ppat_var(_)));
    }

    #[test]
    fn test_is_send_sync() {
        fn assert_send_sync<T: Send + Sync>() {}
        assert_send_sync::<ExprContext>();
        assert_send_sync::<TypeParameter>();
        assert_send_sync::<FundefTermParam>();
        assert_send_sync::<FundefTypeParam>();
        assert_send_sync::<FundefParameter>();
    }
}
