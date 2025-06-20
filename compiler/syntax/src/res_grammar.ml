module Token = Res_token

type t =
  | OpenDescription (* open Belt *)
  | ModuleLongIdent (* Foo or Foo.Bar *) [@live]
  | Ternary (* condExpr ? trueExpr : falseExpr *)
  | Es6ArrowExpr
  | Jsx
  | JsxAttribute
  | JsxChild [@live]
  | ExprOperand
  | ExprUnary
  | ExprSetField
  | ExprBinaryAfterOp of Token.t
  | ExprBlock
  | ExprCall
  | ExprList
  | ExprArrayAccess
  | ExprArrayMutation
  | ExprIf
  | ExprFor
  | IfCondition
  | IfBranch
  | ElseBranch
  | TypeExpression
  | External
  | PatternMatching
  | PatternMatchCase
  | LetBinding
  | PatternList
  | PatternOcamlList
  | PatternRecord
  | TypeDef
  | TypeConstrName
  | TypeParams
  | TypeParam [@live]
  | PackageConstraint
  | TypeRepresentation
  | RecordDecl
  | ConstructorDeclaration
  | ParameterList
  | StringFieldDeclarations
  | FieldDeclarations
  | TypExprList
  | FunctorArgs
  | ModExprList
  | TypeParameters
  | RecordRows
  | RecordRowsStringKey
  | ArgumentList
  | Signature
  | Specification
  | Structure
  | Implementation
  | Attribute
  | TypeConstraint
  | AtomicTypExpr
  | ListExpr
  | Pattern
  | AttributePayload
  | TagNames
  | DictRows

let to_string = function
  | OpenDescription -> "an open description"
  | ModuleLongIdent -> "a module path"
  | Ternary -> "a ternary expression"
  | Es6ArrowExpr -> "an es6 arrow function"
  | Jsx -> "a jsx expression"
  | JsxAttribute -> "a jsx attribute"
  | ExprOperand -> "a basic expression"
  | ExprUnary -> "a unary expression"
  | ExprBinaryAfterOp op ->
    "an expression after the operator \"" ^ Token.to_string op ^ "\""
  | ExprIf -> "an if expression"
  | IfCondition -> "the condition of an if expression"
  | IfBranch -> "the true-branch of an if expression"
  | ElseBranch -> "the else-branch of an if expression"
  | TypeExpression -> "a type"
  | External -> "an external"
  | PatternMatching -> "the cases of a pattern match"
  | ExprBlock -> "a block with expressions"
  | ExprSetField -> "a record field mutation"
  | ExprCall -> "a function application"
  | ExprArrayAccess -> "an array access expression"
  | ExprArrayMutation -> "an array mutation"
  | LetBinding -> "a let binding"
  | TypeDef -> "a type definition"
  | TypeParams -> "type parameters"
  | TypeParam -> "a type parameter"
  | TypeConstrName -> "a type-constructor name"
  | TypeRepresentation -> "a type representation"
  | RecordDecl -> "a record declaration"
  | PatternMatchCase -> "a pattern match case"
  | ConstructorDeclaration -> "a constructor declaration"
  | ExprList -> "multiple expressions"
  | PatternList -> "multiple patterns"
  | PatternOcamlList -> "a list pattern"
  | PatternRecord -> "a record pattern"
  | ParameterList -> "parameters"
  | StringFieldDeclarations -> "string field declarations"
  | FieldDeclarations -> "field declarations"
  | TypExprList -> "list of types"
  | FunctorArgs -> "functor arguments"
  | ModExprList -> "list of module expressions"
  | TypeParameters -> "list of type parameters"
  | RecordRows -> "rows of a record"
  | RecordRowsStringKey -> "rows of a record with string keys"
  | ArgumentList -> "arguments"
  | Signature -> "signature"
  | Specification -> "specification"
  | Structure -> "structure"
  | Implementation -> "implementation"
  | Attribute -> "an attribute"
  | TypeConstraint -> "constraints on a type"
  | AtomicTypExpr -> "a type"
  | ListExpr -> "an ocaml list expr"
  | PackageConstraint -> "a package constraint"
  | JsxChild -> "jsx child"
  | Pattern -> "pattern"
  | ExprFor -> "a for expression"
  | AttributePayload -> "an attribute payload"
  | TagNames -> "tag names"
  | DictRows -> "rows of a dict"

let is_signature_item_start = function
  | Token.At | Let | Typ | External | Exception | Open | Include | Module | AtAt
  | PercentPercent ->
    true
  | _ -> false

let is_atomic_pattern_start = function
  | Token.Int _ | String _ | Codepoint _ | Backtick | Lparen | Lbracket | Lbrace
  | Underscore | Lident _ | Uident _ | List | Dict | Exception | Percent ->
    true
  | _ -> false

let is_atomic_expr_start = function
  | Token.True | False | Int _ | String _ | Float _ | Codepoint _ | Backtick
  | Uident _ | Lident _ | Hash | Lparen | List | Lbracket | Lbrace | LessThan
  | Module | Percent | Forwardslash | ForwardslashDot | Dict ->
    true
  | _ -> false

let is_atomic_typ_expr_start = function
  | Token.SingleQuote | Underscore | Lparen | Lbrace | Uident _ | Lident _
  | Percent ->
    true
  | _ -> false

let is_expr_start = function
  | Token.Assert | At | Await | Backtick | Bang | Codepoint _ | False | Float _
  | For | Hash | If | Int _ | Lbrace | Lbracket | LessThan | Lident _ | List
  | Lparen | Minus | MinusDot | Module | Percent | Plus | PlusDot | Tilde
  | String _ | Switch | True | Try | Uident _ | Underscore (* _ => doThings() *)
  | While | Forwardslash | ForwardslashDot | Dict ->
    true
  | _ -> false

let is_jsx_attribute_start = function
  | Token.Lident _ | Question | Lbrace -> true
  | _ -> false

let is_structure_item_start = function
  | Token.Open | Let | Typ | External | Exception | Include | Module | AtAt
  | PercentPercent | At ->
    true
  | t when is_expr_start t -> true
  | _ -> false

let is_pattern_start = function
  | Token.Int _ | Float _ | String _ | Codepoint _ | Backtick | True | False
  | Minus | Plus | Lparen | Lbracket | Lbrace | List | Dict | Underscore
  | DotDotDot | Lident _ | Uident _ | Hash | Exception | Percent | Module | At
    ->
    true
  | _ -> false

let is_parameter_start = function
  | Token.Typ | Tilde | Dot -> true
  | token when is_pattern_start token -> true
  | _ -> false

(* TODO: overparse Uident ? *)
let is_string_field_decl_start = function
  | Token.String _ | Lident _ | At | DotDotDot -> true
  | _ -> false

(* TODO: overparse Uident ? *)
let is_field_decl_start = function
  | Token.At | Mutable | Lident _ -> true
  (* recovery, TODO: this is not ideal… *)
  | Uident _ -> true
  | t when Token.is_keyword t -> true
  | _ -> false

let is_record_decl_start = function
  | Token.At | Mutable | Lident _ | DotDotDot -> true
  | _ -> false

let is_typ_expr_start = function
  | Token.At | SingleQuote | Underscore | Lparen | Lbracket | Uident _
  | Lident _ | Module | Percent | Lbrace ->
    true
  | _ -> false

let is_type_parameter_start = function
  | Token.Tilde | Dot -> true
  | token when is_typ_expr_start token -> true
  | _ -> false

let is_type_param_start = function
  | Token.Plus | Minus | SingleQuote | Underscore -> true
  | _ -> false

let is_functor_arg_start = function
  | Token.At | Uident _ | Underscore | Percent | Lbrace | Lparen -> true
  | _ -> false

let is_mod_expr_start = function
  | Token.At | Percent | Uident _ | Lbrace | Lparen | Lident "unpack" | Await ->
    true
  | _ -> false

let is_dict_row_start = function
  | Token.String _ -> true
  | _ -> false

let is_record_row_start = function
  | Token.DotDotDot -> true
  | Token.Uident _ | Lident _ -> true
  (* TODO *)
  | t when Token.is_keyword t -> true
  | _ -> false

let is_record_row_string_key_start = function
  | Token.String _ -> true
  | _ -> false

let is_argument_start = function
  | Token.Tilde | Dot | Underscore -> true
  | t when is_expr_start t -> true
  | _ -> false

let is_pattern_match_start = function
  | Token.Bar -> true
  | t when is_pattern_start t -> true
  | _ -> false

let is_pattern_ocaml_list_start = function
  | Token.DotDotDot -> true
  | t when is_pattern_start t -> true
  | _ -> false

let is_pattern_record_item_start = function
  | Token.DotDotDot | Uident _ | Lident _ | Underscore -> true
  | _ -> false

let is_attribute_start = function
  | Token.At -> true
  | _ -> false

let is_jsx_child_start = is_atomic_expr_start

let is_block_expr_start = function
  | Token.Assert | At | Await | Backtick | Bang | Codepoint _ | Exception
  | False | Float _ | For | Forwardslash | ForwardslashDot | Hash | If | Int _
  | Lbrace | Lbracket | LessThan | Let | Lident _ | List | Lparen | Minus
  | MinusDot | Module | Open | Percent | Plus | PlusDot | String _ | Switch
  | True | Try | Uident _ | Underscore | While | Dict ->
    true
  | _ -> false

let is_list_element grammar token =
  match grammar with
  | ExprList -> token = Token.DotDotDot || is_expr_start token
  | ListExpr -> token = DotDotDot || is_expr_start token
  | PatternList -> token = DotDotDot || is_pattern_start token
  | ParameterList -> is_parameter_start token
  | StringFieldDeclarations -> is_string_field_decl_start token
  | FieldDeclarations -> is_field_decl_start token
  | RecordDecl -> is_record_decl_start token
  | TypExprList -> is_typ_expr_start token || token = Token.LessThan
  | TypeParams -> is_type_param_start token
  | FunctorArgs -> is_functor_arg_start token
  | ModExprList -> is_mod_expr_start token
  | TypeParameters -> is_type_parameter_start token
  | DictRows -> is_dict_row_start token
  | RecordRows -> is_record_row_start token
  | RecordRowsStringKey -> is_record_row_string_key_start token
  | ArgumentList -> is_argument_start token
  | Signature | Specification -> is_signature_item_start token
  | Structure | Implementation -> is_structure_item_start token
  | PatternMatching -> is_pattern_match_start token
  | PatternOcamlList -> is_pattern_ocaml_list_start token
  | PatternRecord -> is_pattern_record_item_start token
  | Attribute -> is_attribute_start token
  | TypeConstraint -> token = Constraint
  | PackageConstraint -> token = And
  | ConstructorDeclaration -> token = Bar
  | JsxAttribute -> is_jsx_attribute_start token
  | AttributePayload -> token = Lparen
  | TagNames -> token = Hash
  | _ -> false

let is_list_terminator grammar token =
  match (grammar, token) with
  | _, Token.Eof
  | ExprList, (Rparen | Forwardslash | Rbracket)
  | ListExpr, Rparen
  | ArgumentList, (Rparen | DotDotDot)
  | TypExprList, (Rparen | Forwardslash | GreaterThan | Equal)
  | ModExprList, Rparen
  | ( (PatternList | PatternOcamlList | PatternRecord),
      ( Forwardslash | Rbracket | Rparen | EqualGreater (* pattern matching => *)
      | In (* for expressions *)
      | Equal (* let {x} = foo *) ) )
  | ExprBlock, Rbrace
  | (Structure | Signature), Rbrace
  | TypeParams, Rparen
  | ParameterList, (EqualGreater | Lbrace)
  | JsxAttribute, (Forwardslash | GreaterThan)
  | StringFieldDeclarations, Rbrace ->
    true
  | Attribute, token when token <> At -> true
  | TypeConstraint, token when token <> Constraint -> true
  | PackageConstraint, token when token <> And -> true
  | ConstructorDeclaration, token when token <> Bar -> true
  | AttributePayload, Rparen -> true
  | TagNames, Rbracket -> true
  | _ -> false

let is_part_of_list grammar token =
  is_list_element grammar token || is_list_terminator grammar token
