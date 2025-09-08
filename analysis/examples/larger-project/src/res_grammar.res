module Token = Res_token

type t =
  | OpenDescription /* open Belt */
  | @live ModuleLongIdent /* Foo or Foo.Bar */
  | Ternary /* condExpr ? trueExpr : falseExpr */
  | Es6ArrowExpr
  | Jsx
  | JsxAttribute
  | @live JsxChild
  | ExprOperand
  | ExprUnary
  | ExprSetField
  | ExprBinaryAfterOp(Token.t)
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
  | @live TypeParam
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
  | JsFfiImport
  | Pattern
  | AttributePayload
  | TagNames

let toString = x =>
  switch x {
  | OpenDescription => "an open description"
  | ModuleLongIdent => "a module path"
  | Ternary => "a ternary expression"
  | Es6ArrowExpr => "an es6 arrow function"
  | Jsx => "a jsx expression"
  | JsxAttribute => "a jsx attribute"
  | ExprOperand => "a basic expression"
  | ExprUnary => "a unary expression"
  | ExprBinaryAfterOp(op) => "an expression after the operator \"" ++ (Token.toString(op) ++ "\"")
  | ExprIf => "an if expression"
  | IfCondition => "the condition of an if expression"
  | IfBranch => "the true-branch of an if expression"
  | ElseBranch => "the else-branch of an if expression"
  | TypeExpression => "a type"
  | External => "an external"
  | PatternMatching => "the cases of a pattern match"
  | ExprBlock => "a block with expressions"
  | ExprSetField => "a record field mutation"
  | ExprCall => "a function application"
  | ExprArrayAccess => "an array access expression"
  | ExprArrayMutation => "an array mutation"
  | LetBinding => "a let binding"
  | TypeDef => "a type definition"
  | TypeParams => "type parameters"
  | TypeParam => "a type parameter"
  | TypeConstrName => "a type-constructor name"
  | TypeRepresentation => "a type representation"
  | RecordDecl => "a record declaration"
  | PatternMatchCase => "a pattern match case"
  | ConstructorDeclaration => "a constructor declaration"
  | ExprList => "multiple expressions"
  | PatternList => "multiple patterns"
  | PatternOcamlList => "a list pattern"
  | PatternRecord => "a record pattern"
  | ParameterList => "parameters"
  | StringFieldDeclarations => "string field declarations"
  | FieldDeclarations => "field declarations"
  | TypExprList => "list of types"
  | FunctorArgs => "functor arguments"
  | ModExprList => "list of module expressions"
  | TypeParameters => "list of type parameters"
  | RecordRows => "rows of a record"
  | RecordRowsStringKey => "rows of a record with string keys"
  | ArgumentList => "arguments"
  | Signature => "signature"
  | Specification => "specification"
  | Structure => "structure"
  | Implementation => "implementation"
  | Attribute => "an attribute"
  | TypeConstraint => "constraints on a type"
  | AtomicTypExpr => "a type"
  | ListExpr => "an ocaml list expr"
  | PackageConstraint => "a package constraint"
  | JsFfiImport => "js ffi import"
  | JsxChild => "jsx child"
  | Pattern => "pattern"
  | ExprFor => "a for expression"
  | AttributePayload => "an attribute payload"
  | TagNames => "tag names"
  }

let isSignatureItemStart = x =>
  switch x {
  | Token.At
  | Let
  | Typ
  | External
  | Exception
  | Open
  | Include
  | Module
  | AtAt
  | Export
  | PercentPercent => true
  | _ => false
  }

let isAtomicPatternStart = x =>
  switch x {
  | Token.Int(_)
  | String(_)
  | Codepoint(_)
  | Backtick
  | Lparen
  | Lbracket
  | Lbrace
  | Underscore
  | Lident(_)
  | Uident(_)
  | List
  | Exception
  | Lazy
  | Percent => true
  | _ => false
  }

let isAtomicExprStart = x =>
  switch x {
  | Token.True
  | False
  | Int(_)
  | String(_)
  | Float(_)
  | Codepoint(_)
  | Backtick
  | Uident(_)
  | Lident(_)
  | Hash
  | Lparen
  | List
  | Lbracket
  | Lbrace
  | LessThan
  | Module
  | Percent => true
  | _ => false
  }

let isAtomicTypExprStart = x =>
  switch x {
  | Token.SingleQuote
  | Underscore
  | Lparen
  | Lbrace
  | Uident(_)
  | Lident(_)
  | Percent => true
  | _ => false
  }

let isExprStart = x =>
  switch x {
  | Token.True
  | False
  | Int(_)
  | String(_)
  | Float(_)
  | Codepoint(_)
  | Backtick
  | Underscore
  | Uident(_)
  | Lident(_)
  | Hash
  | Lparen
  | List
  | Module
  | Lbracket
  | Lbrace
  | LessThan
  | Minus
  | MinusDot
  | Plus
  | PlusDot
  | Bang
  | Percent
  | At
  | If
  | Switch
  | While
  | For
  | Assert
  | Lazy
  | Try => true
  | _ => false
  }

let isJsxAttributeStart = x =>
  switch x {
  | Token.Lident(_) | Question => true
  | _ => false
  }

let isStructureItemStart = x =>
  switch x {
  | Token.Open
  | Let
  | Typ
  | External
  | Import
  | Export
  | Exception
  | Include
  | Module
  | AtAt
  | PercentPercent
  | At => true
  | t if isExprStart(t) => true
  | _ => false
  }

let isPatternStart = x =>
  switch x {
  | Token.Int(_)
  | Float(_)
  | String(_)
  | Codepoint(_)
  | Backtick
  | True
  | False
  | Minus
  | Plus
  | Lparen
  | Lbracket
  | Lbrace
  | List
  | Underscore
  | Lident(_)
  | Uident(_)
  | Hash
  | Exception
  | Lazy
  | Percent
  | Module
  | At => true
  | _ => false
  }

let isParameterStart = x =>
  switch x {
  | Token.Typ | Tilde | Dot => true
  | token if isPatternStart(token) => true
  | _ => false
  }

/* TODO: overparse Uident ? */
let isStringFieldDeclStart = x =>
  switch x {
  | Token.String(_) | Lident(_) | At | DotDotDot => true
  | _ => false
  }

/* TODO: overparse Uident ? */
let isFieldDeclStart = x =>
  switch x {
  | Token.At | Mutable | Lident(_) => true
  /* recovery, TODO: this is not ideal… */
  | Uident(_) => true
  | t if Token.isKeyword(t) => true
  | _ => false
  }

let isRecordDeclStart = x =>
  switch x {
  | Token.At
  | Mutable
  | Lident(_) => true
  | _ => false
  }

let isTypExprStart = x =>
  switch x {
  | Token.At
  | SingleQuote
  | Underscore
  | Lparen
  | Lbracket
  | Uident(_)
  | Lident(_)
  | Module
  | Percent
  | Lbrace => true
  | _ => false
  }

let isTypeParameterStart = x =>
  switch x {
  | Token.Tilde | Dot => true
  | token if isTypExprStart(token) => true
  | _ => false
  }

let isTypeParamStart = x =>
  switch x {
  | Token.Plus | Minus | SingleQuote | Underscore => true
  | _ => false
  }

let isFunctorArgStart = x =>
  switch x {
  | Token.At
  | Uident(_)
  | Underscore
  | Percent
  | Lbrace
  | Lparen => true
  | _ => false
  }

let isModExprStart = x =>
  switch x {
  | Token.At
  | Percent
  | Uident(_)
  | Lbrace
  | Lparen
  | Lident("unpack") => true
  | _ => false
  }

let isRecordRowStart = x =>
  switch x {
  | Token.DotDotDot => true
  | Token.Uident(_) | Lident(_) => true
  /* TODO */
  | t if Token.isKeyword(t) => true
  | _ => false
  }

let isRecordRowStringKeyStart = x =>
  switch x {
  | Token.String(_) => true
  | _ => false
  }

let isArgumentStart = x =>
  switch x {
  | Token.Tilde | Dot | Underscore => true
  | t if isExprStart(t) => true
  | _ => false
  }

let isPatternMatchStart = x =>
  switch x {
  | Token.Bar => true
  | t if isPatternStart(t) => true
  | _ => false
  }

let isPatternOcamlListStart = x =>
  switch x {
  | Token.DotDotDot => true
  | t if isPatternStart(t) => true
  | _ => false
  }

let isPatternRecordItemStart = x =>
  switch x {
  | Token.DotDotDot | Uident(_) | Lident(_) | Underscore => true
  | _ => false
  }

let isAttributeStart = x =>
  switch x {
  | Token.At => true
  | _ => false
  }

let isJsFfiImportStart = x =>
  switch x {
  | Token.Lident(_) | At => true
  | _ => false
  }

let isJsxChildStart = isAtomicExprStart

let isBlockExprStart = x =>
  switch x {
  | Token.At
  | Hash
  | Percent
  | Minus
  | MinusDot
  | Plus
  | PlusDot
  | Bang
  | True
  | False
  | Float(_)
  | Int(_)
  | String(_)
  | Codepoint(_)
  | Lident(_)
  | Uident(_)
  | Lparen
  | List
  | Lbracket
  | Lbrace
  | Forwardslash
  | Assert
  | Lazy
  | If
  | For
  | While
  | Switch
  | Open
  | Module
  | Exception
  | Let
  | LessThan
  | Backtick
  | Try
  | Underscore => true
  | _ => false
  }

let isListElement = (grammar, token) =>
  switch grammar {
  | ExprList => token == Token.DotDotDot || isExprStart(token)
  | ListExpr => token == DotDotDot || isExprStart(token)
  | PatternList => token == DotDotDot || isPatternStart(token)
  | ParameterList => isParameterStart(token)
  | StringFieldDeclarations => isStringFieldDeclStart(token)
  | FieldDeclarations => isFieldDeclStart(token)
  | RecordDecl => isRecordDeclStart(token)
  | TypExprList => isTypExprStart(token) || token == Token.LessThan
  | TypeParams => isTypeParamStart(token)
  | FunctorArgs => isFunctorArgStart(token)
  | ModExprList => isModExprStart(token)
  | TypeParameters => isTypeParameterStart(token)
  | RecordRows => isRecordRowStart(token)
  | RecordRowsStringKey => isRecordRowStringKeyStart(token)
  | ArgumentList => isArgumentStart(token)
  | Signature | Specification => isSignatureItemStart(token)
  | Structure | Implementation => isStructureItemStart(token)
  | PatternMatching => isPatternMatchStart(token)
  | PatternOcamlList => isPatternOcamlListStart(token)
  | PatternRecord => isPatternRecordItemStart(token)
  | Attribute => isAttributeStart(token)
  | TypeConstraint => token == Constraint
  | PackageConstraint => token == And
  | ConstructorDeclaration => token == Bar
  | JsxAttribute => isJsxAttributeStart(token)
  | JsFfiImport => isJsFfiImportStart(token)
  | AttributePayload => token == Lparen
  | TagNames => token == Hash
  | _ => false
  }

let isListTerminator = (grammar, token) =>
  switch (grammar, token) {
  | (_, Token.Eof)
  | (ExprList, Rparen | Forwardslash | Rbracket)
  | (ListExpr, Rparen)
  | (ArgumentList, Rparen)
  | (TypExprList, Rparen | Forwardslash | GreaterThan | Equal)
  | (ModExprList, Rparen)
  | (
    PatternList | PatternOcamlList | PatternRecord,
    Forwardslash | Rbracket | Rparen | EqualGreater | In | Equal /* let {x} = foo */,
  )
  | (ExprBlock, Rbrace)
  | (Structure | Signature, Rbrace)
  | (TypeParams, Rparen)
  | (ParameterList, EqualGreater | Lbrace)
  | (JsxAttribute, Forwardslash | GreaterThan)
  | (JsFfiImport, Rbrace)
  | (StringFieldDeclarations, Rbrace) => true

  | (Attribute, token) if token != At => true
  | (TypeConstraint, token) if token != Constraint => true
  | (PackageConstraint, token) if token != And => true
  | (ConstructorDeclaration, token) if token != Bar => true
  | (AttributePayload, Rparen) => true
  | (TagNames, Rbracket) => true

  | _ => false
  }

let isPartOfList = (grammar, token) =>
  isListElement(grammar, token) || isListTerminator(grammar, token)
