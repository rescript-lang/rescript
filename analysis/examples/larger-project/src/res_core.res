module Doc = Res_doc
module Grammar = Res_grammar
module Token = Res_token
module Diagnostics = Res_diagnostics
module CommentTable = Res_comments_table
module ResPrinter = Res_printer
module Scanner = Res_scanner
module JsFfi = Res_js_ffi
module Parser = Res_parser

let mkLoc = (startLoc, endLoc) => {
  open Location
  {
    loc_start: startLoc,
    loc_end: endLoc,
    loc_ghost: false,
  }
}

module Recover = {
  let defaultExpr = () => {
    let id = Location.mknoloc("rescript.exprhole")
    Ast_helper.Exp.mk(Pexp_extension(id, PStr(list{})))
  }

  let defaultType = () => {
    let id = Location.mknoloc("rescript.typehole")
    Ast_helper.Typ.extension((id, PStr(list{})))
  }

  let defaultPattern = () => {
    let id = Location.mknoloc("rescript.patternhole")
    Ast_helper.Pat.extension((id, PStr(list{})))
  }

  let defaultModuleExpr = () => Ast_helper.Mod.structure(list{})
  let defaultModuleType = () => Ast_helper.Mty.signature(list{})

  let defaultSignatureItem = {
    let id = Location.mknoloc("rescript.sigitemhole")
    Ast_helper.Sig.extension((id, PStr(list{})))
  }

  let recoverEqualGreater = p => {
    Parser.expect(EqualGreater, p)
    switch p.Parser.token {
    | MinusGreater => Parser.next(p)
    | _ => ()
    }
  }

  let shouldAbortListParse = p => {
    let rec check = breadcrumbs =>
      switch breadcrumbs {
      | list{} => false
      | list{(grammar, _), ...rest} =>
        if Grammar.isPartOfList(grammar, p.Parser.token) {
          true
        } else {
          check(rest)
        }
      }

    check(p.breadcrumbs)
  }
}

module ErrorMessages = {
  let listPatternSpread = "List pattern matches only supports one `...` spread, at the end.\n\
Explanation: a list spread at the tail is efficient, but a spread in the middle would create new list[s]; out of performance concern, our pattern matching currently guarantees to never create new intermediate data."

  @live
  let recordPatternSpread = "Record's `...` spread is not supported in pattern matches.\n\
Explanation: you can't collect a subset of a record's field into its own record, since a record needs an explicit declaration and that subset wouldn't have one.\n\
Solution: you need to pull out each field you want explicitly."

  /* let recordPatternUnderscore = "Record patterns only support one `_`, at the end." */

  let arrayPatternSpread = "Array's `...` spread is not supported in pattern matches.\n\
Explanation: such spread would create a subarray; out of performance concern, our pattern matching currently guarantees to never create new intermediate data.\n\
Solution: if it's to validate the first few elements, use a `when` clause + Array size check + `get` checks on the current pattern. If it's to obtain a subarray, use `Array.sub` or `Belt.Array.slice`."

  let arrayExprSpread = "Arrays can't use the `...` spread currently. Please use `concat` or other Array helpers."

  let recordExprSpread = "Records can only have one `...` spread, at the beginning.\n\
Explanation: since records have a known, fixed shape, a spread like `{a, ...b}` wouldn't make sense, as `b` would override every field of `a` anyway."

  let listExprSpread = "Lists can only have one `...` spread, and at the end.\n\
Explanation: lists are singly-linked list, where a node contains a value and points to the next node. `list[a, ...bc]` efficiently creates a new item and links `bc` as its next nodes. `[...bc, a]` would be expensive, as it'd need to traverse `bc` and prepend each item to `a` one by one. We therefore disallow such syntax sugar.\n\
Solution: directly use `concat`."

  let variantIdent = "A polymorphic variant (e.g. #id) must start with an alphabetical letter or be a number (e.g. #742)"

  let experimentalIfLet = expr => {
    let switchExpr = {...expr, Parsetree.pexp_attributes: list{}}
    Doc.concat(list{
      Doc.text("If-let is currently highly experimental."),
      Doc.line,
      Doc.text("Use a regular `switch` with pattern matching instead:"),
      Doc.concat(list{
        Doc.hardLine,
        Doc.hardLine,
        ResPrinter.printExpression(switchExpr, CommentTable.empty),
      }),
    }) |> Doc.toString(~width=80)
  }

  let typeParam = "A type param consists of a singlequote followed by a name like `'a` or `'A`"
  let typeVar = "A type variable consists of a singlequote followed by a name like `'a` or `'A`"

  let attributeWithoutNode = (attr: Parsetree.attribute) => {
    let ({Asttypes.txt: attrName}, _) = attr
    "Did you forget to attach `" ++
    (attrName ++
    ("` to an item?\n  Standalone attributes start with `@@` like: `@@" ++ (attrName ++ "`")))
  }

  let typeDeclarationNameLongident = longident =>
    "A type declaration's name cannot contain a module access. Did you mean `" ++
    (Longident.last(longident) ++
    "`?")

  let tupleSingleElement = "A tuple needs at least two elements"

  let missingTildeLabeledParameter = name =>
    if name == "" {
      "A labeled parameter starts with a `~`."
    } else {
      "A labeled parameter starts with a `~`. Did you mean: `~" ++ (name ++ "`?")
    }

  let stringInterpolationInPattern = "String interpolation is not supported in pattern matching."

  let spreadInRecordDeclaration = "A record type declaration doesn't support the ... spread. Only an object (with quoted field names) does."

  let objectQuotedFieldName = name =>
    "An object type declaration needs quoted field names. Did you mean \"" ++ (name ++ "\"?")

  let forbiddenInlineRecordDeclaration = "An inline record type declaration is only allowed in a variant constructor's declaration"

  let sameTypeSpread = "You're using a ... spread without extra fields. This is the same type."

  let polyVarIntWithSuffix = number =>
    "A numeric polymorphic variant cannot be followed by a letter. Did you mean `#" ++
    (number ++
    "`?")
}

let jsxAttr = (Location.mknoloc("JSX"), Parsetree.PStr(list{}))
let uncurryAttr = (Location.mknoloc("bs"), Parsetree.PStr(list{}))
let ternaryAttr = (Location.mknoloc("ns.ternary"), Parsetree.PStr(list{}))
let ifLetAttr = (Location.mknoloc("ns.iflet"), Parsetree.PStr(list{}))
let suppressFragileMatchWarningAttr = (
  Location.mknoloc("warning"),
  Parsetree.PStr(list{Ast_helper.Str.eval(Ast_helper.Exp.constant(Pconst_string("-4", None)))}),
)
let makeBracesAttr = loc => (Location.mkloc("ns.braces", loc), Parsetree.PStr(list{}))
let templateLiteralAttr = (Location.mknoloc("res.template"), Parsetree.PStr(list{}))

type stringLiteralState =
  | Start
  | Backslash
  | HexEscape
  | DecimalEscape
  | OctalEscape
  | UnicodeEscape
  | UnicodeCodePointEscape
  | UnicodeEscapeStart
  | EscapedLineBreak

type typDefOrExt =
  | TypeDef({recFlag: Asttypes.rec_flag, types: list<Parsetree.type_declaration>})
  | TypeExt(Parsetree.type_extension)

type labelledParameter =
  | TermParameter({
      uncurried: bool,
      attrs: Parsetree.attributes,
      label: Asttypes.arg_label,
      expr: option<Parsetree.expression>,
      pat: Parsetree.pattern,
      pos: Lexing.position,
    })
  | TypeParameter({
      uncurried: bool,
      attrs: Parsetree.attributes,
      locs: list<Location.loc<string>>,
      pos: Lexing.position,
    })

type recordPatternItem =
  | PatUnderscore
  | PatField((Ast_helper.lid, Parsetree.pattern))

type context =
  | OrdinaryExpr
  | TernaryTrueBranchExpr
  | WhenExpr

let getClosingToken = x =>
  switch x {
  | Token.Lparen => Token.Rparen
  | Lbrace => Rbrace
  | Lbracket => Rbracket
  | List => Rbrace
  | LessThan => GreaterThan
  | _ => assert false
  }

let rec goToClosing = (closingToken, state) =>
  switch (state.Parser.token, closingToken) {
  | (Rparen, Token.Rparen) | (Rbrace, Rbrace) | (Rbracket, Rbracket) | (GreaterThan, GreaterThan) =>
    Parser.next(state)
    ()
  | ((Token.Lbracket | Lparen | Lbrace | List | LessThan) as t, _) =>
    Parser.next(state)
    goToClosing(getClosingToken(t), state)
    goToClosing(closingToken, state)
  | (Rparen | Token.Rbrace | Rbracket | Eof, _) => () /* TODO: how do report errors here? */
  | _ =>
    Parser.next(state)
    goToClosing(closingToken, state)
  }

/* Madness */
let isEs6ArrowExpression = (~inTernary, p) =>
  Parser.lookahead(p, state =>
    switch state.Parser.token {
    | Lident(_) | Underscore =>
      Parser.next(state)
      switch state.Parser.token {
      /* Don't think that this valid
       * Imagine: let x = (a: int)
       * This is a parenthesized expression with a type constraint, wait for
       * the arrow */
      /* | Colon when not inTernary -> true */
      | EqualGreater => true
      | _ => false
      }
    | Lparen =>
      let prevEndPos = state.prevEndPos
      Parser.next(state)
      switch state.token {
      /* arrived at `()` here */
      | Rparen =>
        Parser.next(state)
        switch state.Parser.token {
        /* arrived at `() :` here */
        | Colon if !inTernary =>
          Parser.next(state)
          switch state.Parser.token {
          /* arrived at `() :typ` here */
          | Lident(_) =>
            Parser.next(state)
            switch state.Parser.token {
            /* arrived at `() :typ<` here */
            | LessThan =>
              Parser.next(state)
              goToClosing(GreaterThan, state)
            | _ => ()
            }
            switch state.Parser.token {
            /* arrived at `() :typ =>` or `() :typ<'a,'b> =>` here */
            | EqualGreater => true
            | _ => false
            }
          | _ => true
          }
        | EqualGreater => true
        | _ => false
        }
      | Dot /* uncurried */ => true
      | Tilde => true
      | Backtick => false /* (` always indicates the start of an expr, can't be es6 parameter */
      | _ =>
        goToClosing(Rparen, state)
        switch state.Parser.token {
        | EqualGreater => true
        /* | Lbrace TODO: detect missing =>, is this possible? */
        | Colon if !inTernary => true
        | Rparen => /* imagine having something as :
           * switch colour {
           * | Red
           *    when l == l'
           *    || (&Clflags.classic && (l == Nolabel && !is_optional(l'))) => (t1, t2)
           * We'll arrive at the outer rparen just before the =>.
           * This is not an es6 arrow.
           * */
          false
        | _ =>
          Parser.nextUnsafe(state)
          /* error recovery, peek at the next token,
           * (elements, providerId] => {
           *  in the example above, we have an unbalanced ] here
           */
          switch state.Parser.token {
          | EqualGreater if state.startPos.pos_lnum === prevEndPos.pos_lnum => true
          | _ => false
          }
        }
      }
    | _ => false
    }
  )

let isEs6ArrowFunctor = p =>
  Parser.lookahead(p, state =>
    switch state.Parser.token {
    /* | Uident _ | Underscore -> */
    /* Parser.next state; */
    /* begin match state.Parser.token with */
    /* | EqualGreater -> true */
    /* | _ -> false */
    /* end */
    | Lparen =>
      Parser.next(state)
      switch state.token {
      | Rparen =>
        Parser.next(state)
        switch state.token {
        | Colon | EqualGreater => true
        | _ => false
        }
      | _ =>
        goToClosing(Rparen, state)
        switch state.Parser.token {
        | EqualGreater | Lbrace => true
        | Colon => true
        | _ => false
        }
      }
    | _ => false
    }
  )

let isEs6ArrowType = p =>
  Parser.lookahead(p, state =>
    switch state.Parser.token {
    | Lparen =>
      Parser.next(state)
      switch state.Parser.token {
      | Rparen =>
        Parser.next(state)
        switch state.Parser.token {
        | EqualGreater => true
        | _ => false
        }
      | Tilde | Dot => true
      | _ =>
        goToClosing(Rparen, state)
        switch state.Parser.token {
        | EqualGreater => true
        | _ => false
        }
      }
    | Tilde => true
    | _ => false
    }
  )

let buildLongident = words =>
  switch List.rev(words) {
  | list{} => assert false
  | list{hd, ...tl} => List.fold_left((p, s) => Longident.Ldot(p, s), Lident(hd), tl)
  }

let makeInfixOperator = (p, token, startPos, endPos) => {
  let stringifiedToken = if token == Token.MinusGreater {
    "|."
  } else if token == Token.PlusPlus {
    "^"
  } else if token == Token.BangEqual {
    "<>"
  } else if token == Token.BangEqualEqual {
    "!="
  } else if token == Token.Equal {
    /* TODO: could have a totally different meaning like x->fooSet(y) */
    Parser.err(~startPos, ~endPos, p, Diagnostics.message("Did you mean `==` here?"))
    "="
  } else if token == Token.EqualEqual {
    "="
  } else if token == Token.EqualEqualEqual {
    "=="
  } else {
    Token.toString(token)
  }

  let loc = mkLoc(startPos, endPos)
  let operator = Location.mkloc(Longident.Lident(stringifiedToken), loc)

  Ast_helper.Exp.ident(~loc, operator)
}

let negateString = s =>
  if String.length(s) > 0 && @doesNotRaise String.get(s, 0) == '-' {
    (@doesNotRaise String.sub)(s, 1, String.length(s) - 1)
  } else {
    "-" ++ s
  }

let makeUnaryExpr = (startPos, tokenEnd, token, operand) =>
  switch (token, operand.Parsetree.pexp_desc) {
  | (Token.Plus | PlusDot, Pexp_constant(Pconst_integer(_) | Pconst_float(_))) => operand
  | (Minus, Pexp_constant(Pconst_integer(n, m))) => {
      ...operand,
      pexp_desc: Pexp_constant(Pconst_integer(negateString(n), m)),
    }
  | (Minus | MinusDot, Pexp_constant(Pconst_float(n, m))) => {
      ...operand,
      pexp_desc: Pexp_constant(Pconst_float(negateString(n), m)),
    }
  | (Token.Plus | PlusDot | Minus | MinusDot, _) =>
    let tokenLoc = mkLoc(startPos, tokenEnd)
    let operator = "~" ++ Token.toString(token)
    Ast_helper.Exp.apply(
      ~loc=mkLoc(startPos, operand.Parsetree.pexp_loc.loc_end),
      Ast_helper.Exp.ident(~loc=tokenLoc, Location.mkloc(Longident.Lident(operator), tokenLoc)),
      list{(Nolabel, operand)},
    )
  | (Token.Bang, _) =>
    let tokenLoc = mkLoc(startPos, tokenEnd)
    Ast_helper.Exp.apply(
      ~loc=mkLoc(startPos, operand.Parsetree.pexp_loc.loc_end),
      Ast_helper.Exp.ident(~loc=tokenLoc, Location.mkloc(Longident.Lident("not"), tokenLoc)),
      list{(Nolabel, operand)},
    )
  | _ => operand
  }

let makeListExpression = (loc, seq, extOpt) => {
  let rec handleSeq = x =>
    switch x {
    | list{} =>
      switch extOpt {
      | Some(ext) => ext
      | None =>
        let loc = {...loc, Location.loc_ghost: true}
        let nil = Location.mkloc(Longident.Lident("[]"), loc)
        Ast_helper.Exp.construct(~loc, nil, None)
      }
    | list{e1, ...el} =>
      let exp_el = handleSeq(el)
      let loc = mkLoc(e1.Parsetree.pexp_loc.Location.loc_start, exp_el.pexp_loc.loc_end)

      let arg = Ast_helper.Exp.tuple(~loc, list{e1, exp_el})
      Ast_helper.Exp.construct(~loc, Location.mkloc(Longident.Lident("::"), loc), Some(arg))
    }

  let expr = handleSeq(seq)
  {...expr, pexp_loc: loc}
}

let makeListPattern = (loc, seq, ext_opt) => {
  let rec handle_seq = x =>
    switch x {
    | list{} =>
      let base_case = switch ext_opt {
      | Some(ext) => ext
      | None =>
        let loc = {...loc, Location.loc_ghost: true}
        let nil = {Location.txt: Longident.Lident("[]"), loc: loc}
        Ast_helper.Pat.construct(~loc, nil, None)
      }

      base_case
    | list{p1, ...pl} =>
      let pat_pl = handle_seq(pl)
      let loc = mkLoc(p1.Parsetree.ppat_loc.loc_start, pat_pl.ppat_loc.loc_end)
      let arg = Ast_helper.Pat.mk(~loc, Ppat_tuple(list{p1, pat_pl}))
      Ast_helper.Pat.mk(
        ~loc,
        Ppat_construct(Location.mkloc(Longident.Lident("::"), loc), Some(arg)),
      )
    }

  handle_seq(seq)
}

/* TODO: diagnostic reporting */
let lidentOfPath = longident =>
  switch Longident.flatten(longident) |> List.rev {
  | list{} => ""
  | list{ident, ..._} => ident
  }

let makeNewtypes = (~attrs, ~loc, newtypes, exp) => {
  let expr = List.fold_right(
    (newtype, exp) => Ast_helper.Exp.mk(~loc, Pexp_newtype(newtype, exp)),
    newtypes,
    exp,
  )
  {...expr, pexp_attributes: attrs}
}

/* locally abstract types syntax sugar
 * Transforms
 *  let f: type t u v. = (foo : list</t, u, v/>) => ...
 * into
 *  let f = (type t u v. foo : list</t, u, v/>) => ...
 */
let wrapTypeAnnotation = (~loc, newtypes, core_type, body) => {
  let exp = makeNewtypes(
    ~attrs=list{},
    ~loc,
    newtypes,
    Ast_helper.Exp.constraint_(~loc, body, core_type),
  )

  let typ = Ast_helper.Typ.poly(
    ~loc,
    newtypes,
    Ast_helper.Typ.varify_constructors(newtypes, core_type),
  )

  (exp, typ)
}

@ocaml.doc("
  * process the occurrence of _ in the arguments of a function application
  * replace _ with a new variable, currently __x, in the arguments
  * return a wrapping function that wraps ((__x) => ...) around an expression
  * e.g. foo(_, 3) becomes (__x) => foo(__x, 3)
  ")
let processUnderscoreApplication = args => {
  let exp_question = ref(None)
  let hidden_var = "__x"
  let check_arg = ((lab, exp) as arg) =>
    switch exp.Parsetree.pexp_desc {
    | Pexp_ident({txt: Lident("_")} as id) =>
      let new_id = Location.mkloc(Longident.Lident(hidden_var), id.loc)
      let new_exp = Ast_helper.Exp.mk(Pexp_ident(new_id), ~loc=exp.pexp_loc)
      exp_question := Some(new_exp)
      (lab, new_exp)
    | _ => arg
    }

  let args = List.map(check_arg, args)
  let wrap = exp_apply =>
    switch exp_question.contents {
    | Some({pexp_loc: loc}) =>
      let pattern = Ast_helper.Pat.mk(Ppat_var(Location.mkloc(hidden_var, loc)), ~loc)
      Ast_helper.Exp.mk(Pexp_fun(Nolabel, None, pattern, exp_apply), ~loc)
    | None => exp_apply
    }

  (args, wrap)
}

let hexValue = ch =>
  switch ch {
  | '0' .. '9' => Char.code(ch) - 48
  | 'a' .. 'f' => Char.code(ch) - Char.code('a') + 10
  | 'A' .. 'F' => Char.code(ch) + 32 - Char.code('a') + 10
  | _ => 16
  } /* larger than any legal value */

/* Transform A.a into a. For use with punned record fields as in {A.a, b}. */
let removeModuleNameFromPunnedFieldValue = exp =>
  switch exp.Parsetree.pexp_desc {
  | Pexp_ident(pathIdent) => {
      ...exp,
      pexp_desc: Pexp_ident({...pathIdent, txt: Lident(Longident.last(pathIdent.txt))}),
    }
  | _ => exp
  }

let parseStringLiteral = s => {
  let len = String.length(s)
  let b = Buffer.create(String.length(s))

  let rec parse = (state, i, d) =>
    if i == len {
      switch state {
      | HexEscape | DecimalEscape | OctalEscape | UnicodeEscape | UnicodeCodePointEscape => false
      | _ => true
      }
    } else {
      let c = String.unsafe_get(s, i)
      switch state {
      | Start =>
        switch c {
        | '\\' => parse(Backslash, i + 1, d)
        | c =>
          Buffer.add_char(b, c)
          parse(Start, i + 1, d)
        }
      | Backslash =>
        switch c {
        | 'n' =>
          Buffer.add_char(b, '\n')
          parse(Start, i + 1, d)
        | 'r' =>
          Buffer.add_char(b, '\r')
          parse(Start, i + 1, d)
        | 'b' =>
          Buffer.add_char(b, '\b')
          parse(Start, i + 1, d)
        | 't' =>
          Buffer.add_char(b, '\t')
          parse(Start, i + 1, d)
        | ('\\' | ' ' | '\'' | '"') as c =>
          Buffer.add_char(b, c)
          parse(Start, i + 1, d)
        | 'x' => parse(HexEscape, i + 1, 0)
        | 'o' => parse(OctalEscape, i + 1, 0)
        | 'u' => parse(UnicodeEscapeStart, i + 1, 0)
        | '0' .. '9' => parse(DecimalEscape, i, 0)
        | '\n' | '\r' => parse(EscapedLineBreak, i + 1, d)
        | c =>
          Buffer.add_char(b, '\\')
          Buffer.add_char(b, c)
          parse(Start, i + 1, d)
        }
      | HexEscape =>
        if d === 1 {
          let c0 = String.unsafe_get(s, i - 1)
          let c1 = String.unsafe_get(s, i)
          let c = 16 * hexValue(c0) + hexValue(c1)
          if c < 0 || c > 255 {
            false
          } else {
            Buffer.add_char(b, Char.unsafe_chr(c))
            parse(Start, i + 1, 0)
          }
        } else {
          parse(HexEscape, i + 1, d + 1)
        }
      | DecimalEscape =>
        if d === 2 {
          let c0 = String.unsafe_get(s, i - 2)
          let c1 = String.unsafe_get(s, i - 1)
          let c2 = String.unsafe_get(s, i)
          let c = 100 * (Char.code(c0) - 48) + 10 * (Char.code(c1) - 48) + (Char.code(c2) - 48)
          if c < 0 || c > 255 {
            false
          } else {
            Buffer.add_char(b, Char.unsafe_chr(c))
            parse(Start, i + 1, 0)
          }
        } else {
          parse(DecimalEscape, i + 1, d + 1)
        }
      | OctalEscape =>
        if d === 2 {
          let c0 = String.unsafe_get(s, i - 2)
          let c1 = String.unsafe_get(s, i - 1)
          let c2 = String.unsafe_get(s, i)
          let c = 64 * (Char.code(c0) - 48) + 8 * (Char.code(c1) - 48) + (Char.code(c2) - 48)
          if c < 0 || c > 255 {
            false
          } else {
            Buffer.add_char(b, Char.unsafe_chr(c))
            parse(Start, i + 1, 0)
          }
        } else {
          parse(OctalEscape, i + 1, d + 1)
        }
      | UnicodeEscapeStart =>
        switch c {
        | '{' => parse(UnicodeCodePointEscape, i + 1, 0)
        | _ => parse(UnicodeEscape, i + 1, 1)
        }
      | UnicodeEscape =>
        if d === 3 {
          let c0 = String.unsafe_get(s, i - 3)
          let c1 = String.unsafe_get(s, i - 2)
          let c2 = String.unsafe_get(s, i - 1)
          let c3 = String.unsafe_get(s, i)
          let c = 4096 * hexValue(c0) + 256 * hexValue(c1) + 16 * hexValue(c2) + hexValue(c3)
          if Res_utf8.isValidCodePoint(c) {
            let codePoint = Res_utf8.encodeCodePoint(c)
            Buffer.add_string(b, codePoint)
            parse(Start, i + 1, 0)
          } else {
            false
          }
        } else {
          parse(UnicodeEscape, i + 1, d + 1)
        }
      | UnicodeCodePointEscape =>
        switch c {
        | '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' => parse(UnicodeCodePointEscape, i + 1, d + 1)
        | '}' =>
          let x = ref(0)
          for remaining in d downto 1 {
            let ix = i - remaining
            x := x.contents * 16 + hexValue(String.unsafe_get(s, ix))
          }
          let c = x.contents
          if Res_utf8.isValidCodePoint(c) {
            let codePoint = Res_utf8.encodeCodePoint(x.contents)
            Buffer.add_string(b, codePoint)
            parse(Start, i + 1, 0)
          } else {
            false
          }
        | _ => false
        }
      | EscapedLineBreak =>
        switch c {
        | ' ' | '\t' => parse(EscapedLineBreak, i + 1, d)
        | c =>
          Buffer.add_char(b, c)
          parse(Start, i + 1, d)
        }
      }
    }

  if parse(Start, 0, 0) {
    Buffer.contents(b)
  } else {
    s
  }
}

let rec parseLident = p => {
  let recoverLident = p =>
    if Token.isKeyword(p.Parser.token) && p.Parser.prevEndPos.pos_lnum === p.startPos.pos_lnum {
      Parser.err(p, Diagnostics.lident(p.Parser.token))
      Parser.next(p)
      None
    } else {
      let rec loop = p =>
        if !Recover.shouldAbortListParse(p) {
          Parser.next(p)
          loop(p)
        }

      Parser.err(p, Diagnostics.lident(p.Parser.token))
      Parser.next(p)
      loop(p)
      switch p.Parser.token {
      | Lident(_) => Some()
      | _ => None
      }
    }

  let startPos = p.Parser.startPos
  switch p.Parser.token {
  | Lident(ident) =>
    Parser.next(p)
    let loc = mkLoc(startPos, p.prevEndPos)
    (ident, loc)
  | _ =>
    switch recoverLident(p) {
    | Some() => parseLident(p)
    | None => ("_", mkLoc(startPos, p.prevEndPos))
    }
  }
}

let parseIdent = (~msg, ~startPos, p) =>
  switch p.Parser.token {
  | Lident(ident)
  | Uident(ident) =>
    Parser.next(p)
    let loc = mkLoc(startPos, p.prevEndPos)
    (ident, loc)
  | token if Token.isKeyword(token) && p.prevEndPos.pos_lnum === p.startPos.pos_lnum =>
    let tokenTxt = Token.toString(token)
    let msg =
      "`" ++
      (tokenTxt ++
      ("` is a reserved keyword. Keywords need to be escaped: \\\"" ++ (tokenTxt ++ "\"")))

    Parser.err(~startPos, p, Diagnostics.message(msg))
    Parser.next(p)
    (tokenTxt, mkLoc(startPos, p.prevEndPos))
  | _token =>
    Parser.err(~startPos, p, Diagnostics.message(msg))
    Parser.next(p)
    ("", mkLoc(startPos, p.prevEndPos))
  }

let parseHashIdent = (~startPos, p) => {
  Parser.expect(Hash, p)
  switch p.token {
  | String(text) =>
    let text = if p.mode == ParseForTypeChecker {
      parseStringLiteral(text)
    } else {
      text
    }
    Parser.next(p)
    (text, mkLoc(startPos, p.prevEndPos))
  | Int({i, suffix}) =>
    let () = switch suffix {
    | Some(_) => Parser.err(p, Diagnostics.message(ErrorMessages.polyVarIntWithSuffix(i)))
    | None => ()
    }

    Parser.next(p)
    (i, mkLoc(startPos, p.prevEndPos))
  | _ => parseIdent(~startPos, ~msg=ErrorMessages.variantIdent, p)
  }
}

/* Ldot (Ldot (Lident "Foo", "Bar"), "baz") */
let parseValuePath = p => {
  let startPos = p.Parser.startPos
  let rec aux = (p, path) =>
    switch p.Parser.token {
    | Lident(ident) => Longident.Ldot(path, ident)
    | Uident(uident) =>
      Parser.next(p)
      if p.Parser.token == Dot {
        Parser.expect(Dot, p)
        aux(p, Ldot(path, uident))
      } else {
        Parser.err(p, Diagnostics.unexpected(p.Parser.token, p.breadcrumbs))
        path
      }
    | token =>
      Parser.err(p, Diagnostics.unexpected(token, p.breadcrumbs))
      Longident.Ldot(path, "_")
    }

  let ident = switch p.Parser.token {
  | Lident(ident) => Longident.Lident(ident)
  | Uident(ident) =>
    Parser.next(p)
    if p.Parser.token == Dot {
      Parser.expect(Dot, p)
      aux(p, Lident(ident))
    } else {
      Parser.err(p, Diagnostics.unexpected(p.Parser.token, p.breadcrumbs))
      Longident.Lident(ident)
    }
  | token =>
    Parser.err(p, Diagnostics.unexpected(token, p.breadcrumbs))
    Longident.Lident("_")
  }

  Parser.next(p)
  Location.mkloc(ident, mkLoc(startPos, p.prevEndPos))
}

let parseValuePathAfterDot = p => {
  let startPos = p.Parser.startPos
  switch p.Parser.token {
  | Lident(_)
  | Uident(_) =>
    parseValuePath(p)
  | token =>
    Parser.err(p, Diagnostics.unexpected(token, p.breadcrumbs))
    Location.mkloc(Longident.Lident("_"), mkLoc(startPos, p.prevEndPos))
  }
}

let parseValuePathTail = (p, startPos, ident) => {
  let rec loop = (p, path) =>
    switch p.Parser.token {
    | Lident(ident) =>
      Parser.next(p)
      Location.mkloc(Longident.Ldot(path, ident), mkLoc(startPos, p.prevEndPos))
    | Uident(ident) =>
      Parser.next(p)
      Parser.expect(Dot, p)
      loop(p, Longident.Ldot(path, ident))
    | token =>
      Parser.err(p, Diagnostics.unexpected(token, p.breadcrumbs))
      Location.mkloc(Longident.Ldot(path, "_"), mkLoc(startPos, p.prevEndPos))
    }

  loop(p, ident)
}

let parseModuleLongIdentTail = (~lowercase, p, startPos, ident) => {
  let rec loop = (p, acc) =>
    switch p.Parser.token {
    | Lident(ident) if lowercase =>
      Parser.next(p)
      let lident = Longident.Ldot(acc, ident)
      Location.mkloc(lident, mkLoc(startPos, p.prevEndPos))
    | Uident(ident) =>
      Parser.next(p)
      let endPos = p.prevEndPos
      let lident = Longident.Ldot(acc, ident)
      switch p.Parser.token {
      | Dot =>
        Parser.next(p)
        loop(p, lident)
      | _ => Location.mkloc(lident, mkLoc(startPos, endPos))
      }
    | t =>
      Parser.err(p, Diagnostics.uident(t))
      Location.mkloc(Longident.Ldot(acc, "_"), mkLoc(startPos, p.prevEndPos))
    }

  loop(p, ident)
}

/* Parses module identifiers:
     Foo
     Foo.Bar */
let parseModuleLongIdent = (~lowercase, p) => {
  /* Parser.leaveBreadcrumb p Reporting.ModuleLongIdent; */
  let startPos = p.Parser.startPos
  let moduleIdent = switch p.Parser.token {
  | Lident(ident) if lowercase =>
    let loc = mkLoc(startPos, p.endPos)
    let lident = Longident.Lident(ident)
    Parser.next(p)
    Location.mkloc(lident, loc)
  | Uident(ident) =>
    let lident = Longident.Lident(ident)
    let endPos = p.endPos
    Parser.next(p)
    switch p.Parser.token {
    | Dot =>
      Parser.next(p)
      parseModuleLongIdentTail(~lowercase, p, startPos, lident)
    | _ => Location.mkloc(lident, mkLoc(startPos, endPos))
    }
  | t =>
    Parser.err(p, Diagnostics.uident(t))
    Location.mkloc(Longident.Lident("_"), mkLoc(startPos, p.prevEndPos))
  }

  /* Parser.eatBreadcrumb p; */
  moduleIdent
}

/* `window.location` or `Math` or `Foo.Bar` */
let parseIdentPath = p => {
  let rec loop = (p, acc) =>
    switch p.Parser.token {
    | Uident(ident) | Lident(ident) =>
      Parser.next(p)
      let lident = Longident.Ldot(acc, ident)
      switch p.Parser.token {
      | Dot =>
        Parser.next(p)
        loop(p, lident)
      | _ => lident
      }
    | _t => acc
    }

  switch p.Parser.token {
  | Lident(ident) | Uident(ident) =>
    Parser.next(p)
    switch p.Parser.token {
    | Dot =>
      Parser.next(p)
      loop(p, Longident.Lident(ident))
    | _ => Longident.Lident(ident)
    }
  | _ => Longident.Lident("_")
  }
}

let verifyJsxOpeningClosingName = (p, nameExpr) => {
  let closing = switch p.Parser.token {
  | Lident(lident) =>
    Parser.next(p)
    Longident.Lident(lident)
  | Uident(_) => parseModuleLongIdent(~lowercase=true, p).txt
  | _ => Longident.Lident("")
  }

  switch nameExpr.Parsetree.pexp_desc {
  | Pexp_ident(openingIdent) =>
    let opening = {
      let withoutCreateElement =
        Longident.flatten(openingIdent.txt) |> List.filter(s => s != "createElement")

      switch Longident.unflatten(withoutCreateElement) {
      | Some(li) => li
      | None => Longident.Lident("")
      }
    }

    opening == closing
  | _ => assert false
  }
}

let string_of_pexp_ident = nameExpr =>
  switch nameExpr.Parsetree.pexp_desc {
  | Pexp_ident(openingIdent) =>
    Longident.flatten(openingIdent.txt)
    |> List.filter(s => s != "createElement")
    |> String.concat(".")
  | _ => ""
  }

/* open-def ::=
 *   | open module-path
 *   | open! module-path */
let parseOpenDescription = (~attrs, p) => {
  Parser.leaveBreadcrumb(p, Grammar.OpenDescription)
  let startPos = p.Parser.startPos
  Parser.expect(Open, p)
  let override = if Parser.optional(p, Token.Bang) {
    Asttypes.Override
  } else {
    Asttypes.Fresh
  }

  let modident = parseModuleLongIdent(~lowercase=false, p)
  let loc = mkLoc(startPos, p.prevEndPos)
  Parser.eatBreadcrumb(p)
  Ast_helper.Opn.mk(~loc, ~attrs, ~override, modident)
}

let parseTemplateStringLiteral = s => {
  let len = String.length(s)
  let b = Buffer.create(len)

  let rec loop = i =>
    if i < len {
      let c = String.unsafe_get(s, i)
      switch c {
      | '\\' as c =>
        if i + 1 < len {
          let nextChar = String.unsafe_get(s, i + 1)
          switch nextChar {
          | '\\' as c =>
            Buffer.add_char(b, c)
            loop(i + 2)
          | '$' as c =>
            Buffer.add_char(b, c)
            loop(i + 2)
          | '`' as c =>
            Buffer.add_char(b, c)
            loop(i + 2)
          | '\n' | '\r' =>
            /* line break */
            loop(i + 2)
          | c =>
            Buffer.add_char(b, '\\')
            Buffer.add_char(b, c)
            loop(i + 2)
          }
        } else {
          Buffer.add_char(b, c)
        }

      | c =>
        Buffer.add_char(b, c)
        loop(i + 1)
      }
    } else {
      ()
    }

  loop(0)
  Buffer.contents(b)
}

/* constant	::=	integer-literal */
/* ∣	 float-literal */
/* ∣	 string-literal */
let parseConstant = p => {
  let isNegative = switch p.Parser.token {
  | Token.Minus =>
    Parser.next(p)
    true
  | Plus =>
    Parser.next(p)
    false
  | _ => false
  }

  let constant = switch p.Parser.token {
  | Int({i, suffix}) =>
    let intTxt = if isNegative {
      "-" ++ i
    } else {
      i
    }
    Parsetree.Pconst_integer(intTxt, suffix)
  | Float({f, suffix}) =>
    let floatTxt = if isNegative {
      "-" ++ f
    } else {
      f
    }
    Parsetree.Pconst_float(floatTxt, suffix)
  | String(s) =>
    if p.mode == ParseForTypeChecker {
      Pconst_string(s, Some("js"))
    } else {
      Pconst_string(s, None)
    }
  | Codepoint({c, original}) =>
    if p.mode == ParseForTypeChecker {
      Pconst_char(c)
    } else {
      /* Pconst_char char does not have enough information for formatting.
       * When parsing for the printer, we encode the char contents as a string
       * with a special prefix. */
      Pconst_string(original, Some("INTERNAL_RES_CHAR_CONTENTS"))
    }
  | token =>
    Parser.err(p, Diagnostics.unexpected(token, p.breadcrumbs))
    Pconst_string("", None)
  }

  Parser.next(p)
  constant
}

let parseTemplateConstant = (~prefix, p: Parser.t) => {
  /* Arrived at the ` char */
  let startPos = p.startPos
  Parser.nextTemplateLiteralToken(p)
  switch p.token {
  | TemplateTail(txt) =>
    Parser.next(p)
    let txt = if p.mode == ParseForTypeChecker {
      parseTemplateStringLiteral(txt)
    } else {
      txt
    }
    Parsetree.Pconst_string(txt, prefix)
  | _ =>
    let rec skipTokens = () => {
      Parser.next(p)
      switch p.token {
      | Backtick =>
        Parser.next(p)
        ()
      | _ => skipTokens()
      }
    }

    skipTokens()
    Parser.err(
      ~startPos,
      ~endPos=p.prevEndPos,
      p,
      Diagnostics.message(ErrorMessages.stringInterpolationInPattern),
    )
    Pconst_string("", None)
  }
}

let parseCommaDelimitedRegion = (p, ~grammar, ~closing, ~f) => {
  Parser.leaveBreadcrumb(p, grammar)
  let rec loop = nodes =>
    switch f(p) {
    | Some(node) =>
      switch p.Parser.token {
      | Comma =>
        Parser.next(p)
        loop(list{node, ...nodes})
      | token if token == closing || token == Eof => List.rev(list{node, ...nodes})
      | _ if Grammar.isListElement(grammar, p.token) =>
        /* missing comma between nodes in the region and the current token
         * looks like the start of something valid in the current region.
         * Example:
         *   type student<'extraInfo> = {
         *     name: string,
         *     age: int
         *     otherInfo: 'extraInfo
         *   }
         * There is a missing comma between `int` and `otherInfo`.
         * `otherInfo` looks like a valid start of the record declaration.
         * We report the error here and then continue parsing the region.
         */
        Parser.expect(Comma, p)
        loop(list{node, ...nodes})
      | _ =>
        if !(p.token == Eof || (p.token == closing || Recover.shouldAbortListParse(p))) {
          Parser.expect(Comma, p)
        }
        if p.token == Semicolon {
          Parser.next(p)
        }
        loop(list{node, ...nodes})
      }
    | None =>
      if p.token == Eof || (p.token == closing || Recover.shouldAbortListParse(p)) {
        List.rev(nodes)
      } else {
        Parser.err(p, Diagnostics.unexpected(p.token, p.breadcrumbs))
        Parser.next(p)
        loop(nodes)
      }
    }

  let nodes = loop(list{})
  Parser.eatBreadcrumb(p)
  nodes
}

let parseCommaDelimitedReversedList = (p, ~grammar, ~closing, ~f) => {
  Parser.leaveBreadcrumb(p, grammar)
  let rec loop = nodes =>
    switch f(p) {
    | Some(node) =>
      switch p.Parser.token {
      | Comma =>
        Parser.next(p)
        loop(list{node, ...nodes})
      | token if token == closing || token == Eof => list{node, ...nodes}
      | _ if Grammar.isListElement(grammar, p.token) =>
        /* missing comma between nodes in the region and the current token
         * looks like the start of something valid in the current region.
         * Example:
         *   type student<'extraInfo> = {
         *     name: string,
         *     age: int
         *     otherInfo: 'extraInfo
         *   }
         * There is a missing comma between `int` and `otherInfo`.
         * `otherInfo` looks like a valid start of the record declaration.
         * We report the error here and then continue parsing the region.
         */
        Parser.expect(Comma, p)
        loop(list{node, ...nodes})
      | _ =>
        if !(p.token == Eof || (p.token == closing || Recover.shouldAbortListParse(p))) {
          Parser.expect(Comma, p)
        }
        if p.token == Semicolon {
          Parser.next(p)
        }
        loop(list{node, ...nodes})
      }
    | None =>
      if p.token == Eof || (p.token == closing || Recover.shouldAbortListParse(p)) {
        nodes
      } else {
        Parser.err(p, Diagnostics.unexpected(p.token, p.breadcrumbs))
        Parser.next(p)
        loop(nodes)
      }
    }

  let nodes = loop(list{})
  Parser.eatBreadcrumb(p)
  nodes
}

let parseDelimitedRegion = (p, ~grammar, ~closing, ~f) => {
  Parser.leaveBreadcrumb(p, grammar)
  let rec loop = nodes =>
    switch f(p) {
    | Some(node) => loop(list{node, ...nodes})
    | None =>
      if p.Parser.token == Token.Eof || (p.token == closing || Recover.shouldAbortListParse(p)) {
        List.rev(nodes)
      } else {
        Parser.err(p, Diagnostics.unexpected(p.token, p.breadcrumbs))
        Parser.next(p)
        loop(nodes)
      }
    }

  let nodes = loop(list{})
  Parser.eatBreadcrumb(p)
  nodes
}

let parseRegion = (p, ~grammar, ~f) => {
  Parser.leaveBreadcrumb(p, grammar)
  let rec loop = nodes =>
    switch f(p) {
    | Some(node) => loop(list{node, ...nodes})
    | None =>
      if p.Parser.token == Token.Eof || Recover.shouldAbortListParse(p) {
        List.rev(nodes)
      } else {
        Parser.err(p, Diagnostics.unexpected(p.token, p.breadcrumbs))
        Parser.next(p)
        loop(nodes)
      }
    }

  let nodes = loop(list{})
  Parser.eatBreadcrumb(p)
  nodes
}

/* let-binding	::=	pattern =  expr */
/* ∣	 value-name  { parameter }  [: typexpr]  [:> typexpr] =  expr */
/* ∣	 value-name :  poly-typexpr =  expr */

/* pattern	::=	value-name */
/* ∣	 _ */
/* ∣	 constant */
/* ∣	 pattern as  value-name */
/* ∣	 ( pattern ) */
/* ∣	 ( pattern :  typexpr ) */
/* ∣	 pattern |  pattern */
/* ∣	 constr  pattern */
/* ∣	 #variant variant-pattern */
/* ∣	 #...type */
/* ∣	 / pattern  { , pattern }+  / */
/* ∣	 { field  [: typexpr]  [= pattern] { ; field  [: typexpr]  [= pattern] }  [; _ ] [ ; ] } */
/* ∣	 [ pattern  { ; pattern }  [ ; ] ] */
/* ∣	 pattern ::  pattern */
/* ∣	 [| pattern  { ; pattern }  [ ; ] |] */
/* ∣	 char-literal ..  char-literal */
/* 	∣	 exception pattern */
let rec parsePattern = (~alias=true, ~or_=true, p) => {
  let startPos = p.Parser.startPos
  let attrs = parseAttributes(p)
  let pat = switch p.Parser.token {
  | (True | False) as token =>
    let endPos = p.endPos
    Parser.next(p)
    let loc = mkLoc(startPos, endPos)
    Ast_helper.Pat.construct(
      ~loc,
      Location.mkloc(Longident.Lident(Token.toString(token)), loc),
      None,
    )
  | Int(_) | String(_) | Float(_) | Codepoint(_) | Minus | Plus =>
    let c = parseConstant(p)
    switch p.token {
    | DotDot =>
      Parser.next(p)
      let c2 = parseConstant(p)
      Ast_helper.Pat.interval(~loc=mkLoc(startPos, p.prevEndPos), c, c2)
    | _ => Ast_helper.Pat.constant(~loc=mkLoc(startPos, p.prevEndPos), c)
    }
  | Backtick =>
    let constant = parseTemplateConstant(~prefix=Some("js"), p)
    Ast_helper.Pat.constant(
      ~attrs=list{templateLiteralAttr},
      ~loc=mkLoc(startPos, p.prevEndPos),
      constant,
    )
  | Lparen =>
    Parser.next(p)
    switch p.token {
    | Rparen =>
      Parser.next(p)
      let loc = mkLoc(startPos, p.prevEndPos)
      let lid = Location.mkloc(Longident.Lident("()"), loc)
      Ast_helper.Pat.construct(~loc, lid, None)
    | _ =>
      let pat = parseConstrainedPattern(p)
      switch p.token {
      | Comma =>
        Parser.next(p)
        parseTuplePattern(~attrs, ~first=pat, ~startPos, p)
      | _ =>
        Parser.expect(Rparen, p)
        let loc = mkLoc(startPos, p.prevEndPos)
        {...pat, ppat_loc: loc}
      }
    }
  | Lbracket => parseArrayPattern(~attrs, p)
  | Lbrace => parseRecordPattern(~attrs, p)
  | Underscore =>
    let endPos = p.endPos
    let loc = mkLoc(startPos, endPos)
    Parser.next(p)
    Ast_helper.Pat.any(~loc, ~attrs, ())
  | Lident(ident) =>
    let endPos = p.endPos
    let loc = mkLoc(startPos, endPos)
    Parser.next(p)
    switch p.token {
    | Backtick =>
      let constant = parseTemplateConstant(~prefix=Some(ident), p)
      Ast_helper.Pat.constant(~loc=mkLoc(startPos, p.prevEndPos), constant)
    | _ => Ast_helper.Pat.var(~loc, ~attrs, Location.mkloc(ident, loc))
    }
  | Uident(_) =>
    let constr = parseModuleLongIdent(~lowercase=false, p)
    switch p.Parser.token {
    | Lparen => parseConstructorPatternArgs(p, constr, startPos, attrs)
    | _ => Ast_helper.Pat.construct(~loc=constr.loc, ~attrs, constr, None)
    }
  | Hash =>
    Parser.next(p)
    if p.Parser.token === DotDotDot {
      Parser.next(p)
      let ident = parseValuePath(p)
      let loc = mkLoc(startPos, ident.loc.loc_end)
      Ast_helper.Pat.type_(~loc, ~attrs, ident)
    } else {
      let (ident, loc) = switch p.token {
      | String(text) =>
        let text = if p.mode == ParseForTypeChecker {
          parseStringLiteral(text)
        } else {
          text
        }
        Parser.next(p)
        (text, mkLoc(startPos, p.prevEndPos))
      | Int({i, suffix}) =>
        let () = switch suffix {
        | Some(_) => Parser.err(p, Diagnostics.message(ErrorMessages.polyVarIntWithSuffix(i)))
        | None => ()
        }

        Parser.next(p)
        (i, mkLoc(startPos, p.prevEndPos))
      | _ => parseIdent(~msg=ErrorMessages.variantIdent, ~startPos, p)
      }

      switch p.Parser.token {
      | Lparen => parseVariantPatternArgs(p, ident, startPos, attrs)
      | _ => Ast_helper.Pat.variant(~loc, ~attrs, ident, None)
      }
    }
  | Exception =>
    Parser.next(p)
    let pat = parsePattern(~alias=false, ~or_=false, p)
    let loc = mkLoc(startPos, p.prevEndPos)
    Ast_helper.Pat.exception_(~loc, ~attrs, pat)
  | Lazy =>
    Parser.next(p)
    let pat = parsePattern(~alias=false, ~or_=false, p)
    let loc = mkLoc(startPos, p.prevEndPos)
    Ast_helper.Pat.lazy_(~loc, ~attrs, pat)
  | List =>
    Parser.next(p)
    parseListPattern(~startPos, ~attrs, p)
  | Module => parseModulePattern(~attrs, p)
  | Percent =>
    let extension = parseExtension(p)
    let loc = mkLoc(startPos, p.prevEndPos)
    Ast_helper.Pat.extension(~loc, ~attrs, extension)
  | token =>
    Parser.err(p, Diagnostics.unexpected(token, p.breadcrumbs))
    switch skipTokensAndMaybeRetry(p, ~isStartOfGrammar=Grammar.isAtomicPatternStart) {
    | None => Recover.defaultPattern()
    | Some() => parsePattern(p)
    }
  }

  let pat = if alias {
    parseAliasPattern(~attrs, pat, p)
  } else {
    pat
  }
  if or_ {
    parseOrPattern(pat, p)
  } else {
    pat
  }
}

and skipTokensAndMaybeRetry = (p, ~isStartOfGrammar) =>
  if Token.isKeyword(p.Parser.token) && p.Parser.prevEndPos.pos_lnum === p.startPos.pos_lnum {
    Parser.next(p)
    None
  } else if Recover.shouldAbortListParse(p) {
    if isStartOfGrammar(p.Parser.token) {
      Parser.next(p)
      Some()
    } else {
      None
    }
  } else {
    Parser.next(p)
    let rec loop = p =>
      if !Recover.shouldAbortListParse(p) {
        Parser.next(p)
        loop(p)
      }
    loop(p)
    if isStartOfGrammar(p.Parser.token) {
      Some()
    } else {
      None
    }
  }

/* alias ::= pattern as lident */
and parseAliasPattern = (~attrs, pattern, p) =>
  switch p.Parser.token {
  | As =>
    Parser.next(p)
    let (name, loc) = parseLident(p)
    let name = Location.mkloc(name, loc)
    Ast_helper.Pat.alias(~loc={...pattern.ppat_loc, loc_end: p.prevEndPos}, ~attrs, pattern, name)
  | _ => pattern
  }

/* or ::= pattern | pattern
 * precedence: Red | Blue | Green is interpreted as (Red | Blue) | Green */
and parseOrPattern = (pattern1, p) => {
  let rec loop = pattern1 =>
    switch p.Parser.token {
    | Bar =>
      Parser.next(p)
      let pattern2 = parsePattern(~or_=false, p)
      let loc = {
        ...pattern1.Parsetree.ppat_loc,
        loc_end: pattern2.ppat_loc.loc_end,
      }
      loop(Ast_helper.Pat.or_(~loc, pattern1, pattern2))
    | _ => pattern1
    }

  loop(pattern1)
}

and parseNonSpreadPattern = (~msg, p) => {
  let () = switch p.Parser.token {
  | DotDotDot =>
    Parser.err(p, Diagnostics.message(msg))
    Parser.next(p)
  | _ => ()
  }

  switch p.Parser.token {
  | token if Grammar.isPatternStart(token) =>
    let pat = parsePattern(p)
    switch p.Parser.token {
    | Colon =>
      Parser.next(p)
      let typ = parseTypExpr(p)
      let loc = mkLoc(pat.ppat_loc.loc_start, typ.Parsetree.ptyp_loc.loc_end)
      Some(Ast_helper.Pat.constraint_(~loc, pat, typ))
    | _ => Some(pat)
    }
  | _ => None
  }
}

and parseConstrainedPattern = p => {
  let pat = parsePattern(p)
  switch p.Parser.token {
  | Colon =>
    Parser.next(p)
    let typ = parseTypExpr(p)
    let loc = mkLoc(pat.ppat_loc.loc_start, typ.Parsetree.ptyp_loc.loc_end)
    Ast_helper.Pat.constraint_(~loc, pat, typ)
  | _ => pat
  }
}

and parseConstrainedPatternRegion = p =>
  switch p.Parser.token {
  | token if Grammar.isPatternStart(token) => Some(parseConstrainedPattern(p))
  | _ => None
  }

/* field ::=
 *   | longident
 *   | longident : pattern
 *   | longident as lident
 *
 *  row ::=
 *	 | field ,
 *	 | field , _
 *	 | field , _,
 */
and parseRecordPatternField = p => {
  let label = parseValuePath(p)
  let pattern = switch p.Parser.token {
  | Colon =>
    Parser.next(p)
    parsePattern(p)
  | _ => Ast_helper.Pat.var(~loc=label.loc, Location.mkloc(Longident.last(label.txt), label.loc))
  }

  (label, pattern)
}

/* TODO: there are better representations than PatField|Underscore ? */
and parseRecordPatternItem = p =>
  switch p.Parser.token {
  | DotDotDot =>
    Parser.next(p)
    Some(true, PatField(parseRecordPatternField(p)))
  | Uident(_) | Lident(_) => Some(false, PatField(parseRecordPatternField(p)))
  | Underscore =>
    Parser.next(p)
    Some(false, PatUnderscore)
  | _ => None
  }

and parseRecordPattern = (~attrs, p) => {
  let startPos = p.startPos
  Parser.expect(Lbrace, p)
  let rawFields = parseCommaDelimitedReversedList(
    p,
    ~grammar=PatternRecord,
    ~closing=Rbrace,
    ~f=parseRecordPatternItem,
  )

  Parser.expect(Rbrace, p)
  let (fields, closedFlag) = {
    let (rawFields, flag) = switch rawFields {
    | list{(_hasSpread, PatUnderscore), ...rest} => (rest, Asttypes.Open)
    | rawFields => (rawFields, Asttypes.Closed)
    }

    List.fold_left(((fields, flag), curr) => {
      let (hasSpread, field) = curr
      switch field {
      | PatField(field) =>
        if hasSpread {
          let (_, pattern) = field
          Parser.err(
            ~startPos=pattern.Parsetree.ppat_loc.loc_start,
            p,
            Diagnostics.message(ErrorMessages.recordPatternSpread),
          )
        }
        (list{field, ...fields}, flag)
      | PatUnderscore => (fields, flag)
      }
    }, (list{}, flag), rawFields)
  }

  let loc = mkLoc(startPos, p.prevEndPos)
  Ast_helper.Pat.record(~loc, ~attrs, fields, closedFlag)
}

and parseTuplePattern = (~attrs, ~first, ~startPos, p) => {
  let patterns = list{
    first,
    ...parseCommaDelimitedRegion(
      p,
      ~grammar=Grammar.PatternList,
      ~closing=Rparen,
      ~f=parseConstrainedPatternRegion,
    ),
  }

  Parser.expect(Rparen, p)
  let () = switch patterns {
  | list{_} =>
    Parser.err(
      ~startPos,
      ~endPos=p.prevEndPos,
      p,
      Diagnostics.message(ErrorMessages.tupleSingleElement),
    )
  | _ => ()
  }

  let loc = mkLoc(startPos, p.prevEndPos)
  Ast_helper.Pat.tuple(~loc, ~attrs, patterns)
}

and parsePatternRegion = p =>
  switch p.Parser.token {
  | DotDotDot =>
    Parser.next(p)
    Some(true, parseConstrainedPattern(p))
  | token if Grammar.isPatternStart(token) => Some(false, parseConstrainedPattern(p))
  | _ => None
  }

and parseModulePattern = (~attrs, p) => {
  let startPos = p.Parser.startPos
  Parser.expect(Module, p)
  Parser.expect(Lparen, p)
  let uident = switch p.token {
  | Uident(uident) =>
    let loc = mkLoc(p.startPos, p.endPos)
    Parser.next(p)
    Location.mkloc(uident, loc)
  | _ =>
    /* TODO: error recovery */
    Location.mknoloc("_")
  }

  switch p.token {
  | Colon =>
    let colonStart = p.Parser.startPos
    Parser.next(p)
    let packageTypAttrs = parseAttributes(p)
    let packageType = parsePackageType(~startPos=colonStart, ~attrs=packageTypAttrs, p)
    Parser.expect(Rparen, p)
    let loc = mkLoc(startPos, p.prevEndPos)
    let unpack = Ast_helper.Pat.unpack(~loc=uident.loc, uident)
    Ast_helper.Pat.constraint_(~loc, ~attrs, unpack, packageType)
  | _ =>
    Parser.expect(Rparen, p)
    let loc = mkLoc(startPos, p.prevEndPos)
    Ast_helper.Pat.unpack(~loc, ~attrs, uident)
  }
}

and parseListPattern = (~startPos, ~attrs, p) => {
  let listPatterns = parseCommaDelimitedReversedList(
    p,
    ~grammar=Grammar.PatternOcamlList,
    ~closing=Rbrace,
    ~f=parsePatternRegion,
  )

  Parser.expect(Rbrace, p)
  let loc = mkLoc(startPos, p.prevEndPos)
  let filterSpread = ((hasSpread, pattern)) =>
    if hasSpread {
      Parser.err(
        ~startPos=pattern.Parsetree.ppat_loc.loc_start,
        p,
        Diagnostics.message(ErrorMessages.listPatternSpread),
      )
      pattern
    } else {
      pattern
    }

  switch listPatterns {
  | list{(true, pattern), ...patterns} =>
    let patterns = patterns |> List.map(filterSpread) |> List.rev
    let pat = makeListPattern(loc, patterns, Some(pattern))
    {...pat, ppat_loc: loc, ppat_attributes: attrs}
  | patterns =>
    let patterns = patterns |> List.map(filterSpread) |> List.rev
    let pat = makeListPattern(loc, patterns, None)
    {...pat, ppat_loc: loc, ppat_attributes: attrs}
  }
}

and parseArrayPattern = (~attrs, p) => {
  let startPos = p.startPos
  Parser.expect(Lbracket, p)
  let patterns = parseCommaDelimitedRegion(
    p,
    ~grammar=Grammar.PatternList,
    ~closing=Rbracket,
    ~f=parseNonSpreadPattern(~msg=ErrorMessages.arrayPatternSpread),
  )

  Parser.expect(Rbracket, p)
  let loc = mkLoc(startPos, p.prevEndPos)
  Ast_helper.Pat.array(~loc, ~attrs, patterns)
}

and parseConstructorPatternArgs = (p, constr, startPos, attrs) => {
  let lparen = p.startPos
  Parser.expect(Lparen, p)
  let args = parseCommaDelimitedRegion(
    p,
    ~grammar=Grammar.PatternList,
    ~closing=Rparen,
    ~f=parseConstrainedPatternRegion,
  )

  Parser.expect(Rparen, p)
  let args = switch args {
  | list{} =>
    let loc = mkLoc(lparen, p.prevEndPos)
    Some(Ast_helper.Pat.construct(~loc, Location.mkloc(Longident.Lident("()"), loc), None))
  | list{{ppat_desc: Ppat_tuple(_)} as pat} as patterns =>
    if p.mode == ParseForTypeChecker {
      /* Some(1, 2) for type-checker */
      Some(pat)
    } else {
      /* Some((1, 2)) for printer */
      Some(Ast_helper.Pat.tuple(~loc=mkLoc(lparen, p.endPos), patterns))
    }
  | list{pattern} => Some(pattern)
  | patterns => Some(Ast_helper.Pat.tuple(~loc=mkLoc(lparen, p.endPos), patterns))
  }

  Ast_helper.Pat.construct(~loc=mkLoc(startPos, p.prevEndPos), ~attrs, constr, args)
}

and parseVariantPatternArgs = (p, ident, startPos, attrs) => {
  let lparen = p.startPos
  Parser.expect(Lparen, p)
  let patterns = parseCommaDelimitedRegion(
    p,
    ~grammar=Grammar.PatternList,
    ~closing=Rparen,
    ~f=parseConstrainedPatternRegion,
  )
  let args = switch patterns {
  | list{} =>
    let loc = mkLoc(lparen, p.prevEndPos)
    Some(Ast_helper.Pat.construct(~loc, Location.mkloc(Longident.Lident("()"), loc), None))
  | list{{ppat_desc: Ppat_tuple(_)} as pat} as patterns =>
    if p.mode == ParseForTypeChecker {
      /* #ident(1, 2) for type-checker */
      Some(pat)
    } else {
      /* #ident((1, 2)) for printer */
      Some(Ast_helper.Pat.tuple(~loc=mkLoc(lparen, p.endPos), patterns))
    }
  | list{pattern} => Some(pattern)
  | patterns => Some(Ast_helper.Pat.tuple(~loc=mkLoc(lparen, p.endPos), patterns))
  }

  Parser.expect(Rparen, p)
  Ast_helper.Pat.variant(~loc=mkLoc(startPos, p.prevEndPos), ~attrs, ident, args)
}

and parseExpr = (~context=OrdinaryExpr, p) => {
  let expr = parseOperandExpr(~context, p)
  let expr = parseBinaryExpr(~context, ~a=expr, p, 1)
  parseTernaryExpr(expr, p)
}

/* expr ? expr : expr */
and parseTernaryExpr = (leftOperand, p) =>
  switch p.Parser.token {
  | Question =>
    Parser.leaveBreadcrumb(p, Grammar.Ternary)
    Parser.next(p)
    let trueBranch = parseExpr(~context=TernaryTrueBranchExpr, p)
    Parser.expect(Colon, p)
    let falseBranch = parseExpr(p)
    Parser.eatBreadcrumb(p)
    let loc = {
      ...leftOperand.Parsetree.pexp_loc,
      loc_start: leftOperand.pexp_loc.loc_start,
      loc_end: falseBranch.Parsetree.pexp_loc.loc_end,
    }
    Ast_helper.Exp.ifthenelse(
      ~attrs=list{ternaryAttr},
      ~loc,
      leftOperand,
      trueBranch,
      Some(falseBranch),
    )
  | _ => leftOperand
  }

and parseEs6ArrowExpression = (~context=?, ~parameters=?, p) => {
  let startPos = p.Parser.startPos
  Parser.leaveBreadcrumb(p, Grammar.Es6ArrowExpr)
  let parameters = switch parameters {
  | Some(params) => params
  | None => parseParameters(p)
  }

  let returnType = switch p.Parser.token {
  | Colon =>
    Parser.next(p)
    Some(parseTypExpr(~es6Arrow=false, p))
  | _ => None
  }

  Parser.expect(EqualGreater, p)
  let body = {
    let expr = parseExpr(~context?, p)
    switch returnType {
    | Some(typ) =>
      Ast_helper.Exp.constraint_(
        ~loc=mkLoc(expr.pexp_loc.loc_start, typ.Parsetree.ptyp_loc.loc_end),
        expr,
        typ,
      )
    | None => expr
    }
  }

  Parser.eatBreadcrumb(p)
  let endPos = p.prevEndPos
  let arrowExpr = List.fold_right((parameter, expr) =>
    switch parameter {
    | TermParameter({uncurried, attrs, label: lbl, expr: defaultExpr, pat, pos: startPos}) =>
      let attrs = if uncurried {
        list{uncurryAttr, ...attrs}
      } else {
        attrs
      }
      Ast_helper.Exp.fun_(~loc=mkLoc(startPos, endPos), ~attrs, lbl, defaultExpr, pat, expr)
    | TypeParameter({uncurried, attrs, locs: newtypes, pos: startPos}) =>
      let attrs = if uncurried {
        list{uncurryAttr, ...attrs}
      } else {
        attrs
      }
      makeNewtypes(~attrs, ~loc=mkLoc(startPos, endPos), newtypes, expr)
    }
  , parameters, body)

  {...arrowExpr, pexp_loc: {...arrowExpr.pexp_loc, loc_start: startPos}}
}

/*
 * uncurried_parameter ::=
 *   | . parameter
 *
 * parameter ::=
 *   | pattern
 *   | pattern : type
 *   | ~ labelName
 *   | ~ labelName as pattern
 *   | ~ labelName as pattern : type
 *   | ~ labelName = expr
 *   | ~ labelName as pattern = expr
 *   | ~ labelName as pattern : type = expr
 *   | ~ labelName = ?
 *   | ~ labelName as pattern = ?
 *   | ~ labelName as pattern : type = ?
 *
 * labelName ::= lident
 */
and parseParameter = p =>
  if (
    p.Parser.token == Token.Typ ||
      (p.token == Tilde ||
      (p.token == Dot || Grammar.isPatternStart(p.token)))
  ) {
    let startPos = p.Parser.startPos
    let uncurried = Parser.optional(p, Token.Dot)
    /* two scenarios:
     *   attrs ~lbl ...
     *   attrs pattern
     * Attributes before a labelled arg, indicate that it's on the whole arrow expr
     * Otherwise it's part of the pattern
     * */
    let attrs = parseAttributes(p)
    if p.Parser.token == Typ {
      Parser.next(p)
      let lidents = parseLidentList(p)
      Some(TypeParameter({uncurried: uncurried, attrs: attrs, locs: lidents, pos: startPos}))
    } else {
      let (attrs, lbl, pat) = switch p.Parser.token {
      | Tilde =>
        Parser.next(p)
        let (lblName, loc) = parseLident(p)
        let propLocAttr = (Location.mkloc("ns.namedArgLoc", loc), Parsetree.PStr(list{}))
        switch p.Parser.token {
        | Comma | Equal | Rparen =>
          let loc = mkLoc(startPos, p.prevEndPos)
          (
            attrs,
            Asttypes.Labelled(lblName),
            Ast_helper.Pat.var(~attrs=list{propLocAttr}, ~loc, Location.mkloc(lblName, loc)),
          )
        | Colon =>
          let lblEnd = p.prevEndPos
          Parser.next(p)
          let typ = parseTypExpr(p)
          let loc = mkLoc(startPos, lblEnd)
          let pat = {
            let pat = Ast_helper.Pat.var(~loc, Location.mkloc(lblName, loc))
            let loc = mkLoc(startPos, p.prevEndPos)
            Ast_helper.Pat.constraint_(~attrs=list{propLocAttr}, ~loc, pat, typ)
          }
          (attrs, Asttypes.Labelled(lblName), pat)
        | As =>
          Parser.next(p)
          let pat = {
            let pat = parseConstrainedPattern(p)
            {...pat, ppat_attributes: list{propLocAttr, ...pat.ppat_attributes}}
          }

          (attrs, Asttypes.Labelled(lblName), pat)
        | t =>
          Parser.err(p, Diagnostics.unexpected(t, p.breadcrumbs))
          let loc = mkLoc(startPos, p.prevEndPos)
          (
            attrs,
            Asttypes.Labelled(lblName),
            Ast_helper.Pat.var(~loc, Location.mkloc(lblName, loc)),
          )
        }
      | _ =>
        let pattern = parseConstrainedPattern(p)
        let attrs = List.concat(list{attrs, pattern.ppat_attributes})
        (list{}, Asttypes.Nolabel, {...pattern, ppat_attributes: attrs})
      }

      switch p.Parser.token {
      | Equal =>
        Parser.next(p)
        let lbl = switch lbl {
        | Asttypes.Labelled(lblName) => Asttypes.Optional(lblName)
        | Asttypes.Nolabel =>
          let lblName = switch pat.ppat_desc {
          | Ppat_var(var) => var.txt
          | _ => ""
          }
          Parser.err(
            ~startPos,
            ~endPos=p.prevEndPos,
            p,
            Diagnostics.message(ErrorMessages.missingTildeLabeledParameter(lblName)),
          )
          Asttypes.Optional(lblName)
        | lbl => lbl
        }

        switch p.Parser.token {
        | Question =>
          Parser.next(p)
          Some(
            TermParameter({
              uncurried: uncurried,
              attrs: attrs,
              label: lbl,
              expr: None,
              pat: pat,
              pos: startPos,
            }),
          )
        | _ =>
          let expr = parseConstrainedOrCoercedExpr(p)
          Some(
            TermParameter({
              uncurried: uncurried,
              attrs: attrs,
              label: lbl,
              expr: Some(expr),
              pat: pat,
              pos: startPos,
            }),
          )
        }
      | _ =>
        Some(
          TermParameter({
            uncurried: uncurried,
            attrs: attrs,
            label: lbl,
            expr: None,
            pat: pat,
            pos: startPos,
          }),
        )
      }
    }
  } else {
    None
  }

and parseParameterList = p => {
  let parameters = parseCommaDelimitedRegion(
    ~grammar=Grammar.ParameterList,
    ~f=parseParameter,
    ~closing=Rparen,
    p,
  )

  Parser.expect(Rparen, p)
  parameters
}

/* parameters ::=
 *   | _
 *   | lident
 *   | ()
 *   | (.)
 *   | ( parameter {, parameter} [,] )
 */
and parseParameters = p => {
  let startPos = p.Parser.startPos
  switch p.Parser.token {
  | Lident(ident) =>
    Parser.next(p)
    let loc = mkLoc(startPos, p.Parser.prevEndPos)
    list{
      TermParameter({
        uncurried: false,
        attrs: list{},
        label: Asttypes.Nolabel,
        expr: None,
        pat: Ast_helper.Pat.var(~loc, Location.mkloc(ident, loc)),
        pos: startPos,
      }),
    }
  | Underscore =>
    Parser.next(p)
    let loc = mkLoc(startPos, p.Parser.prevEndPos)
    list{
      TermParameter({
        uncurried: false,
        attrs: list{},
        label: Asttypes.Nolabel,
        expr: None,
        pat: Ast_helper.Pat.any(~loc, ()),
        pos: startPos,
      }),
    }
  | Lparen =>
    Parser.next(p)
    switch p.Parser.token {
    | Rparen =>
      Parser.next(p)
      let loc = mkLoc(startPos, p.Parser.prevEndPos)
      let unitPattern = Ast_helper.Pat.construct(
        ~loc,
        Location.mkloc(Longident.Lident("()"), loc),
        None,
      )

      list{
        TermParameter({
          uncurried: false,
          attrs: list{},
          label: Asttypes.Nolabel,
          expr: None,
          pat: unitPattern,
          pos: startPos,
        }),
      }
    | Dot =>
      Parser.next(p)
      switch p.token {
      | Rparen =>
        Parser.next(p)
        let loc = mkLoc(startPos, p.Parser.prevEndPos)
        let unitPattern = Ast_helper.Pat.construct(
          ~loc,
          Location.mkloc(Longident.Lident("()"), loc),
          None,
        )

        list{
          TermParameter({
            uncurried: true,
            attrs: list{},
            label: Asttypes.Nolabel,
            expr: None,
            pat: unitPattern,
            pos: startPos,
          }),
        }
      | _ =>
        switch parseParameterList(p) {
        | list{
            TermParameter({attrs, label: lbl, expr: defaultExpr, pat: pattern, pos: startPos}),
            ...rest,
          } => list{
            TermParameter({
              uncurried: true,
              attrs: attrs,
              label: lbl,
              expr: defaultExpr,
              pat: pattern,
              pos: startPos,
            }),
            ...rest,
          }
        | parameters => parameters
        }
      }
    | _ => parseParameterList(p)
    }
  | token =>
    Parser.err(p, Diagnostics.unexpected(token, p.breadcrumbs))
    list{}
  }
}

and parseCoercedExpr = (~expr: Parsetree.expression, p) => {
  Parser.expect(ColonGreaterThan, p)
  let typ = parseTypExpr(p)
  let loc = mkLoc(expr.pexp_loc.loc_start, p.prevEndPos)
  Ast_helper.Exp.coerce(~loc, expr, None, typ)
}

and parseConstrainedOrCoercedExpr = p => {
  let expr = parseExpr(p)
  switch p.Parser.token {
  | ColonGreaterThan => parseCoercedExpr(~expr, p)
  | Colon =>
    Parser.next(p)
    switch p.token {
    | _ =>
      let typ = parseTypExpr(p)
      let loc = mkLoc(expr.pexp_loc.loc_start, typ.ptyp_loc.loc_end)
      let expr = Ast_helper.Exp.constraint_(~loc, expr, typ)
      switch p.token {
      | ColonGreaterThan => parseCoercedExpr(~expr, p)
      | _ => expr
      }
    }
  | _ => expr
  }
}

and parseConstrainedExprRegion = p =>
  switch p.Parser.token {
  | token if Grammar.isExprStart(token) =>
    let expr = parseExpr(p)
    switch p.Parser.token {
    | Colon =>
      Parser.next(p)
      let typ = parseTypExpr(p)
      let loc = mkLoc(expr.pexp_loc.loc_start, typ.ptyp_loc.loc_end)
      Some(Ast_helper.Exp.constraint_(~loc, expr, typ))
    | _ => Some(expr)
    }
  | _ => None
  }

/* Atomic expressions represent unambiguous expressions.
 * This means that regardless of the context, these expressions
 * are always interpreted correctly. */
and parseAtomicExpr = p => {
  Parser.leaveBreadcrumb(p, Grammar.ExprOperand)
  let startPos = p.Parser.startPos
  let expr = switch p.Parser.token {
  | (True | False) as token =>
    Parser.next(p)
    let loc = mkLoc(startPos, p.prevEndPos)
    Ast_helper.Exp.construct(
      ~loc,
      Location.mkloc(Longident.Lident(Token.toString(token)), loc),
      None,
    )
  | Int(_) | String(_) | Float(_) | Codepoint(_) =>
    let c = parseConstant(p)
    let loc = mkLoc(startPos, p.prevEndPos)
    Ast_helper.Exp.constant(~loc, c)
  | Backtick =>
    let expr = parseTemplateExpr(p)
    {...expr, pexp_loc: mkLoc(startPos, p.prevEndPos)}
  | Uident(_) | Lident(_) => parseValueOrConstructor(p)
  | Hash => parsePolyVariantExpr(p)
  | Lparen =>
    Parser.next(p)
    switch p.Parser.token {
    | Rparen =>
      Parser.next(p)
      let loc = mkLoc(startPos, p.prevEndPos)
      Ast_helper.Exp.construct(~loc, Location.mkloc(Longident.Lident("()"), loc), None)
    | _t =>
      let expr = parseConstrainedOrCoercedExpr(p)
      switch p.token {
      | Comma =>
        Parser.next(p)
        parseTupleExpr(~startPos, ~first=expr, p)
      | _ =>
        Parser.expect(Rparen, p)
        expr
      /* {expr with pexp_loc = mkLoc startPos p.prevEndPos}
       * What does this location mean here? It means that when there's
       * a parenthesized we keep the location here for whitespace interleaving.
       * Without the closing paren in the location there will always be an extra
       * line. For now we don't include it, because it does weird things
       * with for comments. */
      }
    }
  | List =>
    Parser.next(p)
    parseListExpr(~startPos, p)
  | Module =>
    Parser.next(p)
    parseFirstClassModuleExpr(~startPos, p)
  | Lbracket => parseArrayExp(p)
  | Lbrace => parseBracedOrRecordExpr(p)
  | LessThan => parseJsx(p)
  | Percent =>
    let extension = parseExtension(p)
    let loc = mkLoc(startPos, p.prevEndPos)
    Ast_helper.Exp.extension(~loc, extension)
  | Underscore as token =>
    /* This case is for error recovery. Not sure if it's the correct place */
    Parser.err(p, Diagnostics.lident(token))
    Parser.next(p)
    Recover.defaultExpr()
  | token =>
    let errPos = p.prevEndPos
    Parser.err(~startPos=errPos, p, Diagnostics.unexpected(token, p.breadcrumbs))
    switch skipTokensAndMaybeRetry(p, ~isStartOfGrammar=Grammar.isAtomicExprStart) {
    | None => Recover.defaultExpr()
    | Some() => parseAtomicExpr(p)
    }
  }

  Parser.eatBreadcrumb(p)
  expr
}

/* module(module-expr)
 * module(module-expr : package-type) */
and parseFirstClassModuleExpr = (~startPos, p) => {
  Parser.expect(Lparen, p)

  let modExpr = parseModuleExpr(p)
  let modEndLoc = p.prevEndPos
  switch p.Parser.token {
  | Colon =>
    let colonStart = p.Parser.startPos
    Parser.next(p)
    let attrs = parseAttributes(p)
    let packageType = parsePackageType(~startPos=colonStart, ~attrs, p)
    Parser.expect(Rparen, p)
    let loc = mkLoc(startPos, modEndLoc)
    let firstClassModule = Ast_helper.Exp.pack(~loc, modExpr)
    let loc = mkLoc(startPos, p.prevEndPos)
    Ast_helper.Exp.constraint_(~loc, firstClassModule, packageType)
  | _ =>
    Parser.expect(Rparen, p)
    let loc = mkLoc(startPos, p.prevEndPos)
    Ast_helper.Exp.pack(~loc, modExpr)
  }
}

and parseBracketAccess = (p, expr, startPos) => {
  Parser.leaveBreadcrumb(p, Grammar.ExprArrayAccess)
  let lbracket = p.startPos
  Parser.next(p)
  let stringStart = p.startPos
  switch p.Parser.token {
  | String(s) =>
    let s = if p.mode == ParseForTypeChecker {
      parseStringLiteral(s)
    } else {
      s
    }
    Parser.next(p)
    let stringEnd = p.prevEndPos
    Parser.expect(Rbracket, p)
    Parser.eatBreadcrumb(p)
    let rbracket = p.prevEndPos
    let e = {
      let identLoc = mkLoc(stringStart, stringEnd)
      let loc = mkLoc(startPos, rbracket)
      Ast_helper.Exp.send(~loc, expr, Location.mkloc(s, identLoc))
    }

    let e = parsePrimaryExpr(~operand=e, p)
    let equalStart = p.startPos
    switch p.token {
    | Equal =>
      Parser.next(p)
      let equalEnd = p.prevEndPos
      let rhsExpr = parseExpr(p)
      let loc = mkLoc(startPos, rhsExpr.pexp_loc.loc_end)
      let operatorLoc = mkLoc(equalStart, equalEnd)
      Ast_helper.Exp.apply(
        ~loc,
        Ast_helper.Exp.ident(~loc=operatorLoc, Location.mkloc(Longident.Lident("#="), operatorLoc)),
        list{(Nolabel, e), (Nolabel, rhsExpr)},
      )
    | _ => e
    }
  | _ =>
    let accessExpr = parseConstrainedOrCoercedExpr(p)
    Parser.expect(Rbracket, p)
    Parser.eatBreadcrumb(p)
    let rbracket = p.prevEndPos
    let arrayLoc = mkLoc(lbracket, rbracket)
    switch p.token {
    | Equal =>
      Parser.leaveBreadcrumb(p, ExprArrayMutation)
      Parser.next(p)
      let rhsExpr = parseExpr(p)
      let arraySet = Location.mkloc(Longident.Ldot(Lident("Array"), "set"), arrayLoc)

      let endPos = p.prevEndPos
      let arraySet = Ast_helper.Exp.apply(
        ~loc=mkLoc(startPos, endPos),
        Ast_helper.Exp.ident(~loc=arrayLoc, arraySet),
        list{(Nolabel, expr), (Nolabel, accessExpr), (Nolabel, rhsExpr)},
      )

      Parser.eatBreadcrumb(p)
      arraySet
    | _ =>
      let endPos = p.prevEndPos
      let e = Ast_helper.Exp.apply(
        ~loc=mkLoc(startPos, endPos),
        Ast_helper.Exp.ident(
          ~loc=arrayLoc,
          Location.mkloc(Longident.Ldot(Lident("Array"), "get"), arrayLoc),
        ),
        list{(Nolabel, expr), (Nolabel, accessExpr)},
      )

      parsePrimaryExpr(~operand=e, p)
    }
  }
}

/* * A primary expression represents
 *  - atomic-expr
 *  - john.age
 *  - array[0]
 *  - applyFunctionTo(arg1, arg2)
 *
 *  The "operand" represents the expression that is operated on
 */
and parsePrimaryExpr = (~operand, ~noCall=false, p) => {
  let startPos = operand.pexp_loc.loc_start
  let rec loop = (p, expr) =>
    switch p.Parser.token {
    | Dot =>
      Parser.next(p)
      let lident = parseValuePathAfterDot(p)
      switch p.Parser.token {
      | Equal if noCall == false =>
        Parser.leaveBreadcrumb(p, Grammar.ExprSetField)
        Parser.next(p)
        let targetExpr = parseExpr(p)
        let loc = mkLoc(startPos, p.prevEndPos)
        let setfield = Ast_helper.Exp.setfield(~loc, expr, lident, targetExpr)
        Parser.eatBreadcrumb(p)
        setfield
      | _ =>
        let endPos = p.prevEndPos
        let loc = mkLoc(startPos, endPos)
        loop(p, Ast_helper.Exp.field(~loc, expr, lident))
      }
    | Lbracket if noCall == false && p.prevEndPos.pos_lnum === p.startPos.pos_lnum =>
      parseBracketAccess(p, expr, startPos)
    | Lparen if noCall == false && p.prevEndPos.pos_lnum === p.startPos.pos_lnum =>
      loop(p, parseCallExpr(p, expr))
    | Backtick if noCall == false && p.prevEndPos.pos_lnum === p.startPos.pos_lnum =>
      switch expr.pexp_desc {
      | Pexp_ident({txt: Longident.Lident(ident)}) => parseTemplateExpr(~prefix=ident, p)
      | _ =>
        Parser.err(
          ~startPos=expr.pexp_loc.loc_start,
          ~endPos=expr.pexp_loc.loc_end,
          p,
          Diagnostics.message(
            "Tagged template literals are currently restricted to names like: json`null`.",
          ),
        )
        parseTemplateExpr(p)
      }
    | _ => expr
    }

  loop(p, operand)
}

/* a unary expression is an expression with only one operand and
 * unary operator. Examples:
 *   -1
 *   !condition
 *   -. 1.6
 */
and parseUnaryExpr = p => {
  let startPos = p.Parser.startPos
  switch p.Parser.token {
  | (Minus | MinusDot | Plus | PlusDot | Bang) as token =>
    Parser.leaveBreadcrumb(p, Grammar.ExprUnary)
    let tokenEnd = p.endPos
    Parser.next(p)
    let operand = parseUnaryExpr(p)
    let unaryExpr = makeUnaryExpr(startPos, tokenEnd, token, operand)
    Parser.eatBreadcrumb(p)
    unaryExpr
  | _ => parsePrimaryExpr(~operand=parseAtomicExpr(p), p)
  }
}

/* Represents an "operand" in a binary expression.
 * If you have `a + b`, `a` and `b` both represent
 * the operands of the binary expression with opeartor `+` */
and parseOperandExpr = (~context, p) => {
  let startPos = p.Parser.startPos
  let attrs = parseAttributes(p)
  let expr = switch p.Parser.token {
  | Assert =>
    Parser.next(p)
    let expr = parseUnaryExpr(p)
    let loc = mkLoc(startPos, p.prevEndPos)
    Ast_helper.Exp.assert_(~loc, expr)
  | Lazy =>
    Parser.next(p)
    let expr = parseUnaryExpr(p)
    let loc = mkLoc(startPos, p.prevEndPos)
    Ast_helper.Exp.lazy_(~loc, expr)
  | Try => parseTryExpression(p)
  | If => parseIfOrIfLetExpression(p)
  | For => parseForExpression(p)
  | While => parseWhileExpression(p)
  | Switch => parseSwitchExpression(p)
  | _ =>
    if (
      context !== WhenExpr && isEs6ArrowExpression(~inTernary=context == TernaryTrueBranchExpr, p)
    ) {
      parseEs6ArrowExpression(~context, p)
    } else {
      parseUnaryExpr(p)
    }
  }

  /* let endPos = p.Parser.prevEndPos in */
  {
    ...expr,
    pexp_attributes: List.concat(list{expr.Parsetree.pexp_attributes, attrs}),
    /* pexp_loc = mkLoc startPos endPos */
  }
}

/* a binary expression is an expression that combines two expressions with an
 * operator. Examples:
 *    a + b
 *    f(x) |> g(y)
 */
and parseBinaryExpr = (~context=OrdinaryExpr, ~a=?, p, prec) => {
  let a = switch a {
  | Some(e) => e
  | None => parseOperandExpr(~context, p)
  }

  let rec loop = a => {
    let token = p.Parser.token
    let tokenPrec = switch token {
    /* Can the minus be interpreted as a binary operator? Or is it a unary?
     * let w = {
     *   x
     *   -10
     * }
     * vs
     * let w = {
     *   width
     *   - gap
     * }
     *
     * First case is unary, second is a binary operator.
     * See Scanner.isBinaryOp */
    | Minus | MinusDot | LessThan
      if !Scanner.isBinaryOp(p.scanner.src, p.startPos.pos_cnum, p.endPos.pos_cnum) &&
      p.startPos.pos_lnum > p.prevEndPos.pos_lnum => -1
    | token => Token.precedence(token)
    }

    if tokenPrec < prec {
      a
    } else {
      Parser.leaveBreadcrumb(p, Grammar.ExprBinaryAfterOp(token))
      let startPos = p.startPos
      Parser.next(p)
      let endPos = p.prevEndPos
      let b = parseBinaryExpr(~context, p, tokenPrec + 1)
      let loc = mkLoc(a.Parsetree.pexp_loc.loc_start, b.pexp_loc.loc_end)
      let expr = Ast_helper.Exp.apply(
        ~loc,
        makeInfixOperator(p, token, startPos, endPos),
        list{(Nolabel, a), (Nolabel, b)},
      )

      Parser.eatBreadcrumb(p)
      loop(expr)
    }
  }

  loop(a)
}

/* If we even need this, determines if < might be the start of jsx. Not 100% complete */
/* and isStartOfJsx p = */
/* Parser.lookahead p (fun p -> */
/* match p.Parser.token with */
/* | LessThan -> */
/* Parser.next p; */
/* begin match p.token with */
/* | GreaterThan (* <> *) -> true */
/* | Lident _ | Uident _ | List -> */
/* ignore (parseJsxName p); */
/* begin match p.token with */
/* | GreaterThan (* <div> *) -> true */
/* | Question (*<Component ? *) -> true */
/* | Lident _ | List -> */
/* Parser.next p; */
/* begin match p.token with */
/* | Equal (* <Component handleClick= *) -> true */
/* | _ -> false (* TODO *) */
/* end */
/* | Forwardslash (* <Component / *)-> */
/* Parser.next p; */
/* begin match p.token with */
/* | GreaterThan (* <Component /> *) -> true */
/* | _ -> false */
/* end */
/* | _ -> */
/* false */
/* end */
/* | _ -> false */
/* end */
/* | _ -> false */
/* ) */

and parseTemplateExpr = (~prefix="js", p) => {
  let hiddenOperator = {
    let op = Location.mknoloc(Longident.Lident("^"))
    Ast_helper.Exp.ident(op)
  }

  let rec parseParts = acc => {
    let startPos = p.Parser.startPos
    Parser.nextTemplateLiteralToken(p)
    switch p.token {
    | TemplateTail(txt) =>
      Parser.next(p)
      let loc = mkLoc(startPos, p.prevEndPos)
      let txt = if p.mode == ParseForTypeChecker {
        parseTemplateStringLiteral(txt)
      } else {
        txt
      }
      let str = Ast_helper.Exp.constant(
        ~attrs=list{templateLiteralAttr},
        ~loc,
        Pconst_string(txt, Some(prefix)),
      )
      Ast_helper.Exp.apply(
        ~attrs=list{templateLiteralAttr},
        ~loc,
        hiddenOperator,
        list{(Nolabel, acc), (Nolabel, str)},
      )
    | TemplatePart(txt) =>
      Parser.next(p)
      let loc = mkLoc(startPos, p.prevEndPos)
      let expr = parseExprBlock(p)
      let fullLoc = mkLoc(startPos, p.prevEndPos)
      let txt = if p.mode == ParseForTypeChecker {
        parseTemplateStringLiteral(txt)
      } else {
        txt
      }
      let str = Ast_helper.Exp.constant(
        ~attrs=list{templateLiteralAttr},
        ~loc,
        Pconst_string(txt, Some(prefix)),
      )
      let next = {
        let a = Ast_helper.Exp.apply(
          ~attrs=list{templateLiteralAttr},
          ~loc=fullLoc,
          hiddenOperator,
          list{(Nolabel, acc), (Nolabel, str)},
        )
        Ast_helper.Exp.apply(~loc=fullLoc, hiddenOperator, list{(Nolabel, a), (Nolabel, expr)})
      }

      parseParts(next)
    | token =>
      Parser.err(p, Diagnostics.unexpected(token, p.breadcrumbs))
      Ast_helper.Exp.constant(Pconst_string("", None))
    }
  }

  let startPos = p.startPos
  Parser.nextTemplateLiteralToken(p)
  switch p.token {
  | TemplateTail(txt) =>
    Parser.next(p)
    let txt = if p.mode == ParseForTypeChecker {
      parseTemplateStringLiteral(txt)
    } else {
      txt
    }
    Ast_helper.Exp.constant(
      ~attrs=list{templateLiteralAttr},
      ~loc=mkLoc(startPos, p.prevEndPos),
      Pconst_string(txt, Some(prefix)),
    )
  | TemplatePart(txt) =>
    Parser.next(p)
    let constantLoc = mkLoc(startPos, p.prevEndPos)
    let expr = parseExprBlock(p)
    let fullLoc = mkLoc(startPos, p.prevEndPos)
    let txt = if p.mode == ParseForTypeChecker {
      parseTemplateStringLiteral(txt)
    } else {
      txt
    }
    let str = Ast_helper.Exp.constant(
      ~attrs=list{templateLiteralAttr},
      ~loc=constantLoc,
      Pconst_string(txt, Some(prefix)),
    )
    let next = Ast_helper.Exp.apply(
      ~attrs=list{templateLiteralAttr},
      ~loc=fullLoc,
      hiddenOperator,
      list{(Nolabel, str), (Nolabel, expr)},
    )

    parseParts(next)
  | token =>
    Parser.err(p, Diagnostics.unexpected(token, p.breadcrumbs))
    Ast_helper.Exp.constant(Pconst_string("", None))
  }
}

/* Overparse: let f = a : int => a + 1, is it (a : int) => or (a): int =>
 * Also overparse constraints:
 *  let x = {
 *    let a = 1
 *    a + pi: int
 *  }
 *
 *  We want to give a nice error message in these cases
 * */
and overParseConstrainedOrCoercedOrArrowExpression = (p, expr) =>
  switch p.Parser.token {
  | ColonGreaterThan => parseCoercedExpr(~expr, p)
  | Colon =>
    Parser.next(p)
    let typ = parseTypExpr(~es6Arrow=false, p)
    switch p.Parser.token {
    | EqualGreater =>
      Parser.next(p)
      let body = parseExpr(p)
      let pat = switch expr.pexp_desc {
      | Pexp_ident(longident) =>
        Ast_helper.Pat.var(
          ~loc=expr.pexp_loc,
          Location.mkloc(Longident.flatten(longident.txt) |> String.concat("."), longident.loc),
        )
      /* TODO: can we convert more expressions to patterns? */
      | _ => Ast_helper.Pat.var(~loc=expr.pexp_loc, Location.mkloc("pattern", expr.pexp_loc))
      }

      let arrow1 = Ast_helper.Exp.fun_(
        ~loc=mkLoc(expr.pexp_loc.loc_start, body.pexp_loc.loc_end),
        Asttypes.Nolabel,
        None,
        pat,
        Ast_helper.Exp.constraint_(body, typ),
      )

      let arrow2 = Ast_helper.Exp.fun_(
        ~loc=mkLoc(expr.pexp_loc.loc_start, body.pexp_loc.loc_end),
        Asttypes.Nolabel,
        None,
        Ast_helper.Pat.constraint_(pat, typ),
        body,
      )

      let msg =
        Doc.breakableGroup(
          ~forceBreak=true,
          Doc.concat(list{
            Doc.text("Did you mean to annotate the parameter type or the return type?"),
            Doc.indent(
              Doc.concat(list{
                Doc.line,
                Doc.text("1) "),
                ResPrinter.printExpression(arrow1, CommentTable.empty),
                Doc.line,
                Doc.text("2) "),
                ResPrinter.printExpression(arrow2, CommentTable.empty),
              }),
            ),
          }),
        ) |> Doc.toString(~width=80)

      Parser.err(
        ~startPos=expr.pexp_loc.loc_start,
        ~endPos=body.pexp_loc.loc_end,
        p,
        Diagnostics.message(msg),
      )
      arrow1
    | _ =>
      let loc = mkLoc(expr.pexp_loc.loc_start, typ.ptyp_loc.loc_end)
      let expr = Ast_helper.Exp.constraint_(~loc, expr, typ)
      let () = Parser.err(
        ~startPos=expr.pexp_loc.loc_start,
        ~endPos=typ.ptyp_loc.loc_end,
        p,
        Diagnostics.message(
          Doc.breakableGroup(
            ~forceBreak=true,
            Doc.concat(list{
              Doc.text("Expressions with type constraints need to be wrapped in parens:"),
              Doc.indent(
                Doc.concat(list{
                  Doc.line,
                  ResPrinter.addParens(ResPrinter.printExpression(expr, CommentTable.empty)),
                }),
              ),
            }),
          ) |> Doc.toString(~width=80),
        ),
      )

      expr
    }
  | _ => expr
  }

and parseLetBindingBody = (~startPos, ~attrs, p) => {
  Parser.beginRegion(p)
  Parser.leaveBreadcrumb(p, Grammar.LetBinding)
  let (pat, exp) = {
    Parser.leaveBreadcrumb(p, Grammar.Pattern)
    let pat = parsePattern(p)
    Parser.eatBreadcrumb(p)
    switch p.Parser.token {
    | Colon =>
      Parser.next(p)
      switch p.token {
      | Typ =>
        /* locally abstract types */
        Parser.next(p)
        let newtypes = parseLidentList(p)
        Parser.expect(Dot, p)
        let typ = parseTypExpr(p)
        Parser.expect(Equal, p)
        let expr = parseExpr(p)
        let loc = mkLoc(startPos, p.prevEndPos)
        let (exp, poly) = wrapTypeAnnotation(~loc, newtypes, typ, expr)
        let pat = Ast_helper.Pat.constraint_(~loc, pat, poly)
        (pat, exp)
      | _ =>
        let polyType = parsePolyTypeExpr(p)
        let loc = {...pat.ppat_loc, loc_end: polyType.Parsetree.ptyp_loc.loc_end}
        let pat = Ast_helper.Pat.constraint_(~loc, pat, polyType)
        Parser.expect(Token.Equal, p)
        let exp = parseExpr(p)
        let exp = overParseConstrainedOrCoercedOrArrowExpression(p, exp)
        (pat, exp)
      }
    | _ =>
      Parser.expect(Token.Equal, p)
      let exp = overParseConstrainedOrCoercedOrArrowExpression(p, parseExpr(p))
      (pat, exp)
    }
  }

  let loc = mkLoc(startPos, p.prevEndPos)
  let vb = Ast_helper.Vb.mk(~loc, ~attrs, pat, exp)
  Parser.eatBreadcrumb(p)
  Parser.endRegion(p)
  vb
}

/* TODO: find a better way? Is it possible?
 * let a = 1
 * @attr
 * and b = 2
 *
 * The problem is that without semi we need a lookahead to determine
 * if the attr is on the letbinding or the start of a new thing
 *
 * let a = 1
 * @attr
 * let b = 1
 *
 * Here @attr should attach to something "new": `let b = 1`
 * The parser state is forked, which is quite expensive…
 */
and parseAttributesAndBinding = (p: Parser.t) => {
  let err = p.scanner.err
  let ch = p.scanner.ch
  let offset = p.scanner.offset
  let lineOffset = p.scanner.lineOffset
  let lnum = p.scanner.lnum
  let mode = p.scanner.mode
  let token = p.token
  let startPos = p.startPos
  let endPos = p.endPos
  let prevEndPos = p.prevEndPos
  let breadcrumbs = p.breadcrumbs
  let errors = p.errors
  let diagnostics = p.diagnostics
  let comments = p.comments

  switch p.Parser.token {
  | At =>
    let attrs = parseAttributes(p)
    switch p.Parser.token {
    | And => attrs
    | _ =>
      p.scanner.err = err
      p.scanner.ch = ch
      p.scanner.offset = offset
      p.scanner.lineOffset = lineOffset
      p.scanner.lnum = lnum
      p.scanner.mode = mode
      p.token = token
      p.startPos = startPos
      p.endPos = endPos
      p.prevEndPos = prevEndPos
      p.breadcrumbs = breadcrumbs
      p.errors = errors
      p.diagnostics = diagnostics
      p.comments = comments
      list{}
    }
  | _ => list{}
  }
}

/* definition	::=	let [rec] let-binding  { and let-binding } */
and parseLetBindings = (~attrs, p) => {
  let startPos = p.Parser.startPos
  Parser.optional(p, Let) |> ignore
  let recFlag = if Parser.optional(p, Token.Rec) {
    Asttypes.Recursive
  } else {
    Asttypes.Nonrecursive
  }

  let first = parseLetBindingBody(~startPos, ~attrs, p)

  let rec loop = (p, bindings) => {
    let startPos = p.Parser.startPos
    let attrs = parseAttributesAndBinding(p)
    switch p.Parser.token {
    | And =>
      Parser.next(p)
      let attrs = switch p.token {
      | Export =>
        let exportLoc = mkLoc(p.startPos, p.endPos)
        Parser.next(p)
        let genTypeAttr = (Location.mkloc("genType", exportLoc), Parsetree.PStr(list{}))
        list{genTypeAttr, ...attrs}
      | _ => attrs
      }

      ignore(Parser.optional(p, Let)) /* overparse for fault tolerance */
      let letBinding = parseLetBindingBody(~startPos, ~attrs, p)
      loop(p, list{letBinding, ...bindings})
    | _ => List.rev(bindings)
    }
  }

  (recFlag, loop(p, list{first}))
}

/*
 * div -> div
 * Foo -> Foo.createElement
 * Foo.Bar -> Foo.Bar.createElement
 */
and parseJsxName = p => {
  let longident = switch p.Parser.token {
  | Lident(ident) =>
    let identStart = p.startPos
    let identEnd = p.endPos
    Parser.next(p)
    let loc = mkLoc(identStart, identEnd)
    Location.mkloc(Longident.Lident(ident), loc)
  | Uident(_) =>
    let longident = parseModuleLongIdent(~lowercase=true, p)
    Location.mkloc(Longident.Ldot(longident.txt, "createElement"), longident.loc)
  | _ =>
    let msg = "A jsx name must be a lowercase or uppercase name, like: div in <div /> or Navbar in <Navbar />"

    Parser.err(p, Diagnostics.message(msg))
    Location.mknoloc(Longident.Lident("_"))
  }

  Ast_helper.Exp.ident(~loc=longident.loc, longident)
}

and parseJsxOpeningOrSelfClosingElement = (~startPos, p) => {
  let jsxStartPos = p.Parser.startPos
  let name = parseJsxName(p)
  let jsxProps = parseJsxProps(p)
  let children = switch p.Parser.token {
  | Forwardslash =>
    /* <foo a=b /> */
    let childrenStartPos = p.Parser.startPos
    Parser.next(p)
    let childrenEndPos = p.Parser.startPos
    Parser.expect(GreaterThan, p)
    let loc = mkLoc(childrenStartPos, childrenEndPos)
    makeListExpression(loc, list{}, None) /* no children */
  | GreaterThan =>
    /* <foo a=b> bar </foo> */
    let childrenStartPos = p.Parser.startPos
    Scanner.setJsxMode(p.scanner)
    Parser.next(p)
    let (spread, children) = parseJsxChildren(p)
    let childrenEndPos = p.Parser.startPos
    let () = switch p.token {
    | LessThanSlash => Parser.next(p)
    | LessThan =>
      Parser.next(p)
      Parser.expect(Forwardslash, p)
    | token if Grammar.isStructureItemStart(token) => ()
    | _ => Parser.expect(LessThanSlash, p)
    }

    switch p.Parser.token {
    | Lident(_) | Uident(_) if verifyJsxOpeningClosingName(p, name) =>
      Parser.expect(GreaterThan, p)
      let loc = mkLoc(childrenStartPos, childrenEndPos)
      switch (spread, children) {
      | (true, list{child, ..._}) => child
      | _ => makeListExpression(loc, children, None)
      }
    | token =>
      let () = if Grammar.isStructureItemStart(token) {
        let closing = "</" ++ (string_of_pexp_ident(name) ++ ">")
        let msg = Diagnostics.message("Missing " ++ closing)
        Parser.err(~startPos, ~endPos=p.prevEndPos, p, msg)
      } else {
        let opening = "</" ++ (string_of_pexp_ident(name) ++ ">")
        let msg =
          "Closing jsx name should be the same as the opening name. Did you mean " ++
          (opening ++
          " ?")
        Parser.err(~startPos, ~endPos=p.prevEndPos, p, Diagnostics.message(msg))
        Parser.expect(GreaterThan, p)
      }

      let loc = mkLoc(childrenStartPos, childrenEndPos)
      switch (spread, children) {
      | (true, list{child, ..._}) => child
      | _ => makeListExpression(loc, children, None)
      }
    }
  | token =>
    Parser.err(p, Diagnostics.unexpected(token, p.breadcrumbs))
    makeListExpression(Location.none, list{}, None)
  }

  let jsxEndPos = p.prevEndPos
  let loc = mkLoc(jsxStartPos, jsxEndPos)
  Ast_helper.Exp.apply(
    ~loc,
    name,
    List.concat(list{
      jsxProps,
      list{
        (Asttypes.Labelled("children"), children),
        (
          Asttypes.Nolabel,
          Ast_helper.Exp.construct(Location.mknoloc(Longident.Lident("()")), None),
        ),
      },
    }),
  )
}

/*
 *  jsx ::=
 *    | <> jsx-children </>
 *    | <element-name {jsx-prop} />
 *    | <element-name {jsx-prop}> jsx-children </element-name>
 *
 *  jsx-children ::= primary-expr*          * => 0 or more
 */
and parseJsx = p => {
  Parser.leaveBreadcrumb(p, Grammar.Jsx)
  let startPos = p.Parser.startPos
  Parser.expect(LessThan, p)
  let jsxExpr = switch p.Parser.token {
  | Lident(_) | Uident(_) => parseJsxOpeningOrSelfClosingElement(~startPos, p)
  | GreaterThan =>
    /* fragment: <> foo </> */
    parseJsxFragment(p)
  | _ => parseJsxName(p)
  }

  Parser.eatBreadcrumb(p)
  {...jsxExpr, pexp_attributes: list{jsxAttr}}
}

/*
 * jsx-fragment ::=
 *  | <> </>
 *  | <> jsx-children </>
 */
and parseJsxFragment = p => {
  let childrenStartPos = p.Parser.startPos
  Scanner.setJsxMode(p.scanner)
  Parser.expect(GreaterThan, p)
  let (_spread, children) = parseJsxChildren(p)
  let childrenEndPos = p.Parser.startPos
  Parser.expect(LessThanSlash, p)
  Parser.expect(GreaterThan, p)
  let loc = mkLoc(childrenStartPos, childrenEndPos)
  makeListExpression(loc, children, None)
}

/*
 * jsx-prop ::=
 *   |  lident
 *   | ?lident
 *   |  lident =  jsx_expr
 *   |  lident = ?jsx_expr
 */
and parseJsxProp = p =>
  switch p.Parser.token {
  | Question | Lident(_) =>
    let optional = Parser.optional(p, Question)
    let (name, loc) = parseLident(p)
    let propLocAttr = (Location.mkloc("ns.namedArgLoc", loc), Parsetree.PStr(list{}))
    /* optional punning: <foo ?a /> */
    if optional {
      Some(
        Asttypes.Optional(name),
        Ast_helper.Exp.ident(
          ~attrs=list{propLocAttr},
          ~loc,
          Location.mkloc(Longident.Lident(name), loc),
        ),
      )
    } else {
      switch p.Parser.token {
      | Equal =>
        Parser.next(p)
        /* no punning */
        let optional = Parser.optional(p, Question)
        let attrExpr = {
          let e = parsePrimaryExpr(~operand=parseAtomicExpr(p), p)
          {...e, pexp_attributes: list{propLocAttr, ...e.pexp_attributes}}
        }

        let label = if optional {
          Asttypes.Optional(name)
        } else {
          Asttypes.Labelled(name)
        }

        Some(label, attrExpr)
      | _ =>
        let attrExpr = Ast_helper.Exp.ident(
          ~loc,
          ~attrs=list{propLocAttr},
          Location.mkloc(Longident.Lident(name), loc),
        )
        let label = if optional {
          Asttypes.Optional(name)
        } else {
          Asttypes.Labelled(name)
        }

        Some(label, attrExpr)
      }
    }
  | _ => None
  }

and parseJsxProps = p => parseRegion(~grammar=Grammar.JsxAttribute, ~f=parseJsxProp, p)

and parseJsxChildren = p => {
  let rec loop = (p, children) =>
    switch p.Parser.token {
    | Token.Eof | LessThanSlash =>
      Scanner.popMode(p.scanner, Jsx)
      List.rev(children)
    | LessThan =>
      /* Imagine: <div> <Navbar /> <
       * is `<` the start of a jsx-child? <div …
       * or is it the start of a closing tag?  </div>
       * reconsiderLessThan peeks at the next token and
       * determines the correct token to disambiguate */
      let token = Scanner.reconsiderLessThan(p.scanner)
      if token == LessThan {
        let child = parsePrimaryExpr(~operand=parseAtomicExpr(p), ~noCall=true, p)
        loop(p, list{child, ...children})
      } else {
        /* LessThanSlash */
        let () = p.token = token
        let () = Scanner.popMode(p.scanner, Jsx)
        List.rev(children)
      }
    | token if Grammar.isJsxChildStart(token) =>
      let () = Scanner.popMode(p.scanner, Jsx)
      let child = parsePrimaryExpr(~operand=parseAtomicExpr(p), ~noCall=true, p)
      loop(p, list{child, ...children})
    | _ =>
      Scanner.popMode(p.scanner, Jsx)
      List.rev(children)
    }

  switch p.Parser.token {
  | DotDotDot =>
    Parser.next(p)
    (true, list{parsePrimaryExpr(~operand=parseAtomicExpr(p), ~noCall=true, p)})
  | _ => (false, loop(p, list{}))
  }
}

and parseBracedOrRecordExpr = p => {
  let startPos = p.Parser.startPos
  Parser.expect(Lbrace, p)
  switch p.Parser.token {
  | Rbrace =>
    Parser.err(p, Diagnostics.unexpected(Rbrace, p.breadcrumbs))
    Parser.next(p)
    let loc = mkLoc(startPos, p.prevEndPos)
    let braces = makeBracesAttr(loc)
    Ast_helper.Exp.construct(
      ~attrs=list{braces},
      ~loc,
      Location.mkloc(Longident.Lident("()"), loc),
      None,
    )
  | DotDotDot =>
    /* beginning of record spread, parse record */
    Parser.next(p)
    let spreadExpr = parseConstrainedOrCoercedExpr(p)
    Parser.expect(Comma, p)
    let expr = parseRecordExpr(~startPos, ~spread=Some(spreadExpr), list{}, p)
    Parser.expect(Rbrace, p)
    expr
  | String(s) =>
    let s = if p.mode == ParseForTypeChecker {
      parseStringLiteral(s)
    } else {
      s
    }
    let field = {
      let loc = mkLoc(p.startPos, p.endPos)
      Parser.next(p)
      Location.mkloc(Longident.Lident(s), loc)
    }

    switch p.Parser.token {
    | Colon =>
      Parser.next(p)
      let fieldExpr = parseExpr(p)
      Parser.optional(p, Comma) |> ignore
      let expr = parseRecordExprWithStringKeys(~startPos, (field, fieldExpr), p)
      Parser.expect(Rbrace, p)
      expr
    | _ =>
      let tag = if p.mode == ParseForTypeChecker {
        Some("js")
      } else {
        None
      }
      let constant = Ast_helper.Exp.constant(~loc=field.loc, Parsetree.Pconst_string(s, tag))
      let a = parsePrimaryExpr(~operand=constant, p)
      let e = parseBinaryExpr(~a, p, 1)
      let e = parseTernaryExpr(e, p)
      switch p.Parser.token {
      | Semicolon =>
        let expr = parseExprBlock(~first=e, p)
        Parser.expect(Rbrace, p)
        let loc = mkLoc(startPos, p.prevEndPos)
        let braces = makeBracesAttr(loc)
        {...expr, Parsetree.pexp_attributes: list{braces, ...expr.Parsetree.pexp_attributes}}
      | Rbrace =>
        Parser.next(p)
        let loc = mkLoc(startPos, p.prevEndPos)
        let braces = makeBracesAttr(loc)
        {...e, pexp_attributes: list{braces, ...e.pexp_attributes}}
      | _ =>
        let expr = parseExprBlock(~first=e, p)
        Parser.expect(Rbrace, p)
        let loc = mkLoc(startPos, p.prevEndPos)
        let braces = makeBracesAttr(loc)
        {...expr, pexp_attributes: list{braces, ...expr.pexp_attributes}}
      }
    }
  | Uident(_) | Lident(_) =>
    let startToken = p.token
    let valueOrConstructor = parseValueOrConstructor(p)
    switch valueOrConstructor.pexp_desc {
    | Pexp_ident(pathIdent) =>
      let identEndPos = p.prevEndPos
      switch p.Parser.token {
      | Comma =>
        Parser.next(p)
        let valueOrConstructor = switch startToken {
        | Uident(_) => removeModuleNameFromPunnedFieldValue(valueOrConstructor)
        | _ => valueOrConstructor
        }

        let expr = parseRecordExpr(~startPos, list{(pathIdent, valueOrConstructor)}, p)
        Parser.expect(Rbrace, p)
        expr
      | Colon =>
        Parser.next(p)
        let fieldExpr = parseExpr(p)
        switch p.token {
        | Rbrace =>
          Parser.next(p)
          let loc = mkLoc(startPos, p.prevEndPos)
          Ast_helper.Exp.record(~loc, list{(pathIdent, fieldExpr)}, None)
        | _ =>
          Parser.expect(Comma, p)
          let expr = parseRecordExpr(~startPos, list{(pathIdent, fieldExpr)}, p)
          Parser.expect(Rbrace, p)
          expr
        }
      /* error case */
      | Lident(_) =>
        if p.prevEndPos.pos_lnum < p.startPos.pos_lnum {
          Parser.expect(Comma, p)
          let expr = parseRecordExpr(~startPos, list{(pathIdent, valueOrConstructor)}, p)
          Parser.expect(Rbrace, p)
          expr
        } else {
          Parser.expect(Colon, p)
          let expr = parseRecordExpr(~startPos, list{(pathIdent, valueOrConstructor)}, p)
          Parser.expect(Rbrace, p)
          expr
        }
      | Semicolon =>
        let expr = parseExprBlock(~first=Ast_helper.Exp.ident(pathIdent), p)
        Parser.expect(Rbrace, p)
        let loc = mkLoc(startPos, p.prevEndPos)
        let braces = makeBracesAttr(loc)
        {...expr, pexp_attributes: list{braces, ...expr.pexp_attributes}}
      | Rbrace =>
        Parser.next(p)
        let expr = Ast_helper.Exp.ident(~loc=pathIdent.loc, pathIdent)
        let loc = mkLoc(startPos, p.prevEndPos)
        let braces = makeBracesAttr(loc)
        {...expr, pexp_attributes: list{braces, ...expr.pexp_attributes}}
      | EqualGreater =>
        let loc = mkLoc(startPos, identEndPos)
        let ident = Location.mkloc(Longident.last(pathIdent.txt), loc)
        let a = parseEs6ArrowExpression(
          ~parameters=list{
            TermParameter({
              uncurried: false,
              attrs: list{},
              label: Asttypes.Nolabel,
              expr: None,
              pat: Ast_helper.Pat.var(ident),
              pos: startPos,
            }),
          },
          p,
        )

        let e = parseBinaryExpr(~a, p, 1)
        let e = parseTernaryExpr(e, p)
        switch p.Parser.token {
        | Semicolon =>
          let expr = parseExprBlock(~first=e, p)
          Parser.expect(Rbrace, p)
          let loc = mkLoc(startPos, p.prevEndPos)
          let braces = makeBracesAttr(loc)
          {...expr, pexp_attributes: list{braces, ...expr.pexp_attributes}}
        | Rbrace =>
          Parser.next(p)
          let loc = mkLoc(startPos, p.prevEndPos)
          let braces = makeBracesAttr(loc)
          {...e, pexp_attributes: list{braces, ...e.pexp_attributes}}
        | _ =>
          let expr = parseExprBlock(~first=e, p)
          Parser.expect(Rbrace, p)
          let loc = mkLoc(startPos, p.prevEndPos)
          let braces = makeBracesAttr(loc)
          {...expr, pexp_attributes: list{braces, ...expr.pexp_attributes}}
        }
      | _ =>
        Parser.leaveBreadcrumb(p, Grammar.ExprBlock)
        let a = parsePrimaryExpr(~operand=Ast_helper.Exp.ident(~loc=pathIdent.loc, pathIdent), p)
        let e = parseBinaryExpr(~a, p, 1)
        let e = parseTernaryExpr(e, p)
        Parser.eatBreadcrumb(p)
        switch p.Parser.token {
        | Semicolon =>
          let expr = parseExprBlock(~first=e, p)
          Parser.expect(Rbrace, p)
          let loc = mkLoc(startPos, p.prevEndPos)
          let braces = makeBracesAttr(loc)
          {...expr, pexp_attributes: list{braces, ...expr.pexp_attributes}}
        | Rbrace =>
          Parser.next(p)
          let loc = mkLoc(startPos, p.prevEndPos)
          let braces = makeBracesAttr(loc)
          {...e, pexp_attributes: list{braces, ...e.pexp_attributes}}
        | _ =>
          let expr = parseExprBlock(~first=e, p)
          Parser.expect(Rbrace, p)
          let loc = mkLoc(startPos, p.prevEndPos)
          let braces = makeBracesAttr(loc)
          {...expr, pexp_attributes: list{braces, ...expr.pexp_attributes}}
        }
      }
    | _ =>
      Parser.leaveBreadcrumb(p, Grammar.ExprBlock)
      let a = parsePrimaryExpr(~operand=valueOrConstructor, p)
      let e = parseBinaryExpr(~a, p, 1)
      let e = parseTernaryExpr(e, p)
      Parser.eatBreadcrumb(p)
      switch p.Parser.token {
      | Semicolon =>
        let expr = parseExprBlock(~first=e, p)
        Parser.expect(Rbrace, p)
        let loc = mkLoc(startPos, p.prevEndPos)
        let braces = makeBracesAttr(loc)
        {...expr, pexp_attributes: list{braces, ...expr.pexp_attributes}}
      | Rbrace =>
        Parser.next(p)
        let loc = mkLoc(startPos, p.prevEndPos)
        let braces = makeBracesAttr(loc)
        {...e, pexp_attributes: list{braces, ...e.pexp_attributes}}
      | _ =>
        let expr = parseExprBlock(~first=e, p)
        Parser.expect(Rbrace, p)
        let loc = mkLoc(startPos, p.prevEndPos)
        let braces = makeBracesAttr(loc)
        {...expr, pexp_attributes: list{braces, ...expr.pexp_attributes}}
      }
    }
  | _ =>
    let expr = parseExprBlock(p)
    Parser.expect(Rbrace, p)
    let loc = mkLoc(startPos, p.prevEndPos)
    let braces = makeBracesAttr(loc)
    {...expr, pexp_attributes: list{braces, ...expr.pexp_attributes}}
  }
}

and parseRecordRowWithStringKey = p =>
  switch p.Parser.token {
  | String(s) =>
    let loc = mkLoc(p.startPos, p.endPos)
    Parser.next(p)
    let field = Location.mkloc(Longident.Lident(s), loc)
    switch p.Parser.token {
    | Colon =>
      Parser.next(p)
      let fieldExpr = parseExpr(p)
      Some(field, fieldExpr)
    | _ => Some(field, Ast_helper.Exp.ident(~loc=field.loc, field))
    }
  | _ => None
  }

and parseRecordRow = p => {
  let () = switch p.Parser.token {
  | Token.DotDotDot =>
    Parser.err(p, Diagnostics.message(ErrorMessages.recordExprSpread))
    Parser.next(p)
  | _ => ()
  }

  switch p.Parser.token {
  | Lident(_) | Uident(_) =>
    let startToken = p.token
    let field = parseValuePath(p)
    switch p.Parser.token {
    | Colon =>
      Parser.next(p)
      let fieldExpr = parseExpr(p)
      Some(field, fieldExpr)
    | _ =>
      let value = Ast_helper.Exp.ident(~loc=field.loc, field)
      let value = switch startToken {
      | Uident(_) => removeModuleNameFromPunnedFieldValue(value)
      | _ => value
      }

      Some(field, value)
    }
  | _ => None
  }
}

and parseRecordExprWithStringKeys = (~startPos, firstRow, p) => {
  let rows = list{
    firstRow,
    ...parseCommaDelimitedRegion(
      ~grammar=Grammar.RecordRowsStringKey,
      ~closing=Rbrace,
      ~f=parseRecordRowWithStringKey,
      p,
    ),
  }
  let loc = mkLoc(startPos, p.endPos)
  let recordStrExpr = Ast_helper.Str.eval(~loc, Ast_helper.Exp.record(~loc, rows, None))
  Ast_helper.Exp.extension(~loc, (Location.mkloc("obj", loc), Parsetree.PStr(list{recordStrExpr})))
}

and parseRecordExpr = (~startPos, ~spread=None, rows, p) => {
  let exprs = parseCommaDelimitedRegion(
    ~grammar=Grammar.RecordRows,
    ~closing=Rbrace,
    ~f=parseRecordRow,
    p,
  )

  let rows = List.concat(list{rows, exprs})
  let () = switch rows {
  | list{} =>
    let msg = "Record spread needs at least one field that's updated"
    Parser.err(p, Diagnostics.message(msg))
  | _rows => ()
  }

  let loc = mkLoc(startPos, p.endPos)
  Ast_helper.Exp.record(~loc, rows, spread)
}

and parseNewlineOrSemicolonExprBlock = p =>
  switch p.Parser.token {
  | Semicolon => Parser.next(p)
  | token if Grammar.isBlockExprStart(token) =>
    if p.prevEndPos.pos_lnum < p.startPos.pos_lnum {
      ()
    } else {
      Parser.err(
        ~startPos=p.prevEndPos,
        ~endPos=p.endPos,
        p,
        Diagnostics.message(
          "consecutive expressions on a line must be separated by ';' or a newline",
        ),
      )
    }
  | _ => ()
  }

and parseExprBlockItem = p => {
  let startPos = p.Parser.startPos
  let attrs = parseAttributes(p)
  switch p.Parser.token {
  | Module =>
    Parser.next(p)
    switch p.token {
    | Lparen =>
      let expr = parseFirstClassModuleExpr(~startPos, p)
      let a = parsePrimaryExpr(~operand=expr, p)
      let expr = parseBinaryExpr(~a, p, 1)
      parseTernaryExpr(expr, p)
    | _ =>
      let name = switch p.Parser.token {
      | Uident(ident) =>
        let loc = mkLoc(p.startPos, p.endPos)
        Parser.next(p)
        Location.mkloc(ident, loc)
      | t =>
        Parser.err(p, Diagnostics.uident(t))
        Location.mknoloc("_")
      }

      let body = parseModuleBindingBody(p)
      parseNewlineOrSemicolonExprBlock(p)
      let expr = parseExprBlock(p)
      let loc = mkLoc(startPos, p.prevEndPos)
      Ast_helper.Exp.letmodule(~loc, name, body, expr)
    }
  | Exception =>
    let extensionConstructor = parseExceptionDef(~attrs, p)
    parseNewlineOrSemicolonExprBlock(p)
    let blockExpr = parseExprBlock(p)
    let loc = mkLoc(startPos, p.prevEndPos)
    Ast_helper.Exp.letexception(~loc, extensionConstructor, blockExpr)
  | Open =>
    let od = parseOpenDescription(~attrs, p)
    parseNewlineOrSemicolonExprBlock(p)
    let blockExpr = parseExprBlock(p)
    let loc = mkLoc(startPos, p.prevEndPos)
    Ast_helper.Exp.open_(~loc, od.popen_override, od.popen_lid, blockExpr)
  | Let =>
    let (recFlag, letBindings) = parseLetBindings(~attrs, p)
    parseNewlineOrSemicolonExprBlock(p)
    let next = if Grammar.isBlockExprStart(p.Parser.token) {
      parseExprBlock(p)
    } else {
      let loc = mkLoc(p.startPos, p.endPos)
      Ast_helper.Exp.construct(~loc, Location.mkloc(Longident.Lident("()"), loc), None)
    }

    let loc = mkLoc(startPos, p.prevEndPos)
    Ast_helper.Exp.let_(~loc, recFlag, letBindings, next)
  | _ =>
    let e1 = {
      let expr = parseExpr(p)
      {...expr, pexp_attributes: List.concat(list{attrs, expr.pexp_attributes})}
    }

    parseNewlineOrSemicolonExprBlock(p)
    if Grammar.isBlockExprStart(p.Parser.token) {
      let e2 = parseExprBlock(p)
      let loc = {...e1.pexp_loc, loc_end: e2.pexp_loc.loc_end}
      Ast_helper.Exp.sequence(~loc, e1, e2)
    } else {
      e1
    }
  }
}

/* blockExpr ::= expr
 *            |  expr          ;
 *            |  expr          ; blockExpr
 *            |  module    ... ; blockExpr
 *            |  open      ... ; blockExpr
 *            |  exception ... ; blockExpr
 *            |  let       ...
 *            |  let       ... ;
 *            |  let       ... ; blockExpr
 *
 *  note: semi should be made optional
 *  a block of expression is always
 */
and parseExprBlock = (~first=?, p) => {
  Parser.leaveBreadcrumb(p, Grammar.ExprBlock)
  let item = switch first {
  | Some(e) => e
  | None => parseExprBlockItem(p)
  }

  parseNewlineOrSemicolonExprBlock(p)
  let blockExpr = if Grammar.isBlockExprStart(p.Parser.token) {
    let next = parseExprBlockItem(p)
    let loc = {...item.pexp_loc, loc_end: next.pexp_loc.loc_end}
    Ast_helper.Exp.sequence(~loc, item, next)
  } else {
    item
  }

  Parser.eatBreadcrumb(p)
  overParseConstrainedOrCoercedOrArrowExpression(p, blockExpr)
}

and parseTryExpression = p => {
  let startPos = p.Parser.startPos
  Parser.expect(Try, p)
  let expr = parseExpr(~context=WhenExpr, p)
  Parser.expect(Res_token.catch, p)
  Parser.expect(Lbrace, p)
  let cases = parsePatternMatching(p)
  Parser.expect(Rbrace, p)
  let loc = mkLoc(startPos, p.prevEndPos)
  Ast_helper.Exp.try_(~loc, expr, cases)
}

and parseIfCondition = p => {
  Parser.leaveBreadcrumb(p, Grammar.IfCondition)
  /* doesn't make sense to try es6 arrow here? */
  let conditionExpr = parseExpr(~context=WhenExpr, p)
  Parser.eatBreadcrumb(p)
  conditionExpr
}

and parseThenBranch = p => {
  Parser.leaveBreadcrumb(p, IfBranch)
  Parser.expect(Lbrace, p)
  let thenExpr = parseExprBlock(p)
  Parser.expect(Rbrace, p)
  Parser.eatBreadcrumb(p)
  thenExpr
}

and parseElseBranch = p => {
  Parser.expect(Lbrace, p)
  let blockExpr = parseExprBlock(p)
  Parser.expect(Rbrace, p)
  blockExpr
}

and parseIfExpr = (startPos, p) => {
  let conditionExpr = parseIfCondition(p)
  let thenExpr = parseThenBranch(p)
  let elseExpr = switch p.Parser.token {
  | Else =>
    Parser.endRegion(p)
    Parser.leaveBreadcrumb(p, Grammar.ElseBranch)
    Parser.next(p)
    Parser.beginRegion(p)
    let elseExpr = switch p.token {
    | If => parseIfOrIfLetExpression(p)
    | _ => parseElseBranch(p)
    }

    Parser.eatBreadcrumb(p)
    Parser.endRegion(p)
    Some(elseExpr)
  | _ =>
    Parser.endRegion(p)
    None
  }

  let loc = mkLoc(startPos, p.prevEndPos)
  Ast_helper.Exp.ifthenelse(~loc, conditionExpr, thenExpr, elseExpr)
}

and parseIfLetExpr = (startPos, p) => {
  let pattern = parsePattern(p)
  Parser.expect(Equal, p)
  let conditionExpr = parseIfCondition(p)
  let thenExpr = parseThenBranch(p)
  let elseExpr = switch p.Parser.token {
  | Else =>
    Parser.endRegion(p)
    Parser.leaveBreadcrumb(p, Grammar.ElseBranch)
    Parser.next(p)
    Parser.beginRegion(p)
    let elseExpr = switch p.token {
    | If => parseIfOrIfLetExpression(p)
    | _ => parseElseBranch(p)
    }

    Parser.eatBreadcrumb(p)
    Parser.endRegion(p)
    elseExpr
  | _ =>
    Parser.endRegion(p)
    let startPos = p.Parser.startPos
    let loc = mkLoc(startPos, p.prevEndPos)
    Ast_helper.Exp.construct(~loc, Location.mkloc(Longident.Lident("()"), loc), None)
  }

  let loc = mkLoc(startPos, p.prevEndPos)
  Ast_helper.Exp.match_(
    ~attrs=list{ifLetAttr, suppressFragileMatchWarningAttr},
    ~loc,
    conditionExpr,
    list{
      Ast_helper.Exp.case(pattern, thenExpr),
      Ast_helper.Exp.case(Ast_helper.Pat.any(), elseExpr),
    },
  )
}

and parseIfOrIfLetExpression = p => {
  Parser.beginRegion(p)
  Parser.leaveBreadcrumb(p, Grammar.ExprIf)
  let startPos = p.Parser.startPos
  Parser.expect(If, p)
  let expr = switch p.Parser.token {
  | Let =>
    Parser.next(p)
    let ifLetExpr = parseIfLetExpr(startPos, p)
    Parser.err(
      ~startPos=ifLetExpr.pexp_loc.loc_start,
      ~endPos=ifLetExpr.pexp_loc.loc_end,
      p,
      Diagnostics.message(ErrorMessages.experimentalIfLet(ifLetExpr)),
    )
    ifLetExpr
  | _ => parseIfExpr(startPos, p)
  }

  Parser.eatBreadcrumb(p)
  expr
}

and parseForRest = (hasOpeningParen, pattern, startPos, p) => {
  Parser.expect(In, p)
  let e1 = parseExpr(p)
  let direction = switch p.Parser.token {
  | Lident("to") => Asttypes.Upto
  | Lident("downto") => Asttypes.Downto
  | token =>
    Parser.err(p, Diagnostics.unexpected(token, p.breadcrumbs))
    Asttypes.Upto
  }

  Parser.next(p)
  let e2 = parseExpr(~context=WhenExpr, p)
  if hasOpeningParen {
    Parser.expect(Rparen, p)
  }
  Parser.expect(Lbrace, p)
  let bodyExpr = parseExprBlock(p)
  Parser.expect(Rbrace, p)
  let loc = mkLoc(startPos, p.prevEndPos)
  Ast_helper.Exp.for_(~loc, pattern, e1, e2, direction, bodyExpr)
}

and parseForExpression = p => {
  let startPos = p.Parser.startPos
  Parser.leaveBreadcrumb(p, Grammar.ExprFor)
  Parser.expect(For, p)
  Parser.beginRegion(p)
  let forExpr = switch p.token {
  | Lparen =>
    let lparen = p.startPos
    Parser.next(p)
    switch p.token {
    | Rparen =>
      Parser.next(p)
      let unitPattern = {
        let loc = mkLoc(lparen, p.prevEndPos)
        let lid = Location.mkloc(Longident.Lident("()"), loc)
        Ast_helper.Pat.construct(lid, None)
      }

      parseForRest(false, parseAliasPattern(~attrs=list{}, unitPattern, p), startPos, p)
    | _ =>
      Parser.leaveBreadcrumb(p, Grammar.Pattern)
      let pat = parsePattern(p)
      Parser.eatBreadcrumb(p)
      switch p.token {
      | Comma =>
        Parser.next(p)
        let tuplePattern = parseTuplePattern(~attrs=list{}, ~startPos=lparen, ~first=pat, p)

        let pattern = parseAliasPattern(~attrs=list{}, tuplePattern, p)
        parseForRest(false, pattern, startPos, p)
      | _ => parseForRest(true, pat, startPos, p)
      }
    }
  | _ =>
    Parser.leaveBreadcrumb(p, Grammar.Pattern)
    let pat = parsePattern(p)
    Parser.eatBreadcrumb(p)
    parseForRest(false, pat, startPos, p)
  }

  Parser.eatBreadcrumb(p)
  Parser.endRegion(p)
  forExpr
}

and parseWhileExpression = p => {
  let startPos = p.Parser.startPos
  Parser.expect(While, p)
  let expr1 = parseExpr(~context=WhenExpr, p)
  Parser.expect(Lbrace, p)
  let expr2 = parseExprBlock(p)
  Parser.expect(Rbrace, p)
  let loc = mkLoc(startPos, p.prevEndPos)
  Ast_helper.Exp.while_(~loc, expr1, expr2)
}

and parsePatternGuard = p =>
  switch p.Parser.token {
  | When | If =>
    Parser.next(p)
    Some(parseExpr(~context=WhenExpr, p))
  | _ => None
  }

and parsePatternMatchCase = p => {
  Parser.beginRegion(p)
  Parser.leaveBreadcrumb(p, Grammar.PatternMatchCase)
  switch p.Parser.token {
  | Token.Bar =>
    Parser.next(p)
    Parser.leaveBreadcrumb(p, Grammar.Pattern)
    let lhs = parsePattern(p)
    Parser.eatBreadcrumb(p)
    let guard = parsePatternGuard(p)
    let () = switch p.token {
    | EqualGreater => Parser.next(p)
    | _ => Recover.recoverEqualGreater(p)
    }

    let rhs = parseExprBlock(p)
    Parser.endRegion(p)
    Parser.eatBreadcrumb(p)
    Some(Ast_helper.Exp.case(lhs, ~guard?, rhs))
  | _ =>
    Parser.endRegion(p)
    Parser.eatBreadcrumb(p)
    None
  }
}

and parsePatternMatching = p => {
  let cases = parseDelimitedRegion(
    ~grammar=Grammar.PatternMatching,
    ~closing=Rbrace,
    ~f=parsePatternMatchCase,
    p,
  )

  let () = switch cases {
  | list{} =>
    Parser.err(
      ~startPos=p.prevEndPos,
      p,
      Diagnostics.message("Pattern matching needs at least one case"),
    )
  | _ => ()
  }

  cases
}

and parseSwitchExpression = p => {
  let startPos = p.Parser.startPos
  Parser.expect(Switch, p)
  let switchExpr = parseExpr(~context=WhenExpr, p)
  Parser.expect(Lbrace, p)
  let cases = parsePatternMatching(p)
  Parser.expect(Rbrace, p)
  let loc = mkLoc(startPos, p.prevEndPos)
  Ast_helper.Exp.match_(~loc, switchExpr, cases)
}

/*
 * argument ::=
 *   | _                            (* syntax sugar *)
 *   | expr
 *   | expr : type
 *   | ~ label-name
 *   | ~ label-name
 *   | ~ label-name ?
 *   | ~ label-name =   expr
 *   | ~ label-name =   _           (* syntax sugar *)
 *   | ~ label-name =   expr : type
 *   | ~ label-name = ? expr
 *   | ~ label-name = ? _           (* syntax sugar *)
 *   | ~ label-name = ? expr : type
 *
 *  uncurried_argument ::=
 *   | . argument
 */
and parseArgument = p =>
  if (
    p.Parser.token == Token.Tilde ||
      (p.token == Dot ||
      (p.token == Underscore || Grammar.isExprStart(p.token)))
  ) {
    switch p.Parser.token {
    | Dot =>
      let uncurried = true
      Parser.next(p)
      switch p.token {
      /* apply(.) */
      | Rparen =>
        let unitExpr = Ast_helper.Exp.construct(Location.mknoloc(Longident.Lident("()")), None)

        Some(uncurried, Asttypes.Nolabel, unitExpr)
      | _ => parseArgument2(p, ~uncurried)
      }
    | _ => parseArgument2(p, ~uncurried=false)
    }
  } else {
    None
  }

and parseArgument2 = (p, ~uncurried) =>
  switch p.Parser.token {
  /* foo(_), do not confuse with foo(_ => x), TODO: performance */
  | Underscore if !isEs6ArrowExpression(~inTernary=false, p) =>
    let loc = mkLoc(p.startPos, p.endPos)
    Parser.next(p)
    let exp = Ast_helper.Exp.ident(~loc, Location.mkloc(Longident.Lident("_"), loc))
    Some(uncurried, Asttypes.Nolabel, exp)
  | Tilde =>
    Parser.next(p)
    /* TODO: nesting of pattern matches not intuitive for error recovery */
    switch p.Parser.token {
    | Lident(ident) =>
      let startPos = p.startPos
      Parser.next(p)
      let endPos = p.prevEndPos
      let loc = mkLoc(startPos, endPos)
      let propLocAttr = (Location.mkloc("ns.namedArgLoc", loc), Parsetree.PStr(list{}))
      let identExpr = Ast_helper.Exp.ident(
        ~attrs=list{propLocAttr},
        ~loc,
        Location.mkloc(Longident.Lident(ident), loc),
      )
      switch p.Parser.token {
      | Question =>
        Parser.next(p)
        Some(uncurried, Asttypes.Optional(ident), identExpr)
      | Equal =>
        Parser.next(p)
        let label = switch p.Parser.token {
        | Question =>
          Parser.next(p)
          Asttypes.Optional(ident)
        | _ => Labelled(ident)
        }

        let expr = switch p.Parser.token {
        | Underscore if !isEs6ArrowExpression(~inTernary=false, p) =>
          let loc = mkLoc(p.startPos, p.endPos)
          Parser.next(p)
          Ast_helper.Exp.ident(~loc, Location.mkloc(Longident.Lident("_"), loc))
        | _ =>
          let expr = parseConstrainedOrCoercedExpr(p)
          {...expr, pexp_attributes: list{propLocAttr, ...expr.pexp_attributes}}
        }

        Some(uncurried, label, expr)
      | Colon =>
        Parser.next(p)
        let typ = parseTypExpr(p)
        let loc = mkLoc(startPos, p.prevEndPos)
        let expr = Ast_helper.Exp.constraint_(~attrs=list{propLocAttr}, ~loc, identExpr, typ)
        Some(uncurried, Labelled(ident), expr)
      | _ => Some(uncurried, Labelled(ident), identExpr)
      }
    | t =>
      Parser.err(p, Diagnostics.lident(t))
      Some(uncurried, Nolabel, Recover.defaultExpr())
    }
  | _ => Some(uncurried, Nolabel, parseConstrainedOrCoercedExpr(p))
  }

and parseCallExpr = (p, funExpr) => {
  Parser.expect(Lparen, p)
  let startPos = p.Parser.startPos
  Parser.leaveBreadcrumb(p, Grammar.ExprCall)
  let args = parseCommaDelimitedRegion(
    ~grammar=Grammar.ArgumentList,
    ~closing=Rparen,
    ~f=parseArgument,
    p,
  )

  Parser.expect(Rparen, p)
  let args = switch args {
  | list{} =>
    let loc = mkLoc(startPos, p.prevEndPos)
    /* No args -> unit sugar: `foo()` */
    list{
      (
        false,
        Asttypes.Nolabel,
        Ast_helper.Exp.construct(~loc, Location.mkloc(Longident.Lident("()"), loc), None),
      ),
    }
  | list{(
      true,
      Asttypes.Nolabel,
      {
        pexp_desc: Pexp_construct({txt: Longident.Lident("()")}, None),
        pexp_loc: loc,
        pexp_attributes: list{},
      } as expr,
    )}
    if !loc.loc_ghost &&
    p.mode == ParseForTypeChecker => /* Since there is no syntax space for arity zero vs arity one,
     *  we expand
     *    `fn(. ())` into
     *    `fn(. {let __res_unit = (); __res_unit})`
     *  when the parsetree is intended for type checking
     *
     *  Note:
     *    `fn(.)` is treated as zero arity application.
     *  The invisible unit expression here has loc_ghost === true
     *
     *  Related: https://github.com/rescript-lang/syntax/issues/138
     */
    list{
      (
        true,
        Asttypes.Nolabel,
        Ast_helper.Exp.let_(
          Asttypes.Nonrecursive,
          list{Ast_helper.Vb.mk(Ast_helper.Pat.var(Location.mknoloc("__res_unit")), expr)},
          Ast_helper.Exp.ident(Location.mknoloc(Longident.Lident("__res_unit"))),
        ),
      ),
    }
  | args => args
  }

  let loc = {...funExpr.pexp_loc, loc_end: p.prevEndPos}
  let args = switch args {
  | list{(u, lbl, expr), ...args} =>
    let group = ((grp, acc), (uncurried, lbl, expr)) => {
      let (_u, grp) = grp
      if uncurried === true {
        ((true, list{(lbl, expr)}), list{(_u, List.rev(grp)), ...acc})
      } else {
        ((_u, list{(lbl, expr), ...grp}), acc)
      }
    }

    let ((_u, grp), acc) = List.fold_left(group, ((u, list{(lbl, expr)}), list{}), args)
    List.rev(list{(_u, List.rev(grp)), ...acc})
  | list{} => list{}
  }

  let apply = List.fold_left((callBody, group) => {
    let (uncurried, args) = group
    let (args, wrap) = processUnderscoreApplication(args)
    let exp = if uncurried {
      let attrs = list{uncurryAttr}
      Ast_helper.Exp.apply(~loc, ~attrs, callBody, args)
    } else {
      Ast_helper.Exp.apply(~loc, callBody, args)
    }

    wrap(exp)
  }, funExpr, args)

  Parser.eatBreadcrumb(p)
  apply
}

and parseValueOrConstructor = p => {
  let startPos = p.Parser.startPos
  let rec aux = (p, acc) =>
    switch p.Parser.token {
    | Uident(ident) =>
      let endPosLident = p.endPos
      Parser.next(p)
      switch p.Parser.token {
      | Dot =>
        Parser.next(p)
        aux(p, list{ident, ...acc})
      | Lparen if p.prevEndPos.pos_lnum === p.startPos.pos_lnum =>
        let lparen = p.startPos
        let args = parseConstructorArgs(p)
        let rparen = p.prevEndPos
        let lident = buildLongident(list{ident, ...acc})
        let tail = switch args {
        | list{} => None
        | list{{Parsetree.pexp_desc: Pexp_tuple(_)} as arg} as args =>
          let loc = mkLoc(lparen, rparen)
          if p.mode == ParseForTypeChecker {
            /* Some(1, 2) for type-checker */
            Some(arg)
          } else {
            /* Some((1, 2)) for printer */
            Some(Ast_helper.Exp.tuple(~loc, args))
          }
        | list{arg} => Some(arg)
        | args =>
          let loc = mkLoc(lparen, rparen)
          Some(Ast_helper.Exp.tuple(~loc, args))
        }

        let loc = mkLoc(startPos, p.prevEndPos)
        let identLoc = mkLoc(startPos, endPosLident)
        Ast_helper.Exp.construct(~loc, Location.mkloc(lident, identLoc), tail)
      | _ =>
        let loc = mkLoc(startPos, p.prevEndPos)
        let lident = buildLongident(list{ident, ...acc})
        Ast_helper.Exp.construct(~loc, Location.mkloc(lident, loc), None)
      }
    | Lident(ident) =>
      Parser.next(p)
      let loc = mkLoc(startPos, p.prevEndPos)
      let lident = buildLongident(list{ident, ...acc})
      Ast_helper.Exp.ident(~loc, Location.mkloc(lident, loc))
    | token =>
      if acc == list{} {
        Parser.next(p)
        Parser.err(p, Diagnostics.unexpected(token, p.breadcrumbs))
        Recover.defaultExpr()
      } else {
        let loc = mkLoc(startPos, p.prevEndPos)
        Parser.err(p, Diagnostics.unexpected(token, p.breadcrumbs))
        let lident = buildLongident(list{"_", ...acc})
        Ast_helper.Exp.ident(~loc, Location.mkloc(lident, loc))
      }
    }

  aux(p, list{})
}

and parsePolyVariantExpr = p => {
  let startPos = p.startPos
  let (ident, _loc) = parseHashIdent(~startPos, p)
  switch p.Parser.token {
  | Lparen if p.prevEndPos.pos_lnum === p.startPos.pos_lnum =>
    let lparen = p.startPos
    let args = parseConstructorArgs(p)
    let rparen = p.prevEndPos
    let loc_paren = mkLoc(lparen, rparen)
    let tail = switch args {
    | list{} => None
    | list{{Parsetree.pexp_desc: Pexp_tuple(_)} as expr} as args =>
      if p.mode == ParseForTypeChecker {
        /* #a(1, 2) for type-checker */
        Some(expr)
      } else {
        /* #a((1, 2)) for type-checker */
        Some(Ast_helper.Exp.tuple(~loc=loc_paren, args))
      }
    | list{arg} => Some(arg)
    | args =>
      /* #a((1, 2)) for printer */
      Some(Ast_helper.Exp.tuple(~loc=loc_paren, args))
    }

    let loc = mkLoc(startPos, p.prevEndPos)
    Ast_helper.Exp.variant(~loc, ident, tail)
  | _ =>
    let loc = mkLoc(startPos, p.prevEndPos)
    Ast_helper.Exp.variant(~loc, ident, None)
  }
}

and parseConstructorArgs = p => {
  let lparen = p.Parser.startPos
  Parser.expect(Lparen, p)
  let args = parseCommaDelimitedRegion(
    ~grammar=Grammar.ExprList,
    ~f=parseConstrainedExprRegion,
    ~closing=Rparen,
    p,
  )

  Parser.expect(Rparen, p)
  switch args {
  | list{} =>
    let loc = mkLoc(lparen, p.prevEndPos)
    list{Ast_helper.Exp.construct(~loc, Location.mkloc(Longident.Lident("()"), loc), None)}
  | args => args
  }
}

and parseTupleExpr = (~first, ~startPos, p) => {
  let exprs = list{
    first,
    ...parseCommaDelimitedRegion(
      p,
      ~grammar=Grammar.ExprList,
      ~closing=Rparen,
      ~f=parseConstrainedExprRegion,
    ),
  }

  Parser.expect(Rparen, p)
  let () = switch exprs {
  | list{_} =>
    Parser.err(
      ~startPos,
      ~endPos=p.prevEndPos,
      p,
      Diagnostics.message(ErrorMessages.tupleSingleElement),
    )
  | _ => ()
  }

  let loc = mkLoc(startPos, p.prevEndPos)
  Ast_helper.Exp.tuple(~loc, exprs)
}

and parseSpreadExprRegion = p =>
  switch p.Parser.token {
  | DotDotDot =>
    Parser.next(p)
    let expr = parseConstrainedOrCoercedExpr(p)
    Some(true, expr)
  | token if Grammar.isExprStart(token) => Some(false, parseConstrainedOrCoercedExpr(p))
  | _ => None
  }

and parseListExpr = (~startPos, p) => {
  let listExprs = parseCommaDelimitedReversedList(
    p,
    ~grammar=Grammar.ListExpr,
    ~closing=Rbrace,
    ~f=parseSpreadExprRegion,
  )

  Parser.expect(Rbrace, p)
  let loc = mkLoc(startPos, p.prevEndPos)
  switch listExprs {
  | list{(true, expr), ...exprs} =>
    let exprs = exprs |> List.map(snd) |> List.rev
    makeListExpression(loc, exprs, Some(expr))
  | exprs =>
    let exprs =
      exprs
      |> List.map(((spread, expr)) => {
        if spread {
          Parser.err(p, Diagnostics.message(ErrorMessages.listExprSpread))
        }
        expr
      })
      |> List.rev

    makeListExpression(loc, exprs, None)
  }
}

/* Overparse ... and give a nice error message */
and parseNonSpreadExp = (~msg, p) => {
  let () = switch p.Parser.token {
  | DotDotDot =>
    Parser.err(p, Diagnostics.message(msg))
    Parser.next(p)
  | _ => ()
  }

  switch p.Parser.token {
  | token if Grammar.isExprStart(token) =>
    let expr = parseExpr(p)
    switch p.Parser.token {
    | Colon =>
      Parser.next(p)
      let typ = parseTypExpr(p)
      let loc = mkLoc(expr.pexp_loc.loc_start, typ.ptyp_loc.loc_end)
      Some(Ast_helper.Exp.constraint_(~loc, expr, typ))
    | _ => Some(expr)
    }
  | _ => None
  }
}

and parseArrayExp = p => {
  let startPos = p.Parser.startPos
  Parser.expect(Lbracket, p)
  let exprs = parseCommaDelimitedRegion(
    p,
    ~grammar=Grammar.ExprList,
    ~closing=Rbracket,
    ~f=parseNonSpreadExp(~msg=ErrorMessages.arrayExprSpread),
  )

  Parser.expect(Rbracket, p)
  Ast_helper.Exp.array(~loc=mkLoc(startPos, p.prevEndPos), exprs)
}

/* TODO: check attributes in the case of poly type vars,
 * might be context dependend: parseFieldDeclaration (see ocaml) */
and parsePolyTypeExpr = p => {
  let startPos = p.Parser.startPos
  switch p.Parser.token {
  | SingleQuote =>
    let vars = parseTypeVarList(p)
    switch vars {
    | list{_v1, _v2, ..._} =>
      Parser.expect(Dot, p)
      let typ = parseTypExpr(p)
      let loc = mkLoc(startPos, p.prevEndPos)
      Ast_helper.Typ.poly(~loc, vars, typ)
    | list{var} =>
      switch p.Parser.token {
      | Dot =>
        Parser.next(p)
        let typ = parseTypExpr(p)
        let loc = mkLoc(startPos, p.prevEndPos)
        Ast_helper.Typ.poly(~loc, vars, typ)
      | EqualGreater =>
        Parser.next(p)
        let typ = Ast_helper.Typ.var(~loc=var.loc, var.txt)
        let returnType = parseTypExpr(~alias=false, p)
        let loc = mkLoc(typ.Parsetree.ptyp_loc.loc_start, p.prevEndPos)
        Ast_helper.Typ.arrow(~loc, Asttypes.Nolabel, typ, returnType)
      | _ => Ast_helper.Typ.var(~loc=var.loc, var.txt)
      }
    | _ => assert false
    }
  | _ => parseTypExpr(p)
  }
}

/* 'a 'b 'c */
and parseTypeVarList = p => {
  let rec loop = (p, vars) =>
    switch p.Parser.token {
    | SingleQuote =>
      Parser.next(p)
      let (lident, loc) = parseLident(p)
      let var = Location.mkloc(lident, loc)
      loop(p, list{var, ...vars})
    | _ => List.rev(vars)
    }

  loop(p, list{})
}

and parseLidentList = p => {
  let rec loop = (p, ls) =>
    switch p.Parser.token {
    | Lident(lident) =>
      let loc = mkLoc(p.startPos, p.endPos)
      Parser.next(p)
      loop(p, list{Location.mkloc(lident, loc), ...ls})
    | _ => List.rev(ls)
    }

  loop(p, list{})
}

and parseAtomicTypExpr = (~attrs, p) => {
  Parser.leaveBreadcrumb(p, Grammar.AtomicTypExpr)
  let startPos = p.Parser.startPos
  let typ = switch p.Parser.token {
  | SingleQuote =>
    Parser.next(p)
    let (ident, loc) = parseIdent(~msg=ErrorMessages.typeVar, ~startPos=p.startPos, p)
    Ast_helper.Typ.var(~loc, ~attrs, ident)
  | Underscore =>
    let endPos = p.endPos
    Parser.next(p)
    Ast_helper.Typ.any(~loc=mkLoc(startPos, endPos), ~attrs, ())
  | Lparen =>
    Parser.next(p)
    switch p.Parser.token {
    | Rparen =>
      Parser.next(p)
      let loc = mkLoc(startPos, p.prevEndPos)
      let unitConstr = Location.mkloc(Longident.Lident("unit"), loc)
      Ast_helper.Typ.constr(~attrs, unitConstr, list{})
    | _ =>
      let t = parseTypExpr(p)
      switch p.token {
      | Comma =>
        Parser.next(p)
        parseTupleType(~attrs, ~first=t, ~startPos, p)
      | _ =>
        Parser.expect(Rparen, p)
        {
          ...t,
          ptyp_loc: mkLoc(startPos, p.prevEndPos),
          ptyp_attributes: List.concat(list{attrs, t.ptyp_attributes}),
        }
      }
    }
  | Lbracket => parsePolymorphicVariantType(~attrs, p)
  | Uident(_) | Lident(_) =>
    let constr = parseValuePath(p)
    let args = parseTypeConstructorArgs(~constrName=constr, p)
    Ast_helper.Typ.constr(~loc=mkLoc(startPos, p.prevEndPos), ~attrs, constr, args)
  | Module =>
    Parser.next(p)
    Parser.expect(Lparen, p)
    let packageType = parsePackageType(~startPos, ~attrs, p)
    Parser.expect(Rparen, p)
    {...packageType, ptyp_loc: mkLoc(startPos, p.prevEndPos)}
  | Percent =>
    let extension = parseExtension(p)
    let loc = mkLoc(startPos, p.prevEndPos)
    Ast_helper.Typ.extension(~attrs, ~loc, extension)
  | Lbrace => parseRecordOrObjectType(~attrs, p)
  | token =>
    Parser.err(p, Diagnostics.unexpected(token, p.breadcrumbs))
    switch skipTokensAndMaybeRetry(p, ~isStartOfGrammar=Grammar.isAtomicTypExprStart) {
    | Some() => parseAtomicTypExpr(~attrs, p)
    | None =>
      Parser.err(~startPos=p.prevEndPos, p, Diagnostics.unexpected(token, p.breadcrumbs))
      Recover.defaultType()
    }
  }

  Parser.eatBreadcrumb(p)
  typ
}

/* package-type	::=
    | modtype-path
    ∣ modtype-path with package-constraint  { and package-constraint }
 */
and parsePackageType = (~startPos, ~attrs, p) => {
  let modTypePath = parseModuleLongIdent(~lowercase=true, p)
  switch p.Parser.token {
  | Lident("with") =>
    Parser.next(p)
    let constraints = parsePackageConstraints(p)
    let loc = mkLoc(startPos, p.prevEndPos)
    Ast_helper.Typ.package(~loc, ~attrs, modTypePath, constraints)
  | _ =>
    let loc = mkLoc(startPos, p.prevEndPos)
    Ast_helper.Typ.package(~loc, ~attrs, modTypePath, list{})
  }
}

/* package-constraint  { and package-constraint } */
and parsePackageConstraints = p => {
  let first = {
    Parser.expect(Typ, p)
    let typeConstr = parseValuePath(p)
    Parser.expect(Equal, p)
    let typ = parseTypExpr(p)
    (typeConstr, typ)
  }

  let rest = parseRegion(~grammar=Grammar.PackageConstraint, ~f=parsePackageConstraint, p)

  list{first, ...rest}
}

/* and type typeconstr = typexpr */
and parsePackageConstraint = p =>
  switch p.Parser.token {
  | And =>
    Parser.next(p)
    Parser.expect(Typ, p)
    let typeConstr = parseValuePath(p)
    Parser.expect(Equal, p)
    let typ = parseTypExpr(p)
    Some(typeConstr, typ)
  | _ => None
  }

and parseRecordOrObjectType = (~attrs, p) => {
  /* for inline record in constructor */
  let startPos = p.Parser.startPos
  Parser.expect(Lbrace, p)
  let closedFlag = switch p.token {
  | DotDot =>
    Parser.next(p)
    Asttypes.Open
  | Dot =>
    Parser.next(p)
    Asttypes.Closed
  | _ => Asttypes.Closed
  }

  let () = switch p.token {
  | Lident(_) => Parser.err(p, Diagnostics.message(ErrorMessages.forbiddenInlineRecordDeclaration))
  | _ => ()
  }

  let startFirstField = p.startPos
  let fields = parseCommaDelimitedRegion(
    ~grammar=Grammar.StringFieldDeclarations,
    ~closing=Rbrace,
    ~f=parseStringFieldDeclaration,
    p,
  )

  let () = switch fields {
  | list{Parsetree.Oinherit({ptyp_loc})} =>
    /* {...x}, spread without extra fields */
    Parser.err(
      p,
      ~startPos=startFirstField,
      ~endPos=ptyp_loc.loc_end,
      Diagnostics.message(ErrorMessages.sameTypeSpread),
    )
  | _ => ()
  }

  Parser.expect(Rbrace, p)
  let loc = mkLoc(startPos, p.prevEndPos)
  Ast_helper.Typ.object_(~loc, ~attrs, fields, closedFlag)
}

/* TODO: check associativity in combination with attributes */
and parseTypeAlias = (p, typ) =>
  switch p.Parser.token {
  | As =>
    Parser.next(p)
    Parser.expect(SingleQuote, p)
    let (ident, _loc) = parseLident(p)
    /* TODO: how do we parse attributes here? */
    Ast_helper.Typ.alias(~loc=mkLoc(typ.Parsetree.ptyp_loc.loc_start, p.prevEndPos), typ, ident)
  | _ => typ
  }

/* type_parameter ::=
 *  | type_expr
 *  | ~ident: type_expr
 *  | ~ident: type_expr=?
 *
 * note:
 *  | attrs ~ident: type_expr    -> attrs are on the arrow
 *  | attrs type_expr            -> attrs are here part of the type_expr
 *
 * uncurried_type_parameter ::=
 *  | . type_parameter
 */
and parseTypeParameter = p =>
  if p.Parser.token == Token.Tilde || (p.token == Dot || Grammar.isTypExprStart(p.token)) {
    let startPos = p.Parser.startPos
    let uncurried = Parser.optional(p, Dot)
    let attrs = parseAttributes(p)
    switch p.Parser.token {
    | Tilde =>
      Parser.next(p)
      let (name, loc) = parseLident(p)
      let lblLocAttr = (Location.mkloc("ns.namedArgLoc", loc), Parsetree.PStr(list{}))
      Parser.expect(~grammar=Grammar.TypeExpression, Colon, p)
      let typ = {
        let typ = parseTypExpr(p)
        {...typ, ptyp_attributes: list{lblLocAttr, ...typ.ptyp_attributes}}
      }

      switch p.Parser.token {
      | Equal =>
        Parser.next(p)
        Parser.expect(Question, p)
        Some(uncurried, attrs, Asttypes.Optional(name), typ, startPos)
      | _ => Some(uncurried, attrs, Asttypes.Labelled(name), typ, startPos)
      }
    | Lident(_) =>
      let (name, loc) = parseLident(p)
      switch p.token {
      | Colon =>
        let () = {
          let error = Diagnostics.message(ErrorMessages.missingTildeLabeledParameter(name))
          Parser.err(~startPos=loc.loc_start, ~endPos=loc.loc_end, p, error)
        }

        Parser.next(p)
        let typ = parseTypExpr(p)
        switch p.Parser.token {
        | Equal =>
          Parser.next(p)
          Parser.expect(Question, p)
          Some(uncurried, attrs, Asttypes.Optional(name), typ, startPos)
        | _ => Some(uncurried, attrs, Asttypes.Labelled(name), typ, startPos)
        }
      | _ =>
        let constr = Location.mkloc(Longident.Lident(name), loc)
        let args = parseTypeConstructorArgs(~constrName=constr, p)
        let typ = Ast_helper.Typ.constr(~loc=mkLoc(startPos, p.prevEndPos), ~attrs, constr, args)

        let typ = parseArrowTypeRest(~es6Arrow=true, ~startPos, typ, p)
        let typ = parseTypeAlias(p, typ)
        Some(uncurried, list{}, Asttypes.Nolabel, typ, startPos)
      }
    | _ =>
      let typ = parseTypExpr(p)
      let typWithAttributes = {
        ...typ,
        ptyp_attributes: List.concat(list{attrs, typ.ptyp_attributes}),
      }
      Some(uncurried, list{}, Asttypes.Nolabel, typWithAttributes, startPos)
    }
  } else {
    None
  }

/* (int, ~x:string, float) */
and parseTypeParameters = p => {
  let startPos = p.Parser.startPos
  Parser.expect(Lparen, p)
  switch p.Parser.token {
  | Rparen =>
    Parser.next(p)
    let loc = mkLoc(startPos, p.prevEndPos)
    let unitConstr = Location.mkloc(Longident.Lident("unit"), loc)
    let typ = Ast_helper.Typ.constr(unitConstr, list{})
    list{(false, list{}, Asttypes.Nolabel, typ, startPos)}
  | _ =>
    let params = parseCommaDelimitedRegion(
      ~grammar=Grammar.TypeParameters,
      ~closing=Rparen,
      ~f=parseTypeParameter,
      p,
    )

    Parser.expect(Rparen, p)
    params
  }
}

and parseEs6ArrowType = (~attrs, p) => {
  let startPos = p.Parser.startPos
  switch p.Parser.token {
  | Tilde =>
    Parser.next(p)
    let (name, loc) = parseLident(p)
    let lblLocAttr = (Location.mkloc("ns.namedArgLoc", loc), Parsetree.PStr(list{}))
    Parser.expect(~grammar=Grammar.TypeExpression, Colon, p)
    let typ = {
      let typ = parseTypExpr(~alias=false, ~es6Arrow=false, p)
      {...typ, ptyp_attributes: list{lblLocAttr, ...typ.ptyp_attributes}}
    }

    let arg = switch p.Parser.token {
    | Equal =>
      Parser.next(p)
      Parser.expect(Question, p)
      Asttypes.Optional(name)
    | _ => Asttypes.Labelled(name)
    }

    Parser.expect(EqualGreater, p)
    let returnType = parseTypExpr(~alias=false, p)
    let loc = mkLoc(startPos, p.prevEndPos)
    Ast_helper.Typ.arrow(~loc, ~attrs, arg, typ, returnType)
  | _ =>
    let parameters = parseTypeParameters(p)
    Parser.expect(EqualGreater, p)
    let returnType = parseTypExpr(~alias=false, p)
    let endPos = p.prevEndPos
    let typ = List.fold_right(((uncurried, attrs, argLbl, typ, startPos), t) => {
      let attrs = if uncurried {
        list{uncurryAttr, ...attrs}
      } else {
        attrs
      }
      Ast_helper.Typ.arrow(~loc=mkLoc(startPos, endPos), ~attrs, argLbl, typ, t)
    }, parameters, returnType)

    {
      ...typ,
      ptyp_attributes: List.concat(list{typ.ptyp_attributes, attrs}),
      ptyp_loc: mkLoc(startPos, p.prevEndPos),
    }
  }
}

/*
 * typexpr ::=
 *  | 'ident
 *  | _
 *  | (typexpr)
 *  | typexpr => typexpr            --> es6 arrow
 *  | (typexpr, typexpr) => typexpr --> es6 arrow
 *  | /typexpr, typexpr, typexpr/  --> tuple
 *  | typeconstr
 *  | typeconstr<typexpr>
 *  | typeconstr<typexpr, typexpr,>
 *  | typexpr as 'ident
 *  | %attr-id                      --> extension
 *  | %attr-id(payload)             --> extension
 *
 * typeconstr ::=
 *  | lident
 *  | uident.lident
 *  | uident.uident.lident     --> long module path
 */
and parseTypExpr = (~attrs=?, ~es6Arrow=true, ~alias=true, p) => {
  /* Parser.leaveBreadcrumb p Grammar.TypeExpression; */
  let startPos = p.Parser.startPos
  let attrs = switch attrs {
  | Some(attrs) => attrs
  | None => parseAttributes(p)
  }
  let typ = if es6Arrow && isEs6ArrowType(p) {
    parseEs6ArrowType(~attrs, p)
  } else {
    let typ = parseAtomicTypExpr(~attrs, p)
    parseArrowTypeRest(~es6Arrow, ~startPos, typ, p)
  }

  let typ = if alias {
    parseTypeAlias(p, typ)
  } else {
    typ
  }

  /* Parser.eatBreadcrumb p; */
  typ
}

and parseArrowTypeRest = (~es6Arrow, ~startPos, typ, p) =>
  switch p.Parser.token {
  | (EqualGreater | MinusGreater) as token if es6Arrow === true =>
    /* error recovery */
    if token == MinusGreater {
      Parser.expect(EqualGreater, p)
    }
    Parser.next(p)
    let returnType = parseTypExpr(~alias=false, p)
    let loc = mkLoc(startPos, p.prevEndPos)
    Ast_helper.Typ.arrow(~loc, Asttypes.Nolabel, typ, returnType)
  | _ => typ
  }

and parseTypExprRegion = p =>
  if Grammar.isTypExprStart(p.Parser.token) {
    Some(parseTypExpr(p))
  } else {
    None
  }

and parseTupleType = (~attrs, ~first, ~startPos, p) => {
  let typexprs = list{
    first,
    ...parseCommaDelimitedRegion(
      ~grammar=Grammar.TypExprList,
      ~closing=Rparen,
      ~f=parseTypExprRegion,
      p,
    ),
  }

  Parser.expect(Rparen, p)
  let () = switch typexprs {
  | list{_} =>
    Parser.err(
      ~startPos,
      ~endPos=p.prevEndPos,
      p,
      Diagnostics.message(ErrorMessages.tupleSingleElement),
    )
  | _ => ()
  }

  let tupleLoc = mkLoc(startPos, p.prevEndPos)
  Ast_helper.Typ.tuple(~attrs, ~loc=tupleLoc, typexprs)
}

and parseTypeConstructorArgRegion = p =>
  if Grammar.isTypExprStart(p.Parser.token) {
    Some(parseTypExpr(p))
  } else if p.token == LessThan {
    Parser.next(p)
    parseTypeConstructorArgRegion(p)
  } else {
    None
  }

/* Js.Nullable.value<'a> */
and parseTypeConstructorArgs = (~constrName, p) => {
  let opening = p.Parser.token
  let openingStartPos = p.startPos
  switch opening {
  | LessThan | Lparen =>
    Scanner.setDiamondMode(p.scanner)
    Parser.next(p)
    let typeArgs = /* TODO: change Grammar.TypExprList to TypArgList!!! Why did I wrote this? */
    parseCommaDelimitedRegion(
      ~grammar=Grammar.TypExprList,
      ~closing=GreaterThan,
      ~f=parseTypeConstructorArgRegion,
      p,
    )

    let () = switch p.token {
    | Rparen if opening == Token.Lparen =>
      let typ = Ast_helper.Typ.constr(constrName, typeArgs)
      let msg =
        Doc.breakableGroup(
          ~forceBreak=true,
          Doc.concat(list{
            Doc.text("Type parameters require angle brackets:"),
            Doc.indent(
              Doc.concat(list{Doc.line, ResPrinter.printTypExpr(typ, CommentTable.empty)}),
            ),
          }),
        ) |> Doc.toString(~width=80)

      Parser.err(~startPos=openingStartPos, p, Diagnostics.message(msg))
      Parser.next(p)
    | _ => Parser.expect(GreaterThan, p)
    }

    Scanner.popMode(p.scanner, Diamond)
    typeArgs
  | _ => list{}
  }
}

/* string-field-decl ::=
 *  | string: poly-typexpr
 *  | attributes string-field-decl */
and parseStringFieldDeclaration = p => {
  let attrs = parseAttributes(p)
  switch p.Parser.token {
  | String(name) =>
    let nameStartPos = p.startPos
    let nameEndPos = p.endPos
    Parser.next(p)
    let fieldName = Location.mkloc(name, mkLoc(nameStartPos, nameEndPos))
    Parser.expect(~grammar=Grammar.TypeExpression, Colon, p)
    let typ = parsePolyTypeExpr(p)
    Some(Parsetree.Otag(fieldName, attrs, typ))
  | DotDotDot =>
    Parser.next(p)
    let typ = parseTypExpr(p)
    Some(Parsetree.Oinherit(typ))
  | Lident(name) =>
    let nameLoc = mkLoc(p.startPos, p.endPos)
    Parser.err(p, Diagnostics.message(ErrorMessages.objectQuotedFieldName(name)))
    Parser.next(p)
    let fieldName = Location.mkloc(name, nameLoc)
    Parser.expect(~grammar=Grammar.TypeExpression, Colon, p)
    let typ = parsePolyTypeExpr(p)
    Some(Parsetree.Otag(fieldName, attrs, typ))
  | _token => None
  }
}

/* field-decl	::=
 *  | [mutable] field-name : poly-typexpr
 *  | attributes field-decl */
and parseFieldDeclaration = p => {
  let startPos = p.Parser.startPos
  let attrs = parseAttributes(p)
  let mut = if Parser.optional(p, Token.Mutable) {
    Asttypes.Mutable
  } else {
    Asttypes.Immutable
  }

  let (lident, loc) = switch p.token {
  | _ => parseLident(p)
  }

  let name = Location.mkloc(lident, loc)
  let typ = switch p.Parser.token {
  | Colon =>
    Parser.next(p)
    parsePolyTypeExpr(p)
  | _ => Ast_helper.Typ.constr(~loc=name.loc, {...name, txt: Lident(name.txt)}, list{})
  }

  let loc = mkLoc(startPos, typ.ptyp_loc.loc_end)
  Ast_helper.Type.field(~attrs, ~loc, ~mut, name, typ)
}

and parseFieldDeclarationRegion = p => {
  let startPos = p.Parser.startPos
  let attrs = parseAttributes(p)
  let mut = if Parser.optional(p, Token.Mutable) {
    Asttypes.Mutable
  } else {
    Asttypes.Immutable
  }

  switch p.token {
  | Lident(_) =>
    let (lident, loc) = parseLident(p)
    let name = Location.mkloc(lident, loc)
    let typ = switch p.Parser.token {
    | Colon =>
      Parser.next(p)
      parsePolyTypeExpr(p)
    | _ => Ast_helper.Typ.constr(~loc=name.loc, {...name, txt: Lident(name.txt)}, list{})
    }

    let loc = mkLoc(startPos, typ.ptyp_loc.loc_end)
    Some(Ast_helper.Type.field(~attrs, ~loc, ~mut, name, typ))
  | _ => None
  }
}

/* record-decl ::=
 *  | { field-decl }
 *  | { field-decl, field-decl }
 *  | { field-decl, field-decl, field-decl, }
 */
and parseRecordDeclaration = p => {
  Parser.leaveBreadcrumb(p, Grammar.RecordDecl)
  Parser.expect(Lbrace, p)
  let rows = parseCommaDelimitedRegion(
    ~grammar=Grammar.RecordDecl,
    ~closing=Rbrace,
    ~f=parseFieldDeclarationRegion,
    p,
  )

  Parser.expect(Rbrace, p)
  Parser.eatBreadcrumb(p)
  rows
}

/* constr-args ::=
 *  | (typexpr)
 *  | (typexpr, typexpr)
 *  | (typexpr, typexpr, typexpr,)
 *  | (record-decl)
 *
 * TODO: should we overparse inline-records in every position?
 * Give a good error message afterwards?
 */
and parseConstrDeclArgs = p => {
  let constrArgs = switch p.Parser.token {
  | Lparen =>
    Parser.next(p)
    /* TODO: this could use some cleanup/stratification */
    switch p.Parser.token {
    | Lbrace =>
      let lbrace = p.startPos
      Parser.next(p)
      let startPos = p.Parser.startPos
      switch p.Parser.token {
      | DotDot | Dot =>
        let closedFlag = switch p.token {
        | DotDot =>
          Parser.next(p)
          Asttypes.Open
        | Dot =>
          Parser.next(p)
          Asttypes.Closed
        | _ => Asttypes.Closed
        }

        let fields = parseCommaDelimitedRegion(
          ~grammar=Grammar.StringFieldDeclarations,
          ~closing=Rbrace,
          ~f=parseStringFieldDeclaration,
          p,
        )

        Parser.expect(Rbrace, p)
        let loc = mkLoc(startPos, p.prevEndPos)
        let typ = Ast_helper.Typ.object_(~loc, ~attrs=list{}, fields, closedFlag)
        Parser.optional(p, Comma) |> ignore
        let moreArgs = parseCommaDelimitedRegion(
          ~grammar=Grammar.TypExprList,
          ~closing=Rparen,
          ~f=parseTypExprRegion,
          p,
        )

        Parser.expect(Rparen, p)
        Parsetree.Pcstr_tuple(list{typ, ...moreArgs})
      | DotDotDot =>
        let dotdotdotStart = p.startPos
        let dotdotdotEnd = p.endPos
        /* start of object type spreading, e.g. `User({...a, "u": int})` */
        Parser.next(p)
        let typ = parseTypExpr(p)
        let () = switch p.token {
        | Rbrace =>
          /* {...x}, spread without extra fields */
          Parser.err(
            ~startPos=dotdotdotStart,
            ~endPos=dotdotdotEnd,
            p,
            Diagnostics.message(ErrorMessages.sameTypeSpread),
          )
          Parser.next(p)
        | _ => Parser.expect(Comma, p)
        }

        let () = switch p.token {
        | Lident(_) =>
          Parser.err(
            ~startPos=dotdotdotStart,
            ~endPos=dotdotdotEnd,
            p,
            Diagnostics.message(ErrorMessages.spreadInRecordDeclaration),
          )
        | _ => ()
        }

        let fields = list{
          Parsetree.Oinherit(typ),
          ...parseCommaDelimitedRegion(
            ~grammar=Grammar.StringFieldDeclarations,
            ~closing=Rbrace,
            ~f=parseStringFieldDeclaration,
            p,
          ),
        }

        Parser.expect(Rbrace, p)
        let loc = mkLoc(startPos, p.prevEndPos)
        let typ = Ast_helper.Typ.object_(~loc, fields, Asttypes.Closed) |> parseTypeAlias(p)

        let typ = parseArrowTypeRest(~es6Arrow=true, ~startPos, typ, p)
        Parser.optional(p, Comma) |> ignore
        let moreArgs = parseCommaDelimitedRegion(
          ~grammar=Grammar.TypExprList,
          ~closing=Rparen,
          ~f=parseTypExprRegion,
          p,
        )

        Parser.expect(Rparen, p)
        Parsetree.Pcstr_tuple(list{typ, ...moreArgs})
      | _ =>
        let attrs = parseAttributes(p)
        switch p.Parser.token {
        | String(_) =>
          let closedFlag = Asttypes.Closed
          let fields = switch attrs {
          | list{} =>
            parseCommaDelimitedRegion(
              ~grammar=Grammar.StringFieldDeclarations,
              ~closing=Rbrace,
              ~f=parseStringFieldDeclaration,
              p,
            )
          | attrs =>
            let first = {
              Parser.leaveBreadcrumb(p, Grammar.StringFieldDeclarations)
              let field = switch parseStringFieldDeclaration(p) {
              | Some(field) => field
              | None => assert false
              }

              /* parse comma after first */
              let () = switch p.Parser.token {
              | Rbrace | Eof => ()
              | Comma => Parser.next(p)
              | _ => Parser.expect(Comma, p)
              }

              Parser.eatBreadcrumb(p)
              switch field {
              | Parsetree.Otag(label, _, ct) => Parsetree.Otag(label, attrs, ct)
              | Oinherit(ct) => Oinherit(ct)
              }
            }

            list{
              first,
              ...parseCommaDelimitedRegion(
                ~grammar=Grammar.StringFieldDeclarations,
                ~closing=Rbrace,
                ~f=parseStringFieldDeclaration,
                p,
              ),
            }
          }
          Parser.expect(Rbrace, p)
          let loc = mkLoc(startPos, p.prevEndPos)
          let typ =
            Ast_helper.Typ.object_(~loc, ~attrs=list{}, fields, closedFlag) |> parseTypeAlias(p)

          let typ = parseArrowTypeRest(~es6Arrow=true, ~startPos, typ, p)
          Parser.optional(p, Comma) |> ignore
          let moreArgs = parseCommaDelimitedRegion(
            ~grammar=Grammar.TypExprList,
            ~closing=Rparen,
            ~f=parseTypExprRegion,
            p,
          )

          Parser.expect(Rparen, p)
          Parsetree.Pcstr_tuple(list{typ, ...moreArgs})
        | _ =>
          let fields = switch attrs {
          | list{} =>
            parseCommaDelimitedRegion(
              ~grammar=Grammar.FieldDeclarations,
              ~closing=Rbrace,
              ~f=parseFieldDeclarationRegion,
              p,
            )
          | attrs =>
            let first = {
              let field = parseFieldDeclaration(p)
              Parser.expect(Comma, p)
              {...field, Parsetree.pld_attributes: attrs}
            }

            list{
              first,
              ...parseCommaDelimitedRegion(
                ~grammar=Grammar.FieldDeclarations,
                ~closing=Rbrace,
                ~f=parseFieldDeclarationRegion,
                p,
              ),
            }
          }

          let () = switch fields {
          | list{} =>
            Parser.err(
              ~startPos=lbrace,
              p,
              Diagnostics.message("An inline record declaration needs at least one field"),
            )
          | _ => ()
          }

          Parser.expect(Rbrace, p)
          Parser.optional(p, Comma) |> ignore
          Parser.expect(Rparen, p)
          Parsetree.Pcstr_record(fields)
        }
      }
    | _ =>
      let args = parseCommaDelimitedRegion(
        ~grammar=Grammar.TypExprList,
        ~closing=Rparen,
        ~f=parseTypExprRegion,
        p,
      )

      Parser.expect(Rparen, p)
      Parsetree.Pcstr_tuple(args)
    }
  | _ => Pcstr_tuple(list{})
  }

  let res = switch p.Parser.token {
  | Colon =>
    Parser.next(p)
    Some(parseTypExpr(p))
  | _ => None
  }

  (constrArgs, res)
}

/* constr-decl ::=
 *  | constr-name
 *  | attrs constr-name
 *  | constr-name const-args
 *  | attrs constr-name const-args */
and parseTypeConstructorDeclarationWithBar = p =>
  switch p.Parser.token {
  | Bar =>
    let startPos = p.Parser.startPos
    Parser.next(p)
    Some(parseTypeConstructorDeclaration(~startPos, p))
  | _ => None
  }

and parseTypeConstructorDeclaration = (~startPos, p) => {
  Parser.leaveBreadcrumb(p, Grammar.ConstructorDeclaration)
  let attrs = parseAttributes(p)
  switch p.Parser.token {
  | Uident(uident) =>
    let uidentLoc = mkLoc(p.startPos, p.endPos)
    Parser.next(p)
    let (args, res) = parseConstrDeclArgs(p)
    Parser.eatBreadcrumb(p)
    let loc = mkLoc(startPos, p.prevEndPos)
    Ast_helper.Type.constructor(~loc, ~attrs, ~res?, ~args, Location.mkloc(uident, uidentLoc))
  | t =>
    Parser.err(p, Diagnostics.uident(t))
    Ast_helper.Type.constructor(Location.mknoloc("_"))
  }
}

/* [|] constr-decl  { | constr-decl } */
and parseTypeConstructorDeclarations = (~first=?, p) => {
  let firstConstrDecl = switch first {
  | None =>
    let startPos = p.Parser.startPos
    ignore(Parser.optional(p, Token.Bar))
    parseTypeConstructorDeclaration(~startPos, p)
  | Some(firstConstrDecl) => firstConstrDecl
  }

  list{
    firstConstrDecl,
    ...parseRegion(
      ~grammar=Grammar.ConstructorDeclaration,
      ~f=parseTypeConstructorDeclarationWithBar,
      p,
    ),
  }
}

/*
 * type-representation ::=
 *  ∣	 = [ | ] constr-decl  { | constr-decl }
 *  ∣	 = private [ | ] constr-decl  { | constr-decl }
 *  |  = |
 *  ∣	 = private |
 *  ∣	 = record-decl
 *  ∣	 = private record-decl
 *  |  = ..
 */
and parseTypeRepresentation = p => {
  Parser.leaveBreadcrumb(p, Grammar.TypeRepresentation)
  /* = consumed */
  let privateFlag = if Parser.optional(p, Token.Private) {
    Asttypes.Private
  } else {
    Asttypes.Public
  }

  let kind = switch p.Parser.token {
  | Bar | Uident(_) => Parsetree.Ptype_variant(parseTypeConstructorDeclarations(p))
  | Lbrace => Parsetree.Ptype_record(parseRecordDeclaration(p))
  | DotDot =>
    Parser.next(p)
    Ptype_open
  | token =>
    Parser.err(p, Diagnostics.unexpected(token, p.breadcrumbs))
    /* TODO: I have no idea if this is even remotely a good idea */
    Parsetree.Ptype_variant(list{})
  }

  Parser.eatBreadcrumb(p)
  (privateFlag, kind)
}

/* type-param	::=
 *  | variance 'lident
 *  | variance 'uident
 *  | variance _
 *
 * variance ::=
 *   | +
 *   | -
 *   | (* empty *)
 */
and parseTypeParam = p => {
  let variance = switch p.Parser.token {
  | Plus =>
    Parser.next(p)
    Asttypes.Covariant
  | Minus =>
    Parser.next(p)
    Contravariant
  | _ => Invariant
  }

  switch p.Parser.token {
  | SingleQuote =>
    Parser.next(p)
    let (ident, loc) = parseIdent(~msg=ErrorMessages.typeParam, ~startPos=p.startPos, p)
    Some(Ast_helper.Typ.var(~loc, ident), variance)
  | Underscore =>
    let loc = mkLoc(p.startPos, p.endPos)
    Parser.next(p)
    Some(Ast_helper.Typ.any(~loc, ()), variance)
  | (Uident(_) | Lident(_)) as token =>
    Parser.err(
      p,
      Diagnostics.message("Type params start with a singlequote: '" ++ Token.toString(token)),
    )
    let (ident, loc) = parseIdent(~msg=ErrorMessages.typeParam, ~startPos=p.startPos, p)
    Some(Ast_helper.Typ.var(~loc, ident), variance)
  | _token => None
  }
}

/* type-params	::=
 *  | <type-param>
 *  ∣	<type-param, type-param>
 *  ∣	<type-param, type-param, type-param>
 *  ∣	<type-param, type-param, type-param,>
 *
 *  TODO: when we have pretty-printer show an error
 *  with the actual code corrected. */
and parseTypeParams = (~parent, p) => {
  let opening = p.Parser.token
  switch opening {
  | LessThan | Lparen if p.startPos.pos_lnum === p.prevEndPos.pos_lnum =>
    Scanner.setDiamondMode(p.scanner)
    let openingStartPos = p.startPos
    Parser.leaveBreadcrumb(p, Grammar.TypeParams)
    Parser.next(p)
    let params = parseCommaDelimitedRegion(
      ~grammar=Grammar.TypeParams,
      ~closing=GreaterThan,
      ~f=parseTypeParam,
      p,
    )

    let () = switch p.token {
    | Rparen if opening == Token.Lparen =>
      let msg =
        Doc.breakableGroup(
          ~forceBreak=true,
          Doc.concat(list{
            Doc.text("Type parameters require angle brackets:"),
            Doc.indent(
              Doc.concat(list{
                Doc.line,
                Doc.concat(list{
                  ResPrinter.printLongident(parent.Location.txt),
                  ResPrinter.printTypeParams(params, CommentTable.empty),
                }),
              }),
            ),
          }),
        ) |> Doc.toString(~width=80)

      Parser.err(~startPos=openingStartPos, p, Diagnostics.message(msg))
      Parser.next(p)
    | _ => Parser.expect(GreaterThan, p)
    }

    Scanner.popMode(p.scanner, Diamond)
    Parser.eatBreadcrumb(p)
    params
  | _ => list{}
  }
}

/* type-constraint	::=	constraint ' ident =  typexpr */
and parseTypeConstraint = p => {
  let startPos = p.Parser.startPos
  switch p.Parser.token {
  | Token.Constraint =>
    Parser.next(p)
    Parser.expect(SingleQuote, p)
    switch p.Parser.token {
    | Lident(ident) =>
      let identLoc = mkLoc(startPos, p.endPos)
      Parser.next(p)
      Parser.expect(Equal, p)
      let typ = parseTypExpr(p)
      let loc = mkLoc(startPos, p.prevEndPos)
      Some(Ast_helper.Typ.var(~loc=identLoc, ident), typ, loc)
    | t =>
      Parser.err(p, Diagnostics.lident(t))
      let loc = mkLoc(startPos, p.prevEndPos)
      Some(Ast_helper.Typ.any(), parseTypExpr(p), loc)
    }
  | _ => None
  }
}

/* type-constraints ::=
 *  | (* empty *)
 *  | type-constraint
 *  | type-constraint type-constraint
 *  | type-constraint type-constraint type-constraint (* 0 or more *)
 */
and parseTypeConstraints = p =>
  parseRegion(~grammar=Grammar.TypeConstraint, ~f=parseTypeConstraint, p)

and parseTypeEquationOrConstrDecl = p => {
  let uidentStartPos = p.Parser.startPos
  switch p.Parser.token {
  | Uident(uident) =>
    Parser.next(p)
    switch p.Parser.token {
    | Dot =>
      Parser.next(p)
      let typeConstr = parseValuePathTail(p, uidentStartPos, Longident.Lident(uident))

      let loc = mkLoc(uidentStartPos, p.prevEndPos)
      let typ = parseTypeAlias(
        p,
        Ast_helper.Typ.constr(
          ~loc,
          typeConstr,
          parseTypeConstructorArgs(~constrName=typeConstr, p),
        ),
      )
      switch p.token {
      | Equal =>
        Parser.next(p)
        let (priv, kind) = parseTypeRepresentation(p)
        (Some(typ), priv, kind)
      | EqualGreater =>
        Parser.next(p)
        let returnType = parseTypExpr(~alias=false, p)
        let loc = mkLoc(uidentStartPos, p.prevEndPos)
        let arrowType = Ast_helper.Typ.arrow(~loc, Asttypes.Nolabel, typ, returnType)
        let typ = parseTypeAlias(p, arrowType)
        (Some(typ), Asttypes.Public, Parsetree.Ptype_abstract)
      | _ => (Some(typ), Asttypes.Public, Parsetree.Ptype_abstract)
      }
    | _ =>
      let uidentEndPos = p.prevEndPos
      let (args, res) = parseConstrDeclArgs(p)
      let first = Some({
        let uidentLoc = mkLoc(uidentStartPos, uidentEndPos)
        Ast_helper.Type.constructor(
          ~loc=mkLoc(uidentStartPos, p.prevEndPos),
          ~res?,
          ~args,
          Location.mkloc(uident, uidentLoc),
        )
      })
      (None, Asttypes.Public, Parsetree.Ptype_variant(parseTypeConstructorDeclarations(p, ~first?)))
    }
  | t =>
    Parser.err(p, Diagnostics.uident(t))
    /* TODO: is this a good idea? */
    (None, Asttypes.Public, Parsetree.Ptype_abstract)
  }
}

and parseRecordOrObjectDecl = p => {
  let startPos = p.Parser.startPos
  Parser.expect(Lbrace, p)
  switch p.Parser.token {
  | DotDot | Dot =>
    let closedFlag = switch p.token {
    | DotDot =>
      Parser.next(p)
      Asttypes.Open
    | Dot =>
      Parser.next(p)
      Asttypes.Closed
    | _ => Asttypes.Closed
    }

    let fields = parseCommaDelimitedRegion(
      ~grammar=Grammar.StringFieldDeclarations,
      ~closing=Rbrace,
      ~f=parseStringFieldDeclaration,
      p,
    )

    Parser.expect(Rbrace, p)
    let loc = mkLoc(startPos, p.prevEndPos)
    let typ = Ast_helper.Typ.object_(~loc, ~attrs=list{}, fields, closedFlag) |> parseTypeAlias(p)

    let typ = parseArrowTypeRest(~es6Arrow=true, ~startPos, typ, p)
    (Some(typ), Asttypes.Public, Parsetree.Ptype_abstract)
  | DotDotDot =>
    let dotdotdotStart = p.startPos
    let dotdotdotEnd = p.endPos
    /* start of object type spreading, e.g. `type u = {...a, "u": int}` */
    Parser.next(p)
    let typ = parseTypExpr(p)
    let () = switch p.token {
    | Rbrace =>
      /* {...x}, spread without extra fields */
      Parser.err(
        ~startPos=dotdotdotStart,
        ~endPos=dotdotdotEnd,
        p,
        Diagnostics.message(ErrorMessages.sameTypeSpread),
      )
      Parser.next(p)
    | _ => Parser.expect(Comma, p)
    }

    let () = switch p.token {
    | Lident(_) =>
      Parser.err(
        ~startPos=dotdotdotStart,
        ~endPos=dotdotdotEnd,
        p,
        Diagnostics.message(ErrorMessages.spreadInRecordDeclaration),
      )
    | _ => ()
    }

    let fields = list{
      Parsetree.Oinherit(typ),
      ...parseCommaDelimitedRegion(
        ~grammar=Grammar.StringFieldDeclarations,
        ~closing=Rbrace,
        ~f=parseStringFieldDeclaration,
        p,
      ),
    }

    Parser.expect(Rbrace, p)
    let loc = mkLoc(startPos, p.prevEndPos)
    let typ = Ast_helper.Typ.object_(~loc, fields, Asttypes.Closed) |> parseTypeAlias(p)

    let typ = parseArrowTypeRest(~es6Arrow=true, ~startPos, typ, p)
    (Some(typ), Asttypes.Public, Parsetree.Ptype_abstract)
  | _ =>
    let attrs = parseAttributes(p)
    switch p.Parser.token {
    | String(_) =>
      let closedFlag = Asttypes.Closed
      let fields = switch attrs {
      | list{} =>
        parseCommaDelimitedRegion(
          ~grammar=Grammar.StringFieldDeclarations,
          ~closing=Rbrace,
          ~f=parseStringFieldDeclaration,
          p,
        )
      | attrs =>
        let first = {
          Parser.leaveBreadcrumb(p, Grammar.StringFieldDeclarations)
          let field = switch parseStringFieldDeclaration(p) {
          | Some(field) => field
          | None => assert false
          }

          /* parse comma after first */
          let () = switch p.Parser.token {
          | Rbrace | Eof => ()
          | Comma => Parser.next(p)
          | _ => Parser.expect(Comma, p)
          }

          Parser.eatBreadcrumb(p)
          switch field {
          | Parsetree.Otag(label, _, ct) => Parsetree.Otag(label, attrs, ct)
          | Oinherit(ct) => Oinherit(ct)
          }
        }

        list{
          first,
          ...parseCommaDelimitedRegion(
            ~grammar=Grammar.StringFieldDeclarations,
            ~closing=Rbrace,
            ~f=parseStringFieldDeclaration,
            p,
          ),
        }
      }

      Parser.expect(Rbrace, p)
      let loc = mkLoc(startPos, p.prevEndPos)
      let typ = Ast_helper.Typ.object_(~loc, ~attrs=list{}, fields, closedFlag) |> parseTypeAlias(p)

      let typ = parseArrowTypeRest(~es6Arrow=true, ~startPos, typ, p)
      (Some(typ), Asttypes.Public, Parsetree.Ptype_abstract)
    | _ =>
      Parser.leaveBreadcrumb(p, Grammar.RecordDecl)
      let fields = switch attrs {
      | list{} =>
        parseCommaDelimitedRegion(
          ~grammar=Grammar.FieldDeclarations,
          ~closing=Rbrace,
          ~f=parseFieldDeclarationRegion,
          p,
        )
      | list{attr, ..._} as attrs =>
        let first = {
          let field = parseFieldDeclaration(p)
          Parser.optional(p, Comma) |> ignore
          {
            ...field,
            Parsetree.pld_attributes: attrs,
            pld_loc: {
              ...field.Parsetree.pld_loc,
              loc_start: (attr |> fst).loc.loc_start,
            },
          }
        }

        list{
          first,
          ...parseCommaDelimitedRegion(
            ~grammar=Grammar.FieldDeclarations,
            ~closing=Rbrace,
            ~f=parseFieldDeclarationRegion,
            p,
          ),
        }
      }

      let () = switch fields {
      | list{} => Parser.err(~startPos, p, Diagnostics.message("A record needs at least one field"))
      | _ => ()
      }

      Parser.expect(Rbrace, p)
      Parser.eatBreadcrumb(p)
      (None, Asttypes.Public, Parsetree.Ptype_record(fields))
    }
  }
}

and parsePrivateEqOrRepr = p => {
  Parser.expect(Private, p)
  switch p.Parser.token {
  | Lbrace =>
    let (manifest, _, kind) = parseRecordOrObjectDecl(p)
    (manifest, Asttypes.Private, kind)
  | Uident(_) =>
    let (manifest, _, kind) = parseTypeEquationOrConstrDecl(p)
    (manifest, Asttypes.Private, kind)
  | Bar | DotDot =>
    let (_, kind) = parseTypeRepresentation(p)
    (None, Asttypes.Private, kind)
  | t if Grammar.isTypExprStart(t) => (
      Some(parseTypExpr(p)),
      Asttypes.Private,
      Parsetree.Ptype_abstract,
    )
  | _ =>
    let (_, kind) = parseTypeRepresentation(p)
    (None, Asttypes.Private, kind)
  }
}

/*
  polymorphic-variant-type	::=
                            | [ tag-spec-first  { | tag-spec } ]
                            | [> [ tag-spec ]  { | tag-spec } ]
                            | [< [|] tag-spec-full  { | tag-spec-full }  [ > { `tag-name }+ ] ]

            tag-spec-first	::=	`tag-name  [ of typexpr ]
                            |	[ typexpr ] |  tag-spec

                  tag-spec	::=	`tag-name  [ of typexpr ]
                            |	typexpr

              tag-spec-full	::=	`tag-name  [ of [&] typexpr  { & typexpr } ]
                             |	typexpr
*/
and parsePolymorphicVariantType = (~attrs, p) => {
  let startPos = p.Parser.startPos
  Parser.expect(Lbracket, p)
  switch p.token {
  | GreaterThan =>
    Parser.next(p)
    let rowFields = switch p.token {
    | Rbracket => list{}
    | Bar => parseTagSpecs(p)
    | _ =>
      let rowField = parseTagSpec(p)
      list{rowField, ...parseTagSpecs(p)}
    }

    let variant = {
      let loc = mkLoc(startPos, p.prevEndPos)
      Ast_helper.Typ.variant(~attrs, ~loc, rowFields, Open, None)
    }
    Parser.expect(Rbracket, p)
    variant
  | LessThan =>
    Parser.next(p)
    Parser.optional(p, Bar) |> ignore
    let rowField = parseTagSpecFull(p)
    let rowFields = parseTagSpecFulls(p)
    let tagNames = parseTagNames(p)
    let variant = {
      let loc = mkLoc(startPos, p.prevEndPos)
      Ast_helper.Typ.variant(~attrs, ~loc, list{rowField, ...rowFields}, Closed, Some(tagNames))
    }
    Parser.expect(Rbracket, p)
    variant
  | _ =>
    let rowFields1 = parseTagSpecFirst(p)
    let rowFields2 = parseTagSpecs(p)
    let variant = {
      let loc = mkLoc(startPos, p.prevEndPos)
      Ast_helper.Typ.variant(~attrs, ~loc, \"@"(rowFields1, rowFields2), Closed, None)
    }
    Parser.expect(Rbracket, p)
    variant
  }
}

and parseTagName = p =>
  switch p.Parser.token {
  | Hash =>
    let (ident, _loc) = parseHashIdent(~startPos=p.startPos, p)
    Some(ident)
  | _ => None
  }

and parseTagNames = p =>
  if p.Parser.token === GreaterThan {
    Parser.next(p)
    parseRegion(p, ~grammar=Grammar.TagNames, ~f=parseTagName)
  } else {
    list{}
  }

and parseTagSpecFulls = p =>
  switch p.Parser.token {
  | Rbracket => list{}
  | GreaterThan => list{}
  | Bar =>
    Parser.next(p)
    let rowField = parseTagSpecFull(p)
    list{rowField, ...parseTagSpecFulls(p)}
  | _ => list{}
  }

and parseTagSpecFull = p => {
  let attrs = parseAttributes(p)
  switch p.Parser.token {
  | Hash => parsePolymorphicVariantTypeSpecHash(~attrs, ~full=true, p)
  | _ =>
    let typ = parseTypExpr(~attrs, p)
    Parsetree.Rinherit(typ)
  }
}

and parseTagSpecs = p =>
  switch p.Parser.token {
  | Bar =>
    Parser.next(p)
    let rowField = parseTagSpec(p)
    list{rowField, ...parseTagSpecs(p)}
  | _ => list{}
  }

and parseTagSpec = p => {
  let attrs = parseAttributes(p)
  switch p.Parser.token {
  | Hash => parsePolymorphicVariantTypeSpecHash(~attrs, ~full=false, p)
  | _ =>
    let typ = parseTypExpr(~attrs, p)
    Parsetree.Rinherit(typ)
  }
}

and parseTagSpecFirst = p => {
  let attrs = parseAttributes(p)
  switch p.Parser.token {
  | Bar =>
    Parser.next(p)
    list{parseTagSpec(p)}
  | Hash => list{parsePolymorphicVariantTypeSpecHash(~attrs, ~full=false, p)}
  | _ =>
    let typ = parseTypExpr(~attrs, p)
    switch p.token {
    | Rbracket => /* example: [ListStyleType.t] */
      list{Parsetree.Rinherit(typ)}
    | _ =>
      Parser.expect(Bar, p)
      list{Parsetree.Rinherit(typ), parseTagSpec(p)}
    }
  }
}

and parsePolymorphicVariantTypeSpecHash = (~attrs, ~full, p): Parsetree.row_field => {
  let startPos = p.Parser.startPos
  let (ident, loc) = parseHashIdent(~startPos, p)
  let rec loop = p =>
    switch p.Parser.token {
    | Band if full =>
      Parser.next(p)
      let rowField = parsePolymorphicVariantTypeArgs(p)
      list{rowField, ...loop(p)}
    | _ => list{}
    }

  let (firstTuple, tagContainsAConstantEmptyConstructor) = switch p.Parser.token {
  | Band if full =>
    Parser.next(p)
    (list{parsePolymorphicVariantTypeArgs(p)}, true)
  | Lparen => (list{parsePolymorphicVariantTypeArgs(p)}, false)
  | _ => (list{}, true)
  }

  let tuples = \"@"(firstTuple, loop(p))
  Parsetree.Rtag(Location.mkloc(ident, loc), attrs, tagContainsAConstantEmptyConstructor, tuples)
}

and parsePolymorphicVariantTypeArgs = p => {
  let startPos = p.Parser.startPos
  Parser.expect(Lparen, p)
  let args = parseCommaDelimitedRegion(
    ~grammar=Grammar.TypExprList,
    ~closing=Rparen,
    ~f=parseTypExprRegion,
    p,
  )

  Parser.expect(Rparen, p)
  let attrs = list{}
  let loc = mkLoc(startPos, p.prevEndPos)
  switch args {
  | list{{ptyp_desc: Ptyp_tuple(_)} as typ} as types =>
    if p.mode == ParseForTypeChecker {
      typ
    } else {
      Ast_helper.Typ.tuple(~loc, ~attrs, types)
    }
  | list{typ} => typ
  | types => Ast_helper.Typ.tuple(~loc, ~attrs, types)
  }
}

and parseTypeEquationAndRepresentation = p =>
  switch p.Parser.token {
  | (Equal | Bar) as token =>
    if token == Bar {
      Parser.expect(Equal, p)
    }
    Parser.next(p)
    switch p.Parser.token {
    | Uident(_) => parseTypeEquationOrConstrDecl(p)
    | Lbrace => parseRecordOrObjectDecl(p)
    | Private => parsePrivateEqOrRepr(p)
    | Bar | DotDot =>
      let (priv, kind) = parseTypeRepresentation(p)
      (None, priv, kind)
    | _ =>
      let manifest = Some(parseTypExpr(p))
      switch p.Parser.token {
      | Equal =>
        Parser.next(p)
        let (priv, kind) = parseTypeRepresentation(p)
        (manifest, priv, kind)
      | _ => (manifest, Public, Parsetree.Ptype_abstract)
      }
    }
  | _ => (None, Public, Parsetree.Ptype_abstract)
  }

/* type-definition	::=	type [rec] typedef  { and typedef }
 * typedef	::=	typeconstr-name [type-params] type-information
 * type-information	::=	[type-equation]  [type-representation]  { type-constraint }
 * type-equation	::=	= typexpr */
and parseTypeDef = (~attrs, ~startPos, p) => {
  Parser.leaveBreadcrumb(p, Grammar.TypeDef)
  /* let attrs = match attrs with | Some attrs -> attrs | None -> parseAttributes p in */
  Parser.leaveBreadcrumb(p, Grammar.TypeConstrName)
  let (name, loc) = parseLident(p)
  let typeConstrName = Location.mkloc(name, loc)
  Parser.eatBreadcrumb(p)
  let params = {
    let constrName = Location.mkloc(Longident.Lident(name), loc)
    parseTypeParams(~parent=constrName, p)
  }
  let typeDef = {
    let (manifest, priv, kind) = parseTypeEquationAndRepresentation(p)
    let cstrs = parseTypeConstraints(p)
    let loc = mkLoc(startPos, p.prevEndPos)
    Ast_helper.Type.mk(~loc, ~attrs, ~priv, ~kind, ~params, ~cstrs, ~manifest?, typeConstrName)
  }

  Parser.eatBreadcrumb(p)
  typeDef
}

and parseTypeExtension = (~params, ~attrs, ~name, p) => {
  Parser.expect(PlusEqual, p)
  let priv = if Parser.optional(p, Token.Private) {
    Asttypes.Private
  } else {
    Asttypes.Public
  }

  let constrStart = p.Parser.startPos
  Parser.optional(p, Bar) |> ignore
  let first = {
    let (attrs, name, kind) = switch p.Parser.token {
    | Bar =>
      Parser.next(p)
      parseConstrDef(~parseAttrs=true, p)
    | _ => parseConstrDef(~parseAttrs=true, p)
    }

    let loc = mkLoc(constrStart, p.prevEndPos)
    Ast_helper.Te.constructor(~loc, ~attrs, name, kind)
  }

  let rec loop = (p, cs) =>
    switch p.Parser.token {
    | Bar =>
      let startPos = p.Parser.startPos
      Parser.next(p)
      let (attrs, name, kind) = parseConstrDef(~parseAttrs=true, p)
      let extConstr = Ast_helper.Te.constructor(
        ~attrs,
        ~loc=mkLoc(startPos, p.prevEndPos),
        name,
        kind,
      )

      loop(p, list{extConstr, ...cs})
    | _ => List.rev(cs)
    }

  let constructors = loop(p, list{first})
  Ast_helper.Te.mk(~attrs, ~params, ~priv, name, constructors)
}

and parseTypeDefinitions = (~attrs, ~name, ~params, ~startPos, p) => {
  let typeDef = {
    let (manifest, priv, kind) = parseTypeEquationAndRepresentation(p)
    let cstrs = parseTypeConstraints(p)
    let loc = mkLoc(startPos, p.prevEndPos)
    Ast_helper.Type.mk(
      ~loc,
      ~attrs,
      ~priv,
      ~kind,
      ~params,
      ~cstrs,
      ~manifest?,
      {...name, txt: lidentOfPath(name.Location.txt)},
    )
  }

  let rec loop = (p, defs) => {
    let startPos = p.Parser.startPos
    let attrs = parseAttributesAndBinding(p)
    switch p.Parser.token {
    | And =>
      Parser.next(p)
      let attrs = switch p.token {
      | Export =>
        let exportLoc = mkLoc(p.startPos, p.endPos)
        Parser.next(p)
        let genTypeAttr = (Location.mkloc("genType", exportLoc), Parsetree.PStr(list{}))
        list{genTypeAttr, ...attrs}
      | _ => attrs
      }

      let typeDef = parseTypeDef(~attrs, ~startPos, p)
      loop(p, list{typeDef, ...defs})
    | _ => List.rev(defs)
    }
  }

  loop(p, list{typeDef})
}

/* TODO: decide if we really want type extensions (eg. type x += Blue)
 * It adds quite a bit of complexity that can be avoided,
 * implemented for now. Needed to get a feel for the complexities of
 * this territory of the grammar */
and parseTypeDefinitionOrExtension = (~attrs, p) => {
  let startPos = p.Parser.startPos
  Parser.expect(Token.Typ, p)
  let recFlag = switch p.token {
  | Rec =>
    Parser.next(p)
    Asttypes.Recursive
  | Lident("nonrec") =>
    Parser.next(p)
    Asttypes.Nonrecursive
  | _ => Asttypes.Nonrecursive
  }

  let name = parseValuePath(p)
  let params = parseTypeParams(~parent=name, p)
  switch p.Parser.token {
  | PlusEqual => TypeExt(parseTypeExtension(~params, ~attrs, ~name, p))
  | _ =>
    /* shape of type name should be Lident, i.e. `t` is accepted. `User.t` not */
    let () = switch name.Location.txt {
    | Lident(_) => ()
    | longident =>
      Parser.err(
        ~startPos=name.loc.loc_start,
        ~endPos=name.loc.loc_end,
        p,
        longident |> ErrorMessages.typeDeclarationNameLongident |> Diagnostics.message,
      )
    }

    let typeDefs = parseTypeDefinitions(~attrs, ~name, ~params, ~startPos, p)
    TypeDef({recFlag: recFlag, types: typeDefs})
  }
}

/* external value-name : typexp = external-declaration */
and parseExternalDef = (~attrs, ~startPos, p) => {
  Parser.leaveBreadcrumb(p, Grammar.External)
  Parser.expect(Token.External, p)
  let (name, loc) = parseLident(p)
  let name = Location.mkloc(name, loc)
  Parser.expect(~grammar=Grammar.TypeExpression, Colon, p)
  let typExpr = parseTypExpr(p)
  let equalStart = p.startPos
  let equalEnd = p.endPos
  Parser.expect(Equal, p)
  let prim = switch p.token {
  | String(s) =>
    Parser.next(p)
    list{s}
  | _ =>
    Parser.err(
      ~startPos=equalStart,
      ~endPos=equalEnd,
      p,
      Diagnostics.message(
        "An external requires the name of the JS value you're referring to, like \"" ++
        (name.txt ++
        "\"."),
      ),
    )
    list{}
  }

  let loc = mkLoc(startPos, p.prevEndPos)
  let vb = Ast_helper.Val.mk(~loc, ~attrs, ~prim, name, typExpr)
  Parser.eatBreadcrumb(p)
  vb
}

/* constr-def ::=
 *  | constr-decl
 *  | constr-name = constr
 *
 *  constr-decl ::= constr-name constr-args
 *  constr-name ::= uident
 *  constr      ::= path-uident */
and parseConstrDef = (~parseAttrs, p) => {
  let attrs = if parseAttrs {
    parseAttributes(p)
  } else {
    list{}
  }
  let name = switch p.Parser.token {
  | Uident(name) =>
    let loc = mkLoc(p.startPos, p.endPos)
    Parser.next(p)
    Location.mkloc(name, loc)
  | t =>
    Parser.err(p, Diagnostics.uident(t))
    Location.mknoloc("_")
  }

  let kind = switch p.Parser.token {
  | Lparen =>
    let (args, res) = parseConstrDeclArgs(p)
    Parsetree.Pext_decl(args, res)
  | Equal =>
    Parser.next(p)
    let longident = parseModuleLongIdent(~lowercase=false, p)
    Parsetree.Pext_rebind(longident)
  | Colon =>
    Parser.next(p)
    let typ = parseTypExpr(p)
    Parsetree.Pext_decl(Pcstr_tuple(list{}), Some(typ))
  | _ => Parsetree.Pext_decl(Pcstr_tuple(list{}), None)
  }

  (attrs, name, kind)
}

/*
 * exception-definition	::=
 *  | exception constr-decl
 *  ∣	exception constr-name = constr
 *
 *  constr-name ::= uident
 *  constr ::= long_uident */
and parseExceptionDef = (~attrs, p) => {
  let startPos = p.Parser.startPos
  Parser.expect(Token.Exception, p)
  let (_, name, kind) = parseConstrDef(~parseAttrs=false, p)
  let loc = mkLoc(startPos, p.prevEndPos)
  Ast_helper.Te.constructor(~loc, ~attrs, name, kind)
}

and parseNewlineOrSemicolonStructure = p =>
  switch p.Parser.token {
  | Semicolon => Parser.next(p)
  | token if Grammar.isStructureItemStart(token) =>
    if p.prevEndPos.pos_lnum < p.startPos.pos_lnum {
      ()
    } else {
      Parser.err(
        ~startPos=p.prevEndPos,
        ~endPos=p.endPos,
        p,
        Diagnostics.message(
          "consecutive statements on a line must be separated by ';' or a newline",
        ),
      )
    }
  | _ => ()
  }

@progress((Parser.next, Parser.expect, Parser.checkProgress))
and parseStructureItemRegion = p => {
  let startPos = p.Parser.startPos
  let attrs = parseAttributes(p)
  switch p.Parser.token {
  | Open =>
    let openDescription = parseOpenDescription(~attrs, p)
    parseNewlineOrSemicolonStructure(p)
    let loc = mkLoc(startPos, p.prevEndPos)
    Some(Ast_helper.Str.open_(~loc, openDescription))
  | Let =>
    let (recFlag, letBindings) = parseLetBindings(~attrs, p)
    parseNewlineOrSemicolonStructure(p)
    let loc = mkLoc(startPos, p.prevEndPos)
    Some(Ast_helper.Str.value(~loc, recFlag, letBindings))
  | Typ =>
    Parser.beginRegion(p)
    switch parseTypeDefinitionOrExtension(~attrs, p) {
    | TypeDef({recFlag, types}) =>
      parseNewlineOrSemicolonStructure(p)
      let loc = mkLoc(startPos, p.prevEndPos)
      Parser.endRegion(p)
      Some(Ast_helper.Str.type_(~loc, recFlag, types))
    | TypeExt(ext) =>
      parseNewlineOrSemicolonStructure(p)
      let loc = mkLoc(startPos, p.prevEndPos)
      Parser.endRegion(p)
      Some(Ast_helper.Str.type_extension(~loc, ext))
    }
  | External =>
    let externalDef = parseExternalDef(~attrs, ~startPos, p)
    parseNewlineOrSemicolonStructure(p)
    let loc = mkLoc(startPos, p.prevEndPos)
    Some(Ast_helper.Str.primitive(~loc, externalDef))
  | Import =>
    let importDescr = parseJsImport(~startPos, ~attrs, p)
    parseNewlineOrSemicolonStructure(p)
    let loc = mkLoc(startPos, p.prevEndPos)
    let structureItem = JsFfi.toParsetree(importDescr)
    Some({...structureItem, pstr_loc: loc})
  | Exception =>
    let exceptionDef = parseExceptionDef(~attrs, p)
    parseNewlineOrSemicolonStructure(p)
    let loc = mkLoc(startPos, p.prevEndPos)
    Some(Ast_helper.Str.exception_(~loc, exceptionDef))
  | Include =>
    let includeStatement = parseIncludeStatement(~attrs, p)
    parseNewlineOrSemicolonStructure(p)
    let loc = mkLoc(startPos, p.prevEndPos)
    Some(Ast_helper.Str.include_(~loc, includeStatement))
  | Export =>
    let structureItem = parseJsExport(~attrs, p)
    parseNewlineOrSemicolonStructure(p)
    let loc = mkLoc(startPos, p.prevEndPos)
    Some({...structureItem, pstr_loc: loc})
  | Module =>
    Parser.beginRegion(p)
    let structureItem = parseModuleOrModuleTypeImplOrPackExpr(~attrs, p)
    parseNewlineOrSemicolonStructure(p)
    let loc = mkLoc(startPos, p.prevEndPos)
    Parser.endRegion(p)
    Some({...structureItem, pstr_loc: loc})
  | AtAt =>
    let attr = parseStandaloneAttribute(p)
    parseNewlineOrSemicolonStructure(p)
    let loc = mkLoc(startPos, p.prevEndPos)
    Some(Ast_helper.Str.attribute(~loc, attr))
  | PercentPercent =>
    let extension = parseExtension(~moduleLanguage=true, p)
    parseNewlineOrSemicolonStructure(p)
    let loc = mkLoc(startPos, p.prevEndPos)
    Some(Ast_helper.Str.extension(~attrs, ~loc, extension))
  | token if Grammar.isExprStart(token) =>
    let prevEndPos = p.Parser.endPos
    let exp = parseExpr(p)
    parseNewlineOrSemicolonStructure(p)
    let loc = mkLoc(startPos, p.prevEndPos)
    Parser.checkProgress(~prevEndPos, ~result=Ast_helper.Str.eval(~loc, ~attrs, exp), p)
  | _ =>
    switch attrs {
    | list{({Asttypes.loc: attrLoc}, _) as attr, ..._} =>
      Parser.err(
        ~startPos=attrLoc.loc_start,
        ~endPos=attrLoc.loc_end,
        p,
        Diagnostics.message(ErrorMessages.attributeWithoutNode(attr)),
      )
      let expr = parseExpr(p)
      Some(Ast_helper.Str.eval(~loc=mkLoc(p.startPos, p.prevEndPos), ~attrs, expr))
    | _ => None
    }
  }
}

and parseJsImport = (~startPos, ~attrs, p) => {
  Parser.expect(Token.Import, p)
  let importSpec = switch p.Parser.token {
  | Token.Lident(_) | Token.At =>
    let decl = switch parseJsFfiDeclaration(p) {
    | Some(decl) => decl
    | None => assert false
    }

    JsFfi.Default(decl)
  | _ => JsFfi.Spec(parseJsFfiDeclarations(p))
  }

  let scope = parseJsFfiScope(p)
  let loc = mkLoc(startPos, p.prevEndPos)
  JsFfi.importDescr(~attrs, ~importSpec, ~scope, ~loc)
}

and parseJsExport = (~attrs, p) => {
  let exportStart = p.Parser.startPos
  Parser.expect(Token.Export, p)
  let exportLoc = mkLoc(exportStart, p.prevEndPos)
  let genTypeAttr = (Location.mkloc("genType", exportLoc), Parsetree.PStr(list{}))
  let attrs = list{genTypeAttr, ...attrs}
  switch p.Parser.token {
  | Typ =>
    switch parseTypeDefinitionOrExtension(~attrs, p) {
    | TypeDef({recFlag, types}) => Ast_helper.Str.type_(recFlag, types)
    | TypeExt(ext) => Ast_helper.Str.type_extension(ext)
    }
  /* Let */ | _ =>
    let (recFlag, letBindings) = parseLetBindings(~attrs, p)
    Ast_helper.Str.value(recFlag, letBindings)
  }
}

and parseSignJsExport = (~attrs, p) => {
  let exportStart = p.Parser.startPos
  Parser.expect(Token.Export, p)
  let exportLoc = mkLoc(exportStart, p.prevEndPos)
  let genTypeAttr = (Location.mkloc("genType", exportLoc), Parsetree.PStr(list{}))
  let attrs = list{genTypeAttr, ...attrs}
  switch p.Parser.token {
  | Typ =>
    switch parseTypeDefinitionOrExtension(~attrs, p) {
    | TypeDef({recFlag, types}) =>
      let loc = mkLoc(exportStart, p.prevEndPos)
      Ast_helper.Sig.type_(recFlag, types, ~loc)
    | TypeExt(ext) =>
      let loc = mkLoc(exportStart, p.prevEndPos)
      Ast_helper.Sig.type_extension(ext, ~loc)
    }
  /* Let */ | _ =>
    let valueDesc = parseSignLetDesc(~attrs, p)
    let loc = mkLoc(exportStart, p.prevEndPos)
    Ast_helper.Sig.value(valueDesc, ~loc)
  }
}

and parseJsFfiScope = p =>
  switch p.Parser.token {
  | Token.Lident("from") =>
    Parser.next(p)
    switch p.token {
    | String(s) =>
      Parser.next(p)
      JsFfi.Module(s)
    | Uident(_) | Lident(_) =>
      let value = parseIdentPath(p)
      JsFfi.Scope(value)
    | _ => JsFfi.Global
    }
  | _ => JsFfi.Global
  }

and parseJsFfiDeclarations = p => {
  Parser.expect(Token.Lbrace, p)
  let decls = parseCommaDelimitedRegion(
    ~grammar=Grammar.JsFfiImport,
    ~closing=Rbrace,
    ~f=parseJsFfiDeclaration,
    p,
  )

  Parser.expect(Rbrace, p)
  decls
}

and parseJsFfiDeclaration = p => {
  let startPos = p.Parser.startPos
  let attrs = parseAttributes(p)
  switch p.Parser.token {
  | Lident(_) =>
    let (ident, _) = parseLident(p)
    let alias = switch p.token {
    | As =>
      Parser.next(p)
      let (ident, _) = parseLident(p)
      ident
    | _ => ident
    }

    Parser.expect(Token.Colon, p)
    let typ = parseTypExpr(p)
    let loc = mkLoc(startPos, p.prevEndPos)
    Some(JsFfi.decl(~loc, ~alias, ~attrs, ~name=ident, ~typ))
  | _ => None
  }
}

/* include-statement ::= include module-expr */
and parseIncludeStatement = (~attrs, p) => {
  let startPos = p.Parser.startPos
  Parser.expect(Token.Include, p)
  let modExpr = parseModuleExpr(p)
  let loc = mkLoc(startPos, p.prevEndPos)
  Ast_helper.Incl.mk(~loc, ~attrs, modExpr)
}

and parseAtomicModuleExpr = p => {
  let startPos = p.Parser.startPos
  switch p.Parser.token {
  | Uident(_ident) =>
    let longident = parseModuleLongIdent(~lowercase=false, p)
    Ast_helper.Mod.ident(~loc=longident.loc, longident)
  | Lbrace =>
    Parser.next(p)
    let structure = Ast_helper.Mod.structure(
      parseDelimitedRegion(
        ~grammar=Grammar.Structure,
        ~closing=Rbrace,
        ~f=parseStructureItemRegion,
        p,
      ),
    )
    Parser.expect(Rbrace, p)
    let endPos = p.prevEndPos
    {...structure, pmod_loc: mkLoc(startPos, endPos)}
  | Lparen =>
    Parser.next(p)
    let modExpr = switch p.token {
    | Rparen => Ast_helper.Mod.structure(~loc=mkLoc(startPos, p.prevEndPos), list{})
    | _ => parseConstrainedModExpr(p)
    }

    Parser.expect(Rparen, p)
    modExpr
  | Lident("unpack") =>
    /* TODO: should this be made a keyword?? */
    Parser.next(p)
    Parser.expect(Lparen, p)
    let expr = parseExpr(p)
    switch p.Parser.token {
    | Colon =>
      let colonStart = p.Parser.startPos
      Parser.next(p)
      let attrs = parseAttributes(p)
      let packageType = parsePackageType(~startPos=colonStart, ~attrs, p)
      Parser.expect(Rparen, p)
      let loc = mkLoc(startPos, p.prevEndPos)
      let constraintExpr = Ast_helper.Exp.constraint_(~loc, expr, packageType)

      Ast_helper.Mod.unpack(~loc, constraintExpr)
    | _ =>
      Parser.expect(Rparen, p)
      let loc = mkLoc(startPos, p.prevEndPos)
      Ast_helper.Mod.unpack(~loc, expr)
    }
  | Percent =>
    let extension = parseExtension(p)
    let loc = mkLoc(startPos, p.prevEndPos)
    Ast_helper.Mod.extension(~loc, extension)
  | token =>
    Parser.err(p, Diagnostics.unexpected(token, p.breadcrumbs))
    Recover.defaultModuleExpr()
  }
}

and parsePrimaryModExpr = p => {
  let startPos = p.Parser.startPos
  let modExpr = parseAtomicModuleExpr(p)
  let rec loop = (p, modExpr) =>
    switch p.Parser.token {
    | Lparen if p.prevEndPos.pos_lnum === p.startPos.pos_lnum =>
      loop(p, parseModuleApplication(p, modExpr))
    | _ => modExpr
    }

  let modExpr = loop(p, modExpr)
  {...modExpr, pmod_loc: mkLoc(startPos, p.prevEndPos)}
}

/*
 * functor-arg ::=
 *  | uident : modtype
 *  | _ : modtype
 *  | modtype           --> "punning" for _ : modtype
 *  | attributes functor-arg
 */
and parseFunctorArg = p => {
  let startPos = p.Parser.startPos
  let attrs = parseAttributes(p)
  switch p.Parser.token {
  | Uident(ident) =>
    Parser.next(p)
    let uidentEndPos = p.prevEndPos
    switch p.Parser.token {
    | Colon =>
      Parser.next(p)
      let moduleType = parseModuleType(p)
      let loc = mkLoc(startPos, uidentEndPos)
      let argName = Location.mkloc(ident, loc)
      Some(attrs, argName, Some(moduleType), startPos)
    | Dot =>
      Parser.next(p)
      let moduleType = {
        let moduleLongIdent = parseModuleLongIdentTail(
          ~lowercase=false,
          p,
          startPos,
          Longident.Lident(ident),
        )
        Ast_helper.Mty.ident(~loc=moduleLongIdent.loc, moduleLongIdent)
      }

      let argName = Location.mknoloc("_")
      Some(attrs, argName, Some(moduleType), startPos)
    | _ =>
      let loc = mkLoc(startPos, uidentEndPos)
      let modIdent = Location.mkloc(Longident.Lident(ident), loc)
      let moduleType = Ast_helper.Mty.ident(~loc, modIdent)
      let argName = Location.mknoloc("_")
      Some(attrs, argName, Some(moduleType), startPos)
    }
  | Underscore =>
    Parser.next(p)
    let argName = Location.mkloc("_", mkLoc(startPos, p.prevEndPos))
    Parser.expect(Colon, p)
    let moduleType = parseModuleType(p)
    Some(attrs, argName, Some(moduleType), startPos)
  | Lparen =>
    Parser.next(p)
    Parser.expect(Rparen, p)
    let argName = Location.mkloc("*", mkLoc(startPos, p.prevEndPos))
    Some(attrs, argName, None, startPos)
  | _ => None
  }
}

and parseFunctorArgs = p => {
  let startPos = p.Parser.startPos
  Parser.expect(Lparen, p)
  let args = parseCommaDelimitedRegion(
    ~grammar=Grammar.FunctorArgs,
    ~closing=Rparen,
    ~f=parseFunctorArg,
    p,
  )

  Parser.expect(Rparen, p)
  switch args {
  | list{} => list{(list{}, Location.mkloc("*", mkLoc(startPos, p.prevEndPos)), None, startPos)}
  | args => args
  }
}

and parseFunctorModuleExpr = p => {
  let startPos = p.Parser.startPos
  let args = parseFunctorArgs(p)
  let returnType = switch p.Parser.token {
  | Colon =>
    Parser.next(p)
    Some(parseModuleType(~es6Arrow=false, p))
  | _ => None
  }

  Parser.expect(EqualGreater, p)
  let rhsModuleExpr = {
    let modExpr = parseModuleExpr(p)
    switch returnType {
    | Some(modType) =>
      Ast_helper.Mod.constraint_(
        ~loc=mkLoc(modExpr.pmod_loc.loc_start, modType.Parsetree.pmty_loc.loc_end),
        modExpr,
        modType,
      )
    | None => modExpr
    }
  }

  let endPos = p.prevEndPos
  let modExpr = List.fold_right(
    ((attrs, name, moduleType, startPos), acc) =>
      Ast_helper.Mod.functor_(~loc=mkLoc(startPos, endPos), ~attrs, name, moduleType, acc),
    args,
    rhsModuleExpr,
  )

  {...modExpr, pmod_loc: mkLoc(startPos, endPos)}
}

/* module-expr	::=
 *  | module-path
 *  ∣	{ structure-items }
 *  ∣	functorArgs =>  module-expr
 *  ∣	module-expr(module-expr)
 *  ∣	( module-expr )
 *  ∣	( module-expr : module-type )
 *  | extension
 *  | attributes module-expr */
and parseModuleExpr = p => {
  let attrs = parseAttributes(p)
  let modExpr = if isEs6ArrowFunctor(p) {
    parseFunctorModuleExpr(p)
  } else {
    parsePrimaryModExpr(p)
  }

  {...modExpr, pmod_attributes: List.concat(list{modExpr.pmod_attributes, attrs})}
}

and parseConstrainedModExpr = p => {
  let modExpr = parseModuleExpr(p)
  switch p.Parser.token {
  | Colon =>
    Parser.next(p)
    let modType = parseModuleType(p)
    let loc = mkLoc(modExpr.pmod_loc.loc_start, modType.pmty_loc.loc_end)
    Ast_helper.Mod.constraint_(~loc, modExpr, modType)
  | _ => modExpr
  }
}

and parseConstrainedModExprRegion = p =>
  if Grammar.isModExprStart(p.Parser.token) {
    Some(parseConstrainedModExpr(p))
  } else {
    None
  }

and parseModuleApplication = (p, modExpr) => {
  let startPos = p.Parser.startPos
  Parser.expect(Lparen, p)
  let args = parseCommaDelimitedRegion(
    ~grammar=Grammar.ModExprList,
    ~closing=Rparen,
    ~f=parseConstrainedModExprRegion,
    p,
  )

  Parser.expect(Rparen, p)
  let args = switch args {
  | list{} =>
    let loc = mkLoc(startPos, p.prevEndPos)
    list{Ast_helper.Mod.structure(~loc, list{})}
  | args => args
  }

  List.fold_left(
    (modExpr, arg) =>
      Ast_helper.Mod.apply(
        ~loc=mkLoc(modExpr.Parsetree.pmod_loc.loc_start, arg.Parsetree.pmod_loc.loc_end),
        modExpr,
        arg,
      ),
    modExpr,
    args,
  )
}

and parseModuleOrModuleTypeImplOrPackExpr = (~attrs, p) => {
  let startPos = p.Parser.startPos
  Parser.expect(Module, p)
  switch p.Parser.token {
  | Typ => parseModuleTypeImpl(~attrs, startPos, p)
  | Lparen =>
    let expr = parseFirstClassModuleExpr(~startPos, p)
    let a = parsePrimaryExpr(~operand=expr, p)
    let expr = parseBinaryExpr(~a, p, 1)
    let expr = parseTernaryExpr(expr, p)
    Ast_helper.Str.eval(~attrs, expr)
  | _ => parseMaybeRecModuleBinding(~attrs, ~startPos, p)
  }
}

and parseModuleTypeImpl = (~attrs, startPos, p) => {
  Parser.expect(Typ, p)
  let nameStart = p.Parser.startPos
  let name = switch p.Parser.token {
  | Lident(ident) =>
    Parser.next(p)
    let loc = mkLoc(nameStart, p.prevEndPos)
    Location.mkloc(ident, loc)
  | Uident(ident) =>
    Parser.next(p)
    let loc = mkLoc(nameStart, p.prevEndPos)
    Location.mkloc(ident, loc)
  | t =>
    Parser.err(p, Diagnostics.uident(t))
    Location.mknoloc("_")
  }

  Parser.expect(Equal, p)
  let moduleType = parseModuleType(p)
  let moduleTypeDeclaration = Ast_helper.Mtd.mk(
    ~attrs,
    ~loc=mkLoc(nameStart, p.prevEndPos),
    ~typ=moduleType,
    name,
  )

  let loc = mkLoc(startPos, p.prevEndPos)
  Ast_helper.Str.modtype(~loc, moduleTypeDeclaration)
}

/* definition	::=
  ∣	 module rec module-name :  module-type =  module-expr   { and module-name
  :  module-type =  module-expr } */
and parseMaybeRecModuleBinding = (~attrs, ~startPos, p) =>
  switch p.Parser.token {
  | Token.Rec =>
    Parser.next(p)
    Ast_helper.Str.rec_module(parseModuleBindings(~startPos, ~attrs, p))
  | _ => Ast_helper.Str.module_(parseModuleBinding(~attrs, ~startPos=p.Parser.startPos, p))
  }

and parseModuleBinding = (~attrs, ~startPos, p) => {
  let name = switch p.Parser.token {
  | Uident(ident) =>
    let startPos = p.Parser.startPos
    Parser.next(p)
    let loc = mkLoc(startPos, p.prevEndPos)
    Location.mkloc(ident, loc)
  | t =>
    Parser.err(p, Diagnostics.uident(t))
    Location.mknoloc("_")
  }

  let body = parseModuleBindingBody(p)
  let loc = mkLoc(startPos, p.prevEndPos)
  Ast_helper.Mb.mk(~attrs, ~loc, name, body)
}

and parseModuleBindingBody = p => {
  /* TODO: make required with good error message when rec module binding */
  let returnModType = switch p.Parser.token {
  | Colon =>
    Parser.next(p)
    Some(parseModuleType(p))
  | _ => None
  }

  Parser.expect(Equal, p)
  let modExpr = parseModuleExpr(p)
  switch returnModType {
  | Some(modType) =>
    Ast_helper.Mod.constraint_(
      ~loc=mkLoc(modType.pmty_loc.loc_start, modExpr.pmod_loc.loc_end),
      modExpr,
      modType,
    )
  | None => modExpr
  }
}

/* module-name :  module-type =  module-expr
 * { and module-name :  module-type =  module-expr } */
and parseModuleBindings = (~attrs, ~startPos, p) => {
  let rec loop = (p, acc) => {
    let startPos = p.Parser.startPos
    let attrs = parseAttributesAndBinding(p)
    switch p.Parser.token {
    | And =>
      Parser.next(p)
      ignore(Parser.optional(p, Module)) /* over-parse for fault-tolerance */
      let modBinding = parseModuleBinding(~attrs, ~startPos, p)
      loop(p, list{modBinding, ...acc})
    | _ => List.rev(acc)
    }
  }

  let first = parseModuleBinding(~attrs, ~startPos, p)
  loop(p, list{first})
}

and parseAtomicModuleType = p => {
  let startPos = p.Parser.startPos
  let moduleType = switch p.Parser.token {
  | Uident(_) | Lident(_) =>
    /* Ocaml allows module types to end with lowercase: module Foo : bar = { ... }
     * lets go with uppercase terminal for now */
    let moduleLongIdent = parseModuleLongIdent(~lowercase=true, p)
    Ast_helper.Mty.ident(~loc=moduleLongIdent.loc, moduleLongIdent)
  | Lparen =>
    Parser.next(p)
    let mty = parseModuleType(p)
    Parser.expect(Rparen, p)
    {...mty, pmty_loc: mkLoc(startPos, p.prevEndPos)}
  | Lbrace =>
    Parser.next(p)
    let spec = parseDelimitedRegion(
      ~grammar=Grammar.Signature,
      ~closing=Rbrace,
      ~f=parseSignatureItemRegion,
      p,
    )

    Parser.expect(Rbrace, p)
    let loc = mkLoc(startPos, p.prevEndPos)
    Ast_helper.Mty.signature(~loc, spec)
  | Module =>
    /* TODO: check if this is still atomic when implementing first class modules */
    parseModuleTypeOf(p)
  | Percent =>
    let extension = parseExtension(p)
    let loc = mkLoc(startPos, p.prevEndPos)
    Ast_helper.Mty.extension(~loc, extension)
  | token =>
    Parser.err(p, Diagnostics.unexpected(token, p.breadcrumbs))
    Recover.defaultModuleType()
  }

  let moduleTypeLoc = mkLoc(startPos, p.prevEndPos)
  {...moduleType, pmty_loc: moduleTypeLoc}
}

and parseFunctorModuleType = p => {
  let startPos = p.Parser.startPos
  let args = parseFunctorArgs(p)
  Parser.expect(EqualGreater, p)
  let rhs = parseModuleType(p)
  let endPos = p.prevEndPos
  let modType = List.fold_right(
    ((attrs, name, moduleType, startPos), acc) =>
      Ast_helper.Mty.functor_(~loc=mkLoc(startPos, endPos), ~attrs, name, moduleType, acc),
    args,
    rhs,
  )

  {...modType, pmty_loc: mkLoc(startPos, endPos)}
}

/* Module types are the module-level equivalent of type expressions: they
 * specify the general shape and type properties of modules.
 *
 * module-type ::=
 *  | modtype-path
 *  | { signature }
 *  | ( module-type )               --> parenthesized module-type
 *  | functor-args => module-type   --> functor
 *  | module-type => module-type    --> functor
 *  | module type of module-expr
 *  | attributes module-type
 *  | module-type with-mod-constraints
 *  | extension
 */
and parseModuleType = (~es6Arrow=true, ~with_=true, p) => {
  let attrs = parseAttributes(p)
  let modty = if es6Arrow && isEs6ArrowFunctor(p) {
    parseFunctorModuleType(p)
  } else {
    let modty = parseAtomicModuleType(p)
    switch p.Parser.token {
    | EqualGreater if es6Arrow === true =>
      Parser.next(p)
      let rhs = parseModuleType(~with_=false, p)
      let str = Location.mknoloc("_")
      let loc = mkLoc(modty.pmty_loc.loc_start, p.prevEndPos)
      Ast_helper.Mty.functor_(~loc, str, Some(modty), rhs)
    | _ => modty
    }
  }

  let moduleType = {
    ...modty,
    pmty_attributes: List.concat(list{modty.pmty_attributes, attrs}),
  }
  if with_ {
    parseWithConstraints(moduleType, p)
  } else {
    moduleType
  }
}

and parseWithConstraints = (moduleType, p) =>
  switch p.Parser.token {
  | Lident("with") =>
    Parser.next(p)
    let first = parseWithConstraint(p)
    let rec loop = (p, acc) =>
      switch p.Parser.token {
      | And =>
        Parser.next(p)
        loop(p, list{parseWithConstraint(p), ...acc})
      | _ => List.rev(acc)
      }

    let constraints = loop(p, list{first})
    let loc = mkLoc(moduleType.pmty_loc.loc_start, p.prevEndPos)
    Ast_helper.Mty.with_(~loc, moduleType, constraints)
  | _ => moduleType
  }

/* mod-constraint	::=
 *  |  type typeconstr<type-params> type-equation type-constraints?
 *  ∣	 type typeconstr-name<type-params> := typexpr
 *  ∣	 module module-path = extended-module-path
 *  ∣	 module module-path :=  extended-module-path
 *
 *  TODO: split this up into multiple functions, better errors */
and parseWithConstraint = p =>
  switch p.Parser.token {
  | Module =>
    Parser.next(p)
    let modulePath = parseModuleLongIdent(~lowercase=false, p)
    switch p.Parser.token {
    | ColonEqual =>
      Parser.next(p)
      let lident = parseModuleLongIdent(~lowercase=false, p)
      Parsetree.Pwith_modsubst(modulePath, lident)
    | Equal =>
      Parser.next(p)
      let lident = parseModuleLongIdent(~lowercase=false, p)
      Parsetree.Pwith_module(modulePath, lident)
    | token =>
      /* TODO: revisit */
      Parser.err(p, Diagnostics.unexpected(token, p.breadcrumbs))
      let lident = parseModuleLongIdent(~lowercase=false, p)
      Parsetree.Pwith_modsubst(modulePath, lident)
    }
  | Typ =>
    Parser.next(p)
    let typeConstr = parseValuePath(p)
    let params = parseTypeParams(~parent=typeConstr, p)
    switch p.Parser.token {
    | ColonEqual =>
      Parser.next(p)
      let typExpr = parseTypExpr(p)
      Parsetree.Pwith_typesubst(
        typeConstr,
        Ast_helper.Type.mk(
          ~loc=typeConstr.loc,
          ~params,
          ~manifest=typExpr,
          Location.mkloc(Longident.last(typeConstr.txt), typeConstr.loc),
        ),
      )
    | Equal =>
      Parser.next(p)
      let typExpr = parseTypExpr(p)
      let typeConstraints = parseTypeConstraints(p)
      Parsetree.Pwith_type(
        typeConstr,
        Ast_helper.Type.mk(
          ~loc=typeConstr.loc,
          ~params,
          ~manifest=typExpr,
          ~cstrs=typeConstraints,
          Location.mkloc(Longident.last(typeConstr.txt), typeConstr.loc),
        ),
      )
    | token =>
      /* TODO: revisit */
      Parser.err(p, Diagnostics.unexpected(token, p.breadcrumbs))
      let typExpr = parseTypExpr(p)
      let typeConstraints = parseTypeConstraints(p)
      Parsetree.Pwith_type(
        typeConstr,
        Ast_helper.Type.mk(
          ~loc=typeConstr.loc,
          ~params,
          ~manifest=typExpr,
          ~cstrs=typeConstraints,
          Location.mkloc(Longident.last(typeConstr.txt), typeConstr.loc),
        ),
      )
    }
  | token =>
    /* TODO: implement recovery strategy */
    Parser.err(p, Diagnostics.unexpected(token, p.breadcrumbs))
    Parsetree.Pwith_type(
      Location.mknoloc(Longident.Lident("")),
      Ast_helper.Type.mk(
        ~params=list{},
        ~manifest=Recover.defaultType(),
        ~cstrs=list{},
        Location.mknoloc(""),
      ),
    )
  }

and parseModuleTypeOf = p => {
  let startPos = p.Parser.startPos
  Parser.expect(Module, p)
  Parser.expect(Typ, p)
  Parser.expect(Of, p)
  let moduleExpr = parseModuleExpr(p)
  Ast_helper.Mty.typeof_(~loc=mkLoc(startPos, p.prevEndPos), moduleExpr)
}

and parseNewlineOrSemicolonSignature = p =>
  switch p.Parser.token {
  | Semicolon => Parser.next(p)
  | token if Grammar.isSignatureItemStart(token) =>
    if p.prevEndPos.pos_lnum < p.startPos.pos_lnum {
      ()
    } else {
      Parser.err(
        ~startPos=p.prevEndPos,
        ~endPos=p.endPos,
        p,
        Diagnostics.message(
          "consecutive specifications on a line must be separated by ';' or a newline",
        ),
      )
    }
  | _ => ()
  }

@progress((Parser.next, Parser.expect, Parser.checkProgress))
and parseSignatureItemRegion = p => {
  let startPos = p.Parser.startPos
  let attrs = parseAttributes(p)
  switch p.Parser.token {
  | Let =>
    Parser.beginRegion(p)
    let valueDesc = parseSignLetDesc(~attrs, p)
    parseNewlineOrSemicolonSignature(p)
    let loc = mkLoc(startPos, p.prevEndPos)
    Parser.endRegion(p)
    Some(Ast_helper.Sig.value(~loc, valueDesc))
  | Typ =>
    Parser.beginRegion(p)
    switch parseTypeDefinitionOrExtension(~attrs, p) {
    | TypeDef({recFlag, types}) =>
      parseNewlineOrSemicolonSignature(p)
      let loc = mkLoc(startPos, p.prevEndPos)
      Parser.endRegion(p)
      Some(Ast_helper.Sig.type_(~loc, recFlag, types))
    | TypeExt(ext) =>
      parseNewlineOrSemicolonSignature(p)
      let loc = mkLoc(startPos, p.prevEndPos)
      Parser.endRegion(p)
      Some(Ast_helper.Sig.type_extension(~loc, ext))
    }
  | External =>
    let externalDef = parseExternalDef(~attrs, ~startPos, p)
    parseNewlineOrSemicolonSignature(p)
    let loc = mkLoc(startPos, p.prevEndPos)
    Some(Ast_helper.Sig.value(~loc, externalDef))
  | Export =>
    let signatureItem = parseSignJsExport(~attrs, p)
    parseNewlineOrSemicolonSignature(p)
    let loc = mkLoc(startPos, p.prevEndPos)
    Some({...signatureItem, psig_loc: loc})
  | Exception =>
    let exceptionDef = parseExceptionDef(~attrs, p)
    parseNewlineOrSemicolonSignature(p)
    let loc = mkLoc(startPos, p.prevEndPos)
    Some(Ast_helper.Sig.exception_(~loc, exceptionDef))
  | Open =>
    let openDescription = parseOpenDescription(~attrs, p)
    parseNewlineOrSemicolonSignature(p)
    let loc = mkLoc(startPos, p.prevEndPos)
    Some(Ast_helper.Sig.open_(~loc, openDescription))
  | Include =>
    Parser.next(p)
    let moduleType = parseModuleType(p)
    let includeDescription = Ast_helper.Incl.mk(
      ~loc=mkLoc(startPos, p.prevEndPos),
      ~attrs,
      moduleType,
    )

    parseNewlineOrSemicolonSignature(p)
    let loc = mkLoc(startPos, p.prevEndPos)
    Some(Ast_helper.Sig.include_(~loc, includeDescription))
  | Module =>
    Parser.beginRegion(p)
    Parser.next(p)
    switch p.Parser.token {
    | Uident(_) =>
      let modDecl = parseModuleDeclarationOrAlias(~attrs, p)
      parseNewlineOrSemicolonSignature(p)
      let loc = mkLoc(startPos, p.prevEndPos)
      Parser.endRegion(p)
      Some(Ast_helper.Sig.module_(~loc, modDecl))
    | Rec =>
      let recModule = parseRecModuleSpec(~attrs, ~startPos, p)
      parseNewlineOrSemicolonSignature(p)
      let loc = mkLoc(startPos, p.prevEndPos)
      Parser.endRegion(p)
      Some(Ast_helper.Sig.rec_module(~loc, recModule))
    | Typ =>
      let modTypeDecl = parseModuleTypeDeclaration(~attrs, ~startPos, p)
      Parser.endRegion(p)
      Some(modTypeDecl)
    | _t =>
      let modDecl = parseModuleDeclarationOrAlias(~attrs, p)
      parseNewlineOrSemicolonSignature(p)
      let loc = mkLoc(startPos, p.prevEndPos)
      Parser.endRegion(p)
      Some(Ast_helper.Sig.module_(~loc, modDecl))
    }
  | AtAt =>
    let attr = parseStandaloneAttribute(p)
    parseNewlineOrSemicolonSignature(p)
    let loc = mkLoc(startPos, p.prevEndPos)
    Some(Ast_helper.Sig.attribute(~loc, attr))
  | PercentPercent =>
    let extension = parseExtension(~moduleLanguage=true, p)
    parseNewlineOrSemicolonSignature(p)
    let loc = mkLoc(startPos, p.prevEndPos)
    Some(Ast_helper.Sig.extension(~attrs, ~loc, extension))
  | Import =>
    Parser.next(p)
    parseSignatureItemRegion(p)
  | _ =>
    switch attrs {
    | list{({Asttypes.loc: attrLoc}, _) as attr, ..._} =>
      Parser.err(
        ~startPos=attrLoc.loc_start,
        ~endPos=attrLoc.loc_end,
        p,
        Diagnostics.message(ErrorMessages.attributeWithoutNode(attr)),
      )
      Some(Recover.defaultSignatureItem)
    | _ => None
    }
  }
}

/* module rec module-name :  module-type  { and module-name:  module-type } */
and parseRecModuleSpec = (~attrs, ~startPos, p) => {
  Parser.expect(Rec, p)
  let rec loop = (p, spec) => {
    let startPos = p.Parser.startPos
    let attrs = parseAttributesAndBinding(p)
    switch p.Parser.token {
    | And =>
      /* TODO: give a good error message when with constraint, no parens
       * and ASet: (Set.S with type elt = A.t)
       * and BTree: (Btree.S with type elt = A.t)
       * Without parens, the `and` signals the start of another
       * `with-constraint`
       */
      Parser.expect(And, p)
      let decl = parseRecModuleDeclaration(~attrs, ~startPos, p)
      loop(p, list{decl, ...spec})
    | _ => List.rev(spec)
    }
  }

  let first = parseRecModuleDeclaration(~attrs, ~startPos, p)
  loop(p, list{first})
}

/* module-name : module-type */
and parseRecModuleDeclaration = (~attrs, ~startPos, p) => {
  let name = switch p.Parser.token {
  | Uident(modName) =>
    let loc = mkLoc(p.startPos, p.endPos)
    Parser.next(p)
    Location.mkloc(modName, loc)
  | t =>
    Parser.err(p, Diagnostics.uident(t))
    Location.mknoloc("_")
  }

  Parser.expect(Colon, p)
  let modType = parseModuleType(p)
  Ast_helper.Md.mk(~loc=mkLoc(startPos, p.prevEndPos), ~attrs, name, modType)
}

and parseModuleDeclarationOrAlias = (~attrs, p) => {
  let startPos = p.Parser.startPos
  let moduleName = switch p.Parser.token {
  | Uident(ident) =>
    let loc = mkLoc(p.Parser.startPos, p.endPos)
    Parser.next(p)
    Location.mkloc(ident, loc)
  | t =>
    Parser.err(p, Diagnostics.uident(t))
    Location.mknoloc("_")
  }

  let body = switch p.Parser.token {
  | Colon =>
    Parser.next(p)
    parseModuleType(p)
  | Equal =>
    Parser.next(p)
    let lident = parseModuleLongIdent(~lowercase=false, p)
    Ast_helper.Mty.alias(lident)
  | token =>
    Parser.err(p, Diagnostics.unexpected(token, p.breadcrumbs))
    Recover.defaultModuleType()
  }

  let loc = mkLoc(startPos, p.prevEndPos)
  Ast_helper.Md.mk(~loc, ~attrs, moduleName, body)
}

and parseModuleTypeDeclaration = (~attrs, ~startPos, p) => {
  Parser.expect(Typ, p)
  let moduleName = switch p.Parser.token {
  | Uident(ident) =>
    let loc = mkLoc(p.startPos, p.endPos)
    Parser.next(p)
    Location.mkloc(ident, loc)
  | Lident(ident) =>
    let loc = mkLoc(p.startPos, p.endPos)
    Parser.next(p)
    Location.mkloc(ident, loc)
  | t =>
    Parser.err(p, Diagnostics.uident(t))
    Location.mknoloc("_")
  }

  let typ = switch p.Parser.token {
  | Equal =>
    Parser.next(p)
    Some(parseModuleType(p))
  | _ => None
  }

  let moduleDecl = Ast_helper.Mtd.mk(~attrs, ~typ?, moduleName)
  Ast_helper.Sig.modtype(~loc=mkLoc(startPos, p.prevEndPos), moduleDecl)
}

and parseSignLetDesc = (~attrs, p) => {
  let startPos = p.Parser.startPos
  Parser.optional(p, Let) |> ignore
  let (name, loc) = parseLident(p)
  let name = Location.mkloc(name, loc)
  Parser.expect(Colon, p)
  let typExpr = parsePolyTypeExpr(p)
  let loc = mkLoc(startPos, p.prevEndPos)
  Ast_helper.Val.mk(~loc, ~attrs, name, typExpr)
}

/* attr-id	::=	lowercase-ident
∣	  capitalized-ident
∣	  attr-id .  attr-id */
and parseAttributeId = (~startPos, p) => {
  let rec loop = (p, acc) =>
    switch p.Parser.token {
    | Lident(ident) | Uident(ident) =>
      Parser.next(p)
      let id = acc ++ ident
      switch p.Parser.token {
      | Dot =>
        Parser.next(p)
        loop(p, id ++ ".")
      | _ => id
      }
    | token if Token.isKeyword(token) =>
      Parser.next(p)
      let id = acc ++ Token.toString(token)
      switch p.Parser.token {
      | Dot =>
        Parser.next(p)
        loop(p, id ++ ".")
      | _ => id
      }
    | token =>
      Parser.err(p, Diagnostics.unexpected(token, p.breadcrumbs))
      acc
    }

  let id = loop(p, "")
  let endPos = p.prevEndPos
  Location.mkloc(id, mkLoc(startPos, endPos))
}

/*
 * payload ::=  empty
 *          |  ( structure-item )
 *
 * TODO: what about multiple structure items?
 * @attr({let x = 1; let x = 2})
 *
 * Also what about type-expressions and specifications?
 * @attr(:myType) ???
 */
and parsePayload = p =>
  switch p.Parser.token {
  | Lparen if p.startPos.pos_cnum == p.prevEndPos.pos_cnum =>
    Parser.leaveBreadcrumb(p, Grammar.AttributePayload)
    Parser.next(p)
    switch p.token {
    | Colon =>
      Parser.next(p)
      let payload = if Grammar.isSignatureItemStart(p.token) {
        Parsetree.PSig(
          parseDelimitedRegion(
            ~grammar=Grammar.Signature,
            ~closing=Rparen,
            ~f=parseSignatureItemRegion,
            p,
          ),
        )
      } else {
        Parsetree.PTyp(parseTypExpr(p))
      }

      Parser.expect(Rparen, p)
      Parser.eatBreadcrumb(p)
      payload
    | Question =>
      Parser.next(p)
      let pattern = parsePattern(p)
      let expr = switch p.token {
      | When | If =>
        Parser.next(p)
        Some(parseExpr(p))
      | _ => None
      }

      Parser.expect(Rparen, p)
      Parser.eatBreadcrumb(p)
      Parsetree.PPat(pattern, expr)
    | _ =>
      let items = parseDelimitedRegion(
        ~grammar=Grammar.Structure,
        ~closing=Rparen,
        ~f=parseStructureItemRegion,
        p,
      )

      Parser.expect(Rparen, p)
      Parser.eatBreadcrumb(p)
      Parsetree.PStr(items)
    }
  | _ => Parsetree.PStr(list{})
  }

/* type attribute = string loc * payload */
and parseAttribute = p =>
  switch p.Parser.token {
  | At =>
    let startPos = p.startPos
    Parser.next(p)
    let attrId = parseAttributeId(~startPos, p)
    let payload = parsePayload(p)
    Some(attrId, payload)
  | _ => None
  }

and parseAttributes = p => parseRegion(p, ~grammar=Grammar.Attribute, ~f=parseAttribute)

/*
 * standalone-attribute ::=
 *  | @@ atribute-id
 *  | @@ attribute-id ( structure-item )
 */
and parseStandaloneAttribute = p => {
  let startPos = p.startPos
  Parser.expect(AtAt, p)
  let attrId = parseAttributeId(~startPos, p)
  let payload = parsePayload(p)
  (attrId, payload)
}

/* extension	::=	% attr-id  attr-payload
 *              | %% attr-id(
 *  expr	::=	 ...
 *    ∣	 extension
 *
 *  typexpr	::=	 ...
 *    ∣	 extension
 *
 *  pattern	::=	 ...
 *    ∣	 extension
 *
 *  module-expr	::=	 ...
 *    ∣	 extension
 *
 *  module-type	::=	 ...
 *    ∣	 extension
 *
 *  class-expr	::=	 ...
 *    ∣	 extension
 *
 *  class-type	::=	 ...
 *    ∣	 extension
 *
 *
 * item extension nodes usable in structures and signature
 *
 * item-extension ::= %% attr-id
 *                  | %% attr-id(structure-item)
 *
 *  attr-payload ::= structure-item
 *
 *  ~moduleLanguage represents whether we're on the module level or not
 */
and parseExtension = (~moduleLanguage=false, p) => {
  let startPos = p.Parser.startPos
  if moduleLanguage {
    Parser.expect(PercentPercent, p)
  } else {
    Parser.expect(Percent, p)
  }
  let attrId = parseAttributeId(~startPos, p)
  let payload = parsePayload(p)
  (attrId, payload)
}

/* module signature on the file level */
let parseSpecification = (p): Parsetree.signature =>
  parseRegion(p, ~grammar=Grammar.Specification, ~f=parseSignatureItemRegion)

/* module structure on the file level */
let parseImplementation = (p): Parsetree.structure =>
  parseRegion(p, ~grammar=Grammar.Implementation, ~f=parseStructureItemRegion)


let _ = parseImplementation