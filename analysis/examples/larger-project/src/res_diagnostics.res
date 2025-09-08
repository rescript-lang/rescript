module Grammar = Res_grammar
module Token = Res_token

type category =
  | Unexpected({token: Token.t, context: list<(Grammar.t, Lexing.position)>})
  | Expected({
      context: option<Grammar.t>,
      pos: Lexing.position /* prev token end */,
      token: Token.t,
    })
  | Message(string)
  | Uident(Token.t)
  | Lident(Token.t)
  | UnclosedString
  | UnclosedTemplate
  | UnclosedComment
  | UnknownUchar(Char.t)

type t = {
  startPos: Lexing.position,
  endPos: Lexing.position,
  category: category,
}

type report = list<t>

let getStartPos = t => t.startPos
let getEndPos = t => t.endPos

let defaultUnexpected = token =>
  "I'm not sure what to parse here when looking at \"" ++ (Token.toString(token) ++ "\".")

let reservedKeyword = token => {
  let tokenTxt = Token.toString(token)
  "`" ++
  (tokenTxt ++
  ("` is a reserved keyword. Keywords need to be escaped: \\\"" ++ (tokenTxt ++ "\"")))
}

let explain = t =>
  switch t.category {
  | Uident(currentToken) =>
    switch currentToken {
    | Lident(lident) =>
      let guess = String.capitalize_ascii(lident)
      "Did you mean `" ++ (guess ++ ("` instead of `" ++ (lident ++ "`?")))
    | t if Token.isKeyword(t) =>
      let token = Token.toString(t)
      "`" ++ (token ++ "` is a reserved keyword.")
    | _ => "At this point, I'm looking for an uppercased name like `Belt` or `Array`"
    }
  | Lident(currentToken) =>
    switch currentToken {
    | Uident(uident) =>
      let guess = String.uncapitalize_ascii(uident)
      "Did you mean `" ++ (guess ++ ("` instead of `" ++ (uident ++ "`?")))
    | t if Token.isKeyword(t) =>
      let token = Token.toString(t)
      "`" ++
      (token ++
      ("` is a reserved keyword. Keywords need to be escaped: \\\"" ++ (token ++ "\"")))
    | Underscore => "`_` isn't a valid name."
    | _ => "I'm expecting a lowercase name like `user or `age`"
    }
  | Message(txt) => txt
  | UnclosedString => "This string is missing a double quote at the end"
  | UnclosedTemplate => "Did you forget to close this template expression with a backtick?"
  | UnclosedComment => "This comment seems to be missing a closing `*/`"
  | UnknownUchar(uchar) =>
    switch uchar {
    | '^' =>
      "Not sure what to do with this character.\n" ++
      ("  If you're trying to dereference a mutable value, use `myValue.contents` instead.\n" ++
      "  To concatenate strings, use `\"a\" ++ \"b\"` instead.")
    | _ => "Not sure what to do with this character."
    }
  | Expected({context, token: t}) =>
    let hint = switch context {
    | Some(grammar) => " It signals the start of " ++ Grammar.toString(grammar)
    | None => ""
    }

    "Did you forget a `" ++ (Token.toString(t) ++ ("` here?" ++ hint))
  | Unexpected({token: t, context: breadcrumbs}) =>
    let name = Token.toString(t)
    switch breadcrumbs {
    | list{(AtomicTypExpr, _), ...breadcrumbs} =>
      switch (breadcrumbs, t) {
      | (
          list{(StringFieldDeclarations | FieldDeclarations, _), ..._},
          String(_) | At | Rbrace | Comma | Eof,
        ) => "I'm missing a type here"
      | (_, t) if Grammar.isStructureItemStart(t) || t == Eof => "Missing a type here"
      | _ => defaultUnexpected(t)
      }
    | list{(ExprOperand, _), ...breadcrumbs} =>
      switch (breadcrumbs, t) {
      | (list{(ExprBlock, _), ..._}, Rbrace) => "It seems that this expression block is empty"
      | (list{(ExprBlock, _), ..._}, Bar) => /* Pattern matching */
        "Looks like there might be an expression missing here"
      | (
          list{(ExprSetField, _), ..._},
          _,
        ) => "It seems that this record field mutation misses an expression"
      | (
          list{(ExprArrayMutation, _), ..._},
          _,
        ) => "Seems that an expression is missing, with what do I mutate the array?"
      | (
          list{(ExprBinaryAfterOp(_) | ExprUnary, _), ..._},
          _,
        ) => "Did you forget to write an expression here?"
      | (list{(Grammar.LetBinding, _), ..._}, _) => "This let-binding misses an expression"
      | (list{_, ..._}, Rbracket | Rbrace | Eof) => "Missing expression"
      | _ => "I'm not sure what to parse here when looking at \"" ++ (name ++ "\".")
      }
    | list{(TypeParam, _), ..._} =>
      switch t {
      | Lident(ident) => "Did you mean '" ++ (ident ++ "? A Type parameter starts with a quote.")
      | _ => "I'm not sure what to parse here when looking at \"" ++ (name ++ "\".")
      }
    | list{(Pattern, _), ...breadcrumbs} =>
      switch (t, breadcrumbs) {
      | (
          Equal,
          list{(LetBinding, _), ..._},
        ) => "I was expecting a name for this let-binding. Example: `let message = \"hello\"`"
      | (
          In,
          list{(ExprFor, _), ..._},
        ) => "A for-loop has the following form: `for i in 0 to 10`. Did you forget to supply a name before `in`?"
      | (
          EqualGreater,
          list{(PatternMatchCase, _), ..._},
        ) => "I was expecting a pattern to match on before the `=>`"
      | (token, _) if Token.isKeyword(t) => reservedKeyword(token)
      | (token, _) => defaultUnexpected(token)
      }
    | _ =>
      /* TODO: match on circumstance to verify Lident needed ? */
      if Token.isKeyword(t) {
        "`" ++
        (name ++
        ("` is a reserved keyword. Keywords need to be escaped: \\\"" ++
        (Token.toString(t) ++
        "\"")))
      } else {
        "I'm not sure what to parse here when looking at \"" ++ (name ++ "\".")
      }
    }
  }

let make = (~startPos, ~endPos, category) => {
  startPos: startPos,
  endPos: endPos,
  category: category,
}

let printReport = (diagnostics, src) => {
  let rec print = (diagnostics, src) =>
    switch diagnostics {
    | list{} => ()
    | list{d, ...rest} =>
      Res_diagnostics_printing_utils.Super_location.super_error_reporter(
        Format.err_formatter,
        src,
        {
          open Location
          {
            loc: {loc_start: d.startPos, loc_end: d.endPos, loc_ghost: false},
            msg: explain(d),
            sub: list{},
            if_highlight: "",
          }
        },
      )
      switch rest {
      | list{} => ()
      | _ => Format.fprintf(Format.err_formatter, "@.")
      }
      print(rest, src)
    }

  Format.fprintf(Format.err_formatter, "@[<v>")
  print(List.rev(diagnostics), src)
  Format.fprintf(Format.err_formatter, "@]@.")
}

let unexpected = (token, context) => Unexpected({token: token, context: context})

let expected = (~grammar=?, pos, token) => Expected({context: grammar, pos: pos, token: token})

let uident = currentToken => Uident(currentToken)
let lident = currentToken => Lident(currentToken)
let unclosedString = UnclosedString
let unclosedComment = UnclosedComment
let unclosedTemplate = UnclosedTemplate
let unknownUchar = code => UnknownUchar(code)
let message = txt => Message(txt)

