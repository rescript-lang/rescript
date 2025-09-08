let dump_tokens filename =
  let src =
    try
      let ic = open_in filename in
      let content = really_input_string ic (in_channel_length ic) in
      close_in ic;
      content
    with e ->
      Printf.printf "Error reading file %s: %s\n" filename
        (Printexc.to_string e);
      exit 1
  in

  let scanner = Res_scanner.make ~filename src in

  let rec visit scanner =
    let start_pos, end_pos, token = Res_scanner.scan scanner in
    let token_str =
      match token with
      | Res_token.Await -> "Await"
      | Res_token.Open -> "Open"
      | Res_token.True -> "True"
      | Res_token.False -> "False"
      | Res_token.Codepoint {original} -> "Codepoint(\"" ^ original ^ "\")"
      | Res_token.Int {i} -> "Int(\"" ^ i ^ "\")"
      | Res_token.Float {f} -> "Float(\"" ^ f ^ "\")"
      | Res_token.String s -> "String(\"" ^ s ^ "\")"
      | Res_token.Lident str -> "Lident(\"" ^ str ^ "\")"
      | Res_token.Uident str -> "Uident(\"" ^ str ^ "\")"
      | Res_token.As -> "As"
      | Res_token.Dot -> "Dot"
      | Res_token.DotDot -> "DotDot"
      | Res_token.DotDotDot -> "DotDotDot"
      | Res_token.Bang -> "Bang"
      | Res_token.Semicolon -> "Semicolon"
      | Res_token.Let {unwrap} -> "Let" ^ if unwrap then "?" else ""
      | Res_token.And -> "And"
      | Res_token.Rec -> "Rec"
      | Res_token.Underscore -> "Underscore"
      | Res_token.SingleQuote -> "SingleQuote"
      | Res_token.Equal -> "Equal"
      | Res_token.EqualEqual -> "EqualEqual"
      | Res_token.EqualEqualEqual -> "EqualEqualEqual"
      | Res_token.Bar -> "Bar"
      | Res_token.Lparen -> "Lparen"
      | Res_token.Rparen -> "Rparen"
      | Res_token.Lbracket -> "Lbracket"
      | Res_token.Rbracket -> "Rbracket"
      | Res_token.Lbrace -> "Lbrace"
      | Res_token.Rbrace -> "Rbrace"
      | Res_token.Colon -> "Colon"
      | Res_token.Comma -> "Comma"
      | Res_token.Eof -> "Eof"
      | Res_token.Exception -> "Exception"
      | Res_token.Backslash -> "Backslash"
      | Res_token.Forwardslash -> "Forwardslash"
      | Res_token.ForwardslashDot -> "ForwardslashDot"
      | Res_token.Regex (pattern, flags) ->
        "Regex(\"" ^ pattern ^ "\", \"" ^ flags ^ "\")"
      | Res_token.Asterisk -> "Asterisk"
      | Res_token.AsteriskDot -> "AsteriskDot"
      | Res_token.Exponentiation -> "Exponentiation"
      | Res_token.Minus -> "Minus"
      | Res_token.MinusDot -> "MinusDot"
      | Res_token.Plus -> "Plus"
      | Res_token.PlusDot -> "PlusDot"
      | Res_token.PlusPlus -> "PlusPlus"
      | Res_token.PlusEqual -> "PlusEqual"
      | Res_token.ColonGreaterThan -> "ColonGreaterThan"
      | Res_token.GreaterThan -> "GreaterThan"
      | Res_token.LessThan -> "LessThan"
      | Res_token.Hash -> "Hash"
      | Res_token.HashEqual -> "HashEqual"
      | Res_token.Assert -> "Assert"
      | Res_token.Tilde -> "Tilde"
      | Res_token.Question -> "Question"
      | Res_token.If -> "If"
      | Res_token.Else -> "Else"
      | Res_token.For -> "For"
      | Res_token.In -> "In"
      | Res_token.While -> "While"
      | Res_token.Switch -> "Switch"
      | Res_token.When -> "When"
      | Res_token.EqualGreater -> "EqualGreater"
      | Res_token.MinusGreater -> "MinusGreater"
      | Res_token.External -> "External"
      | Res_token.Typ -> "Typ"
      | Res_token.Private -> "Private"
      | Res_token.Constraint -> "Constraint"
      | Res_token.Mutable -> "Mutable"
      | Res_token.Include -> "Include"
      | Res_token.Module -> "Module"
      | Res_token.Of -> "Of"
      | Res_token.Land -> "Land"
      | Res_token.Lor -> "Lor"
      | Res_token.Band -> "Band"
      | Res_token.Caret -> "Caret"
      | Res_token.BangEqual -> "BangEqual"
      | Res_token.BangEqualEqual -> "BangEqualEqual"
      | Res_token.LessEqual -> "LessEqual"
      | Res_token.GreaterEqual -> "GreaterEqual"
      | Res_token.ColonEqual -> "ColonEqual"
      | Res_token.At -> "At"
      | Res_token.AtAt -> "AtAt"
      | Res_token.Percent -> "Percent"
      | Res_token.PercentPercent -> "PercentPercent"
      | Res_token.Comment c -> "Comment(" ^ Res_comment.to_string c ^ ")"
      | Res_token.List -> "List"
      | Res_token.Dict -> "Dict"
      | Res_token.TemplateTail (text, _) -> "TemplateTail(\"" ^ text ^ "\")"
      | Res_token.TemplatePart (text, _) -> "TemplatePart(\"" ^ text ^ "\")"
      | Res_token.Backtick -> "Backtick"
      | Res_token.Try -> "Try"
      | Res_token.DocComment (_, s) -> "DocComment(\"" ^ s ^ "\")"
      | Res_token.ModuleComment (_, s) -> "ModuleComment(\"" ^ s ^ "\")"
      | Res_token.LeftShift -> "LeftShift"
      | Res_token.RightShift -> "RightShift"
      | Res_token.RightShiftUnsigned -> "RightShiftUnsigned"
    in

    let start_line = start_pos.Lexing.pos_lnum in
    let start_col = start_pos.Lexing.pos_cnum - start_pos.Lexing.pos_bol + 1 in
    let end_line = end_pos.Lexing.pos_lnum in
    let end_col = end_pos.Lexing.pos_cnum - end_pos.Lexing.pos_bol + 1 in

    Printf.printf "%s (%d,%d-%d,%d)\n" token_str start_line start_col end_line
      end_col;

    match token with
    | Res_token.Eof -> ()
    | _ -> visit scanner
  in
  visit scanner

let token_print_engine =
  {
    Res_driver.print_implementation =
      (fun ~width:_ ~filename ~comments:_ _ -> dump_tokens filename);
    print_interface =
      (fun ~width:_ ~filename ~comments:_ _ -> dump_tokens filename);
  }
