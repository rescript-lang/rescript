module Grammar = Res_grammar
module Token = Res_token

type category =
  | Unexpected of {token: Token.t; context: (Grammar.t * Lexing.position) list}
  | Expected of {
      context: Grammar.t option;
      pos: Lexing.position; (* prev token end*)
      token: Token.t;
    }
  | Message of string
  | Uident of Token.t
  | Lident of Token.t
  | UnclosedString
  | UnclosedTemplate
  | UnclosedComment
  | UnknownUchar of Char.t

type t = {
  start_pos: Lexing.position;
  end_pos: Lexing.position;
  category: category;
}

type report = t list

let get_start_pos t = t.start_pos
let get_end_pos t = t.end_pos

let default_unexpected token =
  "I'm not sure what to parse here when looking at \"" ^ Token.to_string token
  ^ "\"."

let reserved_keyword token =
  let token_txt = Token.to_string token in
  "`" ^ token_txt ^ "` is a reserved keyword. Keywords need to be escaped: \\\""
  ^ token_txt ^ "\""

let explain t =
  match t.category with
  | Uident current_token -> (
    match current_token with
    | Lident lident ->
      let guess = String.capitalize_ascii lident in
      "Did you mean `" ^ guess ^ "` instead of `" ^ lident ^ "`?"
    | t when Token.is_keyword t ->
      let token = Token.to_string t in
      "`" ^ token ^ "` is a reserved keyword."
    | _ ->
      "At this point, I'm looking for an uppercased name like `Belt` or `Array`"
    )
  | Lident current_token -> (
    match current_token with
    | Uident uident ->
      let guess = String.uncapitalize_ascii uident in
      "Did you mean `" ^ guess ^ "` instead of `" ^ uident ^ "`?"
    | t when Token.is_keyword t ->
      let token = Token.to_string t in
      "`" ^ token ^ "` is a reserved keyword. Keywords need to be escaped: \\\""
      ^ token ^ "\""
    | Underscore -> "`_` isn't a valid name."
    | _ -> "I'm expecting a lowercase name like `user or `age`")
  | Message txt -> txt
  | UnclosedString -> "This string is missing a double quote at the end"
  | UnclosedTemplate ->
    "Did you forget to close this template expression with a backtick?"
  | UnclosedComment -> "This comment seems to be missing a closing `*/`"
  | UnknownUchar uchar ->
    "Not sure what to do with this character: \"" ^ Char.escaped uchar ^ "\"."
  | Expected {context; token = t} ->
    let hint =
      match context with
      | Some grammar -> " It signals the start of " ^ Grammar.to_string grammar
      | None -> ""
    in
    "Did you forget a `" ^ Token.to_string t ^ "` here?" ^ hint
  | Unexpected {token = t; context = breadcrumbs} -> (
    let name = Token.to_string t in
    match breadcrumbs with
    | (AtomicTypExpr, _) :: breadcrumbs -> (
      match (breadcrumbs, t) with
      | ( ((StringFieldDeclarations | FieldDeclarations), _) :: _,
          (String _ | At | Rbrace | Comma | Eof) ) ->
        "I'm missing a type here"
      | _, t when Grammar.is_structure_item_start t || t = Eof ->
        "Missing a type here"
      | _ -> default_unexpected t)
    | (ExprOperand, _) :: breadcrumbs -> (
      match (breadcrumbs, t) with
      | (ExprBlock, _) :: _, Rbrace ->
        "It seems that this expression block is empty"
      | (ExprBlock, _) :: _, Bar ->
        (* Pattern matching *)
        "Looks like there might be an expression missing here"
      | (ExprSetField, _) :: _, _ ->
        "It seems that this record field mutation misses an expression"
      | (ExprArrayMutation, _) :: _, _ ->
        "Seems that an expression is missing, with what do I mutate the array?"
      | ((ExprBinaryAfterOp _ | ExprUnary), _) :: _, _ ->
        "Did you forget to write an expression here?"
      | (Grammar.LetBinding, _) :: _, _ ->
        "This let-binding misses an expression"
      | _ :: _, (Rbracket | Rbrace | Eof) -> "Missing expression"
      | _ -> "I'm not sure what to parse here when looking at \"" ^ name ^ "\"."
      )
    | (TypeParam, _) :: _ -> (
      match t with
      | Lident ident ->
        "Did you mean '" ^ ident ^ "? A Type parameter starts with a quote."
      | _ -> "I'm not sure what to parse here when looking at \"" ^ name ^ "\"."
      )
    | (Pattern, _) :: breadcrumbs -> (
      match (t, breadcrumbs) with
      | Equal, (LetBinding, _) :: _ ->
        "I was expecting a name for this let-binding. Example: `let message = \
         \"hello\"`"
      | In, (ExprFor, _) :: _ ->
        "A for-loop has the following form: `for i in 0 to 10`. Did you forget \
         to supply a name before `in`?"
      | EqualGreater, (PatternMatchCase, _) :: _ ->
        "I was expecting a pattern to match on before the `=>`"
      | token, _ when Token.is_keyword t -> reserved_keyword token
      | token, _ -> default_unexpected token)
    | _ ->
      (* TODO: match on circumstance to verify Lident needed ? *)
      if Token.is_keyword t then
        "`" ^ name
        ^ "` is a reserved keyword. Keywords need to be escaped: \\\""
        ^ Token.to_string t ^ "\""
      else "I'm not sure what to parse here when looking at \"" ^ name ^ "\".")

let make ~start_pos ~end_pos category = {start_pos; end_pos; category}

let print_report ?(custom_intro = None) ?(formatter = Format.err_formatter)
    diagnostics src =
  let rec print diagnostics src =
    match diagnostics with
    | [] -> ()
    | d :: rest ->
      (* A few specializations for best-effort error messages for old syntax etc. *)
      let msg =
        match d.category with
        | Unexpected {token = Token.Bar; _} ->
          let idx_prev = d.start_pos.pos_cnum - 1 in
          let idx_next = d.end_pos.pos_cnum in
          if
            idx_prev >= 0
            && idx_prev < String.length src
            && String.get src idx_prev = '['
          then
            let base = explain d in
            base
            ^ "\n\n\
              \  Did you mean to write an array literal? Arrays are written \
               with `[ ... ]` (not `[| ... |]`)."
            ^ "\n  Quick fix: replace `[|` with `[` and `|]` with `]`."
            ^ "\n  Example: `[|1, 2, 3|]` -> `[1, 2, 3]`"
          else if
            idx_next >= 0
            && idx_next < String.length src
            && String.get src idx_next = '>'
          then
            let base = explain d in
            base
            ^ "\n\n\
              \  The old data-last pipe `|>` has been removed from the language.\n\
              \  Refactor to use a data-first `->` pipe instead."
          else explain d
        | _ -> explain d
      in
      Location.report_error ~custom_intro ~src:(Some src) formatter
        Location.
          {
            loc =
              {loc_start = d.start_pos; loc_end = d.end_pos; loc_ghost = false};
            msg;
            sub = [];
            if_highlight = "";
          };
      (match rest with
      | [] -> ()
      | _ -> Format.fprintf formatter "@.");
      print rest src
  in
  Format.fprintf formatter "@[<v>";
  print (List.rev diagnostics) src;
  Format.fprintf formatter "@]@."

let unexpected token context = Unexpected {token; context}

let expected ?grammar pos token = Expected {context = grammar; pos; token}

let uident current_token = Uident current_token
let lident current_token = Lident current_token
let unclosed_string = UnclosedString
let unclosed_comment = UnclosedComment
let unclosed_template = UnclosedTemplate
let unknown_uchar code = UnknownUchar code
let message txt = Message txt
