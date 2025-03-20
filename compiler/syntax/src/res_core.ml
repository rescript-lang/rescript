module Doc = Res_doc
module Grammar = Res_grammar
module Token = Res_token
module Diagnostics = Res_diagnostics
module CommentTable = Res_comments_table
module ResPrinter = Res_printer
module Scanner = Res_scanner
module Parser = Res_parser

let mk_loc start_loc end_loc =
  Location.{loc_start = start_loc; loc_end = end_loc; loc_ghost = false}

type inline_types_context = {
  mutable found_inline_types:
    (string * Warnings.loc * Parsetree.type_kind) list;
  params: (Parsetree.core_type * Asttypes.variance) list;
}

module Recover = struct
  let default_expr () =
    let id = Location.mknoloc "rescript.exprhole" in
    Ast_helper.Exp.mk (Pexp_extension (id, PStr []))

  let default_type () =
    let id = Location.mknoloc "rescript.typehole" in
    Ast_helper.Typ.extension (id, PStr [])

  let default_pattern () =
    let id = Location.mknoloc "rescript.patternhole" in
    Ast_helper.Pat.extension (id, PStr [])

  let default_module_expr () = Ast_helper.Mod.structure []
  let default_module_type () = Ast_helper.Mty.signature []

  let default_signature_item =
    let id = Location.mknoloc "rescript.sigitemhole" in
    Ast_helper.Sig.extension (id, PStr [])

  let recover_equal_greater p =
    Parser.expect EqualGreater p;
    match p.Parser.token with
    | MinusGreater -> Parser.next p
    | _ -> ()

  let should_abort_list_parse p =
    let rec check breadcrumbs =
      match breadcrumbs with
      | [] -> false
      | (grammar, _) :: rest ->
        if Grammar.is_part_of_list grammar p.Parser.token then true
        else check rest
    in
    check p.breadcrumbs
end

module ErrorMessages = struct
  let list_pattern_spread =
    "List pattern matches only supports one `...` spread, at the end.\n\
     Explanation: a list spread at the tail is efficient, but a spread in the \
     middle would create new lists; out of performance concern, our pattern \
     matching currently guarantees to never create new intermediate data."

  let record_pattern_spread =
    "Record's `...` spread is not supported in pattern matches.\n\
     Explanation: you can't collect a subset of a record's field into its own \
     record, since a record needs an explicit declaration and that subset \
     wouldn't have one.\n\
     Solution: you need to pull out each field you want explicitly."
  (* let recordPatternUnderscore = "Record patterns only support one `_`, at the end." *)
  [@@live]

  let array_pattern_spread =
    "Array's `...` spread is not supported in pattern matches.\n\
     Explanation: such spread would create a subarray; out of performance \
     concern, our pattern matching currently guarantees to never create new \
     intermediate data.\n\
     Solution: if it's to validate the first few elements, use a `when` clause \
     + Array size check + `get` checks on the current pattern. If it's to \
     obtain a subarray, use `Array.sub` or `Belt.Array.slice`."

  let record_expr_spread =
    "Records can only have one `...` spread, at the beginning.\n\
     Explanation: since records have a known, fixed shape, a spread like `{a, \
     ...b}` wouldn't make sense, as `b` would override every field of `a` \
     anyway."

  let variant_ident =
    "A polymorphic variant (e.g. #id) must start with an alphabetical letter \
     or be a number (e.g. #742)"

  let experimental_if_let expr =
    let switch_expr = {expr with Parsetree.pexp_attributes = []} in
    Doc.concat
      [
        Doc.text "If-let is currently highly experimental.";
        Doc.line;
        Doc.text "Use a regular `switch` with pattern matching instead:";
        Doc.concat
          [
            Doc.hard_line;
            Doc.hard_line;
            ResPrinter.print_expression switch_expr CommentTable.empty;
          ];
      ]
    |> Doc.to_string ~width:80

  let type_param =
    "A type param consists of a singlequote followed by a name like `'a` or \
     `'A`"
  let type_var =
    "A type variable consists of a singlequote followed by a name like `'a` or \
     `'A`"

  let attribute_without_node (attr : Parsetree.attribute) =
    let {Asttypes.txt = attr_name}, _ = attr in
    "Did you forget to attach `" ^ attr_name
    ^ "` to an item?\n  Standalone attributes start with `@@` like: `@@"
    ^ attr_name ^ "`"

  let type_declaration_name_longident longident =
    "A type declaration's name cannot contain a module access. Did you mean `"
    ^ Longident.last longident ^ "`?"

  let tuple_single_element = "A tuple needs at least two elements"

  let missing_tilde_labeled_parameter name =
    if name = "" then "A labeled parameter starts with a `~`."
    else "A labeled parameter starts with a `~`. Did you mean: `~" ^ name ^ "`?"

  let string_interpolation_in_pattern =
    "String interpolation is not supported in pattern matching."

  let spread_in_record_declaration =
    "A record type declaration doesn't support the ... spread. Only an object \
     (with quoted field names) does."

  let object_quoted_field_name name =
    "An object type declaration needs quoted field names. Did you mean \""
    ^ name ^ "\"?"

  let forbidden_inline_record_declaration =
    "An inline record type declaration is only allowed in a variant \
     constructor's declaration or nested inside of a record type declaration"

  let poly_var_int_with_suffix number =
    "A numeric polymorphic variant cannot be followed by a letter. Did you \
     mean `#" ^ number ^ "`?"

  let multiple_inline_record_definitions_at_same_path =
    "Only one inline record definition is allowed per record field. This \
     defines more than one inline record."
end

module InExternal = struct
  let status = ref false
end

let jsx_attr = (Location.mknoloc "JSX", Parsetree.PStr [])
let ternary_attr = (Location.mknoloc "res.ternary", Parsetree.PStr [])
let if_let_attr = (Location.mknoloc "res.iflet", Parsetree.PStr [])
let make_await_attr loc = (Location.mkloc "res.await" loc, Parsetree.PStr [])
let suppress_fragile_match_warning_attr =
  ( Location.mknoloc "warning",
    Parsetree.PStr
      [
        Ast_helper.Str.eval
          (Ast_helper.Exp.constant (Pconst_string ("-4", None)));
      ] )
let make_braces_attr loc = (Location.mkloc "res.braces" loc, Parsetree.PStr [])
let template_literal_attr = (Location.mknoloc "res.template", Parsetree.PStr [])
let make_pat_variant_spread_attr =
  (Location.mknoloc "res.patVariantSpread", Parsetree.PStr [])

let tagged_template_literal_attr =
  (Location.mknoloc "res.taggedTemplate", Parsetree.PStr [])

let spread_attr = (Location.mknoloc "res.spread", Parsetree.PStr [])

type argument = {label: Asttypes.arg_label; expr: Parsetree.expression}

type type_parameter = {
  attrs: Ast_helper.attrs;
  label: Asttypes.arg_label;
  typ: Parsetree.core_type;
  start_pos: Lexing.position;
}

type typ_def_or_ext =
  | TypeDef of {
      rec_flag: Asttypes.rec_flag;
      types: Parsetree.type_declaration list;
    }
  | TypeExt of Parsetree.type_extension

type fundef_type_param = {
  attrs: Parsetree.attributes;
  locs: string Location.loc list;
  p_pos: Lexing.position;
}

type fundef_term_param = {
  attrs: Parsetree.attributes;
  p_label: Asttypes.arg_label;
  expr: Parsetree.expression option;
  pat: Parsetree.pattern;
  p_pos: Lexing.position;
}

(* Single parameter of a function definition  (type a b, x, ~y) *)
type fundef_parameter =
  | TermParameter of fundef_term_param
  | TypeParameter of fundef_type_param

type record_pattern_item =
  | PatUnderscore
  | PatField of (Ast_helper.lid * Parsetree.pattern * bool (* optional *))

type context = OrdinaryExpr | TernaryTrueBranchExpr | WhenExpr

(* Extracts type and term parameters from a list of function definition parameters, combining all type parameters into one *)
let rec extract_fundef_params ~(type_acc : fundef_type_param option)
    ~(term_acc : fundef_term_param list) (params : fundef_parameter list) :
    fundef_type_param option * fundef_term_param list =
  match params with
  | TermParameter tp :: rest ->
    extract_fundef_params ~type_acc ~term_acc:(tp :: term_acc) rest
  | TypeParameter tp :: rest ->
    let type_acc =
      match type_acc with
      | Some tpa ->
        Some
          {
            attrs = tpa.attrs @ tp.attrs;
            locs = tpa.locs @ tp.locs;
            p_pos = tpa.p_pos;
          }
      | None -> Some tp
    in
    extract_fundef_params ~type_acc ~term_acc rest
  | [] -> (type_acc, List.rev term_acc)

let get_closing_token = function
  | Token.Lparen -> Token.Rparen
  | Lbrace -> Rbrace
  | Lbracket -> Rbracket
  | List -> Rbrace
  | Dict -> Rbrace
  | LessThan -> GreaterThan
  | _ -> assert false

let rec go_to_closing closing_token state =
  match (state.Parser.token, closing_token) with
  | Rparen, Token.Rparen
  | Rbrace, Rbrace
  | Rbracket, Rbracket
  | GreaterThan, GreaterThan ->
    Parser.next state;
    ()
  | ((Token.Lbracket | Lparen | Lbrace | List | Dict | LessThan) as t), _ ->
    Parser.next state;
    go_to_closing (get_closing_token t) state;
    go_to_closing closing_token state
  | (Rparen | Token.Rbrace | Rbracket | Eof), _ ->
    () (* TODO: how do report errors here? *)
  | _ ->
    Parser.next state;
    go_to_closing closing_token state

(* Madness *)
let is_es6_arrow_expression ~in_ternary p =
  Parser.lookahead p (fun state ->
      let async =
        match state.Parser.token with
        | Lident "async" ->
          Parser.next state;
          true
        | _ -> false
      in
      match state.Parser.token with
      | Lident _ | Underscore -> (
        Parser.next state;
        match state.Parser.token with
        (* Don't think that this valid
         * Imagine: let x = (a: int)
         * This is a parenthesized expression with a type constraint, wait for
         * the arrow *)
        (* | Colon when not inTernary -> true *)
        | EqualGreater -> true
        | _ -> false)
      | Lparen -> (
        let prev_end_pos = state.prev_end_pos in
        Parser.next state;
        match state.token with
        (* arrived at `()` here *)
        | Rparen -> (
          Parser.next state;
          match state.Parser.token with
          (* arrived at `() :` here *)
          | Colon when not in_ternary -> (
            Parser.next state;
            match state.Parser.token with
            (* arrived at `() :typ` here *)
            | Lident _ -> (
              Parser.next state;
              (match state.Parser.token with
              (* arrived at `() :typ<` here *)
              | LessThan ->
                Parser.next state;
                go_to_closing GreaterThan state
              | _ -> ());
              match state.Parser.token with
              (* arrived at `() :typ =>` or `() :typ<'a,'b> =>` here *)
              | EqualGreater -> true
              | _ -> false)
            | _ -> true)
          | EqualGreater -> true
          | _ -> false)
        | Dot (* uncurried *) -> true
        | Tilde when not async -> true
        | Backtick ->
          false
          (* (` always indicates the start of an expr, can't be es6 parameter *)
        | _ -> (
          go_to_closing Rparen state;
          match state.Parser.token with
          | EqualGreater -> true
          (* | Lbrace TODO: detect missing =>, is this possible? *)
          | Colon when not in_ternary -> true
          | Rparen ->
            (* imagine having something as :
             * switch colour {
             * | Red
             *    when l == l'
             *    || (&Clflags.classic && (l == Nolabel && !is_optional(l'))) => (t1, t2)
             * We'll arrive at the outer rparen just before the =>.
             * This is not an es6 arrow.
             *)
            false
          | _ -> (
            Parser.next_unsafe state;
            (* error recovery, peek at the next token,
             * (elements, providerId] => {
             *  in the example above, we have an unbalanced ] here
             *)
            match state.Parser.token with
            | EqualGreater
              when state.start_pos.pos_lnum == prev_end_pos.pos_lnum ->
              true
            | _ -> false)))
      | _ -> false)

let is_es6_arrow_functor p =
  Parser.lookahead p (fun state ->
      match state.Parser.token with
      (* | Uident _ | Underscore -> *)
      (* Parser.next state; *)
      (* begin match state.Parser.token with *)
      (* | EqualGreater -> true *)
      (* | _ -> false *)
      (* end *)
      | Lparen -> (
        Parser.next state;
        match state.token with
        | Rparen -> (
          Parser.next state;
          match state.token with
          | Colon | EqualGreater -> true
          | _ -> false)
        | _ -> (
          go_to_closing Rparen state;
          match state.Parser.token with
          | EqualGreater | Lbrace -> true
          | Colon -> true
          | _ -> false))
      | _ -> false)

let is_es6_arrow_type p =
  Parser.lookahead p (fun state ->
      match state.Parser.token with
      | Lparen -> (
        Parser.next state;
        match state.Parser.token with
        | Rparen -> (
          Parser.next state;
          match state.Parser.token with
          | EqualGreater -> true
          | _ -> false)
        | Tilde | Dot -> true
        | _ -> (
          go_to_closing Rparen state;
          match state.Parser.token with
          | EqualGreater -> true
          | _ -> false))
      | Tilde -> true
      | _ -> false)

let build_longident words =
  match List.rev words with
  | [] -> assert false
  | hd :: tl -> List.fold_left (fun p s -> Longident.Ldot (p, s)) (Lident hd) tl

let make_infix_operator (p : Parser.t) token start_pos end_pos =
  let stringified_token =
    if token = Token.Equal then (
      (* TODO: could have a totally different meaning like x->fooSet(y)*)
      Parser.err ~start_pos ~end_pos p
        (Diagnostics.message "Did you mean `==` here?");
      "=")
    else Token.to_string token
  in
  let loc = mk_loc start_pos end_pos in
  let operator = Location.mkloc (Longident.Lident stringified_token) loc in
  Ast_helper.Exp.ident ~loc operator

let negate_string s =
  if String.length s > 0 && (s.[0] [@doesNotRaise]) = '-' then
    (String.sub [@doesNotRaise]) s 1 (String.length s - 1)
  else "-" ^ s

let make_unary_expr start_pos token_end token operand =
  match (token, operand.Parsetree.pexp_desc) with
  | (Token.Plus | PlusDot), Pexp_constant (Pconst_integer _ | Pconst_float _) ->
    operand
  | Minus, Pexp_constant (Pconst_integer (n, m)) ->
    {
      operand with
      pexp_desc = Pexp_constant (Pconst_integer (negate_string n, m));
    }
  | (Minus | MinusDot), Pexp_constant (Pconst_float (n, m)) ->
    {operand with pexp_desc = Pexp_constant (Pconst_float (negate_string n, m))}
  | (Token.Plus | PlusDot | Minus | MinusDot), _ ->
    let token_loc = mk_loc start_pos token_end in
    let operator = "~" ^ Token.to_string token in
    Ast_helper.Exp.apply
      ~loc:(mk_loc start_pos operand.Parsetree.pexp_loc.loc_end)
      (Ast_helper.Exp.ident ~loc:token_loc
         (Location.mkloc (Longident.Lident operator) token_loc))
      [(Nolabel, operand)]
  | Token.Bang, _ ->
    let token_loc = mk_loc start_pos token_end in
    Ast_helper.Exp.apply
      ~loc:(mk_loc start_pos operand.Parsetree.pexp_loc.loc_end)
      (Ast_helper.Exp.ident ~loc:token_loc
         (Location.mkloc (Longident.Lident "not") token_loc))
      [(Nolabel, operand)]
  | _ -> operand

let make_list_expression loc seq ext_opt =
  let rec handle_seq = function
    | [] -> (
      match ext_opt with
      | Some ext -> ext
      | None ->
        let loc = {loc with Location.loc_ghost = true} in
        let nil = Location.mkloc (Longident.Lident "[]") loc in
        Ast_helper.Exp.construct ~loc nil None)
    | e1 :: el ->
      let exp_el = handle_seq el in
      let loc =
        mk_loc e1.Parsetree.pexp_loc.Location.loc_start exp_el.pexp_loc.loc_end
      in
      let arg = Ast_helper.Exp.tuple ~loc [e1; exp_el] in
      Ast_helper.Exp.construct ~loc
        (Location.mkloc (Longident.Lident "::") loc)
        (Some arg)
  in
  let expr = handle_seq seq in
  {expr with pexp_loc = loc}

let make_list_pattern loc seq ext_opt =
  let rec handle_seq = function
    | [] ->
      let base_case =
        match ext_opt with
        | Some ext -> ext
        | None ->
          let loc = {loc with Location.loc_ghost = true} in
          let nil = {Location.txt = Longident.Lident "[]"; loc} in
          Ast_helper.Pat.construct ~loc nil None
      in
      base_case
    | p1 :: pl ->
      let pat_pl = handle_seq pl in
      let loc =
        mk_loc p1.Parsetree.ppat_loc.loc_start pat_pl.ppat_loc.loc_end
      in
      let arg = Ast_helper.Pat.mk ~loc (Ppat_tuple [p1; pat_pl]) in
      Ast_helper.Pat.mk ~loc
        (Ppat_construct (Location.mkloc (Longident.Lident "::") loc, Some arg))
  in
  handle_seq seq

(* TODO: diagnostic reporting *)
let lident_of_path longident =
  match Longident.flatten longident |> List.rev with
  | [] -> ""
  | ident :: _ -> ident

let make_newtypes ~attrs ~loc newtypes exp =
  let expr =
    List.fold_right
      (fun newtype exp -> Ast_helper.Exp.mk ~loc (Pexp_newtype (newtype, exp)))
      newtypes exp
  in
  {expr with pexp_attributes = attrs}

(* locally abstract types syntax sugar
 * Transforms
 *  let f: type t u v. = (foo : list</t, u, v/>) => ...
 * into
 *  let f = (type t u v. foo : list</t, u, v/>) => ...
 *)
let wrap_type_annotation ~loc newtypes core_type body =
  let exp =
    make_newtypes ~attrs:[] ~loc newtypes
      (Ast_helper.Exp.constraint_ ~loc body core_type)
  in
  let typ =
    Ast_helper.Typ.poly ~loc newtypes
      (Ast_helper.Typ.varify_constructors newtypes core_type)
  in
  (exp, typ)

(**
  * process the occurrence of _ in the arguments of a function application
  * replace _ with a new variable, currently __x, in the arguments
  * return a wrapping function that wraps ((__x) => ...) around an expression
  * e.g. foo(_, 3) becomes (__x) => foo(__x, 3)
  *)
let process_underscore_application args =
  let exp_question = ref None in
  let hidden_var = "__x" in
  let check_arg ((lab, exp) as arg) =
    match exp.Parsetree.pexp_desc with
    | Pexp_ident ({txt = Lident "_"} as id) ->
      let new_id = Location.mkloc (Longident.Lident hidden_var) id.loc in
      let new_exp = Ast_helper.Exp.mk (Pexp_ident new_id) ~loc:exp.pexp_loc in
      exp_question := Some new_exp;
      (lab, new_exp)
    | _ -> arg
  in
  let args = List.map check_arg args in
  let wrap (exp_apply : Parsetree.expression) =
    match !exp_question with
    | Some {pexp_loc = loc} ->
      let pattern =
        Ast_helper.Pat.mk
          (Ppat_var (Location.mkloc hidden_var loc))
          ~loc:Location.none
      in
      let fun_expr =
        Ast_helper.Exp.fun_ ~loc ~arity:(Some 1) Nolabel None pattern exp_apply
      in
      Ast_uncurried.uncurried_fun ~arity:1 fun_expr
    | None -> exp_apply
  in
  (args, wrap)

(* Transform A.a into a. For use with punned record fields as in {A.a, b}. *)
let remove_module_name_from_punned_field_value exp =
  match exp.Parsetree.pexp_desc with
  | Pexp_ident path_ident ->
    {
      exp with
      pexp_desc =
        Pexp_ident
          {path_ident with txt = Lident (Longident.last path_ident.txt)};
    }
  | _ -> exp

let rec parse_lident p =
  let recover_lident p =
    if
      Token.is_keyword p.Parser.token
      && p.Parser.prev_end_pos.pos_lnum == p.start_pos.pos_lnum
    then (
      Parser.err p (Diagnostics.lident p.Parser.token);
      Parser.next p;
      None)
    else
      let rec loop p =
        if (not (Recover.should_abort_list_parse p)) && p.token <> Eof then (
          Parser.next p;
          loop p)
      in
      Parser.err p (Diagnostics.lident p.Parser.token);
      Parser.next p;
      loop p;
      match p.Parser.token with
      | Lident _ -> Some ()
      | _ -> None
  in
  let start_pos = p.Parser.start_pos in
  match p.Parser.token with
  | Lident ident ->
    Parser.next p;
    let loc = mk_loc start_pos p.prev_end_pos in
    (ident, loc)
  | Eof ->
    Parser.err ~start_pos p
      (Diagnostics.unexpected p.Parser.token p.breadcrumbs);
    ("_", mk_loc start_pos p.prev_end_pos)
  | _ -> (
    match recover_lident p with
    | Some () -> parse_lident p
    | None -> ("_", mk_loc start_pos p.prev_end_pos))

let parse_ident ~msg ~start_pos p =
  match p.Parser.token with
  | Lident ident | Uident ident ->
    Parser.next p;
    let loc = mk_loc start_pos p.prev_end_pos in
    (ident, loc)
  | token
    when Token.is_keyword token
         && p.prev_end_pos.pos_lnum == p.start_pos.pos_lnum ->
    let token_txt = Token.to_string token in
    let msg =
      "`" ^ token_txt
      ^ "` is a reserved keyword. Keywords need to be escaped: \\\"" ^ token_txt
      ^ "\""
    in
    Parser.err ~start_pos p (Diagnostics.message msg);
    Parser.next p;
    (token_txt, mk_loc start_pos p.prev_end_pos)
  | _token ->
    Parser.err ~start_pos p (Diagnostics.message msg);
    Parser.next p;
    ("", mk_loc start_pos p.prev_end_pos)

let parse_hash_ident ~start_pos p =
  Parser.expect Hash p;
  match p.token with
  | String text ->
    Parser.next p;
    (text, mk_loc start_pos p.prev_end_pos)
  | Int {i; suffix} ->
    let () =
      match suffix with
      | Some _ ->
        Parser.err p
          (Diagnostics.message (ErrorMessages.poly_var_int_with_suffix i))
      | None -> ()
    in
    Parser.next p;
    (i, mk_loc start_pos p.prev_end_pos)
  | Eof ->
    Parser.err ~start_pos p (Diagnostics.unexpected p.token p.breadcrumbs);
    ("", mk_loc start_pos p.prev_end_pos)
  | _ -> parse_ident ~start_pos ~msg:ErrorMessages.variant_ident p

(* Ldot (Ldot (Lident "Foo", "Bar"), "baz") *)
let parse_value_path p =
  let start_pos = p.Parser.start_pos in
  let rec aux p path =
    let start_pos = p.Parser.start_pos in
    let token = p.token in

    Parser.next p;
    if p.Parser.token = Dot then (
      Parser.expect Dot p;

      match p.Parser.token with
      | Lident ident -> Longident.Ldot (path, ident)
      | Uident uident -> aux p (Ldot (path, uident))
      | token ->
        Parser.err p (Diagnostics.unexpected token p.breadcrumbs);
        Longident.Ldot (path, "_"))
    else (
      Parser.err p ~start_pos ~end_pos:p.prev_end_pos (Diagnostics.lident token);
      path)
  in
  let ident =
    match p.Parser.token with
    | Lident ident ->
      Parser.next p;
      Longident.Lident ident
    | Uident ident ->
      let res = aux p (Lident ident) in
      Parser.next_unsafe p;
      res
    | token ->
      Parser.err p (Diagnostics.unexpected token p.breadcrumbs);
      Parser.next_unsafe p;
      Longident.Lident "_"
  in
  Location.mkloc ident (mk_loc start_pos p.prev_end_pos)

let parse_value_path_after_dot p =
  let start_pos = p.Parser.start_pos in
  match p.Parser.token with
  | Lident _ | Uident _ -> parse_value_path p
  | token ->
    Parser.err p (Diagnostics.unexpected token p.breadcrumbs);
    Location.mkloc (Longident.Lident "_") (mk_loc start_pos p.prev_end_pos)

let parse_value_path_tail p start_pos ident =
  let rec loop p path =
    match p.Parser.token with
    | Lident ident ->
      Parser.next p;
      Location.mkloc
        (Longident.Ldot (path, ident))
        (mk_loc start_pos p.prev_end_pos)
    | Uident ident ->
      Parser.next p;
      Parser.expect Dot p;
      loop p (Longident.Ldot (path, ident))
    | token ->
      Parser.err p (Diagnostics.unexpected token p.breadcrumbs);
      Location.mkloc
        (Longident.Ldot (path, "_"))
        (mk_loc start_pos p.prev_end_pos)
  in
  loop p ident

let parse_module_long_ident_tail ~lowercase p start_pos ident =
  let rec loop p acc =
    match p.Parser.token with
    | Lident ident when lowercase ->
      Parser.next p;
      let lident = Longident.Ldot (acc, ident) in
      Location.mkloc lident (mk_loc start_pos p.prev_end_pos)
    | Uident ident -> (
      Parser.next p;
      let end_pos = p.prev_end_pos in
      let lident = Longident.Ldot (acc, ident) in
      match p.Parser.token with
      | Dot ->
        Parser.next p;
        loop p lident
      | _ -> Location.mkloc lident (mk_loc start_pos end_pos))
    | t ->
      Parser.err p (Diagnostics.uident t);
      Location.mkloc
        (Longident.Ldot (acc, "_"))
        (mk_loc start_pos p.prev_end_pos)
  in
  loop p ident

(* Parses module identifiers:
     Foo
     Foo.Bar *)
let parse_module_long_ident ~lowercase p =
  (* Parser.leaveBreadcrumb p Reporting.ModuleLongIdent; *)
  let start_pos = p.Parser.start_pos in
  let module_ident =
    match p.Parser.token with
    | Lident ident when lowercase ->
      let loc = mk_loc start_pos p.end_pos in
      let lident = Longident.Lident ident in
      Parser.next p;
      Location.mkloc lident loc
    | Uident ident -> (
      let lident = Longident.Lident ident in
      let end_pos = p.end_pos in
      Parser.next p;
      match p.Parser.token with
      | Dot ->
        Parser.next p;
        parse_module_long_ident_tail ~lowercase p start_pos lident
      | _ -> Location.mkloc lident (mk_loc start_pos end_pos))
    | t ->
      Parser.err p (Diagnostics.uident t);
      Location.mkloc (Longident.Lident "_") (mk_loc start_pos p.prev_end_pos)
  in
  (* Parser.eatBreadcrumb p; *)
  module_ident

let verify_jsx_opening_closing_name p name_expr =
  let closing =
    match p.Parser.token with
    | Lident lident ->
      Parser.next p;
      Longident.Lident lident
    | Uident _ -> (parse_module_long_ident ~lowercase:true p).txt
    | _ -> Longident.Lident ""
  in
  match name_expr.Parsetree.pexp_desc with
  | Pexp_ident opening_ident ->
    let opening =
      let without_create_element =
        Longident.flatten opening_ident.txt
        |> List.filter (fun s -> s <> "createElement")
      in
      match Longident.unflatten without_create_element with
      | Some li -> li
      | None -> Longident.Lident ""
    in
    opening = closing
  | _ -> assert false

let string_of_pexp_ident name_expr =
  match name_expr.Parsetree.pexp_desc with
  | Pexp_ident opening_ident ->
    Longident.flatten opening_ident.txt
    |> List.filter (fun s -> s <> "createElement")
    |> String.concat "."
  | _ -> ""

(* open-def ::=
 *   | open module-path
 *   | open! module-path *)
let parse_open_description ~attrs p =
  Parser.leave_breadcrumb p Grammar.OpenDescription;
  let start_pos = p.Parser.start_pos in
  Parser.expect Open p;
  let override =
    if Parser.optional p Token.Bang then Asttypes.Override else Asttypes.Fresh
  in
  let modident = parse_module_long_ident ~lowercase:false p in
  let loc = mk_loc start_pos p.prev_end_pos in
  Parser.eat_breadcrumb p;
  Ast_helper.Opn.mk ~loc ~attrs ~override modident

(* constant	::=	integer-literal   *)
(* ∣	 float-literal   *)
(* ∣	 string-literal   *)
let parse_constant p =
  let is_negative =
    match p.Parser.token with
    | Token.Minus ->
      Parser.next p;
      true
    | Plus ->
      Parser.next p;
      false
    | _ -> false
  in
  let constant =
    match p.Parser.token with
    | Int {i; suffix} ->
      (* Only decimal literal is allowed for bigint *)
      if suffix = Some 'n' && not (Bigint_utils.is_valid i) then
        Parser.err p
          (Diagnostics.message
             "Invalid bigint literal. Only decimal literal is allowed for \
              bigint.");
      let int_txt = if is_negative then "-" ^ i else i in
      Parsetree.Pconst_integer (int_txt, suffix)
    | Float {f; suffix} ->
      let float_txt = if is_negative then "-" ^ f else f in
      Parsetree.Pconst_float (float_txt, suffix)
    | String s ->
      Pconst_string (s, if p.mode = ParseForTypeChecker then Some "js" else None)
    | Codepoint {c; original} ->
      if p.mode = ParseForTypeChecker then Pconst_char c
      else
        (* Pconst_char char does not have enough information for formatting.
         * When parsing for the printer, we encode the char contents as a string
         * with a special prefix. *)
        Pconst_string (original, Some "INTERNAL_RES_CHAR_CONTENTS")
    | token ->
      Parser.err p (Diagnostics.unexpected token p.breadcrumbs);
      Pconst_string ("", None)
  in
  Parser.next_unsafe p;
  constant

let parse_template_constant ~prefix (p : Parser.t) =
  (* Arrived at the ` char *)
  let start_pos = p.start_pos in
  Parser.next_template_literal_token p;
  match p.token with
  | TemplateTail (txt, _) ->
    Parser.next p;
    Parsetree.Pconst_string (txt, prefix)
  | _ ->
    let rec skip_tokens () =
      if p.token <> Eof then (
        Parser.next p;
        match p.token with
        | Backtick ->
          Parser.next p;
          ()
        | _ -> skip_tokens ())
    in
    skip_tokens ();
    Parser.err ~start_pos ~end_pos:p.prev_end_pos p
      (Diagnostics.message ErrorMessages.string_interpolation_in_pattern);
    Pconst_string ("", None)

let parse_comma_delimited_region p ~grammar ~closing ~f =
  Parser.leave_breadcrumb p grammar;
  let rec loop nodes =
    match f p with
    | Some node -> (
      match p.Parser.token with
      | Comma ->
        Parser.next p;
        loop (node :: nodes)
      | token when token = closing || token = Eof -> List.rev (node :: nodes)
      | _ when Grammar.is_list_element grammar p.token ->
        (* missing comma between nodes in the region and the current token
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
         *)
        Parser.expect Comma p;
        loop (node :: nodes)
      | _ ->
        if
          not
            (p.token = Eof || p.token = closing
            || Recover.should_abort_list_parse p)
        then Parser.expect Comma p;
        if p.token = Semicolon then Parser.next p;
        loop (node :: nodes))
    | None ->
      if p.token = Eof || p.token = closing || Recover.should_abort_list_parse p
      then List.rev nodes
      else (
        Parser.err p (Diagnostics.unexpected p.token p.breadcrumbs);
        Parser.next p;
        loop nodes)
  in
  let nodes = loop [] in
  Parser.eat_breadcrumb p;
  nodes

let parse_comma_delimited_reversed_list p ~grammar ~closing ~f =
  Parser.leave_breadcrumb p grammar;
  let rec loop nodes =
    match f p with
    | Some node -> (
      match p.Parser.token with
      | Comma ->
        Parser.next p;
        loop (node :: nodes)
      | token when token = closing || token = Eof -> node :: nodes
      | _ when Grammar.is_list_element grammar p.token ->
        (* missing comma between nodes in the region and the current token
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
         *)
        Parser.expect Comma p;
        loop (node :: nodes)
      | _ ->
        if
          not
            (p.token = Eof || p.token = closing
            || Recover.should_abort_list_parse p)
        then Parser.expect Comma p;
        if p.token = Semicolon then Parser.next p;
        loop (node :: nodes))
    | None ->
      if p.token = Eof || p.token = closing || Recover.should_abort_list_parse p
      then nodes
      else (
        Parser.err p (Diagnostics.unexpected p.token p.breadcrumbs);
        Parser.next p;
        loop nodes)
  in
  let nodes = loop [] in
  Parser.eat_breadcrumb p;
  nodes

let parse_delimited_region p ~grammar ~closing ~f =
  Parser.leave_breadcrumb p grammar;
  let rec loop nodes =
    match f p with
    | Some node -> loop (node :: nodes)
    | None ->
      if
        p.Parser.token = Token.Eof || p.token = closing
        || Recover.should_abort_list_parse p
      then List.rev nodes
      else (
        Parser.err p (Diagnostics.unexpected p.token p.breadcrumbs);
        Parser.next p;
        loop nodes)
  in
  let nodes = loop [] in
  Parser.eat_breadcrumb p;
  nodes

let parse_region p ~grammar ~f =
  Parser.leave_breadcrumb p grammar;
  let rec loop nodes =
    match f p with
    | Some node -> loop (node :: nodes)
    | None ->
      if p.Parser.token = Token.Eof || Recover.should_abort_list_parse p then
        List.rev nodes
      else (
        Parser.err p (Diagnostics.unexpected p.token p.breadcrumbs);
        Parser.next p;
        loop nodes)
  in
  let nodes = loop [] in
  Parser.eat_breadcrumb p;
  nodes

(* let-binding	::=	pattern =  expr   *)
(* ∣	 value-name  { parameter }  [: typexpr]  [:> typexpr] =  expr   *)
(* ∣	 value-name :  poly-typexpr =  expr   *)

(* pattern	::=	value-name   *)
(* ∣	 _   *)
(* ∣	 constant   *)
(* ∣	 pattern as  value-name   *)
(* ∣	 ( pattern )   *)
(* ∣	 ( pattern :  typexpr )   *)
(* ∣	 pattern |  pattern   *)
(* ∣	 constr  pattern   *)
(* ∣	 #variant variant-pattern *)
(* ∣	 #...type  *)
(* ∣	 / pattern  { , pattern }+  /   *)
(* ∣	 { field  [: typexpr]  [= pattern] { ; field  [: typexpr]  [= pattern] }  [; _ ] [ ; ] }   *)
(* ∣	 [ pattern  { ; pattern }  [ ; ] ]   *)
(* ∣	 pattern ::  pattern   *)
(* ∣	 [| pattern  { ; pattern }  [ ; ] |]   *)
(* ∣	 char-literal ..  char-literal *)
(*	∣	 exception pattern  *)
let rec parse_pattern ?(alias = true) ?(or_ = true) p =
  let start_pos = p.Parser.start_pos in
  let attrs = parse_attributes p in
  let pat =
    match p.Parser.token with
    | (True | False) as token ->
      let end_pos = p.end_pos in
      Parser.next p;
      let loc = mk_loc start_pos end_pos in
      Ast_helper.Pat.construct ~loc
        (Location.mkloc (Longident.Lident (Token.to_string token)) loc)
        None
    | Int _ | String _ | Float _ | Codepoint _ | Minus | Plus -> (
      let c = parse_constant p in
      match p.token with
      | DotDot ->
        Parser.next p;
        let c2 = parse_constant p in
        Ast_helper.Pat.interval ~loc:(mk_loc start_pos p.prev_end_pos) c c2
      | _ -> Ast_helper.Pat.constant ~loc:(mk_loc start_pos p.prev_end_pos) c)
    | Backtick ->
      let constant = parse_template_constant ~prefix:(Some "js") p in
      Ast_helper.Pat.constant ~attrs:[template_literal_attr]
        ~loc:(mk_loc start_pos p.prev_end_pos)
        constant
    | Lparen -> (
      Parser.next p;
      match p.token with
      | Rparen ->
        Parser.next p;
        let loc = mk_loc start_pos p.prev_end_pos in
        let lid = Location.mkloc (Longident.Lident "()") loc in
        Ast_helper.Pat.construct ~loc lid None
      | _ -> (
        let pat = parse_constrained_pattern p in
        match p.token with
        | Comma ->
          Parser.next p;
          parse_tuple_pattern ~attrs ~first:pat ~start_pos p
        | _ ->
          Parser.expect Rparen p;
          let loc = mk_loc start_pos p.prev_end_pos in
          {
            pat with
            ppat_loc = loc;
            ppat_attributes = attrs @ pat.Parsetree.ppat_attributes;
          }))
    | Lbracket -> parse_array_pattern ~attrs p
    | Lbrace -> parse_record_pattern ~attrs p
    | Underscore ->
      let end_pos = p.end_pos in
      let loc = mk_loc start_pos end_pos in
      Parser.next p;
      Ast_helper.Pat.any ~loc ~attrs ()
    | Lident ident -> (
      let end_pos = p.end_pos in
      let loc = mk_loc start_pos end_pos in
      Parser.next p;
      match p.token with
      | Backtick ->
        let constant = parse_template_constant ~prefix:(Some ident) p in
        Ast_helper.Pat.constant ~loc:(mk_loc start_pos p.prev_end_pos) constant
      | _ -> Ast_helper.Pat.var ~loc ~attrs (Location.mkloc ident loc))
    | Uident _ -> (
      let constr = parse_module_long_ident ~lowercase:false p in
      match p.Parser.token with
      | Lparen -> parse_constructor_pattern_args p constr start_pos attrs
      | _ -> Ast_helper.Pat.construct ~loc:constr.loc ~attrs constr None)
    | DotDotDot ->
      Parser.next p;
      let ident = parse_value_path p in
      let loc = mk_loc start_pos ident.loc.loc_end in
      Ast_helper.Pat.type_ ~loc
        ~attrs:(make_pat_variant_spread_attr :: attrs)
        ident
    | Hash -> (
      Parser.next p;
      if p.Parser.token == DotDotDot then (
        Parser.next p;
        let ident = parse_value_path p in
        let loc = mk_loc start_pos ident.loc.loc_end in
        Ast_helper.Pat.type_ ~loc ~attrs ident)
      else
        let ident, loc =
          match p.token with
          | String text ->
            Parser.next p;
            (text, mk_loc start_pos p.prev_end_pos)
          | Int {i; suffix} ->
            let () =
              match suffix with
              | Some _ ->
                Parser.err p
                  (Diagnostics.message
                     (ErrorMessages.poly_var_int_with_suffix i))
              | None -> ()
            in
            Parser.next p;
            (i, mk_loc start_pos p.prev_end_pos)
          | Eof ->
            Parser.err ~start_pos p
              (Diagnostics.unexpected p.token p.breadcrumbs);
            ("", mk_loc start_pos p.prev_end_pos)
          | _ -> parse_ident ~msg:ErrorMessages.variant_ident ~start_pos p
        in
        match p.Parser.token with
        | Lparen -> parse_variant_pattern_args p ident start_pos attrs
        | _ -> Ast_helper.Pat.variant ~loc ~attrs ident None)
    | Exception ->
      Parser.next p;
      let pat = parse_pattern ~alias:false ~or_:false p in
      let loc = mk_loc start_pos p.prev_end_pos in
      Ast_helper.Pat.exception_ ~loc ~attrs pat
    | List ->
      Parser.next p;
      parse_list_pattern ~start_pos ~attrs p
    | Dict ->
      Parser.next p;
      parse_dict_pattern ~start_pos ~attrs p
    | Module -> parse_module_pattern ~attrs p
    | Percent ->
      let extension = parse_extension p in
      let loc = mk_loc start_pos p.prev_end_pos in
      Ast_helper.Pat.extension ~loc ~attrs extension
    | Eof ->
      Parser.err p (Diagnostics.unexpected p.Parser.token p.breadcrumbs);
      Recover.default_pattern ()
    | token -> (
      Parser.err p (Diagnostics.unexpected token p.breadcrumbs);
      match
        skip_tokens_and_maybe_retry p
          ~is_start_of_grammar:Grammar.is_atomic_pattern_start
      with
      | None -> Recover.default_pattern ()
      | Some () -> parse_pattern p)
  in
  let pat = if alias then parse_alias_pattern ~attrs pat p else pat in
  if or_ then parse_or_pattern pat p else pat

and skip_tokens_and_maybe_retry p ~is_start_of_grammar =
  if
    Token.is_keyword p.Parser.token
    && p.Parser.prev_end_pos.pos_lnum == p.start_pos.pos_lnum
  then (
    Parser.next p;
    None)
  else if Recover.should_abort_list_parse p then
    if is_start_of_grammar p.Parser.token then (
      Parser.next p;
      Some ())
    else None
  else (
    Parser.next p;
    let rec loop p =
      if not (Recover.should_abort_list_parse p) then (
        Parser.next p;
        loop p)
    in
    loop p;
    if is_start_of_grammar p.Parser.token then Some () else None)

(* alias ::= pattern as lident *)
and parse_alias_pattern ~attrs pattern p =
  match p.Parser.token with
  | As ->
    Parser.next p;
    let name, loc = parse_lident p in
    let name = Location.mkloc name loc in
    Ast_helper.Pat.alias
      ~loc:{pattern.ppat_loc with loc_end = p.prev_end_pos}
      ~attrs pattern name
  | _ -> pattern

(* or ::= pattern | pattern
 * precedence: Red | Blue | Green is interpreted as (Red | Blue) | Green *)
and parse_or_pattern pattern1 p =
  let rec loop pattern1 =
    match p.Parser.token with
    | Bar ->
      Parser.next p;
      let pattern2 = parse_pattern ~or_:false p in
      let loc =
        {pattern1.Parsetree.ppat_loc with loc_end = pattern2.ppat_loc.loc_end}
      in
      loop (Ast_helper.Pat.or_ ~loc pattern1 pattern2)
    | _ -> pattern1
  in
  loop pattern1

and parse_non_spread_pattern ~msg p =
  let () =
    match p.Parser.token with
    | DotDotDot ->
      Parser.err p (Diagnostics.message msg);
      Parser.next p
    | _ -> ()
  in
  match p.Parser.token with
  | token when Grammar.is_pattern_start token -> (
    let pat = parse_pattern p in
    match p.Parser.token with
    | Colon ->
      Parser.next p;
      let typ = parse_typ_expr p in
      let loc = mk_loc pat.ppat_loc.loc_start typ.Parsetree.ptyp_loc.loc_end in
      Some (Ast_helper.Pat.constraint_ ~loc pat typ)
    | _ -> Some pat)
  | _ -> None

and parse_constrained_pattern p =
  let pat = parse_pattern p in
  match p.Parser.token with
  | Colon ->
    Parser.next p;
    let typ = parse_typ_expr p in
    let loc = mk_loc pat.ppat_loc.loc_start typ.Parsetree.ptyp_loc.loc_end in
    Ast_helper.Pat.constraint_ ~loc pat typ
  | _ -> pat

and parse_constrained_pattern_region p =
  match p.Parser.token with
  | token when Grammar.is_pattern_start token ->
    Some (parse_constrained_pattern p)
  | _ -> None

and parse_optional_label p =
  match p.Parser.token with
  | Question ->
    Parser.next p;
    true
  | _ -> false

(* field ::=
 *   | longident
 *   | longident : pattern
 *   | longident as lident
 *
 *  row ::=
 *	 | field ,
 *	 | field , _
 *	 | field , _,
 *)
and parse_record_pattern_row_field ~attrs p =
  let label = parse_value_path p in
  let pattern, optional =
    match p.Parser.token with
    | Colon ->
      Parser.next p;
      let optional = parse_optional_label p in
      let pat = parse_pattern p in
      (pat, optional)
    | _ ->
      ( Ast_helper.Pat.var ~loc:label.loc ~attrs
          (Location.mkloc (Longident.last label.txt) label.loc),
        false )
  in
  (label, pattern, optional)

(* TODO: there are better representations than PatField|Underscore ? *)
and parse_record_pattern_row p =
  let attrs = parse_attributes p in
  match p.Parser.token with
  | DotDotDot ->
    Parser.next p;
    Some (true, PatField (parse_record_pattern_row_field ~attrs p))
  | Uident _ | Lident _ ->
    Some (false, PatField (parse_record_pattern_row_field ~attrs p))
  | Question -> (
    Parser.next p;
    match p.token with
    | Uident _ | Lident _ ->
      let lid, pat, _ = parse_record_pattern_row_field ~attrs p in
      Some (false, PatField (lid, pat, true))
    | _ -> None)
  | Underscore ->
    Parser.next p;
    Some (false, PatUnderscore)
  | _ -> None

and parse_record_pattern ~attrs p =
  let start_pos = p.start_pos in
  Parser.expect Lbrace p;
  let raw_fields =
    parse_comma_delimited_reversed_list p ~grammar:PatternRecord ~closing:Rbrace
      ~f:parse_record_pattern_row
  in
  Parser.expect Rbrace p;
  let fields, closed_flag =
    let raw_fields, flag =
      match raw_fields with
      | (_hasSpread, PatUnderscore) :: rest -> (rest, Asttypes.Open)
      | raw_fields -> (raw_fields, Asttypes.Closed)
    in
    List.fold_left
      (fun (fields, flag) curr ->
        let has_spread, field = curr in
        match field with
        | PatField field ->
          (if has_spread then
             let _, pattern, _ = field in
             Parser.err ~start_pos:pattern.Parsetree.ppat_loc.loc_start p
               (Diagnostics.message ErrorMessages.record_pattern_spread));
          (field :: fields, flag)
        | PatUnderscore -> (fields, flag))
      ([], flag) raw_fields
  in
  let loc = mk_loc start_pos p.prev_end_pos in
  Ast_helper.Pat.record ~loc ~attrs fields closed_flag

and parse_tuple_pattern ~attrs ~first ~start_pos p =
  let patterns =
    first
    :: parse_comma_delimited_region p ~grammar:Grammar.PatternList
         ~closing:Rparen ~f:parse_constrained_pattern_region
  in
  Parser.expect Rparen p;
  let () =
    match patterns with
    | [_] ->
      Parser.err ~start_pos ~end_pos:p.prev_end_pos p
        (Diagnostics.message ErrorMessages.tuple_single_element)
    | _ -> ()
  in
  let loc = mk_loc start_pos p.prev_end_pos in
  Ast_helper.Pat.tuple ~loc ~attrs patterns

and parse_pattern_region p =
  match p.Parser.token with
  | DotDotDot ->
    Parser.next p;
    Some (true, parse_constrained_pattern p)
  | token when Grammar.is_pattern_start token ->
    Some (false, parse_constrained_pattern p)
  | _ -> None

and parse_module_pattern ~attrs p =
  let start_pos = p.Parser.start_pos in
  Parser.expect Module p;
  Parser.expect Lparen p;
  let uident =
    match p.token with
    | Uident uident ->
      let loc = mk_loc p.start_pos p.end_pos in
      Parser.next p;
      Location.mkloc uident loc
    | _ ->
      (* TODO: error recovery *)
      Location.mknoloc "_"
  in
  match p.token with
  | Colon ->
    let colon_start = p.Parser.start_pos in
    Parser.next p;
    let package_typ_attrs = parse_attributes p in
    let package_type =
      parse_package_type ~start_pos:colon_start ~attrs:package_typ_attrs p
    in
    Parser.expect Rparen p;
    let loc = mk_loc start_pos p.prev_end_pos in
    let unpack = Ast_helper.Pat.unpack ~loc:uident.loc uident in
    Ast_helper.Pat.constraint_ ~loc ~attrs unpack package_type
  | _ ->
    Parser.expect Rparen p;
    let loc = mk_loc start_pos p.prev_end_pos in
    Ast_helper.Pat.unpack ~loc ~attrs uident

and parse_list_pattern ~start_pos ~attrs p =
  let list_patterns =
    parse_comma_delimited_reversed_list p ~grammar:Grammar.PatternOcamlList
      ~closing:Rbrace ~f:parse_pattern_region
  in
  Parser.expect Rbrace p;
  let loc = mk_loc start_pos p.prev_end_pos in
  let filter_spread (has_spread, pattern) =
    if has_spread then (
      Parser.err ~start_pos:pattern.Parsetree.ppat_loc.loc_start p
        (Diagnostics.message ErrorMessages.list_pattern_spread);
      pattern)
    else pattern
  in
  match list_patterns with
  | (true, pattern) :: patterns ->
    let patterns = patterns |> List.map filter_spread |> List.rev in
    let pat = make_list_pattern loc patterns (Some pattern) in
    {pat with ppat_loc = loc; ppat_attributes = attrs}
  | patterns ->
    let patterns = patterns |> List.map filter_spread |> List.rev in
    let pat = make_list_pattern loc patterns None in
    {pat with ppat_loc = loc; ppat_attributes = attrs}

and parse_dict_pattern_row p =
  match p.Parser.token with
  | String s ->
    let loc = mk_loc p.start_pos p.end_pos in
    Parser.next p;
    let fieldName = Location.mkloc (Longident.Lident s) loc in
    Parser.expect Colon p;
    let optional = parse_optional_label p in
    let pat = parse_pattern p in
    Some (fieldName, pat, optional)
  | _ -> None

and parse_dict_pattern ~start_pos ~attrs (p : Parser.t) =
  let fields =
    parse_comma_delimited_region p ~grammar:DictRows ~closing:Rbrace
      ~f:parse_dict_pattern_row
  in
  Parser.expect Rbrace p;
  let loc = mk_loc start_pos p.prev_end_pos in
  Ast_helper.Pat.record ~loc
    ~attrs:((Location.mknoloc "res.dictPattern", PStr []) :: attrs)
    fields Open

and parse_array_pattern ~attrs p =
  let start_pos = p.start_pos in
  Parser.expect Lbracket p;
  let patterns =
    parse_comma_delimited_region p ~grammar:Grammar.PatternList
      ~closing:Rbracket
      ~f:(parse_non_spread_pattern ~msg:ErrorMessages.array_pattern_spread)
  in
  Parser.expect Rbracket p;
  let loc = mk_loc start_pos p.prev_end_pos in
  Ast_helper.Pat.array ~loc ~attrs patterns

and parse_constructor_pattern_args p constr start_pos attrs =
  let lparen = p.start_pos in
  Parser.expect Lparen p;
  let args =
    parse_comma_delimited_region p ~grammar:Grammar.PatternList ~closing:Rparen
      ~f:parse_constrained_pattern_region
  in
  Parser.expect Rparen p;
  let args =
    match args with
    | [] ->
      let loc = mk_loc lparen p.prev_end_pos in
      Some
        (Ast_helper.Pat.construct ~loc
           (Location.mkloc (Longident.Lident "()") loc)
           None)
    | [({ppat_desc = Ppat_tuple _} as pat)] as patterns ->
      if p.mode = ParseForTypeChecker then
        (* Some(1, 2) for type-checker *)
        Some pat
      else
        (* Some((1, 2)) for printer *)
        Some (Ast_helper.Pat.tuple ~loc:(mk_loc lparen p.end_pos) patterns)
    | [pattern] -> Some pattern
    | patterns ->
      Some (Ast_helper.Pat.tuple ~loc:(mk_loc lparen p.end_pos) patterns)
  in
  Ast_helper.Pat.construct
    ~loc:(mk_loc start_pos p.prev_end_pos)
    ~attrs constr args

and parse_variant_pattern_args p ident start_pos attrs =
  let lparen = p.start_pos in
  Parser.expect Lparen p;
  let patterns =
    parse_comma_delimited_region p ~grammar:Grammar.PatternList ~closing:Rparen
      ~f:parse_constrained_pattern_region
  in
  let args =
    match patterns with
    | [] ->
      let loc = mk_loc lparen p.prev_end_pos in
      Some
        (Ast_helper.Pat.construct ~loc
           (Location.mkloc (Longident.Lident "()") loc)
           None)
    | [({ppat_desc = Ppat_tuple _} as pat)] as patterns ->
      if p.mode = ParseForTypeChecker then
        (* #ident(1, 2) for type-checker *)
        Some pat
      else
        (* #ident((1, 2)) for printer *)
        Some (Ast_helper.Pat.tuple ~loc:(mk_loc lparen p.end_pos) patterns)
    | [pattern] -> Some pattern
    | patterns ->
      Some (Ast_helper.Pat.tuple ~loc:(mk_loc lparen p.end_pos) patterns)
  in
  Parser.expect Rparen p;
  Ast_helper.Pat.variant
    ~loc:(mk_loc start_pos p.prev_end_pos)
    ~attrs ident args

and parse_expr ?(context = OrdinaryExpr) p =
  let expr = parse_operand_expr ~context p in
  let expr = parse_binary_expr ~context ~a:expr p 1 in
  parse_ternary_expr expr p

(* expr ? expr : expr *)
and parse_ternary_expr left_operand p =
  match p.Parser.token with
  | Question ->
    Parser.leave_breadcrumb p Grammar.Ternary;
    Parser.next p;
    let true_branch = parse_expr ~context:TernaryTrueBranchExpr p in
    Parser.expect Colon p;
    let false_branch = parse_expr p in
    Parser.eat_breadcrumb p;
    let loc =
      {
        left_operand.Parsetree.pexp_loc with
        loc_start = left_operand.pexp_loc.loc_start;
        loc_end = false_branch.Parsetree.pexp_loc.loc_end;
      }
    in
    Ast_helper.Exp.ifthenelse ~attrs:[ternary_attr] ~loc left_operand
      true_branch (Some false_branch)
  | _ -> left_operand

and parse_es6_arrow_expression ?(arrow_attrs = []) ?(arrow_start_pos = None)
    ?context ?term_parameters ~async p =
  let start_pos = p.Parser.start_pos in
  Parser.leave_breadcrumb p Grammar.Es6ArrowExpr;
  (* Parsing function parameters and attributes:
     1. Basically, attributes outside of `(...)` are added to the function, except
     the uncurried attribute `(.)` is added to the function. e.g. async, uncurried

     2. Attributes inside `(...)` are added to the arguments regardless of whether
     labeled, optional or nolabeled *)
  let parameters =
    match term_parameters with
    | Some params -> (None, params)
    | None -> parse_parameters p
  in
  let parameters =
    let update_attrs attrs = arrow_attrs @ attrs in
    let update_pos pos =
      match arrow_start_pos with
      | Some start_pos -> start_pos
      | None -> pos
    in
    match parameters with
    | None, termp :: rest ->
      ( None,
        {
          termp with
          attrs = update_attrs termp.attrs;
          p_pos = update_pos termp.p_pos;
        }
        :: rest )
    | Some (tpa : fundef_type_param), term_params ->
      ( Some
          {
            tpa with
            attrs = update_attrs tpa.attrs;
            p_pos = update_pos tpa.p_pos;
          },
        term_params )
    | _ -> parameters
  in
  let return_type =
    match p.Parser.token with
    | Colon ->
      Parser.next p;
      Some (parse_typ_expr ~es6_arrow:false p)
    | _ -> None
  in
  Parser.expect EqualGreater p;
  let body =
    let expr = parse_expr ?context p in
    match return_type with
    | Some typ ->
      Ast_helper.Exp.constraint_
        ~loc:(mk_loc expr.pexp_loc.loc_start typ.Parsetree.ptyp_loc.loc_end)
        expr typ
    | None -> expr
  in
  Parser.eat_breadcrumb p;
  let end_pos = p.prev_end_pos in
  let type_param_opt, term_parameters = parameters in
  let arrow_expr =
    List.fold_right
      (fun parameter expr ->
        let {attrs; p_label = lbl; expr = default_expr; pat; p_pos = start_pos}
            =
          parameter
        in
        let loc = mk_loc start_pos end_pos in
        Ast_helper.Exp.fun_ ~loc ~attrs ~arity:None lbl default_expr pat expr)
      term_parameters body
  in
  let arrow_expr =
    Ast_uncurried.uncurried_fun
      ~arity:(List.length term_parameters)
      ~async arrow_expr
  in
  let arrow_expr =
    match type_param_opt with
    | None -> arrow_expr
    | Some {attrs; locs = newtypes; p_pos = start_pos} ->
      make_newtypes ~attrs ~loc:(mk_loc start_pos end_pos) newtypes arrow_expr
  in
  {arrow_expr with pexp_loc = {arrow_expr.pexp_loc with loc_start = start_pos}}

(*
 * dotted_parameter ::=
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
 *)
and parse_parameter p =
  if
    p.Parser.token = Token.Typ || p.token = Tilde || p.token = Dot
    || Grammar.is_pattern_start p.token
  then
    let start_pos = p.Parser.start_pos in
    let _ =
      Parser.optional p Token.Dot
      (* dot is ignored *)
    in
    let attrs = parse_attributes p in
    if p.Parser.token = Typ then (
      Parser.next p;
      let lidents = parse_lident_list p in
      Some (TypeParameter {attrs; locs = lidents; p_pos = start_pos}))
    else
      let attrs, lbl, lbl_loc, pat =
        match p.Parser.token with
        | Tilde -> (
          Parser.next p;
          let lbl_name, lbl_loc = parse_lident p in
          match p.Parser.token with
          | Comma | Equal | Rparen ->
            let loc = mk_loc start_pos p.prev_end_pos in
            ( [],
              Asttypes.Labelled {txt = lbl_name; loc = lbl_loc},
              lbl_loc,
              Ast_helper.Pat.var ~attrs ~loc (Location.mkloc lbl_name loc) )
          | Colon ->
            let lbl_end = p.prev_end_pos in
            Parser.next p;
            let typ = parse_typ_expr p in
            let loc = mk_loc start_pos lbl_end in
            let pat =
              let pat = Ast_helper.Pat.var ~loc (Location.mkloc lbl_name loc) in
              let loc = mk_loc start_pos p.prev_end_pos in
              Ast_helper.Pat.constraint_ ~attrs ~loc pat typ
            in
            ([], Asttypes.Labelled {txt = lbl_name; loc = lbl_loc}, lbl_loc, pat)
          | As ->
            Parser.next p;
            let pat =
              let pat = parse_constrained_pattern p in
              {pat with ppat_attributes = attrs @ pat.ppat_attributes}
            in
            ([], Asttypes.Labelled {txt = lbl_name; loc = lbl_loc}, lbl_loc, pat)
          | t ->
            Parser.err p (Diagnostics.unexpected t p.breadcrumbs);
            let loc = mk_loc start_pos p.prev_end_pos in
            ( [],
              Asttypes.Labelled {txt = lbl_name; loc = lbl_loc},
              lbl_loc,
              Ast_helper.Pat.var ~attrs ~loc (Location.mkloc lbl_name loc) ))
        | _ ->
          let pattern = parse_constrained_pattern p in
          let attrs = List.concat [pattern.ppat_attributes; attrs] in
          ( [],
            Asttypes.Nolabel,
            Location.none,
            {pattern with ppat_attributes = attrs} )
      in
      match p.Parser.token with
      | Equal -> (
        Parser.next p;
        let lbl =
          match lbl with
          | Asttypes.Labelled lbl_name -> Asttypes.Optional lbl_name
          | Asttypes.Nolabel ->
            let lbl_name =
              match pat.ppat_desc with
              | Ppat_var var -> var.txt
              | _ -> ""
            in
            Parser.err ~start_pos ~end_pos:p.prev_end_pos p
              (Diagnostics.message
                 (ErrorMessages.missing_tilde_labeled_parameter lbl_name));
            Asttypes.Optional {txt = lbl_name; loc = lbl_loc}
          | lbl -> lbl
        in
        match p.Parser.token with
        | Question ->
          Parser.next p;
          Some
            (TermParameter
               {attrs; p_label = lbl; expr = None; pat; p_pos = start_pos})
        | _ ->
          let expr = parse_constrained_or_coerced_expr p in
          Some
            (TermParameter
               {attrs; p_label = lbl; expr = Some expr; pat; p_pos = start_pos})
        )
      | _ ->
        Some
          (TermParameter
             {attrs; p_label = lbl; expr = None; pat; p_pos = start_pos})
  else None

and parse_parameter_list p =
  let parameters =
    parse_comma_delimited_region ~grammar:Grammar.ParameterList
      ~f:parse_parameter ~closing:Rparen p
  in
  Parser.expect Rparen p;
  extract_fundef_params ~type_acc:None ~term_acc:[] parameters

(* parameters ::=
 *   | _
 *   | lident
 *   | ()
 *   | (.)
 *   | ( parameter {, parameter} [,] )
 *)
and parse_parameters p : fundef_type_param option * fundef_term_param list =
  let start_pos = p.Parser.start_pos in
  let unit_term_parameter () =
    let loc = mk_loc start_pos p.Parser.prev_end_pos in
    let unit_pattern =
      Ast_helper.Pat.construct ~loc
        (Location.mkloc (Longident.Lident "()") loc)
        None
    in
    {
      attrs = [];
      p_label = Asttypes.Nolabel;
      expr = None;
      pat = unit_pattern;
      p_pos = start_pos;
    }
  in
  match p.Parser.token with
  | Lident ident ->
    Parser.next p;
    let loc = mk_loc start_pos p.Parser.prev_end_pos in
    ( None,
      [
        {
          attrs = [];
          p_label = Asttypes.Nolabel;
          expr = None;
          pat = Ast_helper.Pat.var ~loc (Location.mkloc ident loc);
          p_pos = start_pos;
        };
      ] )
  | Underscore ->
    Parser.next p;
    let loc = mk_loc start_pos p.Parser.prev_end_pos in
    ( None,
      [
        {
          attrs = [];
          p_label = Asttypes.Nolabel;
          expr = None;
          pat = Ast_helper.Pat.any ~loc ();
          p_pos = start_pos;
        };
      ] )
  | Lparen ->
    Parser.next p;
    ignore (Parser.optional p Dot);
    let type_params, term_params = parse_parameter_list p in
    let term_params =
      if term_params <> [] then term_params else [unit_term_parameter ()]
    in
    (type_params, term_params)
  | token ->
    Parser.err p (Diagnostics.unexpected token p.breadcrumbs);
    (None, [])

and parse_coerced_expr ~(expr : Parsetree.expression) p =
  Parser.expect ColonGreaterThan p;
  let typ = parse_typ_expr p in
  let loc = mk_loc expr.pexp_loc.loc_start p.prev_end_pos in
  Ast_helper.Exp.coerce ~loc expr typ

and parse_constrained_or_coerced_expr p =
  let expr = parse_expr p in
  match p.Parser.token with
  | ColonGreaterThan -> parse_coerced_expr ~expr p
  | Colon -> (
    Parser.next p;
    match p.token with
    | _ -> (
      let typ = parse_typ_expr p in
      let loc = mk_loc expr.pexp_loc.loc_start typ.ptyp_loc.loc_end in
      let expr = Ast_helper.Exp.constraint_ ~loc expr typ in
      match p.token with
      | ColonGreaterThan -> parse_coerced_expr ~expr p
      | _ -> expr))
  | _ -> expr

and parse_constrained_expr_region p =
  match p.Parser.token with
  | token when Grammar.is_expr_start token -> (
    let expr = parse_expr p in
    match p.Parser.token with
    | ColonGreaterThan -> Some (parse_coerced_expr ~expr p)
    | Colon ->
      Parser.next p;
      let typ = parse_typ_expr p in
      let loc = mk_loc expr.pexp_loc.loc_start typ.ptyp_loc.loc_end in
      Some (Ast_helper.Exp.constraint_ ~loc expr typ)
    | _ -> Some expr)
  | _ -> None

and parse_regex p pattern flags =
  let start_pos = p.Parser.start_pos in
  Parser.next p;
  let loc = mk_loc start_pos p.prev_end_pos in
  let payload =
    Parsetree.PStr
      [
        Ast_helper.Str.eval ~loc
          (Ast_helper.Exp.constant ~loc
             (Pconst_string
                ( "/" ^ pattern ^ "/" ^ flags,
                  if p.mode = ParseForTypeChecker then Some "js" else None )));
      ]
  in
  Ast_helper.Exp.extension (Location.mknoloc "re", payload)

(* Atomic expressions represent unambiguous expressions.
 * This means that regardless of the context, these expressions
 * are always interpreted correctly. *)
and parse_atomic_expr p =
  Parser.leave_breadcrumb p Grammar.ExprOperand;
  let start_pos = p.Parser.start_pos in
  let expr =
    match p.Parser.token with
    | (True | False) as token ->
      Parser.next p;
      let loc = mk_loc start_pos p.prev_end_pos in
      Ast_helper.Exp.construct ~loc
        (Location.mkloc (Longident.Lident (Token.to_string token)) loc)
        None
    | Int _ | String _ | Float _ | Codepoint _ ->
      let c = parse_constant p in
      let loc = mk_loc start_pos p.prev_end_pos in
      Ast_helper.Exp.constant ~loc c
    | Backtick ->
      let expr = parse_template_expr p in
      {expr with pexp_loc = mk_loc start_pos p.prev_end_pos}
    | Uident _ | Lident _ -> parse_value_or_constructor p
    | Hash -> parse_poly_variant_expr p
    | Lparen -> (
      Parser.next p;
      match p.Parser.token with
      | Rparen ->
        Parser.next p;
        let loc = mk_loc start_pos p.prev_end_pos in
        Ast_helper.Exp.construct ~loc
          (Location.mkloc (Longident.Lident "()") loc)
          None
      | _t -> (
        let expr = parse_constrained_or_coerced_expr p in
        match p.token with
        | Comma ->
          Parser.next p;
          parse_tuple_expr ~start_pos ~first:expr p
        | _ ->
          Parser.expect Rparen p;
          expr
        (* {expr with pexp_loc = mkLoc startPos p.prevEndPos}
         * What does this location mean here? It means that when there's
         * a parenthesized we keep the location here for whitespace interleaving.
         * Without the closing paren in the location there will always be an extra
         * line. For now we don't include it, because it does weird things
         * with for comments. *)))
    | List ->
      Parser.next p;
      parse_list_expr ~start_pos p
    | Dict ->
      Parser.next p;
      parse_dict_expr ~start_pos p
    | Module ->
      Parser.next p;
      parse_first_class_module_expr ~start_pos p
    | Lbracket -> parse_array_exp p
    | Lbrace -> parse_braced_or_record_expr p
    | LessThan -> parse_jsx p
    | Percent ->
      let extension = parse_extension p in
      let loc = mk_loc start_pos p.prev_end_pos in
      Ast_helper.Exp.extension ~loc extension
    | Underscore as token ->
      (* This case is for error recovery. Not sure if it's the correct place *)
      Parser.err p (Diagnostics.lident token);
      Parser.next p;
      Recover.default_expr ()
    | Eof ->
      Parser.err ~start_pos:p.prev_end_pos p
        (Diagnostics.unexpected p.Parser.token p.breadcrumbs);
      Recover.default_expr ()
    | Forwardslash -> (
      Parser.next_regex_token p;
      match p.token with
      | Regex (pattern, flags) -> parse_regex p pattern flags
      | _ -> Ast_helper.Exp.extension (Location.mknoloc "re", Parsetree.PStr [])
      )
    | ForwardslashDot -> (
      Parser.next_regex_token p;
      match p.token with
      | Regex (pattern, flags) -> parse_regex p ("." ^ pattern) flags
      | _ -> Ast_helper.Exp.extension (Location.mknoloc "re", Parsetree.PStr [])
      )
    | token -> (
      let err_pos = p.prev_end_pos in
      Parser.err ~start_pos:err_pos p
        (Diagnostics.unexpected token p.breadcrumbs);
      match
        skip_tokens_and_maybe_retry p
          ~is_start_of_grammar:Grammar.is_atomic_expr_start
      with
      | None -> Recover.default_expr ()
      | Some () -> parse_atomic_expr p)
  in
  Parser.eat_breadcrumb p;
  expr

(* module(module-expr)
 * module(module-expr : package-type) *)
and parse_first_class_module_expr ~start_pos p =
  Parser.expect Lparen p;

  let mod_expr = parse_module_expr p in
  let mod_end_loc = p.prev_end_pos in
  match p.Parser.token with
  | Colon ->
    let colon_start = p.Parser.start_pos in
    Parser.next p;
    let attrs = parse_attributes p in
    let package_type = parse_package_type ~start_pos:colon_start ~attrs p in
    Parser.expect Rparen p;
    let loc = mk_loc start_pos mod_end_loc in
    let first_class_module = Ast_helper.Exp.pack ~loc mod_expr in
    let loc = mk_loc start_pos p.prev_end_pos in
    Ast_helper.Exp.constraint_ ~loc first_class_module package_type
  | _ ->
    Parser.expect Rparen p;
    let loc = mk_loc start_pos p.prev_end_pos in
    Ast_helper.Exp.pack ~loc mod_expr

and parse_bracket_access p expr start_pos =
  Parser.leave_breadcrumb p Grammar.ExprArrayAccess;
  let lbracket = p.start_pos in
  Parser.expect Lbracket p;
  let string_start = p.start_pos in
  match p.Parser.token with
  | String s -> (
    Parser.next p;
    let string_end = p.prev_end_pos in
    Parser.expect Rbracket p;
    Parser.eat_breadcrumb p;
    let rbracket = p.prev_end_pos in
    let e =
      let ident_loc = mk_loc string_start string_end in
      let loc = mk_loc start_pos rbracket in
      Ast_helper.Exp.send ~loc expr (Location.mkloc s ident_loc)
    in
    let e = parse_primary_expr ~operand:e p in
    let equal_start = p.start_pos in
    match p.token with
    | Equal ->
      Parser.next p;
      let equal_end = p.prev_end_pos in
      let rhs_expr = parse_expr p in
      let loc = mk_loc start_pos rhs_expr.pexp_loc.loc_end in
      let operator_loc = mk_loc equal_start equal_end in
      Ast_helper.Exp.apply ~loc
        (Ast_helper.Exp.ident ~loc:operator_loc
           (Location.mkloc (Longident.Lident "#=") operator_loc))
        [(Nolabel, e); (Nolabel, rhs_expr)]
    | _ -> e)
  | _ -> (
    let access_expr = parse_constrained_or_coerced_expr p in
    Parser.expect Rbracket p;
    Parser.eat_breadcrumb p;
    let rbracket = p.prev_end_pos in
    let array_loc = mk_loc lbracket rbracket in
    match p.token with
    | Equal ->
      Parser.leave_breadcrumb p ExprArrayMutation;
      Parser.next p;
      let rhs_expr = parse_expr p in
      (* FIXME: Do not implicitly rely on specific module name, even primitive one

         This can be abused like
           module Array = MyModule

         Find better mechanism to support it
      *)
      let array_set =
        Location.mkloc (Longident.Ldot (Lident "Array", "set")) array_loc
      in
      let end_pos = p.prev_end_pos in
      let array_set =
        Ast_helper.Exp.apply ~loc:(mk_loc start_pos end_pos)
          (Ast_helper.Exp.ident ~loc:array_loc array_set)
          [(Nolabel, expr); (Nolabel, access_expr); (Nolabel, rhs_expr)]
      in
      Parser.eat_breadcrumb p;
      array_set
    | _ ->
      let end_pos = p.prev_end_pos in
      let e =
        Ast_helper.Exp.apply ~loc:(mk_loc start_pos end_pos)
          (Ast_helper.Exp.ident ~loc:array_loc
             (Location.mkloc (Longident.Ldot (Lident "Array", "get")) array_loc))
          [(Nolabel, expr); (Nolabel, access_expr)]
      in
      parse_primary_expr ~operand:e p)

(* * A primary expression represents
 *  - atomic-expr
 *  - john.age
 *  - array[0]
 *  - applyFunctionTo(arg1, arg2)
 *
 *  The "operand" represents the expression that is operated on
 *)
and parse_primary_expr ~operand ?(no_call = false) p =
  let start_pos = operand.pexp_loc.loc_start in
  let rec loop p expr =
    match p.Parser.token with
    | Dot -> (
      Parser.next p;
      let lident = parse_value_path_after_dot p in
      match p.Parser.token with
      | Equal when no_call = false ->
        Parser.leave_breadcrumb p Grammar.ExprSetField;
        Parser.next p;
        let target_expr = parse_expr p in
        let loc = mk_loc start_pos p.prev_end_pos in
        let setfield = Ast_helper.Exp.setfield ~loc expr lident target_expr in
        Parser.eat_breadcrumb p;
        setfield
      | _ ->
        let end_pos = p.prev_end_pos in
        let loc = mk_loc start_pos end_pos in
        loop p (Ast_helper.Exp.field ~loc expr lident))
    | Lbracket
      when no_call = false && p.prev_end_pos.pos_lnum == p.start_pos.pos_lnum ->
      parse_bracket_access p expr start_pos
    | Lparen
      when no_call = false && p.prev_end_pos.pos_lnum == p.start_pos.pos_lnum ->
      loop p (parse_call_expr p expr)
    | Backtick
      when no_call = false && p.prev_end_pos.pos_lnum == p.start_pos.pos_lnum
      -> (
      match expr.pexp_desc with
      | Pexp_ident long_ident -> parse_template_expr ~prefix:long_ident p
      | _ ->
        Parser.err ~start_pos:expr.pexp_loc.loc_start
          ~end_pos:expr.pexp_loc.loc_end p
          (Diagnostics.message
             "Tagged template literals are currently restricted to names like: \
              myTagFunction`foo ${bar}`.");
        parse_template_expr p)
    | _ -> expr
  in
  loop p operand

(* a unary expression is an expression with only one operand and
 * unary operator. Examples:
 *   -1
 *   !condition
 *   -. 1.6
 *)
and parse_unary_expr p =
  let start_pos = p.Parser.start_pos in
  match p.Parser.token with
  | (Minus | MinusDot | Plus | PlusDot | Bang) as token ->
    Parser.leave_breadcrumb p Grammar.ExprUnary;
    let token_end = p.end_pos in
    Parser.next p;
    let operand = parse_unary_expr p in
    let unary_expr = make_unary_expr start_pos token_end token operand in
    Parser.eat_breadcrumb p;
    unary_expr
  | _ -> parse_primary_expr ~operand:(parse_atomic_expr p) p

(* Represents an "operand" in a binary expression.
 * If you have `a + b`, `a` and `b` both represent
 * the operands of the binary expression with opeartor `+` *)
and parse_operand_expr ~context p =
  let start_pos = p.Parser.start_pos in
  let attrs = ref (parse_attributes p) in
  let expr =
    match p.Parser.token with
    | Assert ->
      Parser.next p;
      let expr = parse_expr p in
      let loc = mk_loc start_pos p.prev_end_pos in
      Ast_helper.Exp.assert_ ~loc expr
    | Lident "async"
    (* we need to be careful when we're in a ternary true branch:
       `condition ? ternary-true-branch : false-branch`
       Arrow expressions could be of the form: `async (): int => stuff()`
       But if we're in a ternary, the `:` of the ternary takes precedence
    *)
      when is_es6_arrow_expression
             ~in_ternary:(context = TernaryTrueBranchExpr)
             p ->
      let arrow_attrs = !attrs in
      let () = attrs := [] in
      parse_async_arrow_expression ~arrow_attrs p
    | Await -> parse_await_expression p
    | Try -> parse_try_expression p
    | If -> parse_if_or_if_let_expression p
    | For -> parse_for_expression p
    | While -> parse_while_expression p
    | Switch -> parse_switch_expression p
    | _ ->
      if
        context != WhenExpr
        && is_es6_arrow_expression
             ~in_ternary:(context = TernaryTrueBranchExpr)
             p
      then
        let arrow_attrs = !attrs in
        let () = attrs := [] in
        parse_es6_arrow_expression ~async:false ~arrow_attrs ~context p
      else parse_unary_expr p
  in
  (* let endPos = p.Parser.prevEndPos in *)
  {
    expr with
    pexp_attributes = List.concat [expr.Parsetree.pexp_attributes; !attrs];
    (* pexp_loc = mkLoc startPos endPos *)
  }

(* a binary expression is an expression that combines two expressions with an
 * operator. Examples:
 *    a + b
 *    f(x) |> g(y)
 *)
and parse_binary_expr ?(context = OrdinaryExpr) ?a p prec =
  let a =
    match a with
    | Some e -> e
    | None -> parse_operand_expr ~context p
  in
  let rec loop a =
    let token = p.Parser.token in
    let token_prec =
      match token with
      (* Can the minus be interpreted as a binary operator? Or is it a unary?
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
       * See Scanner.isBinaryOp *)
      | (Minus | MinusDot | LessThan | Percent)
        when (not
                (Scanner.is_binary_op p.scanner.src p.start_pos.pos_cnum
                   p.end_pos.pos_cnum))
             && p.start_pos.pos_lnum > p.prev_end_pos.pos_lnum ->
        -1
      | token -> Token.precedence token
    in
    if token_prec < prec then a
    else (
      Parser.leave_breadcrumb p (Grammar.ExprBinaryAfterOp token);
      let start_pos = p.start_pos in
      Parser.next p;
      let end_pos = p.prev_end_pos in
      let token_prec =
        (* exponentiation operator is right-associative *)
        if token = Exponentiation then token_prec else token_prec + 1
      in
      let b = parse_binary_expr ~context p token_prec in
      let loc = mk_loc a.Parsetree.pexp_loc.loc_start b.pexp_loc.loc_end in
      let expr =
        match (token, b.pexp_desc) with
        | BarGreater, Pexp_apply {funct = fun_expr; args; partial} ->
          {
            b with
            pexp_desc =
              Pexp_apply
                {funct = fun_expr; args = args @ [(Nolabel, a)]; partial};
          }
        | BarGreater, _ -> Ast_helper.Exp.apply ~loc b [(Nolabel, a)]
        | _ ->
          Ast_helper.Exp.apply ~loc
            (make_infix_operator p token start_pos end_pos)
            [(Nolabel, a); (Nolabel, b)]
      in
      Parser.eat_breadcrumb p;
      loop expr)
  in
  loop a

(* If we even need this, determines if < might be the start of jsx. Not 100% complete *)
(* and isStartOfJsx p = *)
(* Parser.lookahead p (fun p -> *)
(* match p.Parser.token with *)
(* | LessThan -> *)
(* Parser.next p; *)
(* begin match p.token with *)
(* | GreaterThan (* <> *) -> true *)
(* | Lident _ | Uident _ | List -> *)
(* ignore (parseJsxName p); *)
(* begin match p.token with *)
(* | GreaterThan (* <div> *) -> true *)
(* | Question (*<Component ? *) -> true *)
(* | Lident _ | List -> *)
(* Parser.next p; *)
(* begin match p.token with *)
(* | Equal (* <Component handleClick= *) -> true *)
(* | _ -> false (* TODO *) *)
(* end *)
(* | Forwardslash (* <Component / *)-> *)
(* Parser.next p; *)
(* begin match p.token with *)
(* | GreaterThan (* <Component /> *) -> true *)
(* | _ -> false *)
(* end *)
(* | _ -> *)
(* false *)
(* end *)
(* | _ -> false *)
(* end *)
(* | _ -> false *)
(* ) *)

and parse_template_expr ?prefix p =
  let part_prefix =
    (* we could stop treating json prefix as something special
       but we would first need to remove @as(json`true`) feature *)
    match prefix with
    | Some {txt = Longident.Lident ("json" as prefix); _} -> Some prefix
    | _ -> Some "js"
  in

  let parse_parts p =
    let rec aux acc =
      let start_pos = p.Parser.start_pos in
      Parser.next_template_literal_token p;
      match p.token with
      | TemplateTail (txt, last_pos) ->
        Parser.next p;
        let loc = mk_loc start_pos last_pos in
        let str =
          Ast_helper.Exp.constant ~attrs:[template_literal_attr] ~loc
            (Pconst_string (txt, part_prefix))
        in
        List.rev ((str, None) :: acc)
      | TemplatePart (txt, last_pos) ->
        Parser.next p;
        let loc = mk_loc start_pos last_pos in
        let expr = parse_expr_block p in
        let str =
          Ast_helper.Exp.constant ~attrs:[template_literal_attr] ~loc
            (Pconst_string (txt, part_prefix))
        in
        aux ((str, Some expr) :: acc)
      | token ->
        Parser.err p (Diagnostics.unexpected token p.breadcrumbs);
        []
    in
    aux []
  in
  let parts = parse_parts p in
  let strings = List.map fst parts in
  let values = Ext_list.filter_map parts snd in

  let gen_tagged_template_call (lident_loc : Longident.t Location.loc) =
    let ident = Ast_helper.Exp.ident ~attrs:[] ~loc:lident_loc.loc lident_loc in
    let strings_array =
      Ast_helper.Exp.array ~attrs:[] ~loc:Location.none strings
    in
    let values_array =
      Ast_helper.Exp.array ~attrs:[] ~loc:Location.none values
    in
    Ast_helper.Exp.apply
      ~attrs:[tagged_template_literal_attr]
      ~loc:lident_loc.loc ident
      [(Nolabel, strings_array); (Nolabel, values_array)]
  in

  let hidden_operator =
    let op = Location.mknoloc (Longident.Lident "++") in
    Ast_helper.Exp.ident op
  in
  let concat (e1 : Parsetree.expression) (e2 : Parsetree.expression) =
    let loc = mk_loc e1.pexp_loc.loc_start e2.pexp_loc.loc_end in
    Ast_helper.Exp.apply ~attrs:[template_literal_attr] ~loc hidden_operator
      [(Nolabel, e1); (Nolabel, e2)]
  in
  let gen_interpolated_string () =
    let subparts =
      List.flatten
        (List.map
           (fun part ->
             match part with
             | s, Some v -> [s; v]
             | s, None -> [s])
           parts)
    in
    let expr_option =
      List.fold_left
        (fun acc subpart ->
          Some
            (match acc with
            | Some expr -> concat expr subpart
            | None -> subpart))
        None subparts
    in
    match expr_option with
    | Some expr -> expr
    | None -> Ast_helper.Exp.constant (Pconst_string ("", None))
  in

  match prefix with
  | Some {txt = Longident.Lident "json"; _} | None -> gen_interpolated_string ()
  | Some lident_loc -> gen_tagged_template_call lident_loc

(* Overparse: let f = a : int => a + 1, is it (a : int) => or (a): int =>
 * Also overparse constraints:
 *  let x = {
 *    let a = 1
 *    a + pi: int
 *  }
 *
 *  We want to give a nice error message in these cases
 *)
and over_parse_constrained_or_coerced_or_arrow_expression p expr =
  match p.Parser.token with
  | ColonGreaterThan -> parse_coerced_expr ~expr p
  | Colon -> (
    Parser.next p;
    let typ = parse_typ_expr ~es6_arrow:false p in
    match p.Parser.token with
    | EqualGreater ->
      Parser.next p;
      let body = parse_expr p in
      let pat =
        match expr.pexp_desc with
        | Pexp_ident longident ->
          Ast_helper.Pat.var ~loc:expr.pexp_loc
            (Location.mkloc
               (Longident.flatten longident.txt |> String.concat ".")
               longident.loc)
        (* TODO: can we convert more expressions to patterns?*)
        | _ ->
          Ast_helper.Pat.var ~loc:expr.pexp_loc
            (Location.mkloc "pattern" expr.pexp_loc)
      in
      let arrow1 =
        Ast_helper.Exp.fun_
          ~loc:(mk_loc expr.pexp_loc.loc_start body.pexp_loc.loc_end)
          ~arity:None Asttypes.Nolabel None pat
          (Ast_helper.Exp.constraint_ body typ)
      in
      let arrow2 =
        Ast_helper.Exp.fun_
          ~loc:(mk_loc expr.pexp_loc.loc_start body.pexp_loc.loc_end)
          ~arity:None Asttypes.Nolabel None
          (Ast_helper.Pat.constraint_ pat typ)
          body
      in
      let msg =
        Doc.breakable_group ~force_break:true
          (Doc.concat
             [
               Doc.text
                 "Did you mean to annotate the parameter type or the return \
                  type?";
               Doc.indent
                 (Doc.concat
                    [
                      Doc.line;
                      Doc.text "1) ";
                      ResPrinter.print_expression arrow1 CommentTable.empty;
                      Doc.line;
                      Doc.text "2) ";
                      ResPrinter.print_expression arrow2 CommentTable.empty;
                    ]);
             ])
        |> Doc.to_string ~width:80
      in
      Parser.err ~start_pos:expr.pexp_loc.loc_start
        ~end_pos:body.pexp_loc.loc_end p (Diagnostics.message msg);
      arrow1
    | _ ->
      let loc = mk_loc expr.pexp_loc.loc_start typ.ptyp_loc.loc_end in
      let expr = Ast_helper.Exp.constraint_ ~loc expr typ in
      let () =
        Parser.err ~start_pos:expr.pexp_loc.loc_start
          ~end_pos:typ.ptyp_loc.loc_end p
          (Diagnostics.message
             (Doc.breakable_group ~force_break:true
                (Doc.concat
                   [
                     Doc.text
                       "Expressions with type constraints need to be wrapped \
                        in parens:";
                     Doc.indent
                       (Doc.concat
                          [
                            Doc.line;
                            ResPrinter.add_parens
                              (ResPrinter.print_expression expr
                                 CommentTable.empty);
                          ]);
                   ])
             |> Doc.to_string ~width:80))
      in
      expr)
  | _ -> expr

and parse_let_binding_body ~start_pos ~attrs p =
  Parser.begin_region p;
  Parser.leave_breadcrumb p Grammar.LetBinding;
  let pat, exp =
    Parser.leave_breadcrumb p Grammar.Pattern;
    let pat = parse_pattern p in
    Parser.eat_breadcrumb p;
    match p.Parser.token with
    | Colon -> (
      Parser.next p;
      match p.token with
      | Typ ->
        (* locally abstract types *)
        Parser.next p;
        let newtypes = parse_lident_list p in
        Parser.expect Dot p;
        let typ = parse_typ_expr p in
        Parser.expect Equal p;
        let expr = parse_expr p in
        let loc = mk_loc start_pos p.prev_end_pos in
        let exp, poly = wrap_type_annotation ~loc newtypes typ expr in
        let pat = Ast_helper.Pat.constraint_ ~loc pat poly in
        (pat, exp)
      | _ ->
        let poly_type = parse_poly_type_expr p in
        let loc =
          {pat.ppat_loc with loc_end = poly_type.Parsetree.ptyp_loc.loc_end}
        in
        let pat = Ast_helper.Pat.constraint_ ~loc pat poly_type in
        Parser.expect Token.Equal p;
        let exp = parse_expr p in
        let exp = over_parse_constrained_or_coerced_or_arrow_expression p exp in
        (pat, exp))
    | _ ->
      Parser.expect Token.Equal p;
      let exp =
        over_parse_constrained_or_coerced_or_arrow_expression p (parse_expr p)
      in
      (pat, exp)
  in
  let loc = mk_loc start_pos p.prev_end_pos in
  let vb = Ast_helper.Vb.mk ~loc ~attrs pat exp in
  Parser.eat_breadcrumb p;
  Parser.end_region p;
  vb

(* TODO: find a better way? Is it possible?
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
 *)
and parse_attributes_and_binding (p : Parser.t) =
  let err = p.scanner.err in
  let ch = p.scanner.ch in
  let offset = p.scanner.offset in
  let offset16 = p.scanner.offset16 in
  let line_offset = p.scanner.line_offset in
  let lnum = p.scanner.lnum in
  let mode = p.scanner.mode in
  let token = p.token in
  let start_pos = p.start_pos in
  let end_pos = p.end_pos in
  let prev_end_pos = p.prev_end_pos in
  let breadcrumbs = p.breadcrumbs in
  let errors = p.errors in
  let diagnostics = p.diagnostics in
  let comments = p.comments in

  match p.Parser.token with
  | At | DocComment (_, _) -> (
    let attrs = parse_attributes p in
    match p.Parser.token with
    | And -> attrs
    | _ ->
      p.scanner.err <- err;
      p.scanner.ch <- ch;
      p.scanner.offset <- offset;
      p.scanner.offset16 <- offset16;
      p.scanner.line_offset <- line_offset;
      p.scanner.lnum <- lnum;
      p.scanner.mode <- mode;
      p.token <- token;
      p.start_pos <- start_pos;
      p.end_pos <- end_pos;
      p.prev_end_pos <- prev_end_pos;
      p.breadcrumbs <- breadcrumbs;
      p.errors <- errors;
      p.diagnostics <- diagnostics;
      p.comments <- comments;
      [])
  | _ -> []

(* definition	::=	let [rec] let-binding  { and let-binding }   *)
and parse_let_bindings ~attrs ~start_pos p =
  Parser.optional p Let |> ignore;
  let rec_flag =
    if Parser.optional p Token.Rec then Asttypes.Recursive
    else Asttypes.Nonrecursive
  in
  let first = parse_let_binding_body ~start_pos ~attrs p in

  let rec loop p bindings =
    let start_pos = p.Parser.start_pos in
    let attrs = parse_attributes_and_binding p in
    match p.Parser.token with
    | And ->
      Parser.next p;
      ignore (Parser.optional p Let);
      (* overparse for fault tolerance *)
      let let_binding = parse_let_binding_body ~start_pos ~attrs p in
      loop p (let_binding :: bindings)
    | _ -> List.rev bindings
  in
  (rec_flag, loop p [first])

(*
 * div -> div
 * Foo -> Foo.createElement
 * Foo.Bar -> Foo.Bar.createElement
 *)
and parse_jsx_name p =
  let longident =
    match p.Parser.token with
    | Lident ident ->
      let ident_start = p.start_pos in
      let ident_end = p.end_pos in
      Parser.next p;
      let loc = mk_loc ident_start ident_end in
      Location.mkloc (Longident.Lident ident) loc
    | Uident _ ->
      let longident = parse_module_long_ident ~lowercase:true p in
      Location.mkloc
        (Longident.Ldot (longident.txt, "createElement"))
        longident.loc
    | _ ->
      let msg =
        "A jsx name must be a lowercase or uppercase name, like: div in <div \
         /> or Navbar in <Navbar />"
      in
      Parser.err p (Diagnostics.message msg);
      Location.mknoloc (Longident.Lident "_")
  in
  Ast_helper.Exp.ident ~loc:longident.loc longident

and parse_jsx_opening_or_self_closing_element ~start_pos p =
  let jsx_start_pos = p.Parser.start_pos in
  let name = parse_jsx_name p in
  let jsx_props = parse_jsx_props p in
  let children =
    match p.Parser.token with
    | Forwardslash ->
      (* <foo a=b /> *)
      let children_start_pos = p.Parser.start_pos in
      Parser.next p;
      let children_end_pos = p.Parser.start_pos in
      Scanner.pop_mode p.scanner Jsx;
      Parser.expect GreaterThan p;
      let loc = mk_loc children_start_pos children_end_pos in
      make_list_expression loc [] None (* no children *)
    | GreaterThan -> (
      (* <foo a=b> bar </foo> *)
      let children_start_pos = p.Parser.start_pos in
      Parser.next p;
      let spread, children = parse_jsx_children p in
      let children_end_pos = p.Parser.start_pos in
      let () =
        match p.token with
        | LessThanSlash -> Parser.next p
        | LessThan ->
          Parser.next p;
          Parser.expect Forwardslash p
        | token when Grammar.is_structure_item_start token -> ()
        | _ -> Parser.expect LessThanSlash p
      in
      match p.Parser.token with
      | (Lident _ | Uident _) when verify_jsx_opening_closing_name p name -> (
        Scanner.pop_mode p.scanner Jsx;
        Parser.expect GreaterThan p;
        let loc = mk_loc children_start_pos children_end_pos in
        match (spread, children) with
        | true, child :: _ -> child
        | _ -> make_list_expression loc children None)
      | token -> (
        Scanner.pop_mode p.scanner Jsx;
        let () =
          if Grammar.is_structure_item_start token then
            let closing = "</" ^ string_of_pexp_ident name ^ ">" in
            let msg = Diagnostics.message ("Missing " ^ closing) in
            Parser.err ~start_pos ~end_pos:p.prev_end_pos p msg
          else
            let opening = "</" ^ string_of_pexp_ident name ^ ">" in
            let msg =
              "Closing jsx name should be the same as the opening name. Did \
               you mean " ^ opening ^ " ?"
            in
            Parser.err ~start_pos ~end_pos:p.prev_end_pos p
              (Diagnostics.message msg);
            Parser.expect GreaterThan p
        in
        let loc = mk_loc children_start_pos children_end_pos in
        match (spread, children) with
        | true, child :: _ -> child
        | _ -> make_list_expression loc children None))
    | token ->
      Scanner.pop_mode p.scanner Jsx;
      Parser.err p (Diagnostics.unexpected token p.breadcrumbs);
      make_list_expression Location.none [] None
  in
  let jsx_end_pos = p.prev_end_pos in
  let loc = mk_loc jsx_start_pos jsx_end_pos in
  Ast_helper.Exp.apply ~loc name
    (List.concat
       [
         jsx_props;
         [
           (Asttypes.Labelled {txt = "children"; loc = Location.none}, children);
           ( Asttypes.Nolabel,
             Ast_helper.Exp.construct
               (Location.mknoloc (Longident.Lident "()"))
               None );
         ];
       ])

(*
 *  jsx ::=
 *    | <> jsx-children </>
 *    | <element-name {jsx-prop} />
 *    | <element-name {jsx-prop}> jsx-children </element-name>
 *
 *  jsx-children ::= primary-expr*          * => 0 or more
 *)
and parse_jsx p =
  Scanner.set_jsx_mode p.Parser.scanner;
  Parser.leave_breadcrumb p Grammar.Jsx;
  let start_pos = p.Parser.start_pos in
  Parser.expect LessThan p;
  let jsx_expr =
    match p.Parser.token with
    | Lident _ | Uident _ ->
      parse_jsx_opening_or_self_closing_element ~start_pos p
    | GreaterThan ->
      (* fragment: <> foo </> *)
      parse_jsx_fragment p
    | _ -> parse_jsx_name p
  in
  Parser.eat_breadcrumb p;
  {jsx_expr with pexp_attributes = [jsx_attr]}

(*
 * jsx-fragment ::=
 *  | <> </>
 *  | <> jsx-children </>
 *)
and parse_jsx_fragment p =
  let children_start_pos = p.Parser.start_pos in
  Parser.expect GreaterThan p;
  let _spread, children = parse_jsx_children p in
  let children_end_pos = p.Parser.start_pos in
  if p.token = LessThan then p.token <- Scanner.reconsider_less_than p.scanner;
  Parser.expect LessThanSlash p;
  Scanner.pop_mode p.scanner Jsx;
  Parser.expect GreaterThan p;
  let loc = mk_loc children_start_pos children_end_pos in
  make_list_expression loc children None

(*
 * jsx-prop ::=
 *   |  lident
 *   | ?lident
 *   |  lident =  jsx_expr
 *   |  lident = ?jsx_expr
 *   |  {...jsx_expr}
 *)
and parse_jsx_prop p =
  match p.Parser.token with
  | Question | Lident _ -> (
    let optional = Parser.optional p Question in
    let name, loc = parse_lident p in
    (* optional punning: <foo ?a /> *)
    if optional then
      Some
        ( Asttypes.Optional {txt = name; loc},
          Ast_helper.Exp.ident ~loc (Location.mkloc (Longident.Lident name) loc)
        )
    else
      match p.Parser.token with
      | Equal ->
        Parser.next p;
        (* no punning *)
        let optional = Parser.optional p Question in
        Scanner.pop_mode p.scanner Jsx;
        let attr_expr = parse_primary_expr ~operand:(parse_atomic_expr p) p in
        let label =
          if optional then Asttypes.Optional {txt = name; loc}
          else Asttypes.Labelled {txt = name; loc}
        in
        Some (label, attr_expr)
      | _ ->
        let attr_expr =
          Ast_helper.Exp.ident ~loc (Location.mkloc (Longident.Lident name) loc)
        in
        let label =
          if optional then Asttypes.Optional {txt = name; loc}
          else Asttypes.Labelled {txt = name; loc}
        in
        Some (label, attr_expr))
  (* {...props} *)
  | Lbrace -> (
    Scanner.pop_mode p.scanner Jsx;
    Parser.next p;
    match p.Parser.token with
    | DotDotDot -> (
      Scanner.pop_mode p.scanner Jsx;
      Parser.next p;
      let loc = mk_loc p.Parser.start_pos p.prev_end_pos in
      let attr_expr = parse_primary_expr ~operand:(parse_expr p) p in
      (* using label "spreadProps" to distinguish from others *)
      let label = Asttypes.Labelled {txt = "_spreadProps"; loc} in
      match p.Parser.token with
      | Rbrace ->
        Parser.next p;
        Scanner.set_jsx_mode p.scanner;
        Some (label, attr_expr)
      | _ -> None)
    | _ -> None)
  | _ -> None

and parse_jsx_props p =
  parse_region ~grammar:Grammar.JsxAttribute ~f:parse_jsx_prop p

and parse_jsx_children p =
  Scanner.pop_mode p.scanner Jsx;
  let rec loop p children =
    match p.Parser.token with
    | Token.Eof | LessThanSlash -> children
    | LessThan ->
      (* Imagine: <div> <Navbar /> <
       * is `<` the start of a jsx-child? <div …
       * or is it the start of a closing tag?  </div>
       * reconsiderLessThan peeks at the next token and
       * determines the correct token to disambiguate *)
      let token = Scanner.reconsider_less_than p.scanner in
      if token = LessThan then
        let child =
          parse_primary_expr ~operand:(parse_atomic_expr p) ~no_call:true p
        in
        loop p (child :: children)
      else
        (* LessThanSlash *)
        let () = p.token <- token in
        children
    | token when Grammar.is_jsx_child_start token ->
      let child =
        parse_primary_expr ~operand:(parse_atomic_expr p) ~no_call:true p
      in
      loop p (child :: children)
    | _ -> children
  in
  let spread, children =
    match p.Parser.token with
    | DotDotDot ->
      Parser.next p;
      (true, [parse_primary_expr ~operand:(parse_atomic_expr p) ~no_call:true p])
    | _ ->
      let children = List.rev (loop p []) in
      (false, children)
  in
  Scanner.set_jsx_mode p.scanner;
  (spread, children)

and parse_braced_or_record_expr p =
  let start_pos = p.Parser.start_pos in
  Parser.expect Lbrace p;
  match p.Parser.token with
  | Rbrace ->
    Parser.next p;
    let loc = mk_loc start_pos p.prev_end_pos in
    Ast_helper.Exp.record ~loc [] None
  | DotDotDot ->
    (* beginning of record spread, parse record *)
    Parser.next p;
    let spread_expr = parse_constrained_or_coerced_expr p in
    Parser.expect Comma p;
    let expr = parse_record_expr ~start_pos ~spread:(Some spread_expr) [] p in
    Parser.expect Rbrace p;
    expr
  | String s -> (
    let field =
      let loc = mk_loc p.start_pos p.end_pos in
      Parser.next p;
      Location.mkloc (Longident.Lident s) loc
    in
    match p.Parser.token with
    | Colon ->
      Parser.next p;
      let field_expr = parse_expr p in
      Parser.optional p Comma |> ignore;
      let expr =
        parse_record_expr_with_string_keys ~start_pos (field, field_expr, false)
          p
      in
      Parser.expect Rbrace p;
      expr
    | _ -> (
      let tag = if p.mode = ParseForTypeChecker then Some "js" else None in
      let constant =
        Ast_helper.Exp.constant ~loc:field.loc
          (Parsetree.Pconst_string (s, tag))
      in
      let a = parse_primary_expr ~operand:constant p in
      let e = parse_binary_expr ~a p 1 in
      let e = parse_ternary_expr e p in
      match p.Parser.token with
      | Semicolon ->
        let expr = parse_expr_block ~first:e p in
        Parser.expect Rbrace p;
        let loc = mk_loc start_pos p.prev_end_pos in
        let braces = make_braces_attr loc in
        {
          expr with
          Parsetree.pexp_attributes = braces :: expr.Parsetree.pexp_attributes;
        }
      | Rbrace ->
        Parser.next p;
        let loc = mk_loc start_pos p.prev_end_pos in
        let braces = make_braces_attr loc in
        {e with pexp_attributes = braces :: e.pexp_attributes}
      | _ ->
        let expr = parse_expr_block ~first:e p in
        Parser.expect Rbrace p;
        let loc = mk_loc start_pos p.prev_end_pos in
        let braces = make_braces_attr loc in
        {expr with pexp_attributes = braces :: expr.pexp_attributes}))
  | Question ->
    let expr = parse_record_expr ~start_pos [] p in
    Parser.expect Rbrace p;
    expr
  (*
    The branch below takes care of the "braced" expression {async}.
    The big reason that we need all these branches is that {x} isn't a record with a punned field x, but a braced expression… There's lots of "ambiguity" between a record with a single punned field and a braced expression…
    What is {x}?
      1) record {x: x}
      2) expression x which happens to wrapped in braces
    Due to historical reasons, we always follow 2
  *)
  | Lident "async" when is_es6_arrow_expression ~in_ternary:false p ->
    let expr = parse_async_arrow_expression p in
    let expr = parse_expr_block ~first:expr p in
    Parser.expect Rbrace p;
    let loc = mk_loc start_pos p.prev_end_pos in
    let braces = make_braces_attr loc in
    {expr with pexp_attributes = braces :: expr.pexp_attributes}
  | Uident _ | Lident _ -> (
    let start_token = p.token in
    let value_or_constructor = parse_value_or_constructor p in
    match value_or_constructor.pexp_desc with
    | Pexp_ident path_ident -> (
      let ident_end_pos = p.prev_end_pos in
      match p.Parser.token with
      | Comma ->
        Parser.next p;
        let value_or_constructor =
          match start_token with
          | Uident _ ->
            remove_module_name_from_punned_field_value value_or_constructor
          | _ -> value_or_constructor
        in
        let expr =
          parse_record_expr ~start_pos
            [(path_ident, value_or_constructor, false)]
            p
        in
        Parser.expect Rbrace p;
        expr
      | Colon -> (
        Parser.next p;
        let optional = parse_optional_label p in
        let field_expr = parse_expr p in
        match p.token with
        | Rbrace ->
          Parser.next p;
          let loc = mk_loc start_pos p.prev_end_pos in
          Ast_helper.Exp.record ~loc [(path_ident, field_expr, optional)] None
        | _ ->
          Parser.expect Comma p;
          let expr =
            parse_record_expr ~start_pos [(path_ident, field_expr, optional)] p
          in
          Parser.expect Rbrace p;
          expr)
      (* error case *)
      | Lident _ ->
        if p.prev_end_pos.pos_lnum < p.start_pos.pos_lnum then (
          Parser.expect Comma p;
          let expr =
            parse_record_expr ~start_pos
              [(path_ident, value_or_constructor, false)]
              p
          in
          Parser.expect Rbrace p;
          expr)
        else (
          Parser.expect Colon p;
          let expr =
            parse_record_expr ~start_pos
              [(path_ident, value_or_constructor, false)]
              p
          in
          Parser.expect Rbrace p;
          expr)
      | Semicolon ->
        let expr =
          parse_expr_block ~first:(Ast_helper.Exp.ident path_ident) p
        in
        Parser.expect Rbrace p;
        let loc = mk_loc start_pos p.prev_end_pos in
        let braces = make_braces_attr loc in
        {expr with pexp_attributes = braces :: expr.pexp_attributes}
      | Rbrace ->
        Parser.next p;
        let expr = Ast_helper.Exp.ident ~loc:path_ident.loc path_ident in
        let loc = mk_loc start_pos p.prev_end_pos in
        let braces = make_braces_attr loc in
        {expr with pexp_attributes = braces :: expr.pexp_attributes}
      | EqualGreater -> (
        let loc = mk_loc start_pos ident_end_pos in
        let ident = Location.mkloc (Longident.last path_ident.txt) loc in
        let a =
          parse_es6_arrow_expression ~async:false
            ~term_parameters:
              [
                {
                  attrs = [];
                  p_label = Nolabel;
                  expr = None;
                  pat = Ast_helper.Pat.var ~loc:ident.loc ident;
                  p_pos = start_pos;
                };
              ]
            p
        in
        let e = parse_binary_expr ~a p 1 in
        let e = parse_ternary_expr e p in
        match p.Parser.token with
        | Semicolon ->
          let expr = parse_expr_block ~first:e p in
          Parser.expect Rbrace p;
          let loc = mk_loc start_pos p.prev_end_pos in
          let braces = make_braces_attr loc in
          {expr with pexp_attributes = braces :: expr.pexp_attributes}
        | Rbrace ->
          Parser.next p;
          let loc = mk_loc start_pos p.prev_end_pos in
          let braces = make_braces_attr loc in
          {e with pexp_attributes = braces :: e.pexp_attributes}
        | _ ->
          let expr = parse_expr_block ~first:e p in
          Parser.expect Rbrace p;
          let loc = mk_loc start_pos p.prev_end_pos in
          let braces = make_braces_attr loc in
          {expr with pexp_attributes = braces :: expr.pexp_attributes})
      | _ -> (
        Parser.leave_breadcrumb p Grammar.ExprBlock;
        let a =
          parse_primary_expr
            ~operand:(Ast_helper.Exp.ident ~loc:path_ident.loc path_ident)
            p
        in
        let e = parse_binary_expr ~a p 1 in
        let e = parse_ternary_expr e p in
        Parser.eat_breadcrumb p;
        match p.Parser.token with
        | Semicolon ->
          let expr = parse_expr_block ~first:e p in
          Parser.expect Rbrace p;
          let loc = mk_loc start_pos p.prev_end_pos in
          let braces = make_braces_attr loc in
          {expr with pexp_attributes = braces :: expr.pexp_attributes}
        | Rbrace ->
          Parser.next p;
          let loc = mk_loc start_pos p.prev_end_pos in
          let braces = make_braces_attr loc in
          {e with pexp_attributes = braces :: e.pexp_attributes}
        | _ ->
          let expr = parse_expr_block ~first:e p in
          Parser.expect Rbrace p;
          let loc = mk_loc start_pos p.prev_end_pos in
          let braces = make_braces_attr loc in
          {expr with pexp_attributes = braces :: expr.pexp_attributes}))
    | _ -> (
      Parser.leave_breadcrumb p Grammar.ExprBlock;
      let a = parse_primary_expr ~operand:value_or_constructor p in
      let e = parse_binary_expr ~a p 1 in
      let e = parse_ternary_expr e p in
      Parser.eat_breadcrumb p;
      match p.Parser.token with
      | Semicolon ->
        let expr = parse_expr_block ~first:e p in
        Parser.expect Rbrace p;
        let loc = mk_loc start_pos p.prev_end_pos in
        let braces = make_braces_attr loc in
        {expr with pexp_attributes = braces :: expr.pexp_attributes}
      | Rbrace ->
        Parser.next p;
        let loc = mk_loc start_pos p.prev_end_pos in
        let braces = make_braces_attr loc in
        {e with pexp_attributes = braces :: e.pexp_attributes}
      | _ ->
        let expr = parse_expr_block ~first:e p in
        Parser.expect Rbrace p;
        let loc = mk_loc start_pos p.prev_end_pos in
        let braces = make_braces_attr loc in
        {expr with pexp_attributes = braces :: expr.pexp_attributes}))
  | _ ->
    let expr = parse_expr_block p in
    Parser.expect Rbrace p;
    let loc = mk_loc start_pos p.prev_end_pos in
    let braces = make_braces_attr loc in
    {expr with pexp_attributes = braces :: expr.pexp_attributes}

and parse_record_expr_row_with_string_key p =
  match p.Parser.token with
  | String s -> (
    let loc = mk_loc p.start_pos p.end_pos in
    Parser.next p;
    let field = Location.mkloc (Longident.Lident s) loc in
    match p.Parser.token with
    | Colon ->
      Parser.next p;
      let field_expr = parse_expr p in
      Some (field, field_expr, false)
    | _ -> Some (field, Ast_helper.Exp.ident ~loc:field.loc field, false))
  | _ -> None

and parse_record_expr_row p =
  let attrs = parse_attributes p in
  let () =
    match p.Parser.token with
    | Token.DotDotDot ->
      Parser.err p (Diagnostics.message ErrorMessages.record_expr_spread);
      Parser.next p
    | _ -> ()
  in
  match p.Parser.token with
  | Lident _ | Uident _ -> (
    let start_token = p.token in
    let field = parse_value_path p in
    match p.Parser.token with
    | Colon ->
      Parser.next p;
      let optional = parse_optional_label p in
      let field_expr = parse_expr p in
      Some (field, field_expr, optional)
    | _ ->
      let value = Ast_helper.Exp.ident ~loc:field.loc ~attrs field in
      let value =
        match start_token with
        | Uident _ -> remove_module_name_from_punned_field_value value
        | _ -> value
      in
      Some (field, value, false))
  | Question -> (
    Parser.next p;
    match p.Parser.token with
    | Lident _ | Uident _ ->
      let start_token = p.token in
      let field = parse_value_path p in
      let value = Ast_helper.Exp.ident ~loc:field.loc ~attrs field in
      let value =
        match start_token with
        | Uident _ -> remove_module_name_from_punned_field_value value
        | _ -> value
      in
      Some (field, value, true)
    | _ -> None)
  | _ -> None

and parse_dict_expr_row p =
  match p.Parser.token with
  | String s -> (
    let loc = mk_loc p.start_pos p.end_pos in
    Parser.next p;
    let field = Location.mkloc (Longident.Lident s) loc in
    match p.Parser.token with
    | Colon ->
      Parser.next p;
      let fieldExpr = parse_expr p in
      Some (field, fieldExpr)
    | _ -> Some (field, Ast_helper.Exp.ident ~loc:field.loc field))
  | _ -> None

and parse_record_expr_with_string_keys ~start_pos first_row p =
  let rows =
    first_row
    :: parse_comma_delimited_region ~grammar:Grammar.RecordRowsStringKey
         ~closing:Rbrace ~f:parse_record_expr_row_with_string_key p
  in
  let loc = mk_loc start_pos p.end_pos in
  let record_str_expr =
    Ast_helper.Str.eval ~loc (Ast_helper.Exp.record ~loc rows None)
  in
  Ast_helper.Exp.extension ~loc
    (Location.mkloc "obj" loc, Parsetree.PStr [record_str_expr])

and parse_record_expr ~start_pos ?(spread = None) rows p =
  let exprs =
    parse_comma_delimited_region ~grammar:Grammar.RecordRows ~closing:Rbrace
      ~f:parse_record_expr_row p
  in
  let rows = rows @ exprs in
  let () =
    match rows with
    | [] ->
      let msg = "Record spread needs at least one field that's updated" in
      Parser.err p (Diagnostics.message msg)
    | _rows -> ()
  in
  let loc = mk_loc start_pos p.end_pos in
  Ast_helper.Exp.record ~loc rows spread

and parse_newline_or_semicolon_expr_block p =
  match p.Parser.token with
  | Semicolon -> Parser.next p
  | token when Grammar.is_block_expr_start token ->
    if p.prev_end_pos.pos_lnum < p.start_pos.pos_lnum then ()
    else
      Parser.err ~start_pos:p.prev_end_pos ~end_pos:p.end_pos p
        (Diagnostics.message
           "consecutive expressions on a line must be separated by ';' or a \
            newline")
  | _ -> ()

and parse_expr_block_item p =
  let start_pos = p.Parser.start_pos in
  let attrs = parse_attributes p in
  match p.Parser.token with
  | Module -> (
    Parser.next p;
    match p.token with
    | Lparen ->
      let expr = parse_first_class_module_expr ~start_pos p in
      let a = parse_primary_expr ~operand:expr p in
      let expr = parse_binary_expr ~a p 1 in
      parse_ternary_expr expr p
    | _ ->
      let name =
        match p.Parser.token with
        | Uident ident ->
          let loc = mk_loc p.start_pos p.end_pos in
          Parser.next p;
          Location.mkloc ident loc
        | t ->
          Parser.err p (Diagnostics.uident t);
          Location.mknoloc "_"
      in
      let body = parse_module_binding_body p in
      parse_newline_or_semicolon_expr_block p;
      let expr = parse_expr_block p in
      let loc = mk_loc start_pos p.prev_end_pos in
      Ast_helper.Exp.letmodule ~loc name body expr)
  | Exception ->
    let extension_constructor = parse_exception_def ~attrs p in
    parse_newline_or_semicolon_expr_block p;
    let block_expr = parse_expr_block p in
    let loc = mk_loc start_pos p.prev_end_pos in
    Ast_helper.Exp.letexception ~loc extension_constructor block_expr
  | Open ->
    let od = parse_open_description ~attrs p in
    parse_newline_or_semicolon_expr_block p;
    let block_expr = parse_expr_block p in
    let loc = mk_loc start_pos p.prev_end_pos in
    Ast_helper.Exp.open_ ~loc od.popen_override od.popen_lid block_expr
  | Let ->
    let rec_flag, let_bindings = parse_let_bindings ~attrs ~start_pos p in
    parse_newline_or_semicolon_expr_block p;
    let next =
      if Grammar.is_block_expr_start p.Parser.token then parse_expr_block p
      else
        let loc = mk_loc p.start_pos p.end_pos in
        Ast_helper.Exp.construct ~loc
          (Location.mkloc (Longident.Lident "()") loc)
          None
    in
    let loc = mk_loc start_pos p.prev_end_pos in
    Ast_helper.Exp.let_ ~loc rec_flag let_bindings next
  | _ ->
    let e1 =
      let expr = parse_expr p in
      {expr with pexp_attributes = List.concat [attrs; expr.pexp_attributes]}
    in
    parse_newline_or_semicolon_expr_block p;
    if Grammar.is_block_expr_start p.Parser.token then
      let e2 = parse_expr_block p in
      let loc = {e1.pexp_loc with loc_end = e2.pexp_loc.loc_end} in
      Ast_helper.Exp.sequence ~loc e1 e2
    else e1

(* blockExpr ::= expr
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
 *)
and parse_expr_block ?first p =
  Parser.leave_breadcrumb p Grammar.ExprBlock;
  let item =
    match first with
    | Some e -> e
    | None -> parse_expr_block_item p
  in
  parse_newline_or_semicolon_expr_block p;
  let block_expr =
    if Grammar.is_block_expr_start p.Parser.token then
      let next = parse_expr_block_item p in
      let loc = {item.pexp_loc with loc_end = next.pexp_loc.loc_end} in
      Ast_helper.Exp.sequence ~loc item next
    else item
  in
  Parser.eat_breadcrumb p;
  over_parse_constrained_or_coerced_or_arrow_expression p block_expr

and parse_async_arrow_expression ?(arrow_attrs = []) p =
  let start_pos = p.Parser.start_pos in
  Parser.expect (Lident "async") p;
  parse_es6_arrow_expression ~async:true ~arrow_attrs
    ~arrow_start_pos:(Some start_pos) p

and parse_await_expression p =
  let await_loc = mk_loc p.Parser.start_pos p.end_pos in
  let await_attr = make_await_attr await_loc in
  Parser.expect Await p;
  let token_prec = Token.precedence MinusGreater in
  let expr = parse_binary_expr ~context:OrdinaryExpr p token_prec in
  {
    expr with
    pexp_attributes = await_attr :: expr.pexp_attributes;
    pexp_loc = {expr.pexp_loc with loc_start = await_loc.loc_start};
  }

and parse_try_expression p =
  let start_pos = p.Parser.start_pos in
  Parser.expect Try p;
  let expr = parse_expr ~context:WhenExpr p in
  Parser.expect Res_token.catch p;
  Parser.expect Lbrace p;
  let cases = parse_pattern_matching p in
  Parser.expect Rbrace p;
  let loc = mk_loc start_pos p.prev_end_pos in
  Ast_helper.Exp.try_ ~loc expr cases

and parse_if_condition p =
  Parser.leave_breadcrumb p Grammar.IfCondition;
  (* doesn't make sense to try es6 arrow here? *)
  let condition_expr = parse_expr ~context:WhenExpr p in
  Parser.eat_breadcrumb p;
  condition_expr

and parse_then_branch p =
  Parser.leave_breadcrumb p IfBranch;
  Parser.expect Lbrace p;
  let then_expr = parse_expr_block p in
  Parser.expect Rbrace p;
  Parser.eat_breadcrumb p;
  then_expr

and parse_else_branch p =
  Parser.expect Lbrace p;
  let block_expr = parse_expr_block p in
  Parser.expect Rbrace p;
  block_expr

and parse_if_expr start_pos p =
  let condition_expr = parse_if_condition p in
  let then_expr = parse_then_branch p in
  let else_expr =
    match p.Parser.token with
    | Else ->
      Parser.end_region p;
      Parser.leave_breadcrumb p Grammar.ElseBranch;
      Parser.next p;
      Parser.begin_region p;
      let else_expr =
        match p.token with
        | If -> parse_if_or_if_let_expression p
        | _ -> parse_else_branch p
      in
      Parser.eat_breadcrumb p;
      Parser.end_region p;
      Some else_expr
    | _ ->
      Parser.end_region p;
      None
  in
  let loc = mk_loc start_pos p.prev_end_pos in
  Ast_helper.Exp.ifthenelse ~loc condition_expr then_expr else_expr

and parse_if_let_expr start_pos p =
  let pattern = parse_pattern p in
  Parser.expect Equal p;
  let condition_expr = parse_if_condition p in
  let then_expr = parse_then_branch p in
  let else_expr =
    match p.Parser.token with
    | Else ->
      Parser.end_region p;
      Parser.leave_breadcrumb p Grammar.ElseBranch;
      Parser.next p;
      Parser.begin_region p;
      let else_expr =
        match p.token with
        | If -> parse_if_or_if_let_expression p
        | _ -> parse_else_branch p
      in
      Parser.eat_breadcrumb p;
      Parser.end_region p;
      else_expr
    | _ ->
      Parser.end_region p;
      let start_pos = p.Parser.start_pos in
      let loc = mk_loc start_pos p.prev_end_pos in
      Ast_helper.Exp.construct ~loc
        (Location.mkloc (Longident.Lident "()") loc)
        None
  in
  let loc = mk_loc start_pos p.prev_end_pos in
  Ast_helper.Exp.match_
    ~attrs:[if_let_attr; suppress_fragile_match_warning_attr]
    ~loc condition_expr
    [
      Ast_helper.Exp.case pattern then_expr;
      Ast_helper.Exp.case (Ast_helper.Pat.any ()) else_expr;
    ]

and parse_if_or_if_let_expression p =
  Parser.begin_region p;
  Parser.leave_breadcrumb p Grammar.ExprIf;
  let start_pos = p.Parser.start_pos in
  Parser.expect If p;
  let expr =
    match p.Parser.token with
    | Let ->
      Parser.next p;
      let if_let_expr = parse_if_let_expr start_pos p in
      Parser.err ~start_pos:if_let_expr.pexp_loc.loc_start
        ~end_pos:if_let_expr.pexp_loc.loc_end p
        (Diagnostics.message (ErrorMessages.experimental_if_let if_let_expr));
      if_let_expr
    | _ -> parse_if_expr start_pos p
  in
  Parser.eat_breadcrumb p;
  expr

and parse_for_rest has_opening_paren pattern start_pos p =
  Parser.expect In p;
  let e1 = parse_expr p in
  let direction =
    match p.Parser.token with
    | Lident "to" -> Asttypes.Upto
    | Lident "downto" -> Asttypes.Downto
    | token ->
      Parser.err p (Diagnostics.unexpected token p.breadcrumbs);
      Asttypes.Upto
  in
  if p.Parser.token = Eof then
    Parser.err ~start_pos:p.start_pos p
      (Diagnostics.unexpected p.Parser.token p.breadcrumbs)
  else Parser.next p;
  let e2 = parse_expr ~context:WhenExpr p in
  if has_opening_paren then Parser.expect Rparen p;
  Parser.expect Lbrace p;
  let body_expr = parse_expr_block p in
  Parser.expect Rbrace p;
  let loc = mk_loc start_pos p.prev_end_pos in
  Ast_helper.Exp.for_ ~loc pattern e1 e2 direction body_expr

and parse_for_expression p =
  let start_pos = p.Parser.start_pos in
  Parser.leave_breadcrumb p Grammar.ExprFor;
  Parser.expect For p;
  Parser.begin_region p;
  let for_expr =
    match p.token with
    | Lparen -> (
      let lparen = p.start_pos in
      Parser.next p;
      match p.token with
      | Rparen ->
        Parser.next p;
        let unit_pattern =
          let loc = mk_loc lparen p.prev_end_pos in
          let lid = Location.mkloc (Longident.Lident "()") loc in
          Ast_helper.Pat.construct lid None
        in
        parse_for_rest false
          (parse_alias_pattern ~attrs:[] unit_pattern p)
          start_pos p
      | _ -> (
        Parser.leave_breadcrumb p Grammar.Pattern;
        let pat = parse_pattern p in
        Parser.eat_breadcrumb p;
        match p.token with
        | Comma ->
          Parser.next p;
          let tuple_pattern =
            parse_tuple_pattern ~attrs:[] ~start_pos:lparen ~first:pat p
          in
          let pattern = parse_alias_pattern ~attrs:[] tuple_pattern p in
          parse_for_rest false pattern start_pos p
        | _ -> parse_for_rest true pat start_pos p))
    | _ ->
      Parser.leave_breadcrumb p Grammar.Pattern;
      let pat = parse_pattern p in
      Parser.eat_breadcrumb p;
      parse_for_rest false pat start_pos p
  in
  Parser.eat_breadcrumb p;
  Parser.end_region p;
  for_expr

and parse_while_expression p =
  let start_pos = p.Parser.start_pos in
  Parser.expect While p;
  let expr1 = parse_expr ~context:WhenExpr p in
  Parser.expect Lbrace p;
  let expr2 = parse_expr_block p in
  Parser.expect Rbrace p;
  let loc = mk_loc start_pos p.prev_end_pos in
  Ast_helper.Exp.while_ ~loc expr1 expr2

and parse_pattern_guard p =
  match p.Parser.token with
  | When | If ->
    Parser.next p;
    Some (parse_expr ~context:WhenExpr p)
  | _ -> None

and parse_pattern_match_case p =
  Parser.begin_region p;
  Parser.leave_breadcrumb p Grammar.PatternMatchCase;
  match p.Parser.token with
  | Token.Bar ->
    Parser.next p;
    Parser.leave_breadcrumb p Grammar.Pattern;
    let lhs = parse_pattern p in
    Parser.eat_breadcrumb p;
    let guard = parse_pattern_guard p in
    let () =
      match p.token with
      | EqualGreater -> Parser.next p
      | _ -> Recover.recover_equal_greater p
    in
    let rhs = parse_expr_block p in
    Parser.end_region p;
    Parser.eat_breadcrumb p;
    Some (Ast_helper.Exp.case lhs ?guard rhs)
  | _ ->
    Parser.end_region p;
    Parser.eat_breadcrumb p;
    None

and parse_pattern_matching p =
  let cases =
    parse_delimited_region ~grammar:Grammar.PatternMatching ~closing:Rbrace
      ~f:parse_pattern_match_case p
  in
  let () =
    match cases with
    | [] ->
      Parser.err ~start_pos:p.prev_end_pos p
        (Diagnostics.message "Pattern matching needs at least one case")
    | _ -> ()
  in
  cases

and parse_switch_expression p =
  let start_pos = p.Parser.start_pos in
  Parser.expect Switch p;
  let switch_expr = parse_expr ~context:WhenExpr p in
  Parser.expect Lbrace p;
  let cases = parse_pattern_matching p in
  Parser.expect Rbrace p;
  let loc = mk_loc start_pos p.prev_end_pos in
  Ast_helper.Exp.match_ ~loc switch_expr cases

(*
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
 *  dotted_argument ::=
 *   | . argument
 *)
and parse_argument p : argument option =
  if
    p.Parser.token = Token.Tilde
    || p.token = Dot || p.token = Underscore
    || Grammar.is_expr_start p.token
  then
    match p.Parser.token with
    | Dot -> (
      Parser.next p;
      match p.token with
      (* apply(.) *)
      | Rparen ->
        let unit_expr =
          Ast_helper.Exp.construct
            (Location.mknoloc (Longident.Lident "()"))
            None
        in
        Some {label = Asttypes.Nolabel; expr = unit_expr}
      | _ -> parse_argument2 p)
    | _ -> parse_argument2 p
  else None

and parse_argument2 p : argument option =
  match p.Parser.token with
  (* foo(_), do not confuse with foo(_ => x), TODO: performance *)
  | Underscore when not (is_es6_arrow_expression ~in_ternary:false p) ->
    let loc = mk_loc p.start_pos p.end_pos in
    Parser.next p;
    let expr =
      Ast_helper.Exp.ident ~loc (Location.mkloc (Longident.Lident "_") loc)
    in
    Some {label = Nolabel; expr}
  | Tilde -> (
    Parser.next p;
    (* TODO: nesting of pattern matches not intuitive for error recovery *)
    match p.Parser.token with
    | Lident ident -> (
      let start_pos = p.start_pos in
      Parser.next p;
      let end_pos = p.prev_end_pos in
      let loc = mk_loc start_pos end_pos in
      let named_arg_loc = loc in
      let ident_expr =
        Ast_helper.Exp.ident ~loc (Location.mkloc (Longident.Lident ident) loc)
      in
      match p.Parser.token with
      | Question ->
        Parser.next p;
        Some
          {
            label = Optional {txt = ident; loc = named_arg_loc};
            expr = ident_expr;
          }
      | Equal ->
        Parser.next p;
        let label =
          match p.Parser.token with
          | Question ->
            Parser.next p;
            Asttypes.Optional {txt = ident; loc = named_arg_loc}
          | _ -> Asttypes.Labelled {txt = ident; loc = named_arg_loc}
        in
        let expr =
          match p.Parser.token with
          | Underscore when not (is_es6_arrow_expression ~in_ternary:false p) ->
            let loc = mk_loc p.start_pos p.end_pos in
            Parser.next p;
            Ast_helper.Exp.ident ~loc
              (Location.mkloc (Longident.Lident "_") loc)
          | _ -> parse_constrained_or_coerced_expr p
        in
        Some {label; expr}
      | Colon ->
        Parser.next p;
        let typ = parse_typ_expr p in
        let loc = mk_loc start_pos p.prev_end_pos in
        let expr = Ast_helper.Exp.constraint_ ~loc ident_expr typ in
        Some
          {label = Asttypes.Labelled {txt = ident; loc = named_arg_loc}; expr}
      | _ ->
        Some
          {
            label = Asttypes.Labelled {txt = ident; loc = named_arg_loc};
            expr = ident_expr;
          })
    | t ->
      Parser.err p (Diagnostics.lident t);
      Some {label = Nolabel; expr = Recover.default_expr ()})
  | _ -> Some {label = Nolabel; expr = parse_constrained_or_coerced_expr p}

and parse_call_expr p fun_expr =
  Parser.expect Lparen p;
  let start_pos = p.Parser.start_pos in
  Parser.leave_breadcrumb p Grammar.ExprCall;
  let args =
    parse_comma_delimited_region ~grammar:Grammar.ArgumentList ~closing:Rparen
      ~f:parse_argument p
  in
  let partial =
    match p.token with
    | DotDotDot when args <> [] ->
      Parser.next p;
      true
    | _ -> false
  in
  Parser.expect Rparen p;
  let args =
    match args with
    | [] ->
      let loc = mk_loc start_pos p.prev_end_pos in
      (* No args -> unit sugar: `foo()` *)
      [
        {
          label = Nolabel;
          expr =
            Ast_helper.Exp.construct ~loc
              (Location.mkloc (Longident.Lident "()") loc)
              None;
        };
      ]
    | args -> args
  in
  let loc = {fun_expr.pexp_loc with loc_end = p.prev_end_pos} in
  let args =
    match args with
    | {label = lbl; expr} :: args ->
      let group (grp, acc) {label = lbl; expr} = ((lbl, expr) :: grp, acc) in
      let grp, acc = List.fold_left group ([(lbl, expr)], []) args in
      List.rev (List.rev grp :: acc)
    | [] -> []
  in
  let apply =
    Ext_list.fold_left args fun_expr (fun call_body args ->
        let args, wrap = process_underscore_application args in
        let exp = Ast_helper.Exp.apply ~loc ~partial call_body args in
        wrap exp)
  in

  Parser.eat_breadcrumb p;
  apply

and parse_value_or_constructor p =
  let start_pos = p.Parser.start_pos in
  let rec aux p acc =
    match p.Parser.token with
    | Uident ident -> (
      let end_pos_lident = p.end_pos in
      Parser.next p;
      match p.Parser.token with
      | Dot ->
        Parser.next p;
        aux p (ident :: acc)
      | Lparen when p.prev_end_pos.pos_lnum == p.start_pos.pos_lnum ->
        let lparen = p.start_pos in
        let args = parse_constructor_args p in
        let rparen = p.prev_end_pos in
        let lident = build_longident (ident :: acc) in
        let tail =
          match args with
          | [] -> None
          | [({Parsetree.pexp_desc = Pexp_tuple _} as arg)] as args ->
            let loc = mk_loc lparen rparen in
            if p.mode = ParseForTypeChecker then
              (* Some(1, 2) for type-checker *)
              Some arg
            else
              (* Some((1, 2)) for printer *)
              Some (Ast_helper.Exp.tuple ~loc args)
          | [arg] -> Some arg
          | args ->
            let loc = mk_loc lparen rparen in
            Some (Ast_helper.Exp.tuple ~loc args)
        in
        let loc = mk_loc start_pos p.prev_end_pos in
        let ident_loc = mk_loc start_pos end_pos_lident in
        Ast_helper.Exp.construct ~loc (Location.mkloc lident ident_loc) tail
      | _ ->
        let loc = mk_loc start_pos p.prev_end_pos in
        let lident = build_longident (ident :: acc) in
        Ast_helper.Exp.construct ~loc (Location.mkloc lident loc) None)
    | Lident ident ->
      Parser.next p;
      let loc = mk_loc start_pos p.prev_end_pos in
      let lident = build_longident (ident :: acc) in
      Ast_helper.Exp.ident ~loc (Location.mkloc lident loc)
    | token ->
      if acc = [] then (
        Parser.next_unsafe p;
        Parser.err p (Diagnostics.unexpected token p.breadcrumbs);
        Recover.default_expr ())
      else
        let loc = mk_loc start_pos p.prev_end_pos in
        Parser.err p (Diagnostics.unexpected token p.breadcrumbs);
        let lident = build_longident ("_" :: acc) in
        Ast_helper.Exp.ident ~loc (Location.mkloc lident loc)
  in
  aux p []

and parse_poly_variant_expr p =
  let start_pos = p.start_pos in
  let ident, _loc = parse_hash_ident ~start_pos p in
  match p.Parser.token with
  | Lparen when p.prev_end_pos.pos_lnum == p.start_pos.pos_lnum ->
    let lparen = p.start_pos in
    let args = parse_constructor_args p in
    let rparen = p.prev_end_pos in
    let loc_paren = mk_loc lparen rparen in
    let tail =
      match args with
      | [] -> None
      | [({Parsetree.pexp_desc = Pexp_tuple _} as expr)] as args ->
        if p.mode = ParseForTypeChecker then
          (* #a(1, 2) for type-checker *)
          Some expr
        else
          (* #a((1, 2)) for type-checker *)
          Some (Ast_helper.Exp.tuple ~loc:loc_paren args)
      | [arg] -> Some arg
      | args ->
        (* #a((1, 2)) for printer *)
        Some (Ast_helper.Exp.tuple ~loc:loc_paren args)
    in
    let loc = mk_loc start_pos p.prev_end_pos in
    Ast_helper.Exp.variant ~loc ident tail
  | _ ->
    let loc = mk_loc start_pos p.prev_end_pos in
    Ast_helper.Exp.variant ~loc ident None

and parse_constructor_args p =
  let lparen = p.Parser.start_pos in
  Parser.expect Lparen p;
  let args =
    parse_comma_delimited_region ~grammar:Grammar.ExprList
      ~f:parse_constrained_expr_region ~closing:Rparen p
  in
  Parser.expect Rparen p;
  match args with
  | [] ->
    let loc = mk_loc lparen p.prev_end_pos in
    [
      Ast_helper.Exp.construct ~loc
        (Location.mkloc (Longident.Lident "()") loc)
        None;
    ]
  | args -> args

and parse_tuple_expr ~first ~start_pos p =
  let exprs =
    first
    :: parse_comma_delimited_region p ~grammar:Grammar.ExprList ~closing:Rparen
         ~f:parse_constrained_expr_region
  in
  Parser.expect Rparen p;
  let () =
    match exprs with
    | [_] ->
      Parser.err ~start_pos ~end_pos:p.prev_end_pos p
        (Diagnostics.message ErrorMessages.tuple_single_element)
    | _ -> ()
  in
  let loc = mk_loc start_pos p.prev_end_pos in
  Ast_helper.Exp.tuple ~loc exprs

and parse_spread_expr_region_with_loc p =
  let start_pos = p.Parser.prev_end_pos in
  match p.Parser.token with
  | DotDotDot ->
    Parser.next p;
    let expr = parse_constrained_or_coerced_expr p in
    Some (true, expr, start_pos, p.prev_end_pos)
  | token when Grammar.is_expr_start token ->
    Some (false, parse_constrained_or_coerced_expr p, start_pos, p.prev_end_pos)
  | _ -> None

and parse_list_expr ~start_pos p =
  let split_by_spread exprs =
    List.fold_left
      (fun acc curr ->
        match (curr, acc) with
        | (true, expr, start_pos, end_pos), _ ->
          (* find a spread expression, prepend a new sublist *)
          ([], Some expr, start_pos, end_pos) :: acc
        | ( (false, expr, start_pos, _endPos),
            (no_spreads, spread, _accStartPos, acc_end_pos) :: acc ) ->
          (* find a non-spread expression, and the accumulated is not empty,
           * prepend to the first sublist, and update the loc of the first sublist *)
          (expr :: no_spreads, spread, start_pos, acc_end_pos) :: acc
        | (false, expr, start_pos, end_pos), [] ->
          (* find a non-spread expression, and the accumulated is empty *)
          [([expr], None, start_pos, end_pos)])
      [] exprs
  in
  let make_sub_expr = function
    | exprs, Some spread, start_pos, end_pos ->
      make_list_expression (mk_loc start_pos end_pos) exprs (Some spread)
    | exprs, None, start_pos, end_pos ->
      make_list_expression (mk_loc start_pos end_pos) exprs None
  in
  let list_exprs_rev =
    parse_comma_delimited_reversed_list p ~grammar:Grammar.ListExpr
      ~closing:Rbrace ~f:parse_spread_expr_region_with_loc
  in
  Parser.expect Rbrace p;
  let loc = mk_loc start_pos p.prev_end_pos in
  match split_by_spread list_exprs_rev with
  | [] -> make_list_expression loc [] None
  | [(exprs, Some spread, _, _)] -> make_list_expression loc exprs (Some spread)
  | [(exprs, None, _, _)] -> make_list_expression loc exprs None
  | exprs ->
    let list_exprs = List.map make_sub_expr exprs in
    Ast_helper.Exp.apply ~loc
      (Ast_helper.Exp.ident ~loc ~attrs:[spread_attr]
         (Location.mkloc
            (Longident.Ldot
               (Longident.Ldot (Longident.Lident "Belt", "List"), "concatMany"))
            loc))
      [(Asttypes.Nolabel, Ast_helper.Exp.array ~loc list_exprs)]

and parse_dict_expr ~start_pos p =
  let rows =
    parse_comma_delimited_region ~grammar:Grammar.DictRows ~closing:Rbrace
      ~f:parse_dict_expr_row p
  in
  let loc = mk_loc start_pos p.end_pos in
  let to_key_value_pair
      (record_item : Longident.t Location.loc * Parsetree.expression) =
    match record_item with
    | ( {Location.txt = Longident.Lident key; loc = keyLoc},
        ({pexp_loc = value_loc} as value_expr) ) ->
      Some
        (Ast_helper.Exp.tuple
           ~loc:(mk_loc keyLoc.loc_start value_loc.loc_end)
           [
             Ast_helper.Exp.constant ~loc:keyLoc (Pconst_string (key, None));
             value_expr;
           ])
    | _ -> None
  in
  let key_value_pairs = List.filter_map to_key_value_pair rows in
  Parser.expect Rbrace p;
  Ast_helper.Exp.apply ~loc
    (Ast_helper.Exp.ident ~loc
       (Location.mkloc
          (Longident.Ldot (Longident.Lident Primitive_modules.dict, "make"))
          loc))
    [(Asttypes.Nolabel, Ast_helper.Exp.array ~loc key_value_pairs)]

and parse_array_exp p =
  let start_pos = p.Parser.start_pos in
  Parser.expect Lbracket p;
  let split_by_spread exprs =
    List.fold_left
      (fun acc curr ->
        match (curr, acc) with
        | (true, expr, start_pos, end_pos), _ ->
          (* find a spread expression, prepend a new sublist *)
          ([], Some expr, start_pos, end_pos) :: acc
        | ( (false, expr, start_pos, _endPos),
            (no_spreads, spread, _accStartPos, acc_end_pos) :: acc ) ->
          (* find a non-spread expression, and the accumulated is not empty,
           * prepend to the first sublist, and update the loc of the first sublist *)
          (expr :: no_spreads, spread, start_pos, acc_end_pos) :: acc
        | (false, expr, start_pos, end_pos), [] ->
          (* find a non-spread expression, and the accumulated is empty *)
          [([expr], None, start_pos, end_pos)])
      [] exprs
  in
  let list_exprs_rev =
    parse_comma_delimited_reversed_list p ~grammar:Grammar.ExprList
      ~closing:Rbracket ~f:parse_spread_expr_region_with_loc
  in
  Parser.expect Rbracket p;
  let loc = mk_loc start_pos p.prev_end_pos in
  let collect_exprs = function
    | [], Some spread, _startPos, _endPos -> [spread]
    | exprs, Some spread, _startPos, _endPos ->
      let els = Ast_helper.Exp.array ~loc exprs in
      [els; spread]
    | exprs, None, _startPos, _endPos ->
      let els = Ast_helper.Exp.array ~loc exprs in
      [els]
  in
  match split_by_spread list_exprs_rev with
  | [] -> Ast_helper.Exp.array ~loc:(mk_loc start_pos p.prev_end_pos) []
  | [(exprs, None, _, _)] ->
    Ast_helper.Exp.array ~loc:(mk_loc start_pos p.prev_end_pos) exprs
  | exprs ->
    let xs = List.map collect_exprs exprs in
    let list_exprs =
      List.fold_right
        (fun exprs1 acc ->
          List.fold_right (fun expr1 acc1 -> expr1 :: acc1) exprs1 acc)
        xs []
    in
    Ast_helper.Exp.apply ~loc
      (Ast_helper.Exp.ident ~loc ~attrs:[spread_attr]
         (Location.mkloc
            (Longident.Ldot
               (Longident.Ldot (Longident.Lident "Belt", "Array"), "concatMany"))
            loc))
      [(Nolabel, Ast_helper.Exp.array ~loc list_exprs)]

(* TODO: check attributes in the case of poly type vars,
 * might be context dependend: parseFieldDeclaration (see ocaml) *)
and parse_poly_type_expr ?current_type_name_path ?inline_types_context p =
  let start_pos = p.Parser.start_pos in
  match p.Parser.token with
  | SingleQuote -> (
    let vars = parse_type_var_list p in
    match vars with
    | _v1 :: _v2 :: _ ->
      Parser.expect Dot p;
      let typ = parse_typ_expr p in
      let loc = mk_loc start_pos p.prev_end_pos in
      Ast_helper.Typ.poly ~loc vars typ
    | [var] -> (
      match p.Parser.token with
      | Dot ->
        Parser.next p;
        let typ = parse_typ_expr p in
        let loc = mk_loc start_pos p.prev_end_pos in
        Ast_helper.Typ.poly ~loc vars typ
      | EqualGreater ->
        Parser.next p;
        let typ = Ast_helper.Typ.var ~loc:var.loc var.txt in
        let return_type = parse_typ_expr ~alias:false p in
        let loc = mk_loc typ.Parsetree.ptyp_loc.loc_start p.prev_end_pos in
        Ast_helper.Typ.arrow ~loc ~arity:(Some 1) Nolabel typ return_type
      | _ -> Ast_helper.Typ.var ~loc:var.loc var.txt)
    | _ -> assert false)
  | _ -> parse_typ_expr ?current_type_name_path ?inline_types_context p

(* 'a 'b 'c *)
and parse_type_var_list p =
  let rec loop p vars =
    match p.Parser.token with
    | SingleQuote ->
      Parser.next p;
      let lident, loc =
        parse_ident ~msg:ErrorMessages.type_param ~start_pos:p.start_pos p
      in
      let var = Location.mkloc lident loc in
      loop p (var :: vars)
    | _ -> List.rev vars
  in
  loop p []

and parse_lident_list p =
  let rec loop p ls =
    match p.Parser.token with
    | Lident lident ->
      let loc = mk_loc p.start_pos p.end_pos in
      Parser.next p;
      loop p (Location.mkloc lident loc :: ls)
    | _ -> List.rev ls
  in
  loop p []

and parse_atomic_typ_expr ?current_type_name_path ?inline_types_context ~attrs p
    =
  Parser.leave_breadcrumb p Grammar.AtomicTypExpr;
  let start_pos = p.Parser.start_pos in
  let typ =
    match p.Parser.token with
    | SingleQuote ->
      Parser.next p;
      let ident, loc =
        if p.Parser.token = Eof then (
          Parser.err ~start_pos:p.start_pos p
            (Diagnostics.unexpected p.Parser.token p.breadcrumbs);
          ("", mk_loc p.start_pos p.prev_end_pos))
        else parse_ident ~msg:ErrorMessages.type_var ~start_pos:p.start_pos p
      in
      Ast_helper.Typ.var ~loc ~attrs ident
    | Underscore ->
      let end_pos = p.end_pos in
      Parser.next p;
      Ast_helper.Typ.any ~loc:(mk_loc start_pos end_pos) ~attrs ()
    | Lparen -> (
      Parser.next p;
      match p.Parser.token with
      | Rparen ->
        Parser.next p;
        let loc = mk_loc start_pos p.prev_end_pos in
        let unit_constr = Location.mkloc (Longident.Lident "unit") loc in
        Ast_helper.Typ.constr ~attrs unit_constr []
      | _ -> (
        let t = parse_typ_expr p in
        match p.token with
        | Comma ->
          Parser.next p;
          parse_tuple_type ~attrs ~first:t ~start_pos p
        | _ ->
          Parser.expect Rparen p;
          {
            t with
            ptyp_loc = mk_loc start_pos p.prev_end_pos;
            ptyp_attributes = List.concat [attrs; t.ptyp_attributes];
          }))
    | Lbracket -> parse_polymorphic_variant_type ~attrs p
    | Uident _ | Lident _ ->
      let constr = parse_value_path p in
      let args =
        parse_type_constructor_args ?inline_types_context
          ?current_type_name_path ~constr_name:constr p
      in
      let number_of_inline_records_in_args =
        match inline_types_context with
        | None -> 0
        | Some inline_types_context ->
          let inline_types = inline_types_context.found_inline_types in
          args
          |> List.filter (fun (c : Parsetree.core_type) ->
                 match c.ptyp_desc with
                 | Ptyp_constr ({txt = Lident typename}, _) ->
                   inline_types
                   |> List.exists (fun (name, _, _) -> name = typename)
                 | _ -> false)
          |> List.length
      in
      if number_of_inline_records_in_args > 1 then
        Parser.err ~start_pos ~end_pos:p.prev_end_pos p
          (Diagnostics.message
             ErrorMessages.multiple_inline_record_definitions_at_same_path);
      Ast_helper.Typ.constr
        ~loc:(mk_loc start_pos p.prev_end_pos)
        ~attrs constr args
    | Module ->
      Parser.next p;
      Parser.expect Lparen p;
      let package_type = parse_package_type ~start_pos ~attrs p in
      Parser.expect Rparen p;
      {package_type with ptyp_loc = mk_loc start_pos p.prev_end_pos}
    | Percent ->
      let extension = parse_extension p in
      let loc = mk_loc start_pos p.prev_end_pos in
      Ast_helper.Typ.extension ~attrs ~loc extension
    | Lbrace ->
      parse_record_or_object_type ?current_type_name_path ?inline_types_context
        ~attrs p
    | Eof ->
      Parser.err p (Diagnostics.unexpected p.Parser.token p.breadcrumbs);
      Recover.default_type ()
    | token -> (
      Parser.err p (Diagnostics.unexpected token p.breadcrumbs);
      match
        skip_tokens_and_maybe_retry p
          ~is_start_of_grammar:Grammar.is_atomic_typ_expr_start
      with
      | Some () -> parse_atomic_typ_expr ~attrs p
      | None ->
        Parser.err ~start_pos:p.prev_end_pos p
          (Diagnostics.unexpected token p.breadcrumbs);
        Recover.default_type ())
  in
  Parser.eat_breadcrumb p;
  typ

(* package-type	::=
    | modtype-path
    ∣ modtype-path with package-constraint  { and package-constraint }
*)
and parse_package_type ~start_pos ~attrs p =
  let mod_type_path = parse_module_long_ident ~lowercase:true p in
  match p.Parser.token with
  | Lident "with" ->
    Parser.next p;
    let constraints = parse_package_constraints p in
    let loc = mk_loc start_pos p.prev_end_pos in
    Ast_helper.Typ.package ~loc ~attrs mod_type_path constraints
  | _ ->
    let loc = mk_loc start_pos p.prev_end_pos in
    Ast_helper.Typ.package ~loc ~attrs mod_type_path []

(* package-constraint  { and package-constraint } *)
and parse_package_constraints p =
  let first =
    Parser.expect Typ p;
    let type_constr = parse_value_path p in
    Parser.expect Equal p;
    let typ = parse_typ_expr p in
    (type_constr, typ)
  in
  let rest =
    parse_region ~grammar:Grammar.PackageConstraint ~f:parse_package_constraint
      p
  in
  first :: rest

(* and type typeconstr = typexpr *)
and parse_package_constraint p =
  match p.Parser.token with
  | And ->
    Parser.next p;
    Parser.expect Typ p;
    let type_constr = parse_value_path p in
    Parser.expect Equal p;
    let typ = parse_typ_expr p in
    Some (type_constr, typ)
  | _ -> None

and parse_record_or_object_type ?current_type_name_path ?inline_types_context
    ~attrs p =
  (* for inline record in constructor *)
  let start_pos = p.Parser.start_pos in
  Parser.expect Lbrace p;
  let closed_flag =
    match p.token with
    | DotDot ->
      Parser.next p;
      Asttypes.Open
    | Dot ->
      Parser.next p;
      Asttypes.Closed
    | _ -> Asttypes.Closed
  in
  match (p.token, inline_types_context, current_type_name_path) with
  | Lident _, Some inline_types_context, Some current_type_name_path ->
    let labels =
      parse_comma_delimited_region ~grammar:Grammar.RecordDecl ~closing:Rbrace
        ~f:
          (parse_field_declaration_region ~current_type_name_path
             ~inline_types_context)
        p
    in
    Parser.expect Rbrace p;
    let loc = mk_loc start_pos p.prev_end_pos in
    let inline_type_name = current_type_name_path |> String.concat "." in

    inline_types_context.found_inline_types <-
      (inline_type_name, loc, Parsetree.Ptype_record labels)
      :: inline_types_context.found_inline_types;

    let lid = Location.mkloc (Longident.Lident inline_type_name) loc in
    Ast_helper.Typ.constr ~loc lid (inline_types_context.params |> List.map fst)
  | _ ->
    let () =
      match p.token with
      | Lident _ ->
        Parser.err p
          (Diagnostics.message ErrorMessages.forbidden_inline_record_declaration)
      | _ -> ()
    in
    let fields =
      parse_comma_delimited_region ~grammar:Grammar.StringFieldDeclarations
        ~closing:Rbrace ~f:parse_string_field_declaration p
    in
    Parser.expect Rbrace p;
    let loc = mk_loc start_pos p.prev_end_pos in
    Ast_helper.Typ.object_ ~loc ~attrs fields closed_flag

(* TODO: check associativity in combination with attributes *)
and parse_type_alias p typ =
  match p.Parser.token with
  | As ->
    Parser.next p;
    Parser.expect SingleQuote p;
    let ident, _loc =
      parse_ident ~msg:ErrorMessages.type_param ~start_pos:p.start_pos p
    in
    (* TODO: how do we parse attributes here? *)
    Ast_helper.Typ.alias
      ~loc:(mk_loc typ.Parsetree.ptyp_loc.loc_start p.prev_end_pos)
      typ ident
  | _ -> typ

(* type_parameter ::=
 *  | type_expr
 *  | ~ident: type_expr
 *  | ~ident: type_expr=?
 *
 * note:
 *  | attrs ~ident: type_expr    -> attrs are on the arrow
 *  | attrs type_expr            -> attrs are here part of the type_expr
 *
 * dotted_type_parameter ::=
 *  | . type_parameter
 *)
and parse_type_parameter p =
  let doc_attr : Parsetree.attributes =
    match p.Parser.token with
    | DocComment (loc, s) ->
      Parser.next p;
      [doc_comment_to_attribute loc s]
    | _ -> []
  in
  if
    p.Parser.token = Token.Tilde
    || p.token = Dot
    || Grammar.is_typ_expr_start p.token
  then
    let start_pos = p.Parser.start_pos in
    let _ =
      Parser.optional p Dot
      (* dot is ignored *)
    in
    let attrs = doc_attr @ parse_attributes p in
    match p.Parser.token with
    | Tilde -> (
      Parser.next p;
      let name, loc = parse_lident p in
      Parser.expect ~grammar:Grammar.TypeExpression Colon p;
      let typ = parse_typ_expr p in
      match p.Parser.token with
      | Equal ->
        Parser.next p;
        Parser.expect Question p;
        Some {attrs; label = Optional {txt = name; loc}; typ; start_pos}
      | _ -> Some {attrs; label = Labelled {txt = name; loc}; typ; start_pos})
    | Lident _ -> (
      let name, loc = parse_lident p in
      match p.token with
      | Colon -> (
        let () =
          let error =
            Diagnostics.message
              (ErrorMessages.missing_tilde_labeled_parameter name)
          in
          Parser.err ~start_pos:loc.loc_start ~end_pos:loc.loc_end p error
        in
        Parser.next p;
        let typ = parse_typ_expr p in
        match p.Parser.token with
        | Equal ->
          Parser.next p;
          Parser.expect Question p;
          Some {attrs; label = Optional {txt = name; loc}; typ; start_pos}
        | _ -> Some {attrs; label = Labelled {txt = name; loc}; typ; start_pos})
      | _ ->
        let constr = Location.mkloc (Longident.Lident name) loc in
        let args = parse_type_constructor_args ~constr_name:constr p in
        let typ =
          Ast_helper.Typ.constr
            ~loc:(mk_loc start_pos p.prev_end_pos)
            ~attrs constr args
        in

        let typ = parse_arrow_type_rest ~es6_arrow:true ~start_pos typ p in
        let typ = parse_type_alias p typ in
        Some {attrs = []; label = Nolabel; typ; start_pos})
    | _ ->
      let typ = parse_typ_expr p in
      let typ_with_attributes =
        {typ with ptyp_attributes = List.concat [attrs; typ.ptyp_attributes]}
      in
      Some {attrs = []; label = Nolabel; typ = typ_with_attributes; start_pos}
  else None

(* (int, ~x:string, float) *)
and parse_type_parameters p =
  let start_pos = p.Parser.start_pos in
  Parser.expect Lparen p;
  match p.Parser.token with
  | Rparen ->
    Parser.next p;
    let loc = mk_loc start_pos p.prev_end_pos in
    let unit_constr = Location.mkloc (Longident.Lident "unit") loc in
    let typ = Ast_helper.Typ.constr unit_constr [] in
    [{attrs = []; label = Nolabel; typ; start_pos}]
  | _ ->
    let params =
      parse_comma_delimited_region ~grammar:Grammar.TypeParameters
        ~closing:Rparen ~f:parse_type_parameter p
    in
    Parser.expect Rparen p;
    params

and parse_es6_arrow_type ~attrs p =
  let start_pos = p.Parser.start_pos in
  match p.Parser.token with
  | Tilde ->
    Parser.next p;
    let name, label_loc = parse_lident p in
    Parser.expect ~grammar:Grammar.TypeExpression Colon p;
    let typ = parse_typ_expr ~alias:false ~es6_arrow:false p in
    let arg =
      match p.Parser.token with
      | Equal ->
        Parser.next p;
        Parser.expect Question p;
        Asttypes.Optional {txt = name; loc = label_loc}
      | _ -> Asttypes.Labelled {txt = name; loc = label_loc}
    in
    Parser.expect EqualGreater p;
    let return_type = parse_typ_expr ~alias:false p in
    let loc = mk_loc start_pos p.prev_end_pos in
    Ast_helper.Typ.arrow ~loc ~attrs ~arity:None arg typ return_type
  | DocComment _ -> assert false
  | _ ->
    let parameters = parse_type_parameters p in
    Parser.expect EqualGreater p;
    let return_type = parse_typ_expr ~alias:false p in
    let end_pos = p.prev_end_pos in
    let return_type_arity = 0 in
    let _paramNum, typ, _arity =
      List.fold_right
        (fun {attrs; label = arg_lbl; typ; start_pos} (param_num, t, arity) ->
          let loc = mk_loc start_pos end_pos in
          let arity =
            (* Workaround for ~lbl: @as(json`false`) _, which changes the arity *)
            match arg_lbl with
            | Labelled _s ->
              let typ_is_any =
                match typ.ptyp_desc with
                | Ptyp_any -> true
                | _ -> false
              in
              let has_as =
                Ext_list.exists typ.ptyp_attributes (fun (x, _) -> x.txt = "as")
              in
              if !InExternal.status && typ_is_any && has_as then arity - 1
              else arity
            | _ -> arity
          in
          let t_arg =
            Ast_helper.Typ.arrow ~loc ~attrs ~arity:None arg_lbl typ t
          in
          if param_num = 1 then
            (param_num - 1, Ast_uncurried.uncurried_type ~arity t_arg, 1)
          else (param_num - 1, t_arg, arity + 1))
        parameters
        (List.length parameters, return_type, return_type_arity + 1)
    in
    {
      typ with
      ptyp_attributes = List.concat [typ.ptyp_attributes; attrs];
      ptyp_loc = mk_loc start_pos p.prev_end_pos;
    }

(*
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
 *)
and parse_typ_expr ?current_type_name_path ?inline_types_context ?attrs
    ?(es6_arrow = true) ?(alias = true) p =
  (* Parser.leaveBreadcrumb p Grammar.TypeExpression; *)
  let start_pos = p.Parser.start_pos in
  let attrs =
    match attrs with
    | Some attrs -> attrs
    | None -> parse_attributes p
  in
  let typ =
    if es6_arrow && is_es6_arrow_type p then parse_es6_arrow_type ~attrs p
    else
      let typ =
        parse_atomic_typ_expr ?current_type_name_path ?inline_types_context
          ~attrs p
      in
      parse_arrow_type_rest ~es6_arrow ~start_pos typ p
  in
  let typ = if alias then parse_type_alias p typ else typ in
  (* Parser.eatBreadcrumb p; *)
  typ

and parse_arrow_type_rest ~es6_arrow ~start_pos typ p =
  match p.Parser.token with
  | (EqualGreater | MinusGreater) as token when es6_arrow == true ->
    (* error recovery *)
    if token = MinusGreater then Parser.expect EqualGreater p;
    Parser.next p;
    let return_type = parse_typ_expr ~alias:false p in
    let loc = mk_loc start_pos p.prev_end_pos in
    Ast_helper.Typ.arrow ~loc ~arity:(Some 1) Nolabel typ return_type
  | _ -> typ

and parse_typ_expr_region p =
  if Grammar.is_typ_expr_start p.Parser.token then Some (parse_typ_expr p)
  else None

and parse_tuple_type ~attrs ~first ~start_pos p =
  let typexprs =
    first
    :: parse_comma_delimited_region ~grammar:Grammar.TypExprList ~closing:Rparen
         ~f:parse_typ_expr_region p
  in
  Parser.expect Rparen p;
  let () =
    match typexprs with
    | [_] ->
      Parser.err ~start_pos ~end_pos:p.prev_end_pos p
        (Diagnostics.message ErrorMessages.tuple_single_element)
    | _ -> ()
  in
  let tuple_loc = mk_loc start_pos p.prev_end_pos in
  Ast_helper.Typ.tuple ~attrs ~loc:tuple_loc typexprs

and parse_type_constructor_arg_region ?inline_types_context
    ?current_type_name_path p =
  if Grammar.is_typ_expr_start p.Parser.token then
    Some (parse_typ_expr ?inline_types_context ?current_type_name_path p)
  else if p.token = LessThan then (
    Parser.next p;
    parse_type_constructor_arg_region ?inline_types_context
      ?current_type_name_path p)
  else None

(* Js.Nullable.value<'a> *)
and parse_type_constructor_args ?inline_types_context ?current_type_name_path
    ~constr_name p =
  let opening = p.Parser.token in
  let opening_start_pos = p.start_pos in
  match opening with
  | LessThan | Lparen ->
    Scanner.set_diamond_mode p.scanner;
    Parser.next p;
    let type_args =
      (* TODO: change Grammar.TypExprList to TypArgList!!! Why did I wrote this? *)
      parse_comma_delimited_region ~grammar:Grammar.TypExprList
        ~closing:GreaterThan
        ~f:
          (parse_type_constructor_arg_region ?inline_types_context
             ?current_type_name_path)
        p
    in
    let () =
      match p.token with
      | Rparen when opening = Token.Lparen ->
        let typ = Ast_helper.Typ.constr constr_name type_args in
        let msg =
          Doc.breakable_group ~force_break:true
            (Doc.concat
               [
                 Doc.text "Type parameters require angle brackets:";
                 Doc.indent
                   (Doc.concat
                      [
                        Doc.line;
                        ResPrinter.print_typ_expr typ CommentTable.empty;
                      ]);
               ])
          |> Doc.to_string ~width:80
        in
        Parser.err ~start_pos:opening_start_pos p (Diagnostics.message msg);
        Parser.next p
      | _ -> Parser.expect GreaterThan p
    in
    Scanner.pop_mode p.scanner Diamond;
    type_args
  | _ -> []

(* string-field-decl ::=
 *  | string: poly-typexpr
 *  | attributes string-field-decl *)
and parse_string_field_declaration p =
  let attrs = parse_attributes p in
  match p.Parser.token with
  | String name ->
    let name_start_pos = p.start_pos in
    let name_end_pos = p.end_pos in
    Parser.next p;
    let field_name = Location.mkloc name (mk_loc name_start_pos name_end_pos) in
    Parser.expect ~grammar:Grammar.TypeExpression Colon p;
    let typ = parse_poly_type_expr p in
    Some (Parsetree.Otag (field_name, attrs, typ))
  | DotDotDot ->
    Parser.next p;
    let typ = parse_typ_expr p in
    Some (Parsetree.Oinherit typ)
  | Lident name ->
    let name_loc = mk_loc p.start_pos p.end_pos in
    Parser.err p
      (Diagnostics.message (ErrorMessages.object_quoted_field_name name));
    Parser.next p;
    let field_name = Location.mkloc name name_loc in
    Parser.expect ~grammar:Grammar.TypeExpression Colon p;
    let typ = parse_poly_type_expr p in
    Some (Parsetree.Otag (field_name, attrs, typ))
  | _token -> None

(* field-decl	::=
 *  | [mutable] field-name : poly-typexpr
 *  | attributes field-decl *)
and parse_field_declaration p =
  let start_pos = p.Parser.start_pos in
  let attrs = parse_attributes p in
  let mut =
    if Parser.optional p Token.Mutable then Asttypes.Mutable
    else Asttypes.Immutable
  in
  let lident, loc =
    match p.token with
    | _ -> parse_lident p
  in
  let optional = parse_optional_label p in
  let name = Location.mkloc lident loc in
  let typ =
    match p.Parser.token with
    | Colon ->
      Parser.next p;
      parse_poly_type_expr p
    | _ ->
      Ast_helper.Typ.constr ~loc:name.loc {name with txt = Lident name.txt} []
  in
  let loc = mk_loc start_pos typ.ptyp_loc.loc_end in
  Ast_helper.Type.field ~attrs ~loc ~mut ~optional name typ

and parse_field_declaration_region ?current_type_name_path ?inline_types_context
    ?found_object_field p =
  let start_pos = p.Parser.start_pos in
  let attrs = parse_attributes p in
  let mut =
    if Parser.optional p Token.Mutable then Asttypes.Mutable
    else Asttypes.Immutable
  in
  match p.token with
  | DotDotDot ->
    Parser.next p;
    let name = Location.mkloc "..." (mk_loc start_pos p.prev_end_pos) in
    let typ = parse_poly_type_expr p in
    let loc = mk_loc start_pos typ.ptyp_loc.loc_end in
    Some (Ast_helper.Type.field ~attrs ~loc ~mut name typ)
  | String s when found_object_field <> None ->
    Option.get found_object_field := true;
    Parser.next p;
    let name = Location.mkloc s (mk_loc start_pos p.prev_end_pos) in
    Parser.expect Colon p;
    let typ = parse_poly_type_expr p in
    let loc = mk_loc start_pos typ.ptyp_loc.loc_end in
    Some (Ast_helper.Type.field ~attrs ~loc ~mut name typ)
  | Lident _ ->
    let lident, loc = parse_lident p in
    let name = Location.mkloc lident loc in
    let current_type_name_path =
      match current_type_name_path with
      | None -> None
      | Some current_type_name_path -> Some (current_type_name_path @ [name.txt])
    in
    let optional = parse_optional_label p in
    let typ =
      match p.Parser.token with
      | Colon ->
        Parser.next p;
        parse_poly_type_expr ?current_type_name_path ?inline_types_context p
      | _ ->
        Ast_helper.Typ.constr ~loc:name.loc ~attrs
          {name with txt = Lident name.txt}
          []
    in
    let loc = mk_loc start_pos typ.ptyp_loc.loc_end in
    Some (Ast_helper.Type.field ~attrs ~loc ~mut ~optional name typ)
  | _ ->
    if attrs <> [] then
      Parser.err ~start_pos p
        (Diagnostics.message
           "Attributes and doc comments can only be used at the beginning of a \
            field declaration");
    if mut = Mutable then
      Parser.err ~start_pos p
        (Diagnostics.message
           "The `mutable` qualifier can only be used at the beginning of a \
            field declaration");
    None

(* record-decl ::=
 *  | { field-decl }
 *  | { field-decl, field-decl }
 *  | { field-decl, field-decl, field-decl, }
 *)
and parse_record_declaration ?current_type_name_path ?inline_types_context p =
  Parser.leave_breadcrumb p Grammar.RecordDecl;
  Parser.expect Lbrace p;
  let rows =
    parse_comma_delimited_region ~grammar:Grammar.RecordDecl ~closing:Rbrace
      ~f:
        (parse_field_declaration_region ?current_type_name_path
           ?inline_types_context)
      p
  in
  Parser.expect Rbrace p;
  Parser.eat_breadcrumb p;
  rows

(* constr-args ::=
 *  | (typexpr)
 *  | (typexpr, typexpr)
 *  | (typexpr, typexpr, typexpr,)
 *  | (record-decl)
 *
 * TODO: should we overparse inline-records in every position?
 * Give a good error message afterwards?
 *)
and parse_constr_decl_args p =
  let constr_args =
    match p.Parser.token with
    | Lparen -> (
      Parser.next p;
      (* TODO: this could use some cleanup/stratification *)
      match p.Parser.token with
      | Lbrace -> (
        Parser.next p;
        let start_pos = p.Parser.start_pos in
        match p.Parser.token with
        | DotDot | Dot ->
          let closed_flag =
            match p.token with
            | DotDot ->
              Parser.next p;
              Asttypes.Open
            | Dot ->
              Parser.next p;
              Asttypes.Closed
            | _ -> Asttypes.Closed
          in
          let fields =
            parse_comma_delimited_region
              ~grammar:Grammar.StringFieldDeclarations ~closing:Rbrace
              ~f:parse_string_field_declaration p
          in
          Parser.expect Rbrace p;
          let loc = mk_loc start_pos p.prev_end_pos in
          let typ = Ast_helper.Typ.object_ ~loc ~attrs:[] fields closed_flag in
          Parser.optional p Comma |> ignore;
          let more_args =
            parse_comma_delimited_region ~grammar:Grammar.TypExprList
              ~closing:Rparen ~f:parse_typ_expr_region p
          in
          Parser.expect Rparen p;
          Parsetree.Pcstr_tuple (typ :: more_args)
        | DotDotDot ->
          let dotdotdot_start = p.start_pos in
          let dotdotdot_end = p.end_pos in
          (* start of object type spreading, e.g. `User({...a, "u": int})` *)
          Parser.next p;
          let typ = parse_typ_expr p in
          let () =
            match p.token with
            | Rbrace ->
              (* {...x}, spread without extra fields *)
              Parser.next p
            | _ -> Parser.expect Comma p
          in
          let () =
            match p.token with
            | Lident _ ->
              Parser.err ~start_pos:dotdotdot_start ~end_pos:dotdotdot_end p
                (Diagnostics.message ErrorMessages.spread_in_record_declaration)
            | _ -> ()
          in
          let fields =
            Parsetree.Oinherit typ
            :: parse_comma_delimited_region
                 ~grammar:Grammar.StringFieldDeclarations ~closing:Rbrace
                 ~f:parse_string_field_declaration p
          in
          Parser.expect Rbrace p;
          let loc = mk_loc start_pos p.prev_end_pos in
          let typ =
            Ast_helper.Typ.object_ ~loc fields Asttypes.Closed
            |> parse_type_alias p
          in
          let typ = parse_arrow_type_rest ~es6_arrow:true ~start_pos typ p in
          Parser.optional p Comma |> ignore;
          let more_args =
            parse_comma_delimited_region ~grammar:Grammar.TypExprList
              ~closing:Rparen ~f:parse_typ_expr_region p
          in
          Parser.expect Rparen p;
          Parsetree.Pcstr_tuple (typ :: more_args)
        | _ -> (
          let attrs = parse_attributes p in
          match p.Parser.token with
          | String _ ->
            let closed_flag = Asttypes.Closed in
            let fields =
              match attrs with
              | [] ->
                parse_comma_delimited_region
                  ~grammar:Grammar.StringFieldDeclarations ~closing:Rbrace
                  ~f:parse_string_field_declaration p
              | attrs ->
                let first =
                  Parser.leave_breadcrumb p Grammar.StringFieldDeclarations;
                  let field =
                    match parse_string_field_declaration p with
                    | Some field -> field
                    | None -> assert false
                  in
                  (* parse comma after first *)
                  let () =
                    match p.Parser.token with
                    | Rbrace | Eof -> ()
                    | Comma -> Parser.next p
                    | _ -> Parser.expect Comma p
                  in
                  Parser.eat_breadcrumb p;
                  match field with
                  | Parsetree.Otag (label, _, ct) ->
                    Parsetree.Otag (label, attrs, ct)
                  | Oinherit ct -> Oinherit ct
                in
                first
                :: parse_comma_delimited_region
                     ~grammar:Grammar.StringFieldDeclarations ~closing:Rbrace
                     ~f:parse_string_field_declaration p
            in
            Parser.expect Rbrace p;
            let loc = mk_loc start_pos p.prev_end_pos in
            let typ =
              Ast_helper.Typ.object_ ~loc ~attrs:[] fields closed_flag
              |> parse_type_alias p
            in
            let typ = parse_arrow_type_rest ~es6_arrow:true ~start_pos typ p in
            Parser.optional p Comma |> ignore;
            let more_args =
              parse_comma_delimited_region ~grammar:Grammar.TypExprList
                ~closing:Rparen ~f:parse_typ_expr_region p
            in
            Parser.expect Rparen p;
            Parsetree.Pcstr_tuple (typ :: more_args)
          | _ ->
            let fields =
              match attrs with
              | [] ->
                parse_comma_delimited_region ~grammar:Grammar.FieldDeclarations
                  ~closing:Rbrace ~f:parse_field_declaration_region p
              | attrs ->
                let first =
                  let field = parse_field_declaration p in
                  {field with Parsetree.pld_attributes = attrs}
                in
                if p.token = Rbrace then [first]
                else (
                  Parser.expect Comma p;
                  first
                  :: parse_comma_delimited_region
                       ~grammar:Grammar.FieldDeclarations ~closing:Rbrace
                       ~f:parse_field_declaration_region p)
            in
            Parser.expect Rbrace p;
            Parser.optional p Comma |> ignore;
            Parser.expect Rparen p;
            Parsetree.Pcstr_record fields))
      | _ ->
        let args =
          parse_comma_delimited_region ~grammar:Grammar.TypExprList
            ~closing:Rparen ~f:parse_typ_expr_region p
        in
        Parser.expect Rparen p;
        Parsetree.Pcstr_tuple args)
    | _ -> Pcstr_tuple []
  in
  let res =
    match p.Parser.token with
    | Colon ->
      Parser.next p;
      Some (parse_typ_expr p)
    | _ -> None
  in
  (constr_args, res)

(* constr-decl ::=
 *  | constr-name
 *  | attrs constr-name
 *  | constr-name const-args
 *  | attrs constr-name const-args *)
and parse_type_constructor_declaration_with_bar p =
  match p.Parser.token with
  | Bar ->
    let start_pos = p.Parser.start_pos in
    Parser.next p;
    Some (parse_type_constructor_declaration ~start_pos p)
  | _ -> None

and parse_type_constructor_declaration ~start_pos p =
  Parser.leave_breadcrumb p Grammar.ConstructorDeclaration;
  let attrs = parse_attributes p in
  match p.Parser.token with
  | DotDotDot ->
    Parser.next p;
    let name = Location.mkloc "..." (mk_loc start_pos p.prev_end_pos) in
    let typ = parse_poly_type_expr p in
    let loc = mk_loc start_pos typ.ptyp_loc.loc_end in
    Ast_helper.Type.constructor ~loc ~attrs ~args:(Pcstr_tuple [typ]) name
  | Uident uident ->
    let uident_loc = mk_loc p.start_pos p.end_pos in
    Parser.next p;
    let args, res = parse_constr_decl_args p in
    Parser.eat_breadcrumb p;
    let loc = mk_loc start_pos p.prev_end_pos in
    Ast_helper.Type.constructor ~loc ~attrs ?res ~args
      (Location.mkloc uident uident_loc)
  | t ->
    Parser.err p (Diagnostics.uident t);
    Ast_helper.Type.constructor (Location.mknoloc "_")

(* [|] constr-decl  { | constr-decl }   *)
and parse_type_constructor_declarations ?first p =
  let first_constr_decl =
    match first with
    | None ->
      let start_pos = p.Parser.start_pos in
      ignore (Parser.optional p Token.Bar);
      parse_type_constructor_declaration ~start_pos p
    | Some first_constr_decl -> first_constr_decl
  in
  first_constr_decl
  :: parse_region ~grammar:Grammar.ConstructorDeclaration
       ~f:parse_type_constructor_declaration_with_bar p

(*
 * type-representation ::=
 *  ∣	 = [ | ] constr-decl  { | constr-decl }
 *  ∣	 = private [ | ] constr-decl  { | constr-decl }
 *  |  = |
 *  ∣	 = private |
 *  ∣	 = record-decl
 *  ∣	 = private record-decl
 *  |  = ..
 *)
and parse_type_representation ?current_type_name_path ?inline_types_context p =
  Parser.leave_breadcrumb p Grammar.TypeRepresentation;
  (* = consumed *)
  let private_flag =
    if Parser.optional p Token.Private then Asttypes.Private
    else Asttypes.Public
  in
  let kind =
    match p.Parser.token with
    | Bar | Uident _ ->
      Parsetree.Ptype_variant (parse_type_constructor_declarations p)
    | Lbrace ->
      Parsetree.Ptype_record
        (parse_record_declaration ?current_type_name_path ?inline_types_context
           p)
    | DotDot ->
      Parser.next p;
      Ptype_open
    | token ->
      Parser.err p (Diagnostics.unexpected token p.breadcrumbs);
      (* TODO: I have no idea if this is even remotely a good idea *)
      Parsetree.Ptype_variant []
  in
  Parser.eat_breadcrumb p;
  (private_flag, kind)

(* type-param	::=
 *  | variance 'lident
 *  | variance 'uident
 *  | variance _
 *
 * variance ::=
 *   | +
 *   | -
 *   | (* empty *)
 *)
and parse_type_param p =
  let variance =
    match p.Parser.token with
    | Plus ->
      Parser.next p;
      Asttypes.Covariant
    | Minus ->
      Parser.next p;
      Contravariant
    | _ -> Invariant
  in
  match p.Parser.token with
  | SingleQuote ->
    Parser.next p;
    let ident, loc =
      if p.Parser.token = Eof then (
        Parser.err ~start_pos:p.start_pos p
          (Diagnostics.unexpected p.Parser.token p.breadcrumbs);
        ("", mk_loc p.start_pos p.prev_end_pos))
      else parse_ident ~msg:ErrorMessages.type_param ~start_pos:p.start_pos p
    in
    Some (Ast_helper.Typ.var ~loc ident, variance)
  | Underscore ->
    let loc = mk_loc p.start_pos p.end_pos in
    Parser.next p;
    Some (Ast_helper.Typ.any ~loc (), variance)
  | (Uident _ | Lident _) as token ->
    Parser.err p
      (Diagnostics.message
         ("Type params start with a singlequote: '" ^ Token.to_string token));
    let ident, loc =
      parse_ident ~msg:ErrorMessages.type_param ~start_pos:p.start_pos p
    in
    Some (Ast_helper.Typ.var ~loc ident, variance)
  | _token -> None

(* type-params	::=
 *  | <type-param>
 *  ∣	<type-param, type-param>
 *  ∣	<type-param, type-param, type-param>
 *  ∣	<type-param, type-param, type-param,>
 *
 *  TODO: when we have pretty-printer show an error
 *  with the actual code corrected. *)
and parse_type_params ~parent p =
  let opening = p.Parser.token in
  match opening with
  | (LessThan | Lparen) when p.start_pos.pos_lnum == p.prev_end_pos.pos_lnum ->
    Scanner.set_diamond_mode p.scanner;
    let opening_start_pos = p.start_pos in
    Parser.leave_breadcrumb p Grammar.TypeParams;
    Parser.next p;
    let params =
      parse_comma_delimited_region ~grammar:Grammar.TypeParams
        ~closing:GreaterThan ~f:parse_type_param p
    in
    let () =
      match p.token with
      | Rparen when opening = Token.Lparen ->
        let msg =
          Doc.breakable_group ~force_break:true
            (Doc.concat
               [
                 Doc.text "Type parameters require angle brackets:";
                 Doc.indent
                   (Doc.concat
                      [
                        Doc.line;
                        Doc.concat
                          [
                            ResPrinter.print_longident parent.Location.txt;
                            ResPrinter.print_type_params params
                              CommentTable.empty;
                          ];
                      ]);
               ])
          |> Doc.to_string ~width:80
        in
        Parser.err ~start_pos:opening_start_pos p (Diagnostics.message msg);
        Parser.next p
      | _ -> Parser.expect GreaterThan p
    in
    Scanner.pop_mode p.scanner Diamond;
    Parser.eat_breadcrumb p;
    params
  | _ -> []

(* type-constraint	::=	constraint ' ident =  typexpr *)
and parse_type_constraint p =
  let start_pos = p.Parser.start_pos in
  match p.Parser.token with
  | Token.Constraint -> (
    Parser.next p;
    Parser.expect SingleQuote p;
    match p.Parser.token with
    | Lident ident | Uident ident ->
      let ident_loc = mk_loc start_pos p.end_pos in
      Parser.next p;
      Parser.expect Equal p;
      let typ = parse_typ_expr p in
      let loc = mk_loc start_pos p.prev_end_pos in
      Some (Ast_helper.Typ.var ~loc:ident_loc ident, typ, loc)
    | t ->
      Parser.err p (Diagnostics.lident t);
      let loc = mk_loc start_pos p.prev_end_pos in
      Some (Ast_helper.Typ.any (), parse_typ_expr p, loc))
  | _ -> None

(* type-constraints ::=
 *  | (* empty *)
 *  | type-constraint
 *  | type-constraint type-constraint
 *  | type-constraint type-constraint type-constraint (* 0 or more *)
 *)
and parse_type_constraints p =
  parse_region ~grammar:Grammar.TypeConstraint ~f:parse_type_constraint p

and parse_type_equation_or_constr_decl p =
  let uident_start_pos = p.Parser.start_pos in
  match p.Parser.token with
  | Uident uident -> (
    Parser.next p;
    match p.Parser.token with
    | Dot -> (
      Parser.next p;
      let type_constr =
        parse_value_path_tail p uident_start_pos (Longident.Lident uident)
      in
      let loc = mk_loc uident_start_pos p.prev_end_pos in
      let typ =
        parse_type_alias p
          (Ast_helper.Typ.constr ~loc type_constr
             (parse_type_constructor_args ~constr_name:type_constr p))
      in
      match p.token with
      | Equal ->
        Parser.next p;
        let priv, kind = parse_type_representation p in
        (Some typ, priv, kind)
      | EqualGreater ->
        Parser.next p;
        let return_type = parse_typ_expr ~alias:false p in
        let loc = mk_loc uident_start_pos p.prev_end_pos in
        let arrow_type =
          Ast_helper.Typ.arrow ~loc ~arity:(Some 1) Nolabel typ return_type
        in
        let typ = parse_type_alias p arrow_type in
        (Some typ, Asttypes.Public, Parsetree.Ptype_abstract)
      | _ -> (Some typ, Asttypes.Public, Parsetree.Ptype_abstract))
    | _ ->
      let uident_end_pos = p.prev_end_pos in
      let args, res = parse_constr_decl_args p in
      let first =
        Some
          (let uident_loc = mk_loc uident_start_pos uident_end_pos in
           Ast_helper.Type.constructor
             ~loc:(mk_loc uident_start_pos p.prev_end_pos)
             ?res ~args
             (Location.mkloc uident uident_loc))
      in
      ( None,
        Asttypes.Public,
        Parsetree.Ptype_variant (parse_type_constructor_declarations p ?first)
      ))
  | t ->
    Parser.err p (Diagnostics.uident t);
    (* TODO: is this a good idea? *)
    (None, Asttypes.Public, Parsetree.Ptype_abstract)

and parse_record_or_object_decl ?current_type_name_path ?inline_types_context p
    =
  let start_pos = p.Parser.start_pos in
  Parser.expect Lbrace p;
  match p.Parser.token with
  | DotDot | Dot ->
    let closed_flag =
      match p.token with
      | DotDot ->
        Parser.next p;
        Asttypes.Open
      | Dot ->
        Parser.next p;
        Asttypes.Closed
      | _ -> Asttypes.Closed
    in
    let fields =
      parse_comma_delimited_region ~grammar:Grammar.StringFieldDeclarations
        ~closing:Rbrace ~f:parse_string_field_declaration p
    in
    Parser.expect Rbrace p;
    let loc = mk_loc start_pos p.prev_end_pos in
    let typ =
      Ast_helper.Typ.object_ ~loc ~attrs:[] fields closed_flag
      |> parse_type_alias p
    in
    let typ = parse_arrow_type_rest ~es6_arrow:true ~start_pos typ p in
    (Some typ, Asttypes.Public, Parsetree.Ptype_abstract)
  | DotDotDot -> (
    let dotdotdot_start = p.start_pos in
    let dotdotdot_end = p.end_pos in
    (* start of object type spreading, e.g. `type u = {...a, "u": int}` *)
    Parser.next p;
    let typ = parse_typ_expr p in
    match p.token with
    | Rbrace ->
      (* {...x}, spread without extra fields *)
      Parser.next p;
      let loc = mk_loc start_pos p.prev_end_pos in
      let dot_field =
        Ast_helper.Type.field ~loc
          {txt = "..."; loc = mk_loc dotdotdot_start dotdotdot_end}
          typ
      in
      let kind = Parsetree.Ptype_record [dot_field] in
      (None, Public, kind)
    | _ ->
      Parser.expect Comma p;
      let loc = mk_loc start_pos p.prev_end_pos in
      let dot_field =
        Ast_helper.Type.field ~loc
          {txt = "..."; loc = mk_loc dotdotdot_start dotdotdot_end}
          typ
      in
      let found_object_field = ref false in
      let fields =
        parse_comma_delimited_region ~grammar:Grammar.RecordDecl ~closing:Rbrace
          ~f:
            (parse_field_declaration_region ?current_type_name_path
               ?inline_types_context ~found_object_field)
          p
      in
      Parser.expect Rbrace p;
      if !found_object_field then
        let fields =
          Ext_list.map fields (fun ld ->
              match ld.pld_name.txt with
              | "..." -> Parsetree.Oinherit ld.pld_type
              | _ -> Otag (ld.pld_name, ld.pld_attributes, ld.pld_type))
        in
        let dot_field = Parsetree.Oinherit typ in
        let typ_obj = Ast_helper.Typ.object_ (dot_field :: fields) Closed in
        let typ_obj = parse_type_alias p typ_obj in
        let typ_obj =
          parse_arrow_type_rest ~es6_arrow:true ~start_pos typ_obj p
        in
        (Some typ_obj, Public, Ptype_abstract)
      else
        let kind = Parsetree.Ptype_record (dot_field :: fields) in
        (None, Public, kind))
  | _ -> (
    let attrs = parse_attributes p in
    match p.Parser.token with
    | String _ ->
      let closed_flag = Asttypes.Closed in
      let fields =
        match attrs with
        | [] ->
          parse_comma_delimited_region ~grammar:Grammar.StringFieldDeclarations
            ~closing:Rbrace ~f:parse_string_field_declaration p
        | attrs ->
          let first =
            Parser.leave_breadcrumb p Grammar.StringFieldDeclarations;
            let field =
              match parse_string_field_declaration p with
              | Some field -> field
              | None -> assert false
            in
            (* parse comma after first *)
            let () =
              match p.Parser.token with
              | Rbrace | Eof -> ()
              | Comma -> Parser.next p
              | _ -> Parser.expect Comma p
            in
            Parser.eat_breadcrumb p;
            match field with
            | Parsetree.Otag (label, _, ct) -> Parsetree.Otag (label, attrs, ct)
            | Oinherit ct -> Oinherit ct
          in
          first
          :: parse_comma_delimited_region
               ~grammar:Grammar.StringFieldDeclarations ~closing:Rbrace
               ~f:parse_string_field_declaration p
      in
      Parser.expect Rbrace p;
      let loc = mk_loc start_pos p.prev_end_pos in
      let typ =
        Ast_helper.Typ.object_ ~loc ~attrs:[] fields closed_flag
        |> parse_type_alias p
      in
      let typ = parse_arrow_type_rest ~es6_arrow:true ~start_pos typ p in
      (Some typ, Asttypes.Public, Parsetree.Ptype_abstract)
    | _ ->
      Parser.leave_breadcrumb p Grammar.RecordDecl;
      let fields =
        (* XXX *)
        match attrs with
        | [] ->
          parse_comma_delimited_region ~grammar:Grammar.FieldDeclarations
            ~closing:Rbrace
            ~f:
              (parse_field_declaration_region ?current_type_name_path
                 ?inline_types_context)
            p
        | attr :: _ as attrs ->
          let first =
            let field = parse_field_declaration p in
            Parser.optional p Comma |> ignore;
            {
              field with
              Parsetree.pld_attributes = attrs;
              pld_loc =
                {
                  field.Parsetree.pld_loc with
                  loc_start = (attr |> fst).loc.loc_start;
                };
            }
          in
          first
          :: parse_comma_delimited_region ~grammar:Grammar.FieldDeclarations
               ~closing:Rbrace
               ~f:
                 (parse_field_declaration_region ?current_type_name_path
                    ?inline_types_context)
               p
      in
      Parser.expect Rbrace p;
      Parser.eat_breadcrumb p;
      (None, Asttypes.Public, Parsetree.Ptype_record fields))

and parse_private_eq_or_repr p =
  Parser.expect Private p;
  match p.Parser.token with
  | Lbrace ->
    let manifest, _, kind = parse_record_or_object_decl p in
    (manifest, Asttypes.Private, kind)
  | Uident _ ->
    let manifest, _, kind = parse_type_equation_or_constr_decl p in
    (manifest, Asttypes.Private, kind)
  | Bar | DotDot ->
    let _, kind = parse_type_representation p in
    (None, Asttypes.Private, kind)
  | t when Grammar.is_typ_expr_start t ->
    (Some (parse_typ_expr p), Asttypes.Private, Parsetree.Ptype_abstract)
  | _ ->
    let _, kind = parse_type_representation p in
    (None, Asttypes.Private, kind)

(*
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
*)
and parse_polymorphic_variant_type ~attrs p =
  let start_pos = p.Parser.start_pos in
  Parser.expect Lbracket p;
  match p.token with
  | GreaterThan ->
    Parser.next p;
    let row_fields =
      match p.token with
      | Rbracket -> []
      | Bar -> parse_tag_specs p
      | _ ->
        let row_field = parse_tag_spec p in
        row_field :: parse_tag_specs p
    in
    let variant =
      let loc = mk_loc start_pos p.prev_end_pos in
      Ast_helper.Typ.variant ~attrs ~loc row_fields Open None
    in
    Parser.expect Rbracket p;
    variant
  | LessThan ->
    Parser.next p;
    Parser.optional p Bar |> ignore;
    let row_field = parse_tag_spec_full p in
    let row_fields = parse_tag_spec_fulls p in
    let tag_names = parse_tag_names p in
    let variant =
      let loc = mk_loc start_pos p.prev_end_pos in
      Ast_helper.Typ.variant ~attrs ~loc (row_field :: row_fields) Closed
        (Some tag_names)
    in
    Parser.expect Rbracket p;
    variant
  | _ ->
    let row_fields1 = parse_tag_spec_first p in
    let row_fields2 = parse_tag_specs p in
    let variant =
      let loc = mk_loc start_pos p.prev_end_pos in
      Ast_helper.Typ.variant ~attrs ~loc (row_fields1 @ row_fields2) Closed None
    in
    Parser.expect Rbracket p;
    variant

and parse_tag_name p =
  match p.Parser.token with
  | Hash ->
    let ident, _loc = parse_hash_ident ~start_pos:p.start_pos p in
    Some ident
  | _ -> None

and parse_tag_names p =
  if p.Parser.token == GreaterThan then (
    Parser.next p;
    parse_region p ~grammar:Grammar.TagNames ~f:parse_tag_name)
  else []

and parse_tag_spec_fulls p =
  match p.Parser.token with
  | Rbracket -> []
  | GreaterThan -> []
  | Bar ->
    Parser.next p;
    let row_field = parse_tag_spec_full p in
    row_field :: parse_tag_spec_fulls p
  | _ -> []

and parse_tag_spec_full p =
  let attrs = parse_attributes p in
  match p.Parser.token with
  | Hash -> parse_polymorphic_variant_type_spec_hash ~attrs ~full:true p
  | _ ->
    let typ = parse_typ_expr ~attrs p in
    Parsetree.Rinherit typ

and parse_tag_specs p =
  match p.Parser.token with
  | Bar ->
    Parser.next p;
    let row_field = parse_tag_spec p in
    row_field :: parse_tag_specs p
  | _ -> []

and parse_tag_spec p =
  let attrs = parse_attributes p in
  match p.Parser.token with
  | Hash -> parse_polymorphic_variant_type_spec_hash ~attrs ~full:false p
  | _ ->
    let typ = parse_typ_expr ~attrs p in
    Parsetree.Rinherit typ

and parse_tag_spec_first p =
  let attrs = parse_attributes p in
  match p.Parser.token with
  | Bar ->
    Parser.next p;
    [parse_tag_spec p]
  | Hash -> [parse_polymorphic_variant_type_spec_hash ~attrs ~full:false p]
  | _ -> (
    let typ = parse_typ_expr ~attrs p in
    match p.token with
    | Rbracket ->
      (* example: [ListStyleType.t] *)
      [Parsetree.Rinherit typ]
    | _ ->
      Parser.expect Bar p;
      [Parsetree.Rinherit typ; parse_tag_spec p])

and parse_polymorphic_variant_type_spec_hash ~attrs ~full p :
    Parsetree.row_field =
  let start_pos = p.Parser.start_pos in
  let ident, loc = parse_hash_ident ~start_pos p in
  let rec loop p =
    match p.Parser.token with
    | Band when full ->
      Parser.next p;
      let row_field = parse_polymorphic_variant_type_args p in
      row_field :: loop p
    | _ -> []
  in
  let first_tuple, tag_contains_a_constant_empty_constructor =
    match p.Parser.token with
    | Band when full ->
      Parser.next p;
      ([parse_polymorphic_variant_type_args p], true)
    | Lparen -> ([parse_polymorphic_variant_type_args p], false)
    | _ -> ([], true)
  in
  let tuples = first_tuple @ loop p in
  Parsetree.Rtag
    ( Location.mkloc ident loc,
      attrs,
      tag_contains_a_constant_empty_constructor,
      tuples )

and parse_polymorphic_variant_type_args p =
  let start_pos = p.Parser.start_pos in
  Parser.expect Lparen p;
  let args =
    parse_comma_delimited_region ~grammar:Grammar.TypExprList ~closing:Rparen
      ~f:parse_typ_expr_region p
  in
  Parser.expect Rparen p;
  let attrs = [] in
  let loc = mk_loc start_pos p.prev_end_pos in
  match args with
  | [({ptyp_desc = Ptyp_tuple _} as typ)] as types ->
    if p.mode = ParseForTypeChecker then typ
    else Ast_helper.Typ.tuple ~loc ~attrs types
  | [typ] -> typ
  | types -> Ast_helper.Typ.tuple ~loc ~attrs types

and parse_type_equation_and_representation ?current_type_name_path
    ?inline_types_context p =
  match p.Parser.token with
  | (Equal | Bar) as token -> (
    if token = Bar then Parser.expect Equal p;
    Parser.next p;
    match p.Parser.token with
    | Uident _ -> parse_type_equation_or_constr_decl p
    | Lbrace ->
      parse_record_or_object_decl ?current_type_name_path ?inline_types_context
        p
    | Private -> parse_private_eq_or_repr p
    | Bar | DotDot ->
      let priv, kind = parse_type_representation p in
      (None, priv, kind)
    | _ -> (
      let manifest = Some (parse_typ_expr p) in
      match p.Parser.token with
      | Equal ->
        Parser.next p;
        let priv, kind =
          parse_type_representation ?current_type_name_path
            ?inline_types_context p
        in
        (manifest, priv, kind)
      | _ -> (manifest, Public, Parsetree.Ptype_abstract)))
  | _ -> (None, Public, Parsetree.Ptype_abstract)

(* type-definition	::=	type [rec] typedef  { and typedef }
 * typedef	::=	typeconstr-name [type-params] type-information
 * type-information	::=	[type-equation]  [type-representation]  { type-constraint }
 * type-equation	::=	= typexpr *)
and parse_type_def ~attrs ~start_pos p =
  Parser.leave_breadcrumb p Grammar.TypeDef;
  (* let attrs = match attrs with | Some attrs -> attrs | None -> parseAttributes p in *)
  Parser.leave_breadcrumb p Grammar.TypeConstrName;
  let name, loc = parse_lident p in
  let type_constr_name = Location.mkloc name loc in
  Parser.eat_breadcrumb p;
  let params =
    let constr_name = Location.mkloc (Longident.Lident name) loc in
    parse_type_params ~parent:constr_name p
  in
  let type_def =
    let manifest, priv, kind = parse_type_equation_and_representation p in
    let cstrs = parse_type_constraints p in
    let loc = mk_loc start_pos p.prev_end_pos in
    Ast_helper.Type.mk ~loc ~attrs ~priv ~kind ~params ~cstrs ?manifest
      type_constr_name
  in
  Parser.eat_breadcrumb p;
  type_def

and parse_type_extension ~params ~attrs ~name p =
  Parser.expect PlusEqual p;
  let priv =
    if Parser.optional p Token.Private then Asttypes.Private
    else Asttypes.Public
  in
  let constr_start = p.Parser.start_pos in
  Parser.optional p Bar |> ignore;
  let first =
    let attrs, name, kind =
      match p.Parser.token with
      | Bar ->
        Parser.next p;
        parse_constr_def ~parse_attrs:true p
      | _ -> parse_constr_def ~parse_attrs:true p
    in
    let loc = mk_loc constr_start p.prev_end_pos in
    Ast_helper.Te.constructor ~loc ~attrs name kind
  in
  let rec loop p cs =
    match p.Parser.token with
    | Bar ->
      let start_pos = p.Parser.start_pos in
      Parser.next p;
      let attrs, name, kind = parse_constr_def ~parse_attrs:true p in
      let ext_constr =
        Ast_helper.Te.constructor ~attrs
          ~loc:(mk_loc start_pos p.prev_end_pos)
          name kind
      in
      loop p (ext_constr :: cs)
    | _ -> List.rev cs
  in
  let constructors = loop p [first] in
  Ast_helper.Te.mk ~attrs ~params ~priv name constructors

and parse_type_definitions ~current_type_name_path ~inline_types_context ~attrs
    ~name ~params ~start_pos p =
  let type_def =
    let manifest, priv, kind =
      parse_type_equation_and_representation ~current_type_name_path
        ~inline_types_context p
    in
    let cstrs = parse_type_constraints p in
    let loc = mk_loc start_pos p.prev_end_pos in
    Ast_helper.Type.mk ~loc ~attrs ~priv ~kind ~params ~cstrs ?manifest
      {name with txt = lident_of_path name.Location.txt}
  in
  let rec loop p defs =
    let start_pos = p.Parser.start_pos in
    let attrs = parse_attributes_and_binding p in
    match p.Parser.token with
    | And ->
      Parser.next p;
      let type_def = parse_type_def ~attrs ~start_pos p in
      loop p (type_def :: defs)
    | _ -> List.rev defs
  in
  loop p [type_def]

(* TODO: decide if we really want type extensions (eg. type x += Blue)
 * It adds quite a bit of complexity that can be avoided,
 * implemented for now. Needed to get a feel for the complexities of
 * this territory of the grammar *)
and parse_type_definition_or_extension ~attrs p =
  let start_pos = p.Parser.start_pos in
  Parser.expect Token.Typ p;
  let rec_flag =
    match p.token with
    | Rec ->
      Parser.next p;
      Asttypes.Recursive
    | Lident "nonrec" ->
      Parser.next p;
      Asttypes.Nonrecursive
    | _ -> Asttypes.Nonrecursive
  in
  let name = parse_value_path p in
  let params = parse_type_params ~parent:name p in
  match p.Parser.token with
  | PlusEqual -> TypeExt (parse_type_extension ~params ~attrs ~name p)
  | _ ->
    (* shape of type name should be Lident, i.e. `t` is accepted. `User.t` not *)
    let () =
      match name.Location.txt with
      | Lident _ -> ()
      | longident ->
        Parser.err ~start_pos:name.loc.loc_start ~end_pos:name.loc.loc_end p
          (longident |> ErrorMessages.type_declaration_name_longident
         |> Diagnostics.message)
    in
    let current_type_name_path = Longident.flatten name.txt in
    let inline_types_context = {found_inline_types = []; params} in
    let type_defs =
      parse_type_definitions ~inline_types_context ~current_type_name_path
        ~attrs ~name ~params ~start_pos p
    in
    let rec_flag =
      if List.length inline_types_context.found_inline_types > 0 then
        Asttypes.Recursive
      else rec_flag
    in
    let inline_types =
      inline_types_context.found_inline_types
      |> List.map (fun (inline_type_name, loc, kind) ->
             Ast_helper.Type.mk ~params
               ~attrs:[(Location.mknoloc "res.inlineRecordDefinition", PStr [])]
               ~loc ~kind
               {name with txt = inline_type_name})
    in
    TypeDef {rec_flag; types = inline_types @ type_defs}

(* external value-name : typexp = external-declaration *)
and parse_external_def ~attrs ~start_pos p =
  let in_external = !InExternal.status in
  InExternal.status := true;
  Parser.leave_breadcrumb p Grammar.External;
  Parser.expect Token.External p;
  let name, loc = parse_lident p in
  let name = Location.mkloc name loc in
  Parser.expect ~grammar:Grammar.TypeExpression Colon p;
  let typ_expr = parse_typ_expr p in
  let equal_start = p.start_pos in
  let equal_end = p.end_pos in
  Parser.expect Equal p;
  let prim =
    match p.token with
    | String s ->
      Parser.next p;
      [s]
    | _ ->
      Parser.err ~start_pos:equal_start ~end_pos:equal_end p
        (Diagnostics.message
           ("An external requires the name of the JS value you're referring \
             to, like \"" ^ name.txt ^ "\"."));
      []
  in
  let loc = mk_loc start_pos p.prev_end_pos in
  let vb = Ast_helper.Val.mk ~loc ~attrs ~prim name typ_expr in
  Parser.eat_breadcrumb p;
  InExternal.status := in_external;
  vb

(* constr-def ::=
 *  | constr-decl
 *  | constr-name = constr
 *
 *  constr-decl ::= constr-name constr-args
 *  constr-name ::= uident
 *  constr      ::= path-uident *)
and parse_constr_def ~parse_attrs p =
  let attrs = if parse_attrs then parse_attributes p else [] in
  let name =
    match p.Parser.token with
    | Uident name ->
      let loc = mk_loc p.start_pos p.end_pos in
      Parser.next p;
      Location.mkloc name loc
    | t ->
      Parser.err p (Diagnostics.uident t);
      Location.mknoloc "_"
  in
  let kind =
    match p.Parser.token with
    | Lparen ->
      let args, res = parse_constr_decl_args p in
      Parsetree.Pext_decl (args, res)
    | Equal ->
      Parser.next p;
      let longident = parse_module_long_ident ~lowercase:false p in
      Parsetree.Pext_rebind longident
    | Colon ->
      Parser.next p;
      let typ = parse_typ_expr p in
      Parsetree.Pext_decl (Pcstr_tuple [], Some typ)
    | _ -> Parsetree.Pext_decl (Pcstr_tuple [], None)
  in
  (attrs, name, kind)

(*
 * exception-definition	::=
 *  | exception constr-decl
 *  ∣	exception constr-name = constr
 *
 *  constr-name ::= uident
 *  constr ::= long_uident *)
and parse_exception_def ~attrs p =
  let start_pos = p.Parser.start_pos in
  Parser.expect Token.Exception p;
  let _, name, kind = parse_constr_def ~parse_attrs:false p in
  let loc = mk_loc start_pos p.prev_end_pos in
  Ast_helper.Te.constructor ~loc ~attrs name kind

and parse_newline_or_semicolon_structure p =
  match p.Parser.token with
  | Semicolon -> Parser.next p
  | token when Grammar.is_structure_item_start token ->
    if p.prev_end_pos.pos_lnum < p.start_pos.pos_lnum then ()
    else
      Parser.err ~start_pos:p.prev_end_pos ~end_pos:p.end_pos p
        (Diagnostics.message
           "consecutive statements on a line must be separated by ';' or a \
            newline")
  | _ -> ()

and parse_structure_item_region p =
  let start_pos = p.Parser.start_pos in
  let attrs = parse_attributes p in
  match p.Parser.token with
  | Open ->
    let open_description = parse_open_description ~attrs p in
    parse_newline_or_semicolon_structure p;
    let loc = mk_loc start_pos p.prev_end_pos in
    Some (Ast_helper.Str.open_ ~loc open_description)
  | Let ->
    let rec_flag, let_bindings = parse_let_bindings ~attrs ~start_pos p in
    parse_newline_or_semicolon_structure p;
    let loc = mk_loc start_pos p.prev_end_pos in
    Some (Ast_helper.Str.value ~loc rec_flag let_bindings)
  | Typ -> (
    Parser.begin_region p;
    match parse_type_definition_or_extension ~attrs p with
    | TypeDef {rec_flag; types} ->
      parse_newline_or_semicolon_structure p;
      let loc = mk_loc start_pos p.prev_end_pos in
      Parser.end_region p;
      Some (Ast_helper.Str.type_ ~loc rec_flag types)
    | TypeExt ext ->
      parse_newline_or_semicolon_structure p;
      let loc = mk_loc start_pos p.prev_end_pos in
      Parser.end_region p;
      Some (Ast_helper.Str.type_extension ~loc ext))
  | External ->
    let external_def = parse_external_def ~attrs ~start_pos p in
    parse_newline_or_semicolon_structure p;
    let loc = mk_loc start_pos p.prev_end_pos in
    Some (Ast_helper.Str.primitive ~loc external_def)
  | Exception ->
    let exception_def = parse_exception_def ~attrs p in
    parse_newline_or_semicolon_structure p;
    let loc = mk_loc start_pos p.prev_end_pos in
    Some (Ast_helper.Str.exception_ ~loc exception_def)
  | Include ->
    let include_statement = parse_include_statement ~attrs p in
    parse_newline_or_semicolon_structure p;
    let loc = mk_loc start_pos p.prev_end_pos in
    Some (Ast_helper.Str.include_ ~loc include_statement)
  | Module ->
    Parser.begin_region p;
    let structure_item =
      parse_module_or_module_type_impl_or_pack_expr ~attrs p
    in
    parse_newline_or_semicolon_structure p;
    let loc = mk_loc start_pos p.prev_end_pos in
    Parser.end_region p;
    Some {structure_item with pstr_loc = loc}
  | ModuleComment (loc, s) ->
    Parser.next p;
    Some
      (Ast_helper.Str.attribute ~loc
         ( {txt = "res.doc"; loc},
           PStr
             [
               Ast_helper.Str.eval ~loc
                 (Ast_helper.Exp.constant ~loc (Pconst_string (s, None)));
             ] ))
  | AtAt ->
    let attr = parse_standalone_attribute p in
    parse_newline_or_semicolon_structure p;
    let loc = mk_loc start_pos p.prev_end_pos in
    Some (Ast_helper.Str.attribute ~loc attr)
  | PercentPercent ->
    let extension = parse_extension ~module_language:true p in
    parse_newline_or_semicolon_structure p;
    let loc = mk_loc start_pos p.prev_end_pos in
    Some (Ast_helper.Str.extension ~attrs ~loc extension)
  | token when Grammar.is_expr_start token ->
    let prev_end_pos = p.Parser.end_pos in
    let exp = parse_expr p in
    parse_newline_or_semicolon_structure p;
    let loc = mk_loc start_pos p.prev_end_pos in
    Parser.check_progress ~prev_end_pos
      ~result:(Ast_helper.Str.eval ~loc ~attrs exp)
      p
  | _ -> (
    match attrs with
    | (({Asttypes.loc = attr_loc}, _) as attr) :: _ ->
      Parser.err ~start_pos:attr_loc.loc_start ~end_pos:attr_loc.loc_end p
        (Diagnostics.message (ErrorMessages.attribute_without_node attr));
      let expr = parse_expr p in
      Some
        (Ast_helper.Str.eval
           ~loc:(mk_loc p.start_pos p.prev_end_pos)
           ~attrs expr)
    | _ -> None)
[@@progress Parser.next, Parser.expect]

(* include-statement ::= include module-expr *)
and parse_include_statement ~attrs p =
  let start_pos = p.Parser.start_pos in
  Parser.expect Token.Include p;
  let mod_expr = parse_module_expr p in
  let loc = mk_loc start_pos p.prev_end_pos in
  Ast_helper.Incl.mk ~loc ~attrs mod_expr

and parse_atomic_module_expr p =
  let start_pos = p.Parser.start_pos in
  match p.Parser.token with
  | Uident _ident ->
    let longident = parse_module_long_ident ~lowercase:false p in
    Ast_helper.Mod.ident ~loc:longident.loc longident
  | Lbrace ->
    Parser.next p;
    let structure =
      Ast_helper.Mod.structure
        (parse_delimited_region ~grammar:Grammar.Structure ~closing:Rbrace
           ~f:parse_structure_item_region p)
    in
    Parser.expect Rbrace p;
    let end_pos = p.prev_end_pos in
    {structure with pmod_loc = mk_loc start_pos end_pos}
  | Lparen ->
    Parser.next p;
    let mod_expr =
      match p.token with
      | Rparen ->
        Ast_helper.Mod.structure ~loc:(mk_loc start_pos p.prev_end_pos) []
      | _ -> parse_constrained_mod_expr p
    in
    Parser.expect Rparen p;
    mod_expr
  | Lident "unpack" -> (
    (* TODO: should this be made a keyword?? *)
    Parser.next p;
    Parser.expect Lparen p;
    let expr = parse_expr p in
    match p.Parser.token with
    | Colon ->
      let colon_start = p.Parser.start_pos in
      Parser.next p;
      let attrs = parse_attributes p in
      let package_type = parse_package_type ~start_pos:colon_start ~attrs p in
      Parser.expect Rparen p;
      let loc = mk_loc start_pos p.prev_end_pos in
      let constraint_expr = Ast_helper.Exp.constraint_ ~loc expr package_type in
      Ast_helper.Mod.unpack ~loc constraint_expr
    | _ ->
      Parser.expect Rparen p;
      let loc = mk_loc start_pos p.prev_end_pos in
      Ast_helper.Mod.unpack ~loc expr)
  | Percent ->
    let extension = parse_extension p in
    let loc = mk_loc start_pos p.prev_end_pos in
    Ast_helper.Mod.extension ~loc extension
  | token ->
    Parser.err p (Diagnostics.unexpected token p.breadcrumbs);
    Recover.default_module_expr ()

and parse_primary_mod_expr p =
  let start_pos = p.Parser.start_pos in
  let mod_expr = parse_atomic_module_expr p in
  let rec loop p mod_expr =
    match p.Parser.token with
    | Lparen when p.prev_end_pos.pos_lnum == p.start_pos.pos_lnum ->
      loop p (parse_module_application p mod_expr)
    | _ -> mod_expr
  in
  let mod_expr = loop p mod_expr in
  {mod_expr with pmod_loc = mk_loc start_pos p.prev_end_pos}

(*
 * functor-arg ::=
 *  | uident : modtype
 *  | _ : modtype
 *  | modtype           --> "punning" for _ : modtype
 *  | attributes functor-arg
 *)
and parse_functor_arg p =
  let start_pos = p.Parser.start_pos in
  let attrs = parse_attributes p in
  match p.Parser.token with
  | Uident ident -> (
    Parser.next p;
    let uident_end_pos = p.prev_end_pos in
    match p.Parser.token with
    | Colon ->
      Parser.next p;
      let module_type = parse_module_type p in
      let loc = mk_loc start_pos uident_end_pos in
      let arg_name = Location.mkloc ident loc in
      Some (attrs, arg_name, Some module_type, start_pos)
    | Dot ->
      Parser.next p;
      let module_type =
        let module_long_ident =
          parse_module_long_ident_tail ~lowercase:false p start_pos
            (Longident.Lident ident)
        in
        Ast_helper.Mty.ident ~loc:module_long_ident.loc module_long_ident
      in
      let arg_name = Location.mknoloc "_" in
      Some (attrs, arg_name, Some module_type, start_pos)
    | _ ->
      let loc = mk_loc start_pos uident_end_pos in
      let mod_ident = Location.mkloc (Longident.Lident ident) loc in
      let module_type = Ast_helper.Mty.ident ~loc mod_ident in
      let arg_name = Location.mknoloc "_" in
      Some (attrs, arg_name, Some module_type, start_pos))
  | Underscore ->
    Parser.next p;
    let arg_name = Location.mkloc "_" (mk_loc start_pos p.prev_end_pos) in
    Parser.expect Colon p;
    let module_type = parse_module_type p in
    Some (attrs, arg_name, Some module_type, start_pos)
  | Lparen ->
    Parser.next p;
    Parser.expect Rparen p;
    let arg_name = Location.mkloc "*" (mk_loc start_pos p.prev_end_pos) in
    Some (attrs, arg_name, None, start_pos)
  | _ -> None

and parse_functor_args p =
  let start_pos = p.Parser.start_pos in
  Parser.expect Lparen p;
  let args =
    parse_comma_delimited_region ~grammar:Grammar.FunctorArgs ~closing:Rparen
      ~f:parse_functor_arg p
  in
  Parser.expect Rparen p;
  match args with
  | [] ->
    [
      ([], Location.mkloc "*" (mk_loc start_pos p.prev_end_pos), None, start_pos);
    ]
  | args -> args

and parse_functor_module_expr p =
  let start_pos = p.Parser.start_pos in
  let args = parse_functor_args p in
  let return_type =
    match p.Parser.token with
    | Colon ->
      Parser.next p;
      Some (parse_module_type ~es6_arrow:false p)
    | _ -> None
  in
  Parser.expect EqualGreater p;
  let rhs_module_expr =
    let mod_expr = parse_module_expr p in
    match return_type with
    | Some mod_type ->
      Ast_helper.Mod.constraint_
        ~loc:
          (mk_loc mod_expr.pmod_loc.loc_start
             mod_type.Parsetree.pmty_loc.loc_end)
        mod_expr mod_type
    | None -> mod_expr
  in
  let end_pos = p.prev_end_pos in
  let mod_expr =
    List.fold_right
      (fun (attrs, name, module_type, start_pos) acc ->
        Ast_helper.Mod.functor_ ~loc:(mk_loc start_pos end_pos) ~attrs name
          module_type acc)
      args rhs_module_expr
  in
  {mod_expr with pmod_loc = mk_loc start_pos end_pos}

(* module-expr	::=
 *  | module-path
 *  ∣	{ structure-items }
 *  ∣	functorArgs =>  module-expr
 *  ∣	module-expr(module-expr)
 *  ∣	( module-expr )
 *  ∣	( module-expr : module-type )
 *  | extension
 *  | attributes module-expr *)
and parse_module_expr p =
  let has_await, loc_await =
    let start_pos = p.start_pos in
    match p.Parser.token with
    | Await ->
      Parser.expect Await p;
      let end_pos = p.end_pos in
      (true, mk_loc start_pos end_pos)
    | _ -> (false, mk_loc start_pos start_pos)
  in
  let attrs = parse_attributes p in
  let attrs = if has_await then make_await_attr loc_await :: attrs else attrs in
  let mod_expr =
    if is_es6_arrow_functor p then parse_functor_module_expr p
    else parse_primary_mod_expr p
  in
  {
    mod_expr with
    pmod_attributes = List.concat [mod_expr.pmod_attributes; attrs];
  }

and parse_constrained_mod_expr p =
  let mod_expr = parse_module_expr p in
  match p.Parser.token with
  | Colon ->
    Parser.next p;
    let mod_type = parse_module_type p in
    let loc = mk_loc mod_expr.pmod_loc.loc_start mod_type.pmty_loc.loc_end in
    Ast_helper.Mod.constraint_ ~loc mod_expr mod_type
  | _ -> mod_expr

and parse_constrained_mod_expr_region p =
  if Grammar.is_mod_expr_start p.Parser.token then
    Some (parse_constrained_mod_expr p)
  else None

and parse_module_application p mod_expr =
  let start_pos = p.Parser.start_pos in
  Parser.expect Lparen p;
  let args =
    parse_comma_delimited_region ~grammar:Grammar.ModExprList ~closing:Rparen
      ~f:parse_constrained_mod_expr_region p
  in
  Parser.expect Rparen p;
  let args =
    match args with
    | [] ->
      let loc = mk_loc start_pos p.prev_end_pos in
      [Ast_helper.Mod.structure ~loc []]
    | args -> args
  in
  List.fold_left
    (fun mod_expr arg ->
      Ast_helper.Mod.apply
        ~loc:
          (mk_loc mod_expr.Parsetree.pmod_loc.loc_start
             arg.Parsetree.pmod_loc.loc_end)
        mod_expr arg)
    mod_expr args

and parse_module_or_module_type_impl_or_pack_expr ~attrs p =
  let start_pos = p.Parser.start_pos in
  Parser.expect Module p;
  match p.Parser.token with
  | Typ -> parse_module_type_impl ~attrs start_pos p
  | Lparen ->
    let expr = parse_first_class_module_expr ~start_pos p in
    let a = parse_primary_expr ~operand:expr p in
    let expr = parse_binary_expr ~a p 1 in
    let expr = parse_ternary_expr expr p in
    Ast_helper.Str.eval ~attrs expr
  | _ -> parse_maybe_rec_module_binding ~attrs ~start_pos p

and parse_module_type_impl ~attrs start_pos p =
  Parser.expect Typ p;
  let name_start = p.Parser.start_pos in
  let name =
    match p.Parser.token with
    | Lident ident ->
      Parser.next p;
      let loc = mk_loc name_start p.prev_end_pos in
      Location.mkloc ident loc
    | Uident ident ->
      Parser.next p;
      let loc = mk_loc name_start p.prev_end_pos in
      Location.mkloc ident loc
    | t ->
      Parser.err p (Diagnostics.uident t);
      Location.mknoloc "_"
  in
  Parser.expect Equal p;
  let module_type = parse_module_type p in
  let module_type_declaration =
    Ast_helper.Mtd.mk ~attrs
      ~loc:(mk_loc name_start p.prev_end_pos)
      ~typ:module_type name
  in
  let loc = mk_loc start_pos p.prev_end_pos in
  Ast_helper.Str.modtype ~loc module_type_declaration

(* definition	::=
   ∣	 module rec module-name :  module-type =  module-expr   { and module-name
   :  module-type =  module-expr } *)
and parse_maybe_rec_module_binding ~attrs ~start_pos p =
  match p.Parser.token with
  | Token.Rec ->
    Parser.next p;
    Ast_helper.Str.rec_module (parse_module_bindings ~start_pos ~attrs p)
  | _ ->
    Ast_helper.Str.module_
      (parse_module_binding ~attrs ~start_pos:p.Parser.start_pos p)

and parse_module_binding ~attrs ~start_pos p =
  let name =
    match p.Parser.token with
    | Uident ident ->
      let start_pos = p.Parser.start_pos in
      Parser.next p;
      let loc = mk_loc start_pos p.prev_end_pos in
      Location.mkloc ident loc
    | t ->
      Parser.err p (Diagnostics.uident t);
      Location.mknoloc "_"
  in
  let body = parse_module_binding_body p in
  let loc = mk_loc start_pos p.prev_end_pos in
  Ast_helper.Mb.mk ~attrs ~loc name body

and parse_module_binding_body p =
  (* TODO: make required with good error message when rec module binding *)
  let return_mod_type =
    match p.Parser.token with
    | Colon ->
      Parser.next p;
      Some (parse_module_type p)
    | _ -> None
  in
  Parser.expect Equal p;
  let mod_expr = parse_module_expr p in
  match return_mod_type with
  | Some mod_type ->
    Ast_helper.Mod.constraint_
      ~loc:(mk_loc mod_type.pmty_loc.loc_start mod_expr.pmod_loc.loc_end)
      mod_expr mod_type
  | None -> mod_expr

(* module-name :  module-type =  module-expr
 * { and module-name :  module-type =  module-expr } *)
and parse_module_bindings ~attrs ~start_pos p =
  let rec loop p acc =
    let start_pos = p.Parser.start_pos in
    let doc_attr : Parsetree.attributes =
      match p.Parser.token with
      | DocComment (loc, s) ->
        Parser.next p;
        [doc_comment_to_attribute loc s]
      | _ -> []
    in
    let attrs = doc_attr @ parse_attributes_and_binding p in
    match p.Parser.token with
    | And ->
      Parser.next p;
      ignore (Parser.optional p Module);
      (* over-parse for fault-tolerance *)
      let mod_binding = parse_module_binding ~attrs ~start_pos p in
      loop p (mod_binding :: acc)
    | _ -> List.rev acc
  in
  let first = parse_module_binding ~attrs ~start_pos p in
  loop p [first]

and parse_atomic_module_type p =
  let start_pos = p.Parser.start_pos in
  let module_type =
    match p.Parser.token with
    | Uident _ | Lident _ ->
      (* Ocaml allows module types to end with lowercase: module Foo : bar = { ... }
       * lets go with uppercase terminal for now *)
      let module_long_ident = parse_module_long_ident ~lowercase:true p in
      Ast_helper.Mty.ident ~loc:module_long_ident.loc module_long_ident
    | Lparen ->
      Parser.next p;
      let mty = parse_module_type p in
      Parser.expect Rparen p;
      {mty with pmty_loc = mk_loc start_pos p.prev_end_pos}
    | Lbrace ->
      Parser.next p;
      let spec =
        parse_delimited_region ~grammar:Grammar.Signature ~closing:Rbrace
          ~f:parse_signature_item_region p
      in
      Parser.expect Rbrace p;
      let loc = mk_loc start_pos p.prev_end_pos in
      Ast_helper.Mty.signature ~loc spec
    | Module ->
      (* TODO: check if this is still atomic when implementing first class modules*)
      parse_module_type_of p
    | Percent ->
      let extension = parse_extension p in
      let loc = mk_loc start_pos p.prev_end_pos in
      Ast_helper.Mty.extension ~loc extension
    | token ->
      Parser.err p (Diagnostics.unexpected token p.breadcrumbs);
      Recover.default_module_type ()
  in
  let module_type_loc = mk_loc start_pos p.prev_end_pos in
  {module_type with pmty_loc = module_type_loc}

and parse_functor_module_type p =
  let start_pos = p.Parser.start_pos in
  let args = parse_functor_args p in
  Parser.expect EqualGreater p;
  let rhs = parse_module_type p in
  let end_pos = p.prev_end_pos in
  let mod_type =
    List.fold_right
      (fun (attrs, name, module_type, start_pos) acc ->
        Ast_helper.Mty.functor_ ~loc:(mk_loc start_pos end_pos) ~attrs name
          module_type acc)
      args rhs
  in
  {mod_type with pmty_loc = mk_loc start_pos end_pos}

(* Module types are the module-level equivalent of type expressions: they
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
 *)
and parse_module_type ?(es6_arrow = true) ?(with_ = true) p =
  let attrs = parse_attributes p in
  let modty =
    if es6_arrow && is_es6_arrow_functor p then parse_functor_module_type p
    else
      let modty = parse_atomic_module_type p in
      match p.Parser.token with
      | EqualGreater when es6_arrow == true ->
        Parser.next p;
        let rhs = parse_module_type ~with_:false p in
        let str = Location.mknoloc "_" in
        let loc = mk_loc modty.pmty_loc.loc_start p.prev_end_pos in
        Ast_helper.Mty.functor_ ~loc str (Some modty) rhs
      | _ -> modty
  in
  let module_type =
    {modty with pmty_attributes = List.concat [modty.pmty_attributes; attrs]}
  in
  if with_ then parse_with_constraints module_type p else module_type

and parse_with_constraints module_type p =
  match p.Parser.token with
  | Lident "with" ->
    Parser.next p;
    let first = parse_with_constraint p in
    let rec loop p acc =
      match p.Parser.token with
      | And ->
        Parser.next p;
        loop p (parse_with_constraint p :: acc)
      | _ -> List.rev acc
    in
    let constraints = loop p [first] in
    let loc = mk_loc module_type.pmty_loc.loc_start p.prev_end_pos in
    Ast_helper.Mty.with_ ~loc module_type constraints
  | _ -> module_type

(* mod-constraint	::=
 *  |  type typeconstr<type-params> type-equation type-constraints?
 *  ∣	 type typeconstr-name<type-params> := typexpr
 *  ∣	 module module-path = extended-module-path
 *  ∣	 module module-path :=  extended-module-path
 *
 *  TODO: split this up into multiple functions, better errors *)
and parse_with_constraint p =
  match p.Parser.token with
  | Module -> (
    Parser.next p;
    let module_path = parse_module_long_ident ~lowercase:false p in
    match p.Parser.token with
    | ColonEqual ->
      Parser.next p;
      let lident = parse_module_long_ident ~lowercase:false p in
      Parsetree.Pwith_modsubst (module_path, lident)
    | Equal ->
      Parser.next p;
      let lident = parse_module_long_ident ~lowercase:false p in
      Parsetree.Pwith_module (module_path, lident)
    | token ->
      (* TODO: revisit *)
      Parser.err p (Diagnostics.unexpected token p.breadcrumbs);
      let lident = parse_module_long_ident ~lowercase:false p in
      Parsetree.Pwith_modsubst (module_path, lident))
  | Typ -> (
    Parser.next p;
    let type_constr = parse_value_path p in
    let params = parse_type_params ~parent:type_constr p in
    match p.Parser.token with
    | ColonEqual ->
      Parser.next p;
      let typ_expr = parse_typ_expr p in
      Parsetree.Pwith_typesubst
        ( type_constr,
          Ast_helper.Type.mk ~loc:type_constr.loc ~params ~manifest:typ_expr
            (Location.mkloc (Longident.last type_constr.txt) type_constr.loc) )
    | Equal ->
      Parser.next p;
      let private_flag =
        if Parser.optional p Token.Private then Asttypes.Private
        else Asttypes.Public
      in
      let typ_expr = parse_typ_expr p in
      let type_constraints = parse_type_constraints p in
      Parsetree.Pwith_type
        ( type_constr,
          Ast_helper.Type.mk ~loc:type_constr.loc ~priv:private_flag ~params
            ~manifest:typ_expr ~cstrs:type_constraints
            (Location.mkloc (Longident.last type_constr.txt) type_constr.loc) )
    | token ->
      (* TODO: revisit *)
      Parser.err p (Diagnostics.unexpected token p.breadcrumbs);
      let typ_expr = parse_typ_expr p in
      let type_constraints = parse_type_constraints p in
      Parsetree.Pwith_type
        ( type_constr,
          Ast_helper.Type.mk ~loc:type_constr.loc ~params ~manifest:typ_expr
            ~cstrs:type_constraints
            (Location.mkloc (Longident.last type_constr.txt) type_constr.loc) ))
  | token ->
    (* TODO: implement recovery strategy *)
    Parser.err p (Diagnostics.unexpected token p.breadcrumbs);
    Parsetree.Pwith_type
      ( Location.mknoloc (Longident.Lident ""),
        Ast_helper.Type.mk ~params:[] ~manifest:(Recover.default_type ())
          ~cstrs:[] (Location.mknoloc "") )

and parse_module_type_of p =
  let start_pos = p.Parser.start_pos in
  Parser.expect Module p;
  Parser.expect Typ p;
  Parser.expect Of p;
  let module_expr = parse_module_expr p in
  Ast_helper.Mty.typeof_ ~loc:(mk_loc start_pos p.prev_end_pos) module_expr

and parse_newline_or_semicolon_signature p =
  match p.Parser.token with
  | Semicolon -> Parser.next p
  | token when Grammar.is_signature_item_start token ->
    if p.prev_end_pos.pos_lnum < p.start_pos.pos_lnum then ()
    else
      Parser.err ~start_pos:p.prev_end_pos ~end_pos:p.end_pos p
        (Diagnostics.message
           "consecutive specifications on a line must be separated by ';' or a \
            newline")
  | _ -> ()

and parse_signature_item_region p =
  let start_pos = p.Parser.start_pos in
  let attrs = parse_attributes p in
  match p.Parser.token with
  | Let ->
    Parser.begin_region p;
    let value_desc = parse_sign_let_desc ~attrs p in
    parse_newline_or_semicolon_signature p;
    let loc = mk_loc start_pos p.prev_end_pos in
    Parser.end_region p;
    Some (Ast_helper.Sig.value ~loc value_desc)
  | Typ -> (
    Parser.begin_region p;
    match parse_type_definition_or_extension ~attrs p with
    | TypeDef {rec_flag; types} ->
      parse_newline_or_semicolon_signature p;
      let loc = mk_loc start_pos p.prev_end_pos in
      Parser.end_region p;
      Some (Ast_helper.Sig.type_ ~loc rec_flag types)
    | TypeExt ext ->
      parse_newline_or_semicolon_signature p;
      let loc = mk_loc start_pos p.prev_end_pos in
      Parser.end_region p;
      Some (Ast_helper.Sig.type_extension ~loc ext))
  | External ->
    let external_def = parse_external_def ~attrs ~start_pos p in
    parse_newline_or_semicolon_signature p;
    let loc = mk_loc start_pos p.prev_end_pos in
    Some (Ast_helper.Sig.value ~loc external_def)
  | Exception ->
    let exception_def = parse_exception_def ~attrs p in
    parse_newline_or_semicolon_signature p;
    let loc = mk_loc start_pos p.prev_end_pos in
    Some (Ast_helper.Sig.exception_ ~loc exception_def)
  | Open ->
    let open_description = parse_open_description ~attrs p in
    parse_newline_or_semicolon_signature p;
    let loc = mk_loc start_pos p.prev_end_pos in
    Some (Ast_helper.Sig.open_ ~loc open_description)
  | Include ->
    Parser.next p;
    let module_type = parse_module_type p in
    let include_description =
      Ast_helper.Incl.mk
        ~loc:(mk_loc start_pos p.prev_end_pos)
        ~attrs module_type
    in
    parse_newline_or_semicolon_signature p;
    let loc = mk_loc start_pos p.prev_end_pos in
    Some (Ast_helper.Sig.include_ ~loc include_description)
  | Module -> (
    Parser.begin_region p;
    Parser.next p;
    match p.Parser.token with
    | Uident _ ->
      let mod_decl = parse_module_declaration_or_alias ~attrs p in
      parse_newline_or_semicolon_signature p;
      let loc = mk_loc start_pos p.prev_end_pos in
      Parser.end_region p;
      Some (Ast_helper.Sig.module_ ~loc mod_decl)
    | Rec ->
      let rec_module = parse_rec_module_spec ~attrs ~start_pos p in
      parse_newline_or_semicolon_signature p;
      let loc = mk_loc start_pos p.prev_end_pos in
      Parser.end_region p;
      Some (Ast_helper.Sig.rec_module ~loc rec_module)
    | Typ ->
      let mod_type_decl = parse_module_type_declaration ~attrs ~start_pos p in
      Parser.end_region p;
      Some mod_type_decl
    | _t ->
      let mod_decl = parse_module_declaration_or_alias ~attrs p in
      parse_newline_or_semicolon_signature p;
      let loc = mk_loc start_pos p.prev_end_pos in
      Parser.end_region p;
      Some (Ast_helper.Sig.module_ ~loc mod_decl))
  | AtAt ->
    let attr = parse_standalone_attribute p in
    parse_newline_or_semicolon_signature p;
    let loc = mk_loc start_pos p.prev_end_pos in
    Some (Ast_helper.Sig.attribute ~loc attr)
  | ModuleComment (loc, s) ->
    Parser.next p;
    Some
      (Ast_helper.Sig.attribute ~loc
         ( {txt = "res.doc"; loc},
           PStr
             [
               Ast_helper.Str.eval ~loc
                 (Ast_helper.Exp.constant ~loc (Pconst_string (s, None)));
             ] ))
  | PercentPercent ->
    let extension = parse_extension ~module_language:true p in
    parse_newline_or_semicolon_signature p;
    let loc = mk_loc start_pos p.prev_end_pos in
    Some (Ast_helper.Sig.extension ~attrs ~loc extension)
  | _ -> (
    match attrs with
    | (({Asttypes.loc = attr_loc}, _) as attr) :: _ ->
      Parser.err ~start_pos:attr_loc.loc_start ~end_pos:attr_loc.loc_end p
        (Diagnostics.message (ErrorMessages.attribute_without_node attr));
      Some Recover.default_signature_item
    | _ -> None)
[@@progress Parser.next, Parser.expect]

(* module rec module-name :  module-type  { and module-name:  module-type } *)
and parse_rec_module_spec ~attrs ~start_pos p =
  Parser.expect Rec p;
  let rec loop p spec =
    let start_pos = p.Parser.start_pos in
    let attrs = parse_attributes_and_binding p in
    match p.Parser.token with
    | And ->
      (* TODO: give a good error message when with constraint, no parens
       * and ASet: (Set.S with type elt = A.t)
       * and BTree: (Btree.S with type elt = A.t)
       * Without parens, the `and` signals the start of another
       * `with-constraint`
       *)
      Parser.expect And p;
      let decl = parse_rec_module_declaration ~attrs ~start_pos p in
      loop p (decl :: spec)
    | _ -> List.rev spec
  in
  let first = parse_rec_module_declaration ~attrs ~start_pos p in
  loop p [first]

(* module-name : module-type *)
and parse_rec_module_declaration ~attrs ~start_pos p =
  let name =
    match p.Parser.token with
    | Uident mod_name ->
      let loc = mk_loc p.start_pos p.end_pos in
      Parser.next p;
      Location.mkloc mod_name loc
    | t ->
      Parser.err p (Diagnostics.uident t);
      Location.mknoloc "_"
  in
  Parser.expect Colon p;
  let mod_type = parse_module_type p in
  Ast_helper.Md.mk ~loc:(mk_loc start_pos p.prev_end_pos) ~attrs name mod_type

and parse_module_declaration_or_alias ~attrs p =
  let start_pos = p.Parser.start_pos in
  let module_name =
    match p.Parser.token with
    | Uident ident ->
      let loc = mk_loc p.Parser.start_pos p.end_pos in
      Parser.next p;
      Location.mkloc ident loc
    | t ->
      Parser.err p (Diagnostics.uident t);
      Location.mknoloc "_"
  in
  let body =
    match p.Parser.token with
    | Colon ->
      Parser.next p;
      parse_module_type p
    | Equal ->
      Parser.next p;
      let lident = parse_module_long_ident ~lowercase:false p in
      Ast_helper.Mty.alias lident
    | token ->
      Parser.err p (Diagnostics.unexpected token p.breadcrumbs);
      Recover.default_module_type ()
  in
  let loc = mk_loc start_pos p.prev_end_pos in
  Ast_helper.Md.mk ~loc ~attrs module_name body

and parse_module_type_declaration ~attrs ~start_pos p =
  Parser.expect Typ p;
  let module_name =
    match p.Parser.token with
    | Uident ident ->
      let loc = mk_loc p.start_pos p.end_pos in
      Parser.next p;
      Location.mkloc ident loc
    | Lident ident ->
      let loc = mk_loc p.start_pos p.end_pos in
      Parser.next p;
      Location.mkloc ident loc
    | t ->
      Parser.err p (Diagnostics.uident t);
      Location.mknoloc "_"
  in
  let typ =
    match p.Parser.token with
    | Equal ->
      Parser.next p;
      Some (parse_module_type p)
    | _ -> None
  in
  let module_decl = Ast_helper.Mtd.mk ~attrs ?typ module_name in
  Ast_helper.Sig.modtype ~loc:(mk_loc start_pos p.prev_end_pos) module_decl

and parse_sign_let_desc ~attrs p =
  let start_pos = p.Parser.start_pos in
  Parser.optional p Let |> ignore;
  let name, loc = parse_lident p in
  let name = Location.mkloc name loc in
  Parser.expect Colon p;
  let typ_expr = parse_poly_type_expr p in
  let loc = mk_loc start_pos p.prev_end_pos in
  Ast_helper.Val.mk ~loc ~attrs name typ_expr

(* attr-id	::=	lowercase-ident
   ∣	  capitalized-ident
   ∣	  attr-id .  attr-id *)
and parse_attribute_id ~start_pos p =
  let rec loop p acc =
    match p.Parser.token with
    | Lident ident | Uident ident -> (
      Parser.next p;
      let id = acc ^ ident in
      match p.Parser.token with
      | Dot ->
        Parser.next p;
        loop p (id ^ ".")
      | _ -> id)
    | token when Token.is_keyword token -> (
      Parser.next p;
      let id = acc ^ Token.to_string token in
      match p.Parser.token with
      | Dot ->
        Parser.next p;
        loop p (id ^ ".")
      | _ -> id)
    | token ->
      Parser.err p (Diagnostics.unexpected token p.breadcrumbs);
      acc
  in
  let id = loop p "" in
  let end_pos = p.prev_end_pos in
  Location.mkloc id (mk_loc start_pos end_pos)

(*
 * payload ::=  empty
 *          |  ( structure-item )
 *
 * TODO: what about multiple structure items?
 * @attr({let x = 1; let x = 2})
 *
 * Also what about type-expressions and specifications?
 * @attr(:myType) ???
 *)
and parse_payload p =
  match p.Parser.token with
  | Lparen when p.start_pos.pos_cnum = p.prev_end_pos.pos_cnum -> (
    Parser.leave_breadcrumb p Grammar.AttributePayload;
    Parser.next p;
    match p.token with
    | Colon ->
      Parser.next p;
      let payload =
        if Grammar.is_signature_item_start p.token then
          Parsetree.PSig
            (parse_delimited_region ~grammar:Grammar.Signature ~closing:Rparen
               ~f:parse_signature_item_region p)
        else Parsetree.PTyp (parse_typ_expr p)
      in
      Parser.expect Rparen p;
      Parser.eat_breadcrumb p;
      payload
    | Question ->
      Parser.next p;
      let pattern = parse_pattern p in
      let expr =
        match p.token with
        | When | If ->
          Parser.next p;
          Some (parse_expr p)
        | _ -> None
      in
      Parser.expect Rparen p;
      Parser.eat_breadcrumb p;
      Parsetree.PPat (pattern, expr)
    | _ ->
      let items =
        parse_delimited_region ~grammar:Grammar.Structure ~closing:Rparen
          ~f:parse_structure_item_region p
      in
      Parser.expect Rparen p;
      Parser.eat_breadcrumb p;
      Parsetree.PStr items)
  | _ -> Parsetree.PStr []

(* type attribute = string loc * payload *)
and parse_attribute p =
  match p.Parser.token with
  | At ->
    let start_pos = p.start_pos in
    Parser.next p;
    let attr_id = parse_attribute_id ~start_pos p in
    let payload = parse_payload p in
    Some (attr_id, payload)
  | DocComment (loc, s) ->
    Parser.next p;
    Some (doc_comment_to_attribute loc s)
  | _ -> None

and doc_comment_to_attribute loc s : Parsetree.attribute =
  ( {txt = "res.doc"; loc},
    PStr
      [
        Ast_helper.Str.eval ~loc
          (Ast_helper.Exp.constant ~loc (Pconst_string (s, None)));
      ] )

and parse_attributes p =
  parse_region p ~grammar:Grammar.Attribute ~f:parse_attribute

(*
 * standalone-attribute ::=
 *  | @@ atribute-id
 *  | @@ attribute-id ( structure-item )
 *)
and parse_standalone_attribute p =
  let start_pos = p.start_pos in
  Parser.expect AtAt p;
  let attr_id = parse_attribute_id ~start_pos p in
  let payload = parse_payload p in
  (attr_id, payload)

(* extension	::=	% attr-id  attr-payload
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
 *)
and parse_extension ?(module_language = false) p =
  let start_pos = p.Parser.start_pos in
  if module_language then Parser.expect PercentPercent p
  else Parser.expect Percent p;
  let attr_id = parse_attribute_id ~start_pos p in
  let payload = parse_payload p in
  (attr_id, payload)

(* module signature on the file level *)
let parse_specification p : Parsetree.signature =
  parse_region p ~grammar:Grammar.Specification ~f:parse_signature_item_region

(* module structure on the file level *)
let parse_implementation p : Parsetree.structure =
  parse_region p ~grammar:Grammar.Implementation ~f:parse_structure_item_region
