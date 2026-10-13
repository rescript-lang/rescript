(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)

type t = Parsetree.payload

let rec unwrap_braces (expression : Parsetree.expression) =
  match expression.pexp_desc with
  | Pexp_braces {expr} -> unwrap_braces expr
  | _ -> expression

let unwrap_payload_expression (payload : t) =
  match payload with
  | PStr [({pstr_desc = Pstr_eval (expression, attrs); _} as item)] ->
    Parsetree.PStr
      [{item with pstr_desc = Pstr_eval (unwrap_braces expression, attrs)}]
  | payload -> payload

let semantic_string_of_expression (expression : Parsetree.expression) =
  match unwrap_braces expression with
  | {pexp_desc = Pexp_constant (Pconst_string payload); _} ->
    Some (String_literal.string_semantic payload)
  | {
   pexp_desc = Pexp_template {source_segments = [source]; values = []};
   pexp_loc;
  } -> (
    match String_literal.decode_js_template_escapes source.txt with
    | Some semantic -> Some semantic
    | None ->
      Location.raise_errorf ~loc:pexp_loc "Invalid string escape sequence")
  | _ -> None

let string_literal_of_expression (expression : Parsetree.expression) =
  match unwrap_braces expression with
  | {pexp_desc = Pexp_constant (Pconst_string payload); _} -> Some payload
  | {
   pexp_desc = Pexp_template {source_segments = [source]; values = []};
   pexp_loc;
  } -> (
    match String_literal.decode_js_template_escapes source.txt with
    | Some semantic -> Some (String_literal.string_from_semantic semantic)
    | None ->
      Location.raise_errorf ~loc:pexp_loc "Invalid string escape sequence")
  | _ -> None

let string_literal_of_payload (x : t) =
  match x with
  | PStr [{pstr_desc = Pstr_eval (expression, _); _}] ->
    string_literal_of_expression expression
  | _ -> None

let semantic_string_of_payload (x : t) =
  match x with
  | PStr [{pstr_desc = Pstr_eval (expression, _); _}] ->
    semantic_string_of_expression expression
  | _ -> None

let quote_js_string semantic =
  "\"" ^ String_literal.encode_js_string semantic ^ "\""

(* Spell a ReScript integer literal as a JavaScript one. Underscore separators
   need ES2021, and a decimal literal with a leading zero is a legacy octal
   literal, which strict-mode JavaScript rejects. *)
let js_integer_source source =
  let source = String.concat "" (String.split_on_char '_' source) in
  let sign, digits =
    if source <> "" && source.[0] = '-' then
      ("-", String.sub source 1 (String.length source - 1))
    else ("", source)
  in
  let length = String.length digits in
  let has_radix_prefix =
    length > 1
    && digits.[0] = '0'
    &&
    match digits.[1] with
    | 'x' | 'X' | 'o' | 'O' | 'b' | 'B' -> true
    | _ -> false
  in
  if has_radix_prefix then sign ^ digits
  else
    let rec first_significant i =
      if i < length - 1 && digits.[i] = '0' then first_significant (i + 1)
      else i
    in
    let start = first_significant 0 in
    sign ^ String.sub digits start (length - start)

let fixed_source_of_expression (expression : Parsetree.expression) =
  match (unwrap_braces expression).pexp_desc with
  | Pexp_constant (Pconst_string payload) ->
    Some (quote_js_string (String_literal.string_semantic payload))
  | Pexp_template {source_segments = [{txt = source}]; values = []} -> (
    match String_literal.decode_js_template_escapes source with
    | Some semantic -> Some (quote_js_string semantic)
    | None -> None)
  | Pexp_constant (Pconst_integer (source, None)) ->
    Some (js_integer_source source)
  | Pexp_tagged_template
      {
        tag = {pexp_desc = Pexp_ident {txt = Lident "json"}};
        raw_sources = [{txt = source}];
        values = [];
      } ->
    Some source
  | _ -> None

let fixed_source_of_payload (payload : t) =
  match payload with
  | PStr [{pstr_desc = Pstr_eval (expression, _); _}] ->
    fixed_source_of_expression expression
  | PStr _ | PSig _ | PTyp _ | PPat _ -> None

let single_int_source (x : t) =
  match unwrap_payload_expression x with
  | PStr
      [
        {
          pstr_desc =
            Pstr_eval
              ({pexp_desc = Pexp_constant (Pconst_integer (name, char)); _}, _);
          _;
        };
      ]
    when match char with
         | Some n when n = 'n' -> false
         | _ -> true ->
    Some name
  | _ -> None

let is_single_int x = Option.map int_of_string (single_int_source x)

let is_single_float (x : t) : string option =
  match unwrap_payload_expression x with
  | PStr
      [
        {
          pstr_desc =
            Pstr_eval
              ({pexp_desc = Pexp_constant (Pconst_float (name, _)); _}, _);
          _;
        };
      ] ->
    Some name
  | _ -> None

let is_single_bigint (x : t) : string option =
  match unwrap_payload_expression x with
  | PStr
      [
        {
          pstr_desc =
            Pstr_eval
              ( {pexp_desc = Pexp_constant (Pconst_integer (name, Some 'n')); _},
                _ );
          _;
        };
      ] ->
    Some name
  | _ -> None

let is_single_bool (x : t) : bool option =
  match unwrap_payload_expression x with
  | PStr
      [
        {
          pstr_desc =
            Pstr_eval
              ( {
                  pexp_desc =
                    Pexp_construct ({txt = Lident (("true" | "false") as b)}, _);
                  _;
                },
                _ );
          _;
        };
      ] ->
    Some (b = "true")
  | _ -> None

let is_single_ident (x : t) =
  match unwrap_payload_expression x with
  | PStr [{pstr_desc = Pstr_eval ({pexp_desc = Pexp_ident lid}, _); _}] ->
    Some lid.txt
  | _ -> None

let constructor_tag_of_payload payload =
  match string_literal_of_payload payload with
  | Some s -> Some (Parsetree.Pct_string s)
  | None -> (
    match single_int_source payload with
    | Some i -> Some (Pct_int i)
    | None -> (
      match is_single_float payload with
      | Some f -> Some (Pct_float f)
      | None -> (
        match is_single_bigint payload with
        | Some i -> Some (Pct_bigint i)
        | None -> (
          match is_single_bool payload with
          | Some b -> Some (Pct_bool b)
          | None -> (
            match is_single_ident payload with
            | Some (Lident "null") -> Some Pct_null
            | Some (Lident "undefined") -> Some Pct_undefined
            | Some _ | None -> None)))))

let validate_raw_source ~(kind : Js_raw_info.raw_kind) ?is_function ~loc ~offset
    str =
  Bs_flow_ast_utils.check_flow_errors ~loc ~offset
    (match kind with
    | Raw_re | Raw_exp ->
      let ((_loc, expression) as program), errors =
        let open Parser_flow in
        let env = Parser_env.init_env None str in
        do_parse env Parse.expression false
      in
      (if kind = Raw_re then
         match expression with
         | RegExpLiteral _ -> ()
         | _ ->
           Location.raise_errorf ~loc
             "Syntax error: a valid JS regex literal expected");
      (match is_function with
      | Some is_function -> (
        match Classify_function.classify_exp program with
        | Js_function {arity; _} -> is_function := Some arity
        | _ -> ())
      | None -> ());
      errors
    | Raw_program -> snd (Parser_flow.parse_program false None str))

let raw_as_string_exp_exn ~(kind : Js_raw_info.raw_kind) ?is_function (x : t) :
    Parsetree.expression option =
  let string_expression =
    match unwrap_payload_expression x with
    (* TODO also need detect empty phrase case *)
    | PStr
        [
          {
            pstr_desc =
              Pstr_eval
                ( ({
                     pexp_desc =
                       Pexp_template {source_segments = [source]; values = []};
                   } as expression),
                  _ );
          };
        ] ->
      let source = String_literal.decode_raw_template_source source.txt in
      Some (source, Bs_flow_ast_utils.flow_deli_offset (Some "js"), expression)
    | PStr
        [
          {
            pstr_desc =
              Pstr_eval
                (({pexp_desc = Pexp_constant constant; _} as expression), _);
            _;
          };
        ] -> (
      match constant with
      | Pconst_raw_source source ->
        Some (source, Bs_flow_ast_utils.flow_deli_offset (Some "js"), expression)
      | Pconst_string payload ->
        Some (String_literal.string_semantic payload, 0, expression)
      | _ -> None)
    | _ -> None
  in
  match string_expression with
  | Some (str, offset, ({pexp_loc = loc} as expression)) ->
    validate_raw_source ~kind ?is_function ~loc ~offset str;
    Some {expression with pexp_desc = Pexp_constant (Pconst_raw_source str)}
  | None -> None

type lid = string Asttypes.loc

type action = lid * Parsetree.expression option
(** None means punning is hit 
    {[ { x } ]}
    otherwise it comes with a payload 
    {[ { x = exp }]}
*)

(** Report to the user, as a warning, that the bs-attribute parser is bailing out. (This is to allow
    external ppx, like ppx_deriving, to pick up where the builtin ppx leave off.) *)
let unrecognized_config_record loc text =
  Location.prerr_warning loc (Warnings.Bs_derive_warning text)

let ident_or_record_as_config loc (x : t) :
    (string Location.loc * Parsetree.expression option) list =
  match unwrap_payload_expression x with
  | PStr
      [
        {
          pstr_desc =
            Pstr_eval
              ( {pexp_desc = Pexp_record (label_exprs, with_obj); pexp_loc = loc},
                _ );
          _;
        };
      ] -> (
    match with_obj with
    | None ->
      Ext_list.map label_exprs (fun u ->
          match u with
          | {
           lid = {txt = Lident name; loc};
           x = {Parsetree.pexp_desc = Pexp_ident {txt = Lident name2}};
          }
            when name2 = name ->
            ({Asttypes.txt = name; loc}, None)
          | {lid = {txt = Lident name; loc}; x = y} ->
            ({Asttypes.txt = name; loc}, Some y)
          | _ -> Location.raise_errorf ~loc "Qualified label is not allowed")
    | Some _ ->
      unrecognized_config_record loc "`with` is not supported, discarding";
      [])
  | PStr
      [
        {
          pstr_desc =
            Pstr_eval
              ({pexp_desc = Pexp_ident {loc = lloc; txt = Lident txt}}, _);
        };
      ] ->
    [({Asttypes.txt; loc = lloc}, None)]
  | PStr [] -> []
  | _ ->
    unrecognized_config_record loc "invalid attribute config-record, ignoring";
    []

let assert_strings loc (x : t) : string list =
  let exception Not_str in
  let semantic_string expression =
    match semantic_string_of_expression expression with
    | Some semantic -> semantic
    | None -> raise Not_str
  in
  match unwrap_payload_expression x with
  | PStr
      [
        {
          pstr_desc = Pstr_eval ({pexp_desc = Pexp_tuple strs; _}, _);
          pstr_loc = loc;
          _;
        };
      ] -> (
    try Ext_list.map strs semantic_string
    with Not_str -> Location.raise_errorf ~loc "expect string tuple list")
  | PStr [{pstr_desc = Pstr_eval (expression, _); _}] -> (
    try [semantic_string expression]
    with Not_str -> Location.raise_errorf ~loc "expect string tuple list")
  | PStr [] -> []
  | PSig _ | PStr _ | PTyp _ | PPat _ ->
    Location.raise_errorf ~loc "expect string tuple list"

let assert_bool_lit (e : Parsetree.expression) =
  match (unwrap_braces e).pexp_desc with
  | Pexp_construct ({txt = Lident "true"}, {txt = []}) -> true
  | Pexp_construct ({txt = Lident "false"}, {txt = []}) -> false
  | _ ->
    Location.raise_errorf ~loc:e.pexp_loc
      "expect `true` or `false` in this field"

let empty : t = Parsetree.PStr []

let table_dispatch table (action : action) =
  match action with
  | {txt = name; loc}, y -> (
    match Map_string.find name table with
    | fn -> fn y
    | exception _ -> Location.raise_errorf ~loc "%s is not supported" name)
