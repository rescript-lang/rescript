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

let unwrap_braces = Ast_helper.Payload.unwrap_braces
let unwrap_payload_expression = Ast_helper.Payload.unwrap_expression

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

(* The string case reports an invalid escape; the other literals come from
   the non-raising reader that Ast_helper uses. *)
let constructor_tag_of_payload payload =
  match string_literal_of_payload payload with
  | Some s -> Some (Parsetree.Pct_string s)
  | None -> Ast_helper.Payload.constructor_tag payload

let is_single_int payload =
  match Ast_helper.Payload.constructor_tag payload with
  | Some (Pct_int i) -> Some (int_of_string i)
  | _ -> None

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
      label_exprs
      |> List.map (fun (u : Parsetree.expression Parsetree.record_element) ->
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
    try List.map semantic_string strs
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
