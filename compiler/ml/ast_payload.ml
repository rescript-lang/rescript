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

let is_single_string (x : t) =
  match x with
  (* TODO also need detect empty phrase case *)
  | PStr
      [
        {
          pstr_desc =
            Pstr_eval
              ({pexp_desc = Pexp_constant (Pconst_string (name, dec)); _}, _);
          _;
        };
      ] ->
    Some (name, dec)
  | _ -> None

let is_single_string_as_ast (x : t) : Parsetree.expression option =
  match x with
  (*TODO also need detect empty phrase case *)
  | PStr
      [
        {
          pstr_desc =
            Pstr_eval
              (({pexp_desc = Pexp_constant (Pconst_string (_, _)); _} as e), _);
          _;
        };
      ] ->
    Some e
  | _ -> None

let is_single_int (x : t) : int option =
  match x with
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
    Some (int_of_string name)
  | _ -> None

let is_single_float (x : t) : string option =
  match x with
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
  match x with
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
  match x with
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
  match x with
  | PStr [{pstr_desc = Pstr_eval ({pexp_desc = Pexp_ident lid}, _); _}] ->
    Some lid.txt
  | _ -> None

let raw_as_string_exp_exn ~(kind : Js_raw_info.raw_kind) ?is_function (x : t) :
    Parsetree.expression option =
  match x with
  (* TODO also need detect empty phrase case *)
  | PStr
      [
        {
          pstr_desc =
            Pstr_eval
              ( ({
                   pexp_desc = Pexp_constant (Pconst_string (str, deli));
                   pexp_loc = loc;
                 } as e),
                _ );
          _;
        };
      ] ->
    Bs_flow_ast_utils.check_flow_errors ~loc
      ~offset:(Bs_flow_ast_utils.flow_deli_offset deli)
      (match kind with
      | Raw_re | Raw_exp ->
        let ((_loc, e) as prog), errors =
          let open Parser_flow in
          let env = Parser_env.init_env None str in
          do_parse env Parse.expression false
        in
        (if kind = Raw_re then
           match e with
           | RegExpLiteral _ -> ()
           | _ ->
             Location.raise_errorf ~loc
               "Syntax error: a valid JS regex literal expected");
        (match is_function with
        | Some is_function -> (
          match Classify_function.classify_exp prog with
          | Js_function {arity; _} -> is_function := Some arity
          | _ -> ())
        | None -> ());
        errors
      | Raw_program -> snd (Parser_flow.parse_program false None str));
    Some {e with pexp_desc = Pexp_constant (Pconst_string (str, None))}
  | _ -> None

let as_core_type loc (x : t) =
  match x with
  | PTyp x -> x
  | _ -> Location.raise_errorf ~loc "except a core type"

let as_ident (x : t) =
  match x with
  | PStr [{pstr_desc = Pstr_eval ({pexp_desc = Pexp_ident ident}, _)}] ->
    Some ident
  | _ -> None

type lid = string Asttypes.loc

type label_expr = lid * Parsetree.expression

type action = lid * Parsetree.expression option
(** None means punning is hit 
    {[ { x } ]}
    otherwise it comes with a payload 
    {[ { x = exp }]}
*)

let unrecognized_config_record loc text =
  Location.prerr_warning loc (Warnings.Bs_derive_warning text)

let ident_or_record_as_config loc (x : t) :
    (string Location.loc * Parsetree.expression option) list =
  match x with
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
          | ( {txt = Lident name; loc},
              {Parsetree.pexp_desc = Pexp_ident {txt = Lident name2}},
              _ )
            when name2 = name ->
            ({Asttypes.txt = name; loc}, None)
          | {txt = Lident name; loc}, y, _ ->
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
  match x with
  | PStr
      [
        {
          pstr_desc = Pstr_eval ({pexp_desc = Pexp_tuple strs; _}, _);
          pstr_loc = loc;
          _;
        };
      ] -> (
    try
      Ext_list.map strs (fun e ->
          match (e : Parsetree.expression) with
          | {pexp_desc = Pexp_constant (Pconst_string (name, _)); _} -> name
          | _ -> raise Not_str)
    with Not_str -> Location.raise_errorf ~loc "expect string tuple list")
  | PStr
      [
        {
          pstr_desc =
            Pstr_eval
              ({pexp_desc = Pexp_constant (Pconst_string (name, _)); _}, _);
          _;
        };
      ] ->
    [name]
  | PStr [] -> []
  | PSig _ | PStr _ | PTyp _ | PPat _ ->
    Location.raise_errorf ~loc "expect string tuple list"

let assert_bool_lit (e : Parsetree.expression) =
  match e.pexp_desc with
  | Pexp_construct ({txt = Lident "true"}, None) -> true
  | Pexp_construct ({txt = Lident "false"}, None) -> false
  | _ ->
    Location.raise_errorf ~loc:e.pexp_loc
      "expect `true` or `false` in this field"

let empty : t = Parsetree.PStr []

let table_dispatch table (action : action) =
  match action with
  | {txt = name; loc}, y -> (
    match Map_string.find_exn table name with
    | fn -> fn y
    | exception _ -> Location.raise_errorf ~loc "%s is not supported" name)
