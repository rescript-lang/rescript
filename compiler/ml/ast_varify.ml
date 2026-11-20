(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                         Alain Frisch, LexiFi                           *)
(*                                                                        *)
(*   Copyright 2012 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Asttypes
open Parsetree

let varify_constructors var_names t =
  let check_variable vl loc v =
    if List.mem v vl then raise Syntaxerr.(Error (Variable_in_scope (loc, v)))
  in
  let var_names = List.map (fun v -> v.txt) var_names in
  let rec loop t =
    let desc =
      match t.ptyp_desc with
      | Ptyp_any -> Ptyp_any
      | Ptyp_var x ->
        check_variable var_names t.ptyp_loc x;
        Ptyp_var x
      | Ptyp_arrow {params; ret} ->
        Ptyp_arrow
          {
            params =
              List.map
                (function
                  | Parsetree.Parg_type arg ->
                    Parsetree.Parg_type {arg with typ = loop arg.typ}
                  | Parsetree.Parg_fixed _ as arg -> arg)
                params;
            ret = loop ret;
          }
      | Ptyp_tuple lst -> Ptyp_tuple (List.map loop lst)
      | Ptyp_constr ({txt = Longident.Lident s}, []) when List.mem s var_names
        ->
        Ptyp_var s
      | Ptyp_constr (longident, lst) ->
        Ptyp_constr (longident, List.map loop lst)
      | Ptyp_object (lst, o) -> Ptyp_object (List.map loop_object_field lst, o)
      | Ptyp_alias (core_type, string) ->
        check_variable var_names t.ptyp_loc string;
        Ptyp_alias (loop core_type, string)
      | Ptyp_variant (row_field_list, flag, lbl_lst_option) ->
        Ptyp_variant
          (List.map loop_row_field row_field_list, flag, lbl_lst_option)
      | Ptyp_poly (string_lst, core_type) ->
        List.iter
          (fun v -> check_variable var_names t.ptyp_loc v.txt)
          string_lst;
        Ptyp_poly (string_lst, loop core_type)
      | Ptyp_package (longident, lst) ->
        Ptyp_package (longident, List.map (fun (n, typ) -> (n, loop typ)) lst)
      | Ptyp_extension (s, arg) -> Ptyp_extension (s, arg)
    in
    {t with ptyp_desc = desc}
  and loop_row_field = function
    | Rtag (label, attrs, flag, groups) ->
      Rtag
        ( label,
          attrs,
          flag,
          List.map
            (fun ({txt} as group) -> {group with txt = List.map loop txt})
            groups )
    | Rinherit t -> Rinherit (loop t)
  and loop_object_field = function
    | Otag (label, attrs, t) -> Otag (label, attrs, loop t)
    | Oinherit t -> Oinherit (loop t)
  in
  loop t
