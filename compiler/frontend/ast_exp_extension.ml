(* Copyright (C) 2018 Hongbo Zhang, Authors of ReScript
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
open Ast_helper

let handle_extension e (self : Bs_ast_mapper.mapper)
    (({txt; loc}, payload) : Parsetree.extension) =
  match txt with
  | "todo" ->
    let todo_message =
      match Ast_payload.is_single_string payload with
      | Some (s, _) -> Some s
      | None -> None
    in
    Location.prerr_warning e.Parsetree.pexp_loc (Bs_todo todo_message);
    let pretext =
      loc.loc_start.pos_fname ^ ":"
      ^ string_of_int loc.loc_start.pos_lnum
      ^ ":"
      ^ string_of_int loc.loc_start.pos_cnum
      ^ "-"
      ^ string_of_int loc.loc_end.pos_cnum
    in

    Exp.apply ~loc
      (Exp.ident ~loc {txt = Longident.parse "Js.Exn.raiseError"; loc})
      [
        ( Nolabel,
          Exp.constant ~loc
            (Pconst_string
               ( (pretext
                 ^
                 match todo_message with
                 | None -> " - Todo"
                 | Some msg -> " - Todo: " ^ msg),
                 None )) );
      ]
  | "ffi" -> Ast_exp_handle_external.handle_ffi ~loc ~payload
  | "raw" -> Ast_exp_handle_external.handle_raw ~kind:Raw_exp loc payload
  | "re" ->
    Exp.constraint_ ~loc
      (Ast_exp_handle_external.handle_raw ~kind:Raw_re loc payload)
      (Ast_comb.to_js_re_type loc)
  | "external" -> (
    Location.deprecated loc
      "%external is deprecated, use %raw or regular FFI syntax instead.";
    match Ast_payload.as_ident payload with
    | Some {txt = Lident x} -> Ast_exp_handle_external.handle_external loc x
    | None | Some _ ->
      Location.raise_errorf ~loc "external expects a single identifier")
  | "debugger" ->
    {e with pexp_desc = Ast_exp_handle_external.handle_debugger loc payload}
  | "obj" -> (
    match payload with
    | PStr
        [
          {
            pstr_desc =
              Pstr_eval (({pexp_desc = Pexp_record (label_exprs, None)} as e), _);
          };
        ] ->
      {
        e with
        pexp_desc = Ast_util.record_as_js_object e.pexp_loc self label_exprs;
      }
    | _ -> Location.raise_errorf ~loc "Expect a record expression here")
  | _ -> e
(* For an unknown extension, we don't really need to process further*)
(* Exp.extension ~loc ~attrs:e.pexp_attributes (
    self.extension self extension) *)
(* Bs_ast_mapper.default_mapper.expr self e   *)
