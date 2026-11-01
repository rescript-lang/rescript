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

(* [%todo]: warns, and throws at runtime with the location and message. [loc]
   is the location of [%todo], [expr_loc] that of the whole expression. *)
let todo ~loc ~expr_loc (message : string option) =
  Location.prerr_warning expr_loc (Bs_todo message);
  (* Like [__LOC__]: the file's base name, so the generated code does not
     depend on where it was built, and columns rather than offsets. *)
  let file, line, start_col = Location.get_pos_info loc.Location.loc_start in
  let end_col = loc.loc_end.pos_cnum - loc.loc_start.pos_cnum + start_col in
  let pretext =
    Printf.sprintf "%s:%d:%d-%d" (Filename.basename file) line start_col end_col
  in
  Exp.apply ~loc
    (* The compilation unit, not [JsError]: a user module of that name would
       shadow the [Stdlib] alias. *)
    (Exp.ident ~loc
       {txt = Ldot (Lident "Stdlib_JsError", "throwWithMessage"); loc})
    [
      ( Nolabel,
        Exp.constant ~loc
          (Ast_helper.Const.string
             (pretext
             ^
             match message with
             | None -> " - Todo"
             | Some msg -> " - Todo: " ^ msg)) );
    ]

let handle_extension e (_self : Ast_mapper.mapper)
    (({txt; loc}, payload) : Parsetree.extension) =
  match txt with
  | "todo" ->
    (* A payload the parser did not turn into [Pexp_todo], or one from a v0
       PPX. A payload other than a string gives no message. *)
    todo ~loc ~expr_loc:e.Parsetree.pexp_loc
      (Ast_payload.semantic_string_of_payload payload)
  | "ffi" -> Ast_exp_handle_external.handle_ffi ~loc ~payload
  | "raw" -> Ast_exp_handle_external.handle_raw loc payload
  | _ -> e
(* For an unknown extension, we don't really need to process further*)
(* Exp.extension ~loc ~attrs:e.pexp_attributes (
    self.extension self extension) *)
(* Ast_mapper.default_mapper.expr self e   *)
