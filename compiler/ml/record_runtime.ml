(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* How a record field is named at run time. A field carries the name it was
   declared with unless an [@as("...")] attribute renames it, and that choice
   is made once here rather than re-derived wherever a field name is needed. *)

let declaration_name (lbl : Types.label_declaration) =
  match lbl.ld_runtime_name with
  | Some name -> name
  | None -> Ident.name lbl.ld_id

(* Two fields collide when they end up with the same runtime name, which a
   rename can cause between fields whose declared names differ. *)
let rec check_duplicated_labels_aux (lbls : Parsetree.label_declaration list)
    (coll : Set_string.t) =
  match lbls with
  | [] -> None
  | ({pld_name = {txt}} as lbl) :: rest -> (
    if Set_string.mem coll txt && txt <> "..." then Some lbl.pld_name
    else
      let coll_with_lbl = Set_string.add coll txt in
      match lbl.pld_runtime_name with
      | None -> check_duplicated_labels_aux rest coll_with_lbl
      | Some {txt; loc} ->
        let name = String_literal.string_semantic txt in
        (* Checked against the fields seen before this one rather than against
           [coll_with_lbl], so that [@as("x") x] renames a field to the name it
           already has. *)
        if Set_string.mem coll name then Some {Asttypes.txt = name; loc}
        else
          check_duplicated_labels_aux rest (Set_string.add coll_with_lbl name))

(* A field has one runtime name, so only the first [@as] naming it is taken
   out; a second one is left behind and reported here. *)
let extra_as_attribute (lbl : Parsetree.label_declaration) =
  Ext_list.find_opt lbl.pld_attributes
    (fun (({txt; loc}, payload) : Parsetree.attribute) ->
      if txt = "as" && Ast_payload.string_literal_of_payload payload <> None
      then Some loc
      else None)

let check_duplicated_labels lbls =
  check_duplicated_labels_aux lbls Set_string.empty
