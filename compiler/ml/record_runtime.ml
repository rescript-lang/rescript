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

let as_name (({txt}, payload) : Parsetree.attribute) =
  if txt = "as" then Ast_payload.semantic_string_of_payload payload else None

let field_name declared_name attributes =
  Ext_list.find_def attributes as_name declared_name

let declaration_name (lbl : Types.label_declaration) =
  field_name (Ident.name lbl.ld_id) lbl.ld_attributes
