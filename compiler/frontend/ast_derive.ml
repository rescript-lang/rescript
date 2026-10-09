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

type tdcls = Parsetree.type_declaration list

type gen = {
  structure_gen: tdcls -> Asttypes.rec_flag -> Ast_structure.t;
  signature_gen: tdcls -> Asttypes.rec_flag -> Ast_signature.t;
  expression_gen: (Parsetree.core_type -> Parsetree.expression) option;
}

(* the first argument is [config] payload
   {[
     { x = {uu} }
   ]}
*)
type derive_table = (Parsetree.expression option -> gen) Map_string.t

let derive_table : derive_table ref = ref Map_string.empty

let register key value = derive_table := Map_string.add !derive_table key value

(* Derivers that used to be built in. They get a dedicated error pointing to
   the replacement instead of the generic "is not supported". *)
let removed_deriver_hint = function
  | "abstract" ->
    Some
      "Use a record type instead. Records compile to plain JavaScript objects \
       and support optional fields (`field?: t`), mutable fields and `@as` \
       field renaming."
  | "jsConverter" ->
    Some
      "Records already compile to plain JavaScript objects and polymorphic \
       variants to their (`@as`-renamed) names, so the conversion functions \
       are no longer needed."
  | _ -> None

let dispatch (({txt = name; loc}, _) as action : Ast_payload.action) =
  match removed_deriver_hint name with
  | Some hint ->
    Location.raise_errorf ~loc "@deriving(%s) has been removed. %s" name hint
  | None -> Ast_payload.table_dispatch !derive_table action

let gen_signature tdcls (actions : Ast_payload.action list)
    (explict_nonrec : Asttypes.rec_flag) : Ast_signature.t =
  Ext_list.flat_map actions (fun action ->
      (dispatch action).signature_gen tdcls explict_nonrec)

open Ast_helper

let gen_structure_signature loc (tdcls : tdcls) (action : Ast_payload.action)
    (explicit_nonrec : Asttypes.rec_flag) =
  let u = dispatch action in

  let a = u.structure_gen tdcls explicit_nonrec in
  let b = u.signature_gen tdcls explicit_nonrec in
  Str.include_ ~loc
    (Incl.mk ~loc
       (Mod.constraint_ ~loc (Mod.structure ~loc a) (Mty.signature ~loc b)))
