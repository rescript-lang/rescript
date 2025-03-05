(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * Copyright (C) 2017 - Hongbo Zhang, Authors of ReScript  
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

module E = Js_exp_make

(* TODO: it would be even better, if the [tag_info] contains more information
   about immutablility
*)
let make_block mutable_flag (tag_info : Lam_tag_info.t) tag args =
  match tag_info with
  | _ -> E.make_block tag tag_info args mutable_flag

let field (field_info : Lam_compat.field_dbg_info) e (i : int32) =
  match field_info with
  | Fld_tuple ->
    E.array_index_by_int ?comment:(Lam_compat.str_of_field_info field_info) e i
  | Fld_poly_var_content -> E.poly_var_value_access e
  | Fld_poly_var_tag -> E.poly_var_tag_access e
  | Fld_record_extension {name} -> E.extension_access e (Some name) i
  | Fld_extension -> E.extension_access e None i
  | Fld_variant -> E.variant_access e i
  | Fld_cons -> E.cons_access e i
  | Fld_record_inline {name} -> E.inline_record_access e name i
  | Fld_record {name} -> E.record_access e name i
  | Fld_module {name} -> E.module_access e name i

let field_by_exp e i = E.array_index e i

let set_field (field_info : Lam_compat.set_field_dbg_info) e i e0 =
  match field_info with
  | Fld_record_extension_set name -> E.extension_assign e i name e0
  | Fld_record_inline_set name | Fld_record_set name ->
    E.record_assign e i name e0

(* This dynamism commes from oo compilaton, it should not happen in record *)
let set_field_by_exp self index value = E.assign_by_exp self index value
