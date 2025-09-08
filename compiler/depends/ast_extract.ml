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

(* type module_name = private string *)

module Set_string = Depend.StringSet

(* FIXME: [Clflags.open_modules] seems not to be properly used *)
module SMap = Depend.StringMap

let bound_vars = SMap.empty

type 'a kind = 'a Ml_binary.kind

let read_parse_and_extract (type t) (k : t kind) (ast : t) : Set_string.t =
  Depend.free_structure_names := Set_string.empty;
  Ext_ref.protect Clflags.transparent_modules false (fun _ ->
      List.iter (* check *)
        (fun modname ->
          ignore @@ Depend.open_module bound_vars (Longident.Lident modname))
        !Clflags.open_modules;
      (match k with
      | Ml_binary.Ml -> Depend.add_implementation bound_vars ast
      | Ml_binary.Mli -> Depend.add_signature bound_vars ast);
      !Depend.free_structure_names)
