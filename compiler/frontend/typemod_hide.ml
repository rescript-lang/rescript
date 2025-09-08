(* Copyright (C) 2020 Hongbo Zhang, Authors of ReScript
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

let attrs : Parsetree.attributes =
  [({txt = "internal.local"; loc = Location.none}, PStr [])]

let no_type_defined (x : Parsetree.structure_item) =
  match x.pstr_desc with
  | Pstr_eval _ | Pstr_value _ | Pstr_primitive _ | Pstr_typext _
  | Pstr_exception _
  (* | Pstr_module {pmb_expr = {pmod_desc = Pmod_ident _} }  *) ->
    true
  | Pstr_include
      {
        pincl_mod =
          {
            pmod_desc =
              Pmod_constraint
                ( {pmod_desc = Pmod_structure [{pstr_desc = Pstr_primitive _}]},
                  _ );
          };
      } ->
    true
  (* FIX #4881
     generated code from:
     {[
       external %private x : int -> int =  "x"
       [@@module "./x"]
     ]}
  *)
  | _ -> false

let check (x : Parsetree.structure) =
  Ext_list.iter x (fun x ->
      if not (no_type_defined x) then
        Location.raise_errorf ~loc:x.pstr_loc
          "the structure is not supported in local extension")
