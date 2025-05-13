(* Copyright (C) 2017 Hongbo Zhang, Authors of ReScript
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

module P = Ext_pp
module L = Js_dump_lit

(**
   https://stackoverflow.com/questions/9367572/rules-for-unquoted-javascript-object-literal-keys
   https://mathiasbynens.be/notes/javascript-properties
   https://mathiasbynens.be/notes/javascript-identifiers

   Let's not do smart things
   {[
     { 003 : 1} 
   ]}
   becomes 
   {[
     { 3 : 1}
   ]}
*)

(** used in printing keys 
    {[
      {"x" : x};;
      {x : x }
        {"50x" : 2 } GPR #1943
]}
    Note we can not treat it in the same way when printing
    [x.id] vs [{id : xx}]
    for example, id can be number in object literal
*)
let obj_property_no_need_quot s =
  let len = String.length s in
  if len > 0 then
    match String.unsafe_get s 0 with
    | '$' | '_' | 'a' .. 'z' | 'A' .. 'Z' ->
      Ext_string.for_all_from s 1 (function
        | 'a' .. 'z' | 'A' .. 'Z' | '$' | '_' | '0' .. '9' -> true
        | _ -> false)
    | _ -> false
  else false

(** used in property access 
    {[
      f.x ;;
      f["x"];;
    ]}
*)
let property_access f s =
  let s = Ext_ident.unwrap_uppercase_exotic s in
  if obj_property_no_need_quot s then (
    P.string f L.dot;
    P.string f s)
  else
    P.bracket_group f 1 (fun _ ->
        (* avoid cases like
           "0123", "123_456"
        *)
        match string_of_int (int_of_string s) with
        | s0 when s0 = s -> P.string f s
        | _ -> Js_dump_string.pp_string f s
        | exception _ -> Js_dump_string.pp_string f s)

let property_key (s : J.property_name) : string =
  match s with
  | Lit s ->
    let s = Ext_ident.unwrap_uppercase_exotic s in
    if obj_property_no_need_quot s then s else Js_dump_string.escape_to_string s
  | Symbol_name -> {|[Symbol.for("name")]|}
