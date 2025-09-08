(* Copyright (C) 2017- Authors of ReScript
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

val try_split_module_name : string -> (string * string) option

(* Note  we have to output uncapitalized file Name,
   or at least be consistent, since by reading cmi file on Case insensitive OS, we don't really know it is `list.cmi` or `List.cmi`, so that `require (./list.js)` or `require(./List.js)`
   relevant issues: #1609, #913

   #1933 when removing ns suffix, don't pass the bound
   of basename
*)
val change_ext_ns_suffix : string -> string -> string

val js_name_of_modulename : string -> Ext_js_file_kind.case -> string -> string
(** [js_name_of_modulename ~little A-Ns]
*)

(* TODO handle cases like
   '@angular/core'
   its directory structure is like
   {[
     @angular
     |-------- core
   ]}
*)
val is_valid_npm_package_name : string -> bool

val namespace_of_package_name : string -> string
