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

type callback =
  [ `Str of string -> unit
  | `Str_loc of string -> Lexing.position -> unit
  | `Flo of string -> unit
  | `Flo_loc of string -> Lexing.position -> unit
  | `Bool of bool -> unit
  | `Obj of Ext_json_types.t Map_string.t -> unit
  | `Arr of Ext_json_types.t array -> unit
  | `Arr_loc of
    Ext_json_types.t array -> Lexing.position -> Lexing.position -> unit
  | `Null of unit -> unit
  | `Not_found of unit -> unit
  | `Id of Ext_json_types.t -> unit ]

type path = string list

type status = No_path | Found of Ext_json_types.t | Wrong_type of path

let test ?(fail = fun () -> ()) key (cb : callback)
    (m : Ext_json_types.t Map_string.t) =
  (match (Map_string.find_exn m key, cb) with
  | exception Not_found -> (
    match cb with
    | `Not_found f -> f ()
    | _ -> fail ())
  | True _, `Bool cb -> cb true
  | False _, `Bool cb -> cb false
  | Flo {flo = s}, `Flo cb -> cb s
  | Flo {flo = s; loc}, `Flo_loc cb -> cb s loc
  | Obj {map = b}, `Obj cb -> cb b
  | Arr {content}, `Arr cb -> cb content
  | Arr {content; loc_start; loc_end}, `Arr_loc cb ->
    cb content loc_start loc_end
  | Null _, `Null cb -> cb ()
  | Str {str = s}, `Str cb -> cb s
  | Str {str = s; loc}, `Str_loc cb -> cb s loc
  | any, `Id cb -> cb any
  | _, _ -> fail ());
  m

let loc_of (x : Ext_json_types.t) =
  match x with
  | True p | False p | Null p -> p
  | Str p -> p.loc
  | Arr p -> p.loc_start
  | Obj p -> p.loc
  | Flo p -> p.loc
