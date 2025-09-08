(* Copyright (C) 2018- Hongbo Zhang, Authors of ReScript
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

let ( // ) = Filename.concat

type t = Global of string | Scope of string * scope

and scope = string

let to_string (x : t) =
  match x with
  | Global s -> s
  | Scope (s, scope) -> scope // s

let print fmt (x : t) =
  match x with
  | Global s -> Format.pp_print_string fmt s
  | Scope (name, scope) -> Format.fprintf fmt "%s/%s" scope name

let equal (x : t) y =
  match (x, y) with
  | Scope (a0, a1), Scope (b0, b1) -> a0 = b0 && a1 = b1
  | Global a0, Global b0 -> a0 = b0
  | Scope _, Global _ | Global _, Scope _ -> false

(**
   input: {[
     @hello/yy/xx
        hello/yy
   ]}
   FIXME: fix invalid input
   {[
     hello//xh//helo
   ]}
*)
let extract_pkg_name_and_file (s : string) =
  let len = String.length s in
  assert (len > 0);
  let v = String.unsafe_get s 0 in
  if v = '@' then (
    let scope_id = Ext_string.no_slash_idx s in
    assert (scope_id > 0);
    let pkg_id = Ext_string.no_slash_idx_from s (scope_id + 1) in
    let scope = String.sub s 0 scope_id in

    if pkg_id < 0 then
      (Scope (String.sub s (scope_id + 1) (len - scope_id - 1), scope), "")
    else
      ( Scope (String.sub s (scope_id + 1) (pkg_id - scope_id - 1), scope),
        String.sub s (pkg_id + 1) (len - pkg_id - 1) ))
  else
    let pkg_id = Ext_string.no_slash_idx s in
    if pkg_id < 0 then (Global s, "")
    else
      ( Global (String.sub s 0 pkg_id),
        String.sub s (pkg_id + 1) (len - pkg_id - 1) )

let string_as_package (s : string) : t =
  let len = String.length s in
  assert (len > 0);
  let v = String.unsafe_get s 0 in
  if v = '@' then (
    let scope_id = Ext_string.no_slash_idx s in
    assert (scope_id > 0);
    Scope
      (String.sub s (scope_id + 1) (len - scope_id - 1), String.sub s 0 scope_id))
  else Global s
