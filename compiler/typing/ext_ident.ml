(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * 2017 - Hongbo Zhang, Authors of ReScript
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

let js_flag = 0b1_000 (* check with ocaml compiler *)

(* let js_module_flag = 0b10_000 (\* javascript external modules *\) *)
(* TODO:
    check name conflicts with javascript conventions
   {[
     Ext_ident.convert "^";;
     - : string = "$caret"
   ]}
*)
let js_object_flag = 0b100_000 (* javascript object flags *)

let is_js (i : Ident.t) = i.flags land js_flag <> 0

let is_js_or_global (i : Ident.t) = i.flags land (8 lor 1) <> 0

let make_js_object (i : Ident.t) = i.flags <- i.flags lor js_object_flag

(* It's a js function hard coded by js api, so when printing,
   it should preserve the name
*)
let create_js (name : string) : Ident.t = {name; flags = js_flag; stamp = 0}

let synthesized_flag = 0b1_000_000

let is_synthesized (i : Ident.t) = i.flags land synthesized_flag <> 0

let create = Ident.create

let create_tmp ?(name = Literals.tmp) () =
  let id = create name in
  id.flags <- id.flags lor synthesized_flag;
  id

let[@inline] convert ~op (c : char) : string =
  match c with
  | '*' -> "$star"
  | '\'' -> "$p"
  | '!' -> "$bang"
  | '>' -> "$great"
  | '<' -> "$less"
  | '=' -> "$eq"
  | '+' -> "$plus"
  | '-' -> if op then "$neg" else "$"
  | '@' -> "$at"
  | '^' -> "$caret"
  | '/' -> "$slash"
  | '|' -> "$pipe"
  | '.' -> "$dot"
  | '%' -> "$percent"
  | '~' -> "$tilde"
  | '#' -> "$hash"
  | ':' -> "$colon"
  | '?' -> "$question"
  | '&' -> "$amp"
  | '(' -> "$lpar"
  | ')' -> "$rpar"
  | '{' -> "$lbrace"
  | '}' -> "$lbrace"
  | '[' -> "$lbrack"
  | ']' -> "$rbrack"
  | _ -> "$unknown"
let[@inline] no_escape (c : char) =
  match c with
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '$' -> true
  | _ -> false

let is_uppercase_exotic name =
  let len = String.length name in
  len >= 3 && name.[0] = '\\' && name.[1] = '\"' && name.[len - 1] = '\"'

(* Keep in sync with the copy in [Res_scanner], which the parser uses, since
   the parser does not depend on ext. *)
let unwrap_uppercase_exotic name =
  if is_uppercase_exotic name then
    let len = String.length name in
    String.sub name 2 (len - 3)
  else name

let derived_name (id : Ident.t) ~suffix =
  unwrap_uppercase_exotic (Ident.name id) ^ suffix

exception Not_normal_letter of int
let name_mangle name =
  let len = String.length name in
  try
    for i = 0 to len - 1 do
      if not (no_escape (String.unsafe_get name i)) then
        raise_notrace (Not_normal_letter i)
    done;
    name (* Normal letter *)
  with Not_normal_letter i ->
    let buffer = Buffer.create len in
    for j = 0 to len - 1 do
      let c = String.unsafe_get name j in
      if no_escape c then Buffer.add_char buffer c
      else Buffer.add_string buffer (convert ~op:(i = 0) c)
    done;
    Buffer.contents buffer

(**
   [convert name] if [name] is a js keyword or js global, add "$$"
   otherwise do the name mangling to make sure ocaml identifier it is
   a valid js identifier
*)
let convert (name : string) =
  let name = unwrap_uppercase_exotic name in
  if Js_reserved_map.is_js_keyword name || Js_reserved_map.is_js_global name
  then "$$" ^ name
  else name_mangle name

(** keyword could be used in property *)

(* It is currently made a persistent ident to avoid fresh ids
    which would result in different signature files
   - other solution: use lazy values
*)
let compare (x : Ident.t) (y : Ident.t) =
  let u = x.stamp - y.stamp in
  if u = 0 then Ext_string.compare x.name y.name else u

let equal (x : Ident.t) (y : Ident.t) =
  if x.stamp <> 0 then x.stamp = y.stamp else y.stamp = 0 && x.name = y.name
