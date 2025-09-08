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

let is_dir_sep_unix c = c = '/'

let is_dir_sep_win_cygwin c = c = '/' || c = '\\' || c = ':'

let is_dir_sep = if Sys.unix then is_dir_sep_unix else is_dir_sep_win_cygwin

(* reference ninja.cc IsKnownShellSafeCharacter *)
let maybe_quote (s : string) =
  let noneed_quote =
    Ext_string.for_all s (function
      | '0' .. '9' | 'a' .. 'z' | 'A' .. 'Z' | '_' | '+' | '-' | '.' | '/' | '@'
        ->
        true
      | _ -> false)
  in
  if noneed_quote then s else Filename.quote s

let chop_extension_maybe name =
  let rec search_dot i =
    if i < 0 || is_dir_sep (String.unsafe_get name i) then name
    else if String.unsafe_get name i = '.' then String.sub name 0 i
    else search_dot (i - 1)
  in
  search_dot (String.length name - 1)

let get_extension_maybe name =
  let name_len = String.length name in
  let rec search_dot name i name_len =
    if i < 0 || is_dir_sep (String.unsafe_get name i) then ""
    else if String.unsafe_get name i = '.' then String.sub name i (name_len - i)
    else search_dot name (i - 1) name_len
  in
  search_dot name (name_len - 1) name_len

let chop_all_extensions_maybe name =
  let rec search_dot i last =
    if i < 0 || is_dir_sep (String.unsafe_get name i) then
      match last with
      | None -> name
      | Some i -> String.sub name 0 i
    else if String.unsafe_get name i = '.' then search_dot (i - 1) (Some i)
    else search_dot (i - 1) last
  in
  search_dot (String.length name - 1) None

let new_extension name (ext : string) =
  let rec search_dot name i ext =
    if i < 0 || is_dir_sep (String.unsafe_get name i) then name ^ ext
    else if String.unsafe_get name i = '.' then (
      let ext_len = String.length ext in
      let buf = Bytes.create (i + ext_len) in
      Bytes.blit_string name 0 buf 0 i;
      Bytes.blit_string ext 0 buf i ext_len;
      Bytes.unsafe_to_string buf)
    else search_dot name (i - 1) ext
  in
  search_dot name (String.length name - 1) ext

(** TODO: improve efficiency
    given a path, calcuate its module name 
    Note that `ocamlc.opt -c aa.xx.mli` gives `aa.xx.cmi`
    we can not strip all extensions, otherwise
    we can not tell the difference between "x.cpp.ml" 
    and "x.ml"
*)
let module_name name =
  let rec search_dot i name =
    if i < 0 then Ext_string.capitalize_ascii name
    else if String.unsafe_get name i = '.' then Ext_string.capitalize_sub name i
    else search_dot (i - 1) name
  in
  let name = Filename.basename name in
  let name_len = String.length name in
  search_dot (name_len - 1) name

type module_info = {module_name: string; case: bool}

let rec valid_module_name_aux name off len =
  if off >= len then true
  else
    let c = String.unsafe_get name off in
    match c with
    | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' | '\'' | '.' | '[' | ']' ->
      valid_module_name_aux name (off + 1) len
    | _ -> false

type state = Invalid | Upper | Lower

let valid_module_name name len =
  if len = 0 then Invalid
  else
    let c = String.unsafe_get name 0 in
    match c with
    | 'A' .. 'Z' -> if valid_module_name_aux name 1 len then Upper else Invalid
    | 'a' .. 'z' | '0' .. '9' | '_' | '[' | ']' ->
      if valid_module_name_aux name 1 len then Lower else Invalid
    | _ -> Invalid

let as_module ~basename =
  let rec search_dot i name name_len =
    if i < 0 then
      (* Input e.g, [a_b] *)
      match valid_module_name name name_len with
      | Invalid -> None
      | Upper -> Some {module_name = name; case = true}
      | Lower ->
        Some {module_name = Ext_string.capitalize_ascii name; case = false}
    else if String.unsafe_get name i = '.' then
      (*Input e.g, [A_b] *)
      match valid_module_name name i with
      | Invalid -> None
      | Upper ->
        Some {module_name = Ext_string.capitalize_sub name i; case = true}
      | Lower ->
        Some {module_name = Ext_string.capitalize_sub name i; case = false}
    else search_dot (i - 1) name name_len
  in
  let name_len = String.length basename in
  search_dot (name_len - 1) basename name_len
