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

let good_hint_name module_name offset =
  let len = String.length module_name in
  len > offset
  && (function
       | 'a' .. 'z' | 'A' .. 'Z' -> true
       | _ -> false)
       (String.unsafe_get module_name offset)
  && Ext_string.for_all_from module_name (offset + 1) (function
       | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true
       | _ -> false)

let rec collect_start buf s off len =
  if off >= len then ()
  else
    let next = succ off in
    match String.unsafe_get s off with
    | 'a' .. 'z' as c ->
      Ext_buffer.add_char buf (Char.uppercase_ascii c);
      collect_next buf s next len
    | 'A' .. 'Z' as c ->
      Ext_buffer.add_char buf c;
      collect_next buf s next len
    | _ -> collect_start buf s next len

and collect_next buf s off len =
  if off >= len then ()
  else
    let next = off + 1 in
    match String.unsafe_get s off with
    | ('a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_') as c ->
      Ext_buffer.add_char buf c;
      collect_next buf s next len
    | '.' | '-' -> collect_start buf s next len
    | _ -> collect_next buf s next len

(** This is for a js exeternal module, we can change it when printing
    for example
    {[
      var React$1 = require('react');
      React$1.render(..)
    ]}
    Given a name, if duplicated, they should  have the same id
*)
let js_id_name_of_hint_name module_name =
  let i = Ext_string.rindex_neg module_name '/' in
  if i >= 0 then (
    let offset = succ i in
    if good_hint_name module_name offset then
      Ext_string.capitalize_ascii (Ext_string.tail_from module_name offset)
    else
      let str_len = String.length module_name in
      let buf = Ext_buffer.create str_len in
      collect_start buf module_name offset str_len;
      if Ext_buffer.is_empty buf then Ext_string.capitalize_ascii module_name
      else Ext_buffer.contents buf)
  else if good_hint_name module_name 0 then
    Ext_string.capitalize_ascii module_name
  else
    let str_len = String.length module_name in
    let buf = Ext_buffer.create str_len in
    collect_start buf module_name 0 str_len;
    if Ext_buffer.is_empty buf then module_name else Ext_buffer.contents buf
