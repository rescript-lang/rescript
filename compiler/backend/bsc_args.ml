(* Copyright (C) 2020- Hongbo Zhang, Authors of ReScript
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

exception Bad = Arg.Bad
exception Help of string

let bad_arg s = raise_notrace (Bad s)

type t = (string * string) array

let ( +> ) = Buffer.add_string

let usage_b (buf : Buffer.t) ~usage (speclist : t) =
  buf +> usage;
  buf +> "\nOptions:\n";
  let max_col = ref 0 in
  speclist
  |> Array.iter (fun (key, _) ->
      if String.length key > !max_col then max_col := String.length key);
  speclist
  |> Array.iter (fun (key, doc) ->
      if not (Ext_string.starts_with doc "*internal*") then (
        buf +> "  ";
        buf +> key;
        buf +> String.make (!max_col - String.length key + 2) ' ';
        let cur = ref 0 in
        let doc_length = String.length doc in
        while !cur < doc_length do
          match String.index_from_opt doc !cur '\n' with
          | None ->
            if !cur <> 0 then (
              buf +> "\n";
              buf +> String.make (!max_col + 4) ' ');
            buf +> String.sub doc !cur (String.length doc - !cur);
            cur := doc_length
          | Some new_line_pos ->
            if !cur <> 0 then (
              buf +> "\n";
              buf +> String.make (!max_col + 4) ' ');
            buf +> String.sub doc !cur (new_line_pos - !cur);
            cur := new_line_pos + 1
        done;
        buf +> "\n"))

let usage_string ~usage (speclist : t) =
  let b = Buffer.create 200 in
  usage_b b ~usage speclist;
  Buffer.contents b
