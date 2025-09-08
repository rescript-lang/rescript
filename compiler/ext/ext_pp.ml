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

module L = struct
  let space = " "

  let indent_str = "  "
end

let indent_length = String.length L.indent_str

type t = {
  output_string: string -> unit;
  output_char: char -> unit;
  flush: unit -> unit;
  mutable indent_level: int;
  mutable last_new_line: bool;
      (* only when we print newline, we print the indent *)
}

let from_channel chan =
  {
    output_string = (fun s -> output_string chan s);
    output_char = (fun c -> output_char chan c);
    flush = (fun _ -> flush chan);
    indent_level = 0;
    last_new_line = false;
  }

let from_buffer buf =
  {
    output_string = (fun s -> Buffer.add_string buf s);
    output_char = (fun c -> Buffer.add_char buf c);
    flush = (fun _ -> ());
    indent_level = 0;
    last_new_line = false;
  }

(* If we have [newline] in [s],
   all indentations will be broken
   in the future, we can detect this in [s]
*)
let string t s =
  t.output_string s;
  t.last_new_line <- false

let newline t =
  if not t.last_new_line then (
    t.output_char '\n';
    for _ = 0 to t.indent_level - 1 do
      t.output_string L.indent_str
    done;
    t.last_new_line <- true)

let at_least_two_lines t =
  if not t.last_new_line then t.output_char '\n';
  t.output_char '\n';
  for _ = 0 to t.indent_level - 1 do
    t.output_string L.indent_str
  done;
  t.last_new_line <- true

let force_newline t =
  t.output_char '\n';
  for _ = 0 to t.indent_level - 1 do
    t.output_string L.indent_str
  done;
  t.last_new_line <- true

let space t = string t L.space

let nspace t n = string t (String.make n ' ')

let group t i action =
  if i = 0 then action ()
  else
    let old = t.indent_level in
    t.indent_level <- t.indent_level + i;
    Ext_pervasives.finally ~clean:(fun _ -> t.indent_level <- old) () action

let vgroup = group

let paren t action =
  string t "(";
  let v = action () in
  string t ")";
  v

let brace fmt u =
  string fmt "{";
  (* break1 fmt ; *)
  let v = u () in
  string fmt "}";
  v

let bracket fmt u =
  string fmt "[";
  let v = u () in
  string fmt "]";
  v

let brace_vgroup st n action =
  string st "{";
  let v =
    vgroup st n (fun _ ->
        newline st;
        let v = action () in
        v)
  in
  force_newline st;
  string st "}";
  v

let bracket_vgroup st n action =
  string st "[";
  let v =
    vgroup st n (fun _ ->
        newline st;
        let v = action () in
        v)
  in
  force_newline st;
  string st "]";
  v

let bracket_group st n action = group st n (fun _ -> bracket st action)

let paren_vgroup st n action =
  string st "(";
  let v =
    group st n (fun _ ->
        newline st;
        let v = action () in
        v)
  in
  newline st;
  string st ")";
  v

let paren_group st n action = group st n (fun _ -> paren st action)

let cond_paren_group st b action =
  if b then paren_group st 0 action else action ()

let brace_group st n action = group st n (fun _ -> brace st action)

(* let indent t n =
   t.indent_level <- t.indent_level + n *)

let flush t () = t.flush ()
