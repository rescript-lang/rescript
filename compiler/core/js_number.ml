(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * Copyright (C) 2017 - Hongbo Zhang, Authors of ReScript 
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

type t = float

(* http://www.ecma-international.org/ecma-262/5.1/#sec-7.8.3
   http://caml.inria.fr/pub/docs/manual-ocaml/lex.html
   {[
     float-literal ::= [-](0...9){0...9|_}[.{0...9|_}][(e|E)][(e|E)[+|-](0...9){0...9|_}]
   ]}
   In ocaml, the interpretation of floating-point literals that
   fall outside the range of representable floating-point values is undefined.
   Also, (_) are accepted

   see https://github.com/ocaml/ocaml/pull/268 that ocaml will have HEXADECIMAL notation
   support in 4.3

   The Hex part is quite different
*)

let to_string (v : float) =
  if v = infinity then "Infinity"
  else if v = neg_infinity then "-Infinity"
  else if v <> v then "NaN"
  else
    let vint =
      int_of_float v
      (* TODO: check if 32-bits will loose some precision *)
    in
    if float_of_int vint = v then string_of_int vint
    else
      let s1 = Printf.sprintf "%.12g" v in
      if v = float_of_string s1 then s1
      else
        let s2 = Printf.sprintf "%.15g" v in
        if v = float_of_string s2 then s2 else Printf.sprintf "%.18g" v

let rec is_hex_format_aux (v : string) cur =
  if v.[cur] = '-' || v.[cur] = '+' then is_hex_format_ox v (cur + 1)
  else is_hex_format_ox v cur

and is_hex_format_ox v cur =
  v.[cur] = '0' && (v.[cur + 1] = 'x' || v.[cur + 1] = 'X')

let is_hex_format (v : string) = try is_hex_format_aux v 0 with _ -> false

(*
  call [to_string (float_of_string v)] 
  directly would loose some precision and lost some information
  like '3.0' -> '3'

*)
let rec aux (v : string) (buf : Buffer.t) i len =
  if i >= len then ()
  else
    let x = v.[i] in
    if x = '_' then aux v buf (i + 1) len
    else if x = '.' && i = len - 1 then ()
    else (
      Buffer.add_char buf x;
      aux v buf (i + 1) len)

let transform v len =
  let buf = Buffer.create len in
  let i = ref 0 in
  while !i + 1 < len && v.[!i] = '0' && v.[!i + 1] <> '.' do
    incr i
  done;
  aux v buf !i len;
  Buffer.contents buf

let caml_float_literal_to_js_string (float_str : string) : string =
  let len = String.length float_str in
  if len >= 2 && is_hex_format float_str then
    to_string (float_of_string float_str)
  else transform float_str len
