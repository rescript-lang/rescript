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

type error =
  | Invalid_code_point
  | Unterminated_backslash
  | Invalid_hex_escape
  | Invalid_unicode_escape
  | Invalid_unicode_codepoint_escape

let pp_error fmt err =
  Format.pp_print_string fmt
  @@
  match err with
  | Invalid_code_point -> "Invalid code point"
  | Unterminated_backslash -> "\\ ended unexpectedly"
  | Invalid_hex_escape -> "Invalid \\x escape"
  | Invalid_unicode_escape -> "Invalid \\u escape"
  | Invalid_unicode_codepoint_escape ->
    "Invalid \\u{…} codepoint escape sequence"

type exn += Error of int (* offset *) * error

let error ~loc error = raise (Error (loc, error))

(** Note the [loc] really should be the utf8-offset, it has nothing to do with our 
    escaping mechanism
*)
(* we can not just print new line in ES5
   seems we don't need
   escape "\b" "\f"
   we need escape "\n" "\r" since
   ocaml multiple-line allows [\n]
   visual input while es5 string
   does not*)

let rec check_and_transform (loc : int) (buf : Buffer.t) (s : string)
    (byte_offset : int) (s_len : int) =
  if byte_offset = s_len then ()
  else
    let current_char = s.[byte_offset] in
    match Ext_utf8.classify current_char with
    | Single 92 (* '\\' *) ->
      escape_code (loc + 1) buf s (byte_offset + 1) s_len
    | Single 34 ->
      Buffer.add_string buf "\\\"";
      check_and_transform (loc + 1) buf s (byte_offset + 1) s_len
    | Single 10 ->
      Buffer.add_string buf "\\n";
      check_and_transform (loc + 1) buf s (byte_offset + 1) s_len
    | Single 13 ->
      Buffer.add_string buf "\\r";
      check_and_transform (loc + 1) buf s (byte_offset + 1) s_len
    | Single _ ->
      Buffer.add_char buf current_char;
      check_and_transform (loc + 1) buf s (byte_offset + 1) s_len
    | Invalid | Cont _ -> error ~loc Invalid_code_point
    | Leading (n, _) ->
      let i' = Ext_utf8.next s ~remaining:n byte_offset in
      if i' < 0 then error ~loc Invalid_code_point
      else (
        for k = byte_offset to i' do
          Buffer.add_char buf s.[k]
        done;
        check_and_transform (loc + 1) buf s (i' + 1) s_len)

(* we share the same escape sequence with js *)
and escape_code loc buf s offset s_len =
  if offset >= s_len then error ~loc Unterminated_backslash
  else Buffer.add_char buf '\\';
  let cur_char = s.[offset] in
  match cur_char with
  | '\\' | 'b' | 't' | 'n' | 'v' | 'f' | 'r' | '0' | '$' ->
    Buffer.add_char buf cur_char;
    check_and_transform (loc + 1) buf s (offset + 1) s_len
  | 'u' ->
    if offset + 1 >= s_len then error ~loc Invalid_unicode_escape
    else (
      Buffer.add_char buf cur_char;
      let next_char = s.[offset + 1] in
      match next_char with
      | '{' ->
        Buffer.add_char buf next_char;
        unicode_codepoint_escape (loc + 2) buf s (offset + 2) s_len
      | _ -> unicode (loc + 1) buf s (offset + 1) s_len)
  | 'x' ->
    Buffer.add_char buf cur_char;
    two_hex (loc + 1) buf s (offset + 1) s_len
  | _ ->
    (* Regular characters, like `a` in `\a`,
     * are valid escape sequences *)
    Buffer.add_char buf cur_char;
    check_and_transform (loc + 1) buf s (offset + 1) s_len

and two_hex loc buf s offset s_len =
  if offset + 1 >= s_len then error ~loc Invalid_hex_escape;
  (*Location.raise_errorf ~loc "\\x need at least two chars";*)
  let a, b = (s.[offset], s.[offset + 1]) in
  if Ext_char.valid_hex a && Ext_char.valid_hex b then (
    Buffer.add_char buf a;
    Buffer.add_char buf b;
    check_and_transform (loc + 2) buf s (offset + 2) s_len)
  else error ~loc Invalid_hex_escape
(*Location.raise_errorf ~loc "%c%c is not a valid hex code" a b*)

and unicode loc buf s offset s_len =
  if offset + 3 >= s_len then error ~loc Invalid_unicode_escape
    (*Location.raise_errorf ~loc "\\u need at least four chars"*);
  let a0, a1, a2, a3 =
    (s.[offset], s.[offset + 1], s.[offset + 2], s.[offset + 3])
  in
  if
    Ext_char.valid_hex a0 && Ext_char.valid_hex a1 && Ext_char.valid_hex a2
    && Ext_char.valid_hex a3
  then (
    Buffer.add_char buf a0;
    Buffer.add_char buf a1;
    Buffer.add_char buf a2;
    Buffer.add_char buf a3;
    check_and_transform (loc + 4) buf s (offset + 4) s_len)
  else error ~loc Invalid_unicode_escape

(*Location.raise_errorf ~loc "%c%c%c%c is not a valid unicode point"
  a0 a1 a2 a3 *)
(* http://www.2ality.com/2015/01/es6-strings.html
   console.log('\uD83D\uDE80'); (* ES6*)
   console.log('\u{1F680}');
*)

(* ES6 unicode codepoint escape sequences: \u{…}
   https://262.ecma-international.org/6.0/#sec-literals-string-literals *)
and unicode_codepoint_escape loc buf s offset s_len =
  if offset >= s_len then error ~loc Invalid_unicode_codepoint_escape
  else
    let cur_char = s.[offset] in
    match cur_char with
    | '}' ->
      Buffer.add_char buf cur_char;
      let x = ref 0 in
      for ix = loc to offset - 1 do
        let c = s.[ix] in
        let value =
          match c with
          | '0' .. '9' -> Char.code c - 48
          | 'a' .. 'f' -> Char.code c - Char.code 'a' + 10
          | 'A' .. 'F' -> Char.code c + 32 - Char.code 'a' + 10
          | _ -> 16
          (* larger than any legal value, unicode_codepoint_escape only makes progress if we have valid hex symbols *)
        in
        (* too long escape sequence will result in an overflow, perform an upperbound check *)
        if !x > 0x10FFFF then error ~loc Invalid_unicode_codepoint_escape
        else x := (!x * 16) + value
      done;
      if Uchar.is_valid !x then
        check_and_transform (offset + 1) buf s (offset + 1) s_len
      else error ~loc Invalid_unicode_codepoint_escape
    | _ ->
      if Ext_char.valid_hex cur_char then (
        Buffer.add_char buf cur_char;
        unicode_codepoint_escape loc buf s (offset + 1) s_len)
      else error ~loc Invalid_unicode_codepoint_escape

let transform_test s =
  let s_len = String.length s in
  let buf = Buffer.create (s_len * 2) in
  check_and_transform 0 buf s 0 s_len;
  Buffer.contents buf

let transform loc s =
  let s_len = String.length s in
  let buf = Buffer.create (s_len * 2) in
  try
    check_and_transform 0 buf s 0 s_len;
    Buffer.contents buf
  with Error (offset, error) ->
    Location.raise_errorf ~loc "Offset: %d, %a" offset pp_error error

let rec check_no_escapes_or_unicode (s : string) (byte_offset : int)
    (s_len : int) =
  if byte_offset = s_len then true
  else
    let current_char = s.[byte_offset] in
    match Ext_utf8.classify current_char with
    | Single 92 (* '\\' *) -> false
    | Single _ -> check_no_escapes_or_unicode s (byte_offset + 1) s_len
    | Invalid | Cont _ | Leading _ -> false

let simple_comparison s = check_no_escapes_or_unicode s 0 (String.length s)
