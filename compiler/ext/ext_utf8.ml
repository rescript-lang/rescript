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

exception Invalid_utf8 of string

let decode_utf8_string s =
  let len = String.length s in
  let rec loop acc index =
    if index = len then List.rev acc
    else
      let decoded = String.get_utf_8_uchar s index in
      if Uchar.utf_decode_is_valid decoded then
        loop
          (Uchar.to_int (Uchar.utf_decode_uchar decoded) :: acc)
          (index + Uchar.utf_decode_length decoded)
      else raise (Invalid_utf8 "Invalid UTF-8 sequence")
  in
  loop [] 0

let encode_codepoint c =
  let buf = Buffer.create 4 in
  Buffer.add_utf_8_uchar buf (Uchar.of_int c);
  Buffer.contents buf
