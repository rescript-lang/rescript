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

type byte = Single of int | Cont of int | Leading of int * int | Invalid

(** [classify chr] returns the {!byte} corresponding to [chr] *)
let classify chr =
  let c = int_of_char chr in
  (* Classify byte according to leftmost 0 bit *)
  if c land 0b1000_0000 = 0 then Single c
  else if
    (* c 0b0____*)
    c land 0b0100_0000 = 0
  then Cont (c land 0b0011_1111)
  else if
    (* c 0b10___*)
    c land 0b0010_0000 = 0
  then Leading (1, c land 0b0001_1111)
  else if
    (* c 0b110__*)
    c land 0b0001_0000 = 0
  then Leading (2, c land 0b0000_1111)
  else if
    (* c 0b1110_ *)
    c land 0b0000_1000 = 0
  then Leading (3, c land 0b0000_0111)
  else if
    (* c 0b1111_0___*)
    c land 0b0000_0100 = 0
  then Leading (4, c land 0b0000_0011)
  else if
    (* c 0b1111_10__*)
    c land 0b0000_0010 = 0
  then Leading (5, c land 0b0000_0001) (* c 0b1111_110__ *)
  else Invalid

exception Invalid_utf8 of string

(* when the first char is [Leading],
   TODO: need more error checking
   when out of bond
*)
let rec follow s n (c : int) offset =
  if n = 0 then (c, offset)
  else
    match classify s.[offset + 1] with
    | Cont cc -> follow s (n - 1) ((c lsl 6) lor (cc land 0x3f)) (offset + 1)
    | _ -> raise (Invalid_utf8 "Continuation byte expected")

let rec next s ~remaining offset =
  if remaining = 0 then offset
  else
    match classify s.[offset + 1] with
    | Cont _cc -> next s ~remaining:(remaining - 1) (offset + 1)
    | _ -> -1
    | exception _ -> -1
(* it can happen when out of bound *)

let decode_utf8_string s =
  let lst = ref [] in
  let add elem = lst := elem :: !lst in
  let rec decode_utf8_cont s i s_len =
    if i = s_len then ()
    else
      match classify s.[i] with
      | Single c ->
        add c;
        decode_utf8_cont s (i + 1) s_len
      | Cont _ -> raise (Invalid_utf8 "Unexpected continuation byte")
      | Leading (n, c) ->
        let c', i' = follow s n c i in
        add c';
        decode_utf8_cont s (i' + 1) s_len
      | Invalid -> raise (Invalid_utf8 "Invalid byte")
  in
  decode_utf8_cont s 0 (String.length s);
  List.rev !lst

(** To decode {j||j} we need verify in the ast so that we have better error 
    location, then we do the decode later
*)

(* let verify s loc =
   assert false *)

let encode_codepoint c =
  (* reused from syntax/src/res_utf8.ml *)
  let h2 = 0b1100_0000 in
  let h3 = 0b1110_0000 in
  let h4 = 0b1111_0000 in
  let cont_mask = 0b0011_1111 in
  if c <= 127 then (
    let bytes = (Bytes.create [@doesNotRaise]) 1 in
    Bytes.unsafe_set bytes 0 (Char.unsafe_chr c);
    Bytes.unsafe_to_string bytes)
  else if c <= 2047 then (
    let bytes = (Bytes.create [@doesNotRaise]) 2 in
    Bytes.unsafe_set bytes 0 (Char.unsafe_chr (h2 lor (c lsr 6)));
    Bytes.unsafe_set bytes 1
      (Char.unsafe_chr (0b1000_0000 lor (c land cont_mask)));
    Bytes.unsafe_to_string bytes)
  else if c <= 65535 then (
    let bytes = (Bytes.create [@doesNotRaise]) 3 in
    Bytes.unsafe_set bytes 0 (Char.unsafe_chr (h3 lor (c lsr 12)));
    Bytes.unsafe_set bytes 1
      (Char.unsafe_chr (0b1000_0000 lor ((c lsr 6) land cont_mask)));
    Bytes.unsafe_set bytes 2
      (Char.unsafe_chr (0b1000_0000 lor (c land cont_mask)));
    Bytes.unsafe_to_string bytes)
  else
    (* if c <= max then *)
    let bytes = (Bytes.create [@doesNotRaise]) 4 in
    Bytes.unsafe_set bytes 0 (Char.unsafe_chr (h4 lor (c lsr 18)));
    Bytes.unsafe_set bytes 1
      (Char.unsafe_chr (0b1000_0000 lor ((c lsr 12) land cont_mask)));
    Bytes.unsafe_set bytes 2
      (Char.unsafe_chr (0b1000_0000 lor ((c lsr 6) land cont_mask)));
    Bytes.unsafe_set bytes 3
      (Char.unsafe_chr (0b1000_0000 lor (c land cont_mask)));
    Bytes.unsafe_to_string bytes
