(* Copyright (C) 2015 - 2016 Bloomberg Finance L.P.
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

(*
   {[ split " test_unsafe_obj_ffi_ppx.cmi" ~keep_empty:false ' ']}
*)
let split_by ?(keep_empty = false) is_delim str =
  let len = String.length str in
  let rec loop acc last_pos pos =
    if pos = -1 then
      if last_pos = 0 && not keep_empty then acc
      else String.sub str 0 last_pos :: acc
    else if is_delim str.[pos] then
      let new_len = last_pos - pos - 1 in
      if new_len <> 0 || keep_empty then
        let v = String.sub str (pos + 1) new_len in
        loop (v :: acc) pos (pos - 1)
      else loop acc pos (pos - 1)
    else loop acc last_pos (pos - 1)
  in
  loop [] len (len - 1)

let split ?keep_empty str on =
  if str = "" then [] else split_by ?keep_empty (fun x -> (x : char) = on) str

let starts_with s beg =
  let beg_len = String.length beg in
  let s_len = String.length s in
  beg_len <= s_len
  &&
  let i = ref 0 in
  while !i < beg_len && String.unsafe_get s !i = String.unsafe_get beg !i do
    incr i
  done;
  !i = beg_len

(* let check_suffix_case = ends_with  *)
(* let check_suffix_case_then_chop = ends_with_then_chop *)

(* let check_any_suffix_case_then_chop s suffixes =
   let rec aux suffixes =
    match suffixes with
    | [] -> None
    | x::xs ->
      let id = ends_with_index s x in
      if id >= 0 then Some (String.sub s 0 id)
      else aux xs in
   aux suffixes     *)

(* it is unsafe to expose such API as unsafe since
   user can provide bad input range

*)
let rec unsafe_for_all_range s ~start ~finish p =
  start > finish
  || p (String.unsafe_get s start)
     && unsafe_for_all_range s ~start:(start + 1) ~finish p

let for_all_from s start p =
  let len = String.length s in
  if start < 0 then invalid_arg "Ext_string.for_all_from"
  else unsafe_for_all_range s ~start ~finish:(len - 1) p

let for_all s (p : char -> bool) =
  unsafe_for_all_range s ~start:0 ~finish:(String.length s - 1) p

let is_empty s = String.length s = 0

let tail_from s x =
  let len = String.length s in
  if x > len then
    invalid_arg ("Ext_string.tail_from " ^ s ^ " : " ^ string_of_int x)
  else String.sub s x (len - x)

let equal (x : string) y = x = y

(* let rec index_rec s lim i c =
   if i >= lim then -1 else
   if String.unsafe_get s i = c then i
   else index_rec s lim (i + 1) c *)

(* let index_next s i c =
   index_count s i c 1  *)

let rec rindex_rec s i c =
  if i < 0 then i
  else if String.unsafe_get s i = c then i
  else rindex_rec s (i - 1) c

let rindex_neg s c = rindex_rec s (String.length s - 1) c

(** TODO: can be improved to return a positive integer instead *)
let rec unsafe_no_char x ch i last_idx =
  i > last_idx
  || (String.unsafe_get x i <> ch && unsafe_no_char x ch (i + 1) last_idx)

let replace_backward_slash (x : string) =
  let len = String.length x in
  if unsafe_no_char x '\\' 0 (len - 1) then x
  else
    String.map
      (function
        | '\\' -> '/'
        | x -> x)
      x

let compare = Ext_platform_primitives.string_length_based_compare
let single_colon = ":"

let capitalize_ascii (s : string) : string =
  if String.length s = 0 then s
  else
    let c = String.unsafe_get s 0 in
    if
      (c >= 'a' && c <= 'z')
      || (c >= '\224' && c <= '\246')
      || (c >= '\248' && c <= '\254')
    then (
      let uc = Char.unsafe_chr (Char.code c - 32) in
      let bytes = Bytes.of_string s in
      Bytes.unsafe_set bytes 0 uc;
      Bytes.unsafe_to_string bytes)
    else s

let capitalize_sub (s : string) len : string =
  let slen = String.length s in
  if len < 0 || len > slen then invalid_arg "Ext_string.capitalize_sub"
  else if len = 0 then ""
  else
    let bytes = Bytes.create len in
    let uc =
      let c = String.unsafe_get s 0 in
      if
        (c >= 'a' && c <= 'z')
        || (c >= '\224' && c <= '\246')
        || (c >= '\248' && c <= '\254')
      then Char.unsafe_chr (Char.code c - 32)
      else c
    in
    Bytes.unsafe_set bytes 0 uc;
    for i = 1 to len - 1 do
      Bytes.unsafe_set bytes i (String.unsafe_get s i)
    done;
    Bytes.unsafe_to_string bytes

let uncapitalize_ascii = String.uncapitalize_ascii

external ( .![] ) : string -> int -> int = "%string_unsafe_get"

let is_valid_hash_number (x : string) =
  let len = String.length x in
  len > 0
  &&
  let a = x.![0] in
  a <= 57
  &&
  if len > 1 then
    a > 48
    && for_all_from x 1 (function
      | '0' .. '9' -> true
      | _ -> false)
  else a >= 48

let hash_number_as_i32_exn (x : string) : int32 = Int32.of_string x
