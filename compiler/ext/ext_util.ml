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

(**
   {[
     (power_2_above 16 63 = 64)
       (power_2_above 16 76 = 128)
   ]}
*)
let rec power_2_above x n =
  if x >= n then x
  else if x * 2 > Sys.max_array_length then x
  else power_2_above (x * 2) n

let stats_to_string
    ({num_bindings; num_buckets; max_bucket_length; bucket_histogram} :
      Hashtbl.statistics) =
  Printf.sprintf "bindings: %d,buckets: %d, longest: %d, hist:[%s]" num_bindings
    num_buckets max_bucket_length
    (String.concat ","
       (Array.to_list (Array.map string_of_int bucket_histogram)))

let string_of_int_as_char (i : int) : string =
  if i <= 255 && i >= 0 then Format.asprintf "%C" (Char.unsafe_chr i)
  else
    let str =
      match Char.unsafe_chr i with
      | '\'' -> "\\'"
      | '\\' -> "\\\\"
      | '\n' -> "\\n"
      | '\t' -> "\\t"
      | '\r' -> "\\r"
      | '\b' -> "\\b"
      | ' ' .. '~' as c ->
        let s = (Bytes.create [@doesNotRaise]) 1 in
        Bytes.unsafe_set s 0 c;
        Bytes.unsafe_to_string s
      | _ -> Ext_utf8.encode_codepoint i
    in
    Printf.sprintf "\'%s\'" str
