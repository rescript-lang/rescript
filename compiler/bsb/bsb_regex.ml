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

let string_after s n = String.sub s n (String.length s - n)

(* There seems to be a bug in {!Str.global_substitute}
   {[
     Str.global_substitute (Str.regexp "\\${rescript:\\([-a-zA-Z0-9]+\\)}") (fun x -> (x^":found")) {|   ${rescript:hello-world}  ${rescript:x} ${x}|}  ;;
     - : bytes =
     "      ${rescript:hello-world}  ${rescript:x} ${x}:found     ${rescript:hello-world}  ${rescript:x} ${x}:found ${x}"
   ]}
*)
let global_substitute text ~reg:expr repl_fun =
  let text_len = String.length text in
  let expr = Str.regexp expr in
  let rec replace accu start last_was_empty =
    let startpos = if last_was_empty then start + 1 else start in
    if startpos > text_len then string_after text start :: accu
    else
      match Str.search_forward expr text startpos with
      | exception Not_found -> string_after text start :: accu
      | pos ->
        let end_pos = Str.match_end () in
        let matched = Str.matched_string text in
        let groups =
          let rec aux n acc =
            match Str.matched_group n text with
            | exception (Not_found | Invalid_argument _) -> acc
            | v -> aux (succ n) (v :: acc)
          in
          aux 1 []
        in
        let repl_text = repl_fun matched groups in
        replace
          (repl_text :: String.sub text start (pos - start) :: accu)
          end_pos (end_pos = pos)
  in
  String.concat "" (List.rev (replace [] 0 false))
