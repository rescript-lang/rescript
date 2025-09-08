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

let () =
  let namespace = ref None in
  let dev_group = ref false in
  let argv = Sys.argv in
  let l = Array.length argv in
  let current = ref 1 in
  let rev_list = ref [] in
  while !current < l do
    let s = argv.(!current) in
    incr current;
    if s <> "" && s.[0] = '-' then (
      match s with
      | "-hash" -> incr current
      | "-bs-ns" ->
        let ns = argv.(!current) in
        namespace := Some ns;
        incr current
      | "-g" -> dev_group := true
      | s ->
        prerr_endline ("unknown options: " ^ s);
        prerr_endline "available options: -hash [hash]; -bs-ns [ns]; -g";
        exit 2)
    else rev_list := s :: !rev_list
  done;
  match !rev_list with
  | [x] -> Bsb_helper_depfile_gen.emit_d !dev_group !namespace x ""
  | [y; x] (* reverse order *) ->
    Bsb_helper_depfile_gen.emit_d !dev_group !namespace x y
  | _ -> ()
