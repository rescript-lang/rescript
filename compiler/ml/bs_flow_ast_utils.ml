(* Copyright (C) 2020 - Hongbo Zhang, Authors of ReScript 
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

let offset_pos ({pos_lnum; pos_bol; pos_cnum} as loc : Lexing.position)
    ({line; column} : Loc.position) first_line_offset : Lexing.position =
  if line = 1 then {loc with pos_cnum = pos_cnum + column + first_line_offset}
  else {loc with pos_lnum = pos_lnum + line - 1; pos_cnum = pos_bol + column}

let flow_deli_offset deli =
  match deli with
  | None -> 1 (* length of '"'*)
  | Some deli -> String.length deli + 2
(* length of "{|"*)

(* Here the loc is  the payload loc *)
let check_flow_errors ~(loc : Location.t) ~offset
    (errors : (Loc.t * Parse_error.t) list) : unit =
  match errors with
  | [] -> ()
  | ({start; _end}, first_error) :: _ ->
    let loc_start = loc.loc_start in
    Location.prerr_warning
      {
        loc with
        loc_start = offset_pos loc_start start offset;
        loc_end = offset_pos loc_start _end offset;
      }
      (Bs_ffi_warning (Parse_error.PP.error first_error))
