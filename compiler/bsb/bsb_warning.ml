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

type warning_error =
  | Warn_error_false
  (* default [false] to make our changes non-intrusive *)
  | Warn_error_true
  | Warn_error_number of string

type t0 = {number: string option; error: warning_error}

type nonrec t = t0 option

let use_default = None

let prepare_warning_concat ~(beg : bool) s =
  let s = Ext_string.trim s in
  if s = "" then s
  else
    match s.[0] with
    | '0' .. '9' -> if beg then "-w +" ^ s else "+" ^ s
    | 'a' .. 'z' -> if beg then "-w " ^ s else "-" ^ s
    | 'A' .. 'Z' -> if beg then "-w " ^ s else "+" ^ s
    | _ -> if beg then "-w " ^ s else s

let to_merlin_string x =
  "-w " ^ Bsc_warnings.defaults_w
  ^
  let customize =
    match x with
    | Some {number = None} | None -> Ext_string.empty
    | Some {number = Some x} -> prepare_warning_concat ~beg:false x
  in
  if customize = "" then customize else customize ^ "-40-42-61"
(* see #4406 to avoid user pass A
   Sync up with {!Warnings.report}
*)

let from_map (m : Ext_json_types.t Map_string.t) =
  let number_opt = Map_string.find_opt m Bsb_build_schemas.number in
  let error_opt = Map_string.find_opt m Bsb_build_schemas.error in
  match (number_opt, error_opt) with
  | None, None -> None
  | _, _ ->
    let error =
      match error_opt with
      | Some (True _) -> Warn_error_true
      | Some (False _) -> Warn_error_false
      | Some (Str {str}) -> Warn_error_number str
      | Some x -> Bsb_exception.config_error x "expect true/false or string"
      | None -> Warn_error_false
      (* To make it less intrusive : warning error has to be enabled*)
    in
    let number =
      match number_opt with
      | Some (Str {str = number}) -> Some number
      | None -> None
      | Some x -> Bsb_exception.config_error x "expect a string"
    in
    Some {number; error}

let to_bsb_string ~(package_kind : Bsb_package_kind.t) warning =
  match package_kind with
  | Toplevel -> (
    match warning with
    | None -> Ext_string.empty
    | Some warning -> (
      (match warning.number with
      | None -> Ext_string.empty
      | Some x -> prepare_warning_concat ~beg:true x)
      ^
      match warning.error with
      | Warn_error_true -> " -warn-error A"
      | Warn_error_number y -> " -warn-error " ^ y
      | Warn_error_false -> Ext_string.empty))
  | Dependency _ -> " -w a"
(* TODO: this is the current default behavior *)
