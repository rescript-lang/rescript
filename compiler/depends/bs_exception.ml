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
  | Cmj_not_found of string
  | Js_not_found of string
  | Bs_cyclic_depends of string list
  | Bs_duplicated_module of string * string
  | Bs_duplicate_exports of string (* gpr_974 *)
  | Bs_package_not_found of string
  | Bs_main_not_exist of string
  | Bs_invalid_path of string
  | Missing_ml_dependency of string
  | Dependency_script_module_dependent_not of string
      (** TODO: we need add location handling *)

exception Error of error

let error err = raise (Error err)

let report_error ppf = function
  | Dependency_script_module_dependent_not s ->
    Format.fprintf ppf
      "%s is compiled in script mode while its dependent is not" s
  | Missing_ml_dependency s ->
    Format.fprintf ppf "Missing dependency %s in search path" s
  | Cmj_not_found s ->
    Format.fprintf ppf
      "%s not found, it means either the module does not exist or it is a \
       namespace"
      s
  | Js_not_found s ->
    Format.fprintf ppf "%s not found, needed in script mode " s
  | Bs_cyclic_depends str ->
    Format.fprintf ppf "Cyclic depends : @[%a@]"
      (Format.pp_print_list ~pp_sep:Format.pp_print_space Format.pp_print_string)
      str
  | Bs_duplicate_exports str -> Format.fprintf ppf "%s is exported twice" str
  | Bs_duplicated_module (a, b) ->
    Format.fprintf ppf
      "The build system does not support two files with same names yet %s, %s" a
      b
  | Bs_main_not_exist main -> Format.fprintf ppf "File %s not found " main
  | Bs_package_not_found package ->
    Format.fprintf ppf
      "Package %s not found or %s/lib/ocaml does not exist or set \
       npm_config_prefix correctly"
      package package
  | Bs_invalid_path path -> Format.pp_print_string ppf ("Invalid path: " ^ path)

let () =
  Location.register_error_of_exn (function
    | Error err -> Some (Location.error_of_printer_file report_error err)
    | _ -> None)
