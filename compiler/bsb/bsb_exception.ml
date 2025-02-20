(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
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

type error =
  | Package_not_found of Bsb_pkg_types.t
  | Json_config of Ext_position.t * string
  | Invalid_spec of string
  | Conflict_module of string * string * string
  | No_implementation of string

exception Error of error

let error err = raise (Error err)

let package_not_found ~pkg = error (Package_not_found pkg)

let print (fmt : Format.formatter) (x : error) =
  match x with
  | Conflict_module (modname, dir1, dir2) ->
    Format.fprintf fmt
      "@{<error>Error:@} %s found in two directories: (%s, %s)\n\
       File names must be unique per project"
      modname dir1 dir2
  | No_implementation modname ->
    Format.fprintf fmt "@{<error>Error:@} %s does not have implementation file"
      modname
  | Package_not_found name ->
    let name = Bsb_pkg_types.to_string name in
    if Ext_string.equal name !Bs_version.package_name then
      Format.fprintf fmt
        "File \"bsconfig.json\", line 1\n\
         @{<error>Error:@} package @{<error>%s@} is not found\n\
         It's the basic, required package. If you have it installed globally,\n\
         Please run `npm link rescript` to make it available"
        name
    else
      Format.fprintf fmt
        "File \"bsconfig.json\", line 1\n\
         @{<error>Error:@} package @{<error>%s@} not found or built\n\
         - Did you install it?"
        name
  | Json_config (pos, s) ->
    Format.fprintf fmt
      "File %S, line %d:\n\
       @{<error>Error:@} %s \n\
       For more details, please check out the schema at \
       https://rescript-lang.org/docs/manual/latest/build-configuration-schema"
      pos.pos_fname pos.pos_lnum s
  | Invalid_spec s ->
    Format.fprintf fmt "@{<error>Error: Invalid bsconfig.json %s@}" s

let conflict_module modname dir1 dir2 =
  Error (Conflict_module (modname, dir1, dir2))

let no_implementation modname = error (No_implementation modname)

let errorf ~loc fmt =
  Format.ksprintf (fun s -> error (Json_config (loc, s))) fmt

let config_error config fmt =
  let loc = Ext_json.loc_of config in

  error (Json_config (loc, fmt))

let invalid_spec s = error (Invalid_spec s)

let () =
  Printexc.register_printer (fun x ->
      match x with
      | Error x -> Some (Format.asprintf "%a" print x)
      | _ -> None)
