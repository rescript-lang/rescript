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

[@@@warning "+9"]

type module_system = Ext_module_system.t
(* ignore node_modules, just calcluating relative path *)

(* ocamlopt could not optimize such simple case..*)
let compatible (dep : module_system) (query : module_system) =
  match query with
  | Commonjs -> dep = Commonjs
  | Esmodule -> dep = Esmodule
  | Es6_global -> dep = Es6_global || dep = Esmodule
(* As a dependency Leaf Node, it is the same either [global] or [not] *)

type package_info = {module_system: module_system; path: string; suffix: string}

type package_name = Pkg_empty | Pkg_runtime | Pkg_normal of string

let ( // ) = Filename.concat

(* in runtime lib, [es6] and [es6] are treated the same wway *)
let runtime_dir_of_module_system (ms : module_system) =
  match ms with
  | Commonjs -> "js"
  | Esmodule | Es6_global -> "es6"

let runtime_package_path (ms : module_system) js_file =
  Runtime_package.name // "lib" // runtime_dir_of_module_system ms // js_file

type t = {name: package_name; module_systems: package_info list}

let runtime_package_specs : t =
  {
    name = Pkg_runtime;
    module_systems =
      [
        {module_system = Esmodule; path = "lib/es6"; suffix = Literals.suffix_js};
        {module_system = Commonjs; path = "lib/js"; suffix = Literals.suffix_js};
      ];
  }

let same_package_by_name (x : t) (y : t) =
  match x.name with
  | Pkg_empty -> y.name = Pkg_empty
  | Pkg_runtime -> y.name = Pkg_runtime
  | Pkg_normal s -> (
    match y.name with
    | Pkg_normal y -> s = y
    | Pkg_empty | Pkg_runtime -> false)

let is_runtime_package (x : t) = x.name = Pkg_runtime

let iter (x : t) cb = Ext_list.iter x.module_systems cb

let map (x : t) cb = Ext_list.map x.module_systems cb

(* let equal (x : t) ({name; module_systems}) =
    x.name = name &&
    Ext_list.for_all2_no_exn
      x.module_systems module_systems
      (fun (a0,a1) (b0,b1) -> a0 = b0 && a1 = b1) *)

(* we don't want force people to use package *)

(** 
   TODO: not allowing user to provide such specific package name 
   For empty package, [-bs-package-output] does not make sense
   it is only allowed to generate commonjs file in the same directory
*)
let empty : t = {name = Pkg_empty; module_systems = []}

let from_name (name : string) : t =
  {name = Pkg_normal name; module_systems = []}

let is_empty (x : t) = x.name = Pkg_empty

let string_of_module_system (ms : module_system) =
  match ms with
  | Commonjs -> "CommonJS"
  | Esmodule -> "ESModule"
  | Es6_global -> "Es6_global"

let module_system_of_string package_name : module_system option =
  match package_name with
  | "commonjs" -> Some Commonjs
  | "esmodule" | "es6" -> Some Esmodule
  | "es6-global" -> Some Es6_global
  | _ -> None

let dump_package_info (fmt : Format.formatter)
    ({module_system = ms; path = name; suffix} : package_info) =
  Format.fprintf fmt "@[%s@ %s@ %s@]" (string_of_module_system ms) name suffix

let dump_package_name fmt (x : package_name) =
  match x with
  | Pkg_empty -> Format.fprintf fmt "@empty_pkg@"
  | Pkg_normal s -> Format.pp_print_string fmt s
  | Pkg_runtime -> Format.pp_print_string fmt "@runtime"

let dump_packages_info (fmt : Format.formatter)
    ({name; module_systems = ls} : t) =
  Format.fprintf fmt "@[%a;@ @[%a@]@]" dump_package_name name
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.pp_print_space fmt ())
       dump_package_info)
    ls

type package_found_info = {
  rel_path: string;
  pkg_rel_path: string;
  suffix: string;
}

type info_query =
  | Package_script
  | Package_not_found
  | Package_found of package_found_info

(* Note that package-name has to be exactly the same as
   npm package name, otherwise the path resolution will be wrong *)
let query_package_infos ({name; module_systems} : t)
    (module_system : module_system) : info_query =
  match name with
  | Pkg_empty -> Package_script
  | Pkg_normal name -> (
    match
      Ext_list.find_first module_systems (fun k ->
          compatible k.module_system module_system)
    with
    | Some k ->
      let rel_path = k.path in
      let pkg_rel_path = name // rel_path in
      Package_found {rel_path; pkg_rel_path; suffix = k.suffix}
    | None -> Package_not_found)
  | Pkg_runtime -> (
    (*FIXME: [compatible] seems not correct *)
    match
      Ext_list.find_first module_systems (fun k ->
          compatible k.module_system module_system)
    with
    | Some k ->
      let rel_path = k.path in
      let pkg_rel_path = Runtime_package.name // rel_path in
      Package_found {rel_path; pkg_rel_path; suffix = k.suffix}
    | None -> Package_not_found)

let get_js_path (x : t) (module_system : module_system) : string =
  match
    Ext_list.find_first x.module_systems (fun k ->
        compatible k.module_system module_system)
  with
  | Some k -> k.path
  | None -> assert false

(* for a single pass compilation, [output_dir]
   can be cached
*)
let get_output_dir (info : t) ~package_dir module_system =
  Filename.concat package_dir (get_js_path info module_system)

let add_npm_package_path (packages_info : t) (s : string) : t =
  if is_empty packages_info then
    Bsc_args.bad_arg "Set package name first using -bs-package-name"
  else
    let handle_module_system module_system =
      match module_system_of_string module_system with
      | Some x -> x
      | None -> Bsc_args.bad_arg ("invalid module system " ^ module_system)
    in
    let m =
      match Ext_string.split ~keep_empty:true s ':' with
      | [path] -> {module_system = Esmodule; path; suffix = Literals.suffix_js}
      | [module_system; path] ->
        {
          module_system = handle_module_system module_system;
          path;
          suffix = Literals.suffix_js;
        }
      | [module_system; path; suffix] ->
        {module_system = handle_module_system module_system; path; suffix}
      | _ -> Bsc_args.bad_arg ("invalid npm package path: " ^ s)
    in
    {packages_info with module_systems = m :: packages_info.module_systems}

(* support es6 modules instead
   TODO: enrich ast to support import export
   http://www.ecma-international.org/ecma-262/6.0/#sec-imports
   For every module, we need [Ident.t] for accessing and [filename] for import,
   they are not necessarily the same.

   Es6 modules is not the same with commonjs, we use commonjs currently
   (play better with node)

   FIXME: the module order matters?
*)
