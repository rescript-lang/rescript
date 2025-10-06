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

let ( // ) = Ext_path.combine

(*FIXME: use assoc list instead *)
module Spec_set = Bsb_spec_set

type t = {
  modules: Spec_set.t;
  runtime: string option;
      (* This has to be resolved as early as possible, since
         the path will be inherited in sub projects
      *)
}

let ( .?() ) = Map_string.find_opt

let bad_module_format_message_exn ~loc format =
  Bsb_exception.errorf ~loc
    "package-specs: `%s` isn't a valid output module format. It has to be one \
     of:  %s or %s"
    format Literals.esmodule Literals.commonjs

let supported_format (x : string) loc : Ext_module_system.t =
  let _ =
    if x = Literals.es6 || x = Literals.es6_global then
      let loc_end =
        {loc with Lexing.pos_cnum = loc.Lexing.pos_cnum + String.length x}
      in
      let loc = {Warnings.loc_start = loc; loc_end; loc_ghost = false} in
      Location.deprecated ~can_be_automigrated:false loc
        (Printf.sprintf "Option \"%s\" is deprecated. Use \"%s\" instead." x
           Literals.esmodule)
  in
  if x = Literals.es6 || x = Literals.esmodule then Esmodule
  else if x = Literals.commonjs then Commonjs
  else if x = Literals.es6_global then Es6_global
  else bad_module_format_message_exn ~loc x

let string_of_format (x : Ext_module_system.t) =
  match x with
  | Commonjs -> Literals.commonjs
  | Esmodule -> Literals.esmodule
  | Es6_global -> Literals.es6_global

let js_suffix_regexp = Str.regexp "[A-Za-z0-9-_.]*\\.[cm]?js"

let validate_js_suffix suffix = Str.string_match js_suffix_regexp suffix 0

let rec from_array suffix (arr : Ext_json_types.t array) : Spec_set.t =
  let spec = ref Spec_set.empty in
  let has_in_source = ref false in
  Ext_array.iter arr (fun x ->
      let result = from_json_single suffix x in
      if result.in_source then
        if not !has_in_source then has_in_source := true
        else
          Bsb_exception.errorf ~loc:(Ext_json.loc_of x)
            "package-specs: detected two module formats that are both \
             configured to be in-source.";
      spec := Spec_set.add result !spec);
  !spec

(* TODO: FIXME: better API without mutating *)
and from_json_single suffix (x : Ext_json_types.t) : Bsb_spec_set.spec =
  match x with
  | Str {str = format; loc} ->
    {format = supported_format format loc; in_source = false; suffix}
  | Obj {map; loc} -> (
    match map.?("module") with
    | Some (Str {str = format}) ->
      let in_source =
        match map.?(Bsb_build_schemas.in_source) with
        | Some (True _) -> true
        | Some _ | None -> false
      in
      let suffix =
        match map.?(Bsb_build_schemas.suffix) with
        | Some (Str {str = suffix; _}) when validate_js_suffix suffix -> suffix
        | Some (Str {str; loc}) ->
          Bsb_exception.errorf ~loc
            "invalid suffix \"%s\". The suffix and may contain letters, \
             digits, \"-\", \"_\" and \".\" and must end with .js, .mjs or \
             .cjs."
            str
        | Some _ ->
          Bsb_exception.errorf ~loc:(Ext_json.loc_of x)
            "expected a string extension like \".js\""
        | None -> suffix
      in
      {format = supported_format format loc; in_source; suffix}
    | Some _ ->
      Bsb_exception.errorf ~loc
        "package-specs: when the configuration is an object, `module` field \
         should be a string, not an array. If you want to pass multiple module \
         specs, try turning package-specs into an array of objects (or \
         strings) instead."
    | None ->
      Bsb_exception.errorf ~loc
        "package-specs: when the configuration is an object, the `module` \
         field is mandatory.")
  | _ ->
    Bsb_exception.errorf ~loc:(Ext_json.loc_of x)
      "package-specs: expected either a string or an object."

let from_json suffix (x : Ext_json_types.t) : Spec_set.t =
  match x with
  | Arr {content; _} -> from_array suffix content
  | _ -> Spec_set.singleton (from_json_single suffix x)

let bs_package_output = "-bs-package-output"

[@@@warning "+9"]

let package_flag ({format; in_source; suffix} : Bsb_spec_set.spec) dir =
  Ext_string.inter2 bs_package_output
    (Ext_string.concat5 (string_of_format format) Ext_string.single_colon
       (if in_source then dir else Bsb_config.top_prefix_of_format format // dir)
       Ext_string.single_colon suffix)

(* FIXME: we should adapt it *)
let package_flag_of_package_specs (package_specs : t) ~(dirname : string) :
    string =
  let res =
    match (package_specs.modules :> Bsb_spec_set.spec list) with
    | [] -> Ext_string.empty
    | [format] ->
      Ext_string.inter2 Ext_string.empty (package_flag format dirname)
    | [a; b] ->
      Ext_string.inter3 Ext_string.empty (package_flag a dirname)
        (package_flag b dirname)
    | [a; b; c] ->
      Ext_string.inter4 Ext_string.empty (package_flag a dirname)
        (package_flag b dirname) (package_flag c dirname)
    | _ ->
      Spec_set.fold
        (fun format acc -> Ext_string.inter2 acc (package_flag format dirname))
        package_specs.modules Ext_string.empty
  in
  match package_specs.runtime with
  | None -> res
  | Some x -> Ext_string.inter3 res "-runtime" x

let default_package_specs suffix =
  (* TODO: swap default to Esmodule in v12 *)
  Spec_set.singleton {format = Commonjs; in_source = false; suffix}

(**
    [get_list_of_output_js specs "src/hi/hello"]

*)
let get_list_of_output_js (package_specs : t)
    (output_file_sans_extension : string) =
  Spec_set.fold
    (fun (spec : Bsb_spec_set.spec) acc ->
      let basename =
        Ext_namespace.change_ext_ns_suffix output_file_sans_extension
          spec.suffix
      in
      (if spec.in_source then Bsb_config.rev_lib_bs_prefix basename
       else Bsb_config.lib_bs_prefix_of_format spec.format // basename)
      :: acc)
    package_specs.modules []

let list_dirs_by (package_specs : t) (f : string -> unit) =
  Spec_set.iter
    (fun (spec : Bsb_spec_set.spec) ->
      if not spec.in_source then f (Bsb_config.top_prefix_of_format spec.format))
    package_specs.modules

type json_map = Ext_json_types.t Map_string.t

let extract_js_suffix_exn (map : json_map) : string =
  match map.?(Bsb_build_schemas.suffix) with
  | None -> Literals.suffix_js
  | Some (Str {str = suffix; _}) when validate_js_suffix suffix -> suffix
  | Some (Str {str; _} as config) ->
    Bsb_exception.config_error config
      ("invalid suffix \"" ^ str
     ^ "\". The suffix and may contain letters, digits, \"-\", \"_\" and \".\" \
        and must end with .js, .mjs or .cjs.")
  | Some config ->
    Bsb_exception.config_error config "expected a string extension like \".js\""

let from_map ~(cwd : string) map =
  let suffix = extract_js_suffix_exn map in
  let modules =
    match map.?(Bsb_build_schemas.package_specs) with
    | Some x -> from_json suffix x
    | None -> default_package_specs suffix
  in
  let runtime =
    match map.?(Bsb_build_schemas.external_stdlib) with
    | None -> None
    | Some (Str {str; _}) ->
      Some
        (Bsb_pkg.resolve_bs_package ~cwd (Bsb_pkg_types.string_as_package str))
    | _ -> assert false
  in
  {runtime; modules}
