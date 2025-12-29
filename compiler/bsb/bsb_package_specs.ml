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

type t = {
  modules: Bsb_spec_set.t;
  runtime: string option;
      (* This has to be resolved as early as possible, since
         the path will be inherited in sub projects
      *)
}

let ( .?() ) = Map_string.find_opt

let bad_module_format_message_exn ~loc format =
  Bsb_exception.errorf ~loc
    "package-specs: `%s` isn't a valid output module format. It has to be one \
     of:  %s, %s, or %s"
    format Literals.esmodule Literals.commonjs Literals.typescript

let suffix_regexp = Str.regexp "[A-Za-z0-9-_.]*\\.[cm]?[jt]s"

let validate_suffix suffix = Str.string_match suffix_regexp suffix 0

(* Valid suffixes for TypeScript module format *)
let valid_typescript_suffixes =
  [".ts"; ".tsx"; ".mts"; ".cts"; ".mtsx"; ".ctsx"]

let validate_typescript_suffix suffix =
  List.mem suffix valid_typescript_suffixes

let rec from_array suffix (arr : Ext_json_types.t array) : Bsb_spec_set.t =
  let spec = ref Bsb_spec_set.empty in
  let has_in_source = ref false in
  Ext_array.iter arr (fun x ->
      let result = from_json_single suffix x in
      if Bsb_spec_set.in_source result then
        if not !has_in_source then has_in_source := true
        else
          Bsb_exception.errorf ~loc:(Ext_json.loc_of x)
            "package-specs: detected two module formats that are both \
             configured to be in-source.";
      spec := Bsb_spec_set.add result !spec);
  !spec

(* TODO: FIXME: better API without mutating *)
and from_json_single suffix (x : Ext_json_types.t) : Bsb_spec_set.spec =
  match x with
  | Str {str = format; loc} -> (
    let _ =
      if format = Literals.es6 || format = Literals.es6_global then
        let loc =
          {Warnings.loc_start = loc; loc_end = loc; loc_ghost = false}
        in
        Location.deprecated ~can_be_automigrated:false loc
          (Printf.sprintf "Option \"%s\" is deprecated. Use \"%s\" instead."
             format Literals.esmodule)
    in
    match format with
    | s when s = Literals.commonjs ->
      Bsb_spec_set.Commonjs {in_source = false; suffix; emit_dts = false}
    | s
      when s = Literals.esmodule || s = Literals.es6 || s = Literals.es6_global
      ->
      Bsb_spec_set.Esmodule {in_source = false; suffix; emit_dts = false}
    | s when s = Literals.typescript ->
      Bsb_spec_set.Typescript {in_source = false; suffix = Literals.suffix_ts}
    | _ -> bad_module_format_message_exn ~loc format)
  | Obj {map; loc} -> (
    match map.?("module") with
    | Some (Str {str = format; loc = format_loc}) -> (
      let _ =
        if format = Literals.es6 || format = Literals.es6_global then
          let loc =
            {
              Warnings.loc_start = format_loc;
              loc_end = format_loc;
              loc_ghost = false;
            }
          in
          Location.deprecated ~can_be_automigrated:false loc
            (Printf.sprintf "Option \"%s\" is deprecated. Use \"%s\" instead."
               format Literals.esmodule)
      in
      let is_ts_module = format = "typescript" in
      let in_source =
        match map.?(Bsb_build_schemas.in_source) with
        | Some (True _) -> true
        | Some _ | None -> false
      in
      let suffix =
        match map.?(Bsb_build_schemas.suffix) with
        | Some (Str {str = suffix; loc = suffix_loc}) ->
          if is_ts_module then
            if validate_typescript_suffix suffix then suffix
            else
              Bsb_exception.errorf ~loc:suffix_loc
                "invalid suffix \"%s\" for typescript module. Allowed: %s"
                suffix
                (String.concat ", " valid_typescript_suffixes)
          else if validate_suffix suffix then suffix
          else
            Bsb_exception.errorf ~loc:suffix_loc
              "invalid suffix \"%s\". The suffix must end with .js, .mjs, \
               .cjs, .ts, .mts, or .cts."
              suffix
        | Some _ ->
          Bsb_exception.errorf ~loc:(Ext_json.loc_of x)
            "expected a string extension like \".js\" or \".ts\""
        | None -> if is_ts_module then Literals.suffix_ts else suffix
      in
      let emit_dts =
        match map.?(Bsb_build_schemas.dts) with
        | Some (True dts_loc) ->
          if is_ts_module then
            Bsb_exception.errorf ~loc:dts_loc
              "dts: true is not allowed with module: typescript (TypeScript \
               already has types)"
          else true
        | Some _ | None -> false
      in
      match format with
      | s when s = Literals.commonjs ->
        Bsb_spec_set.Commonjs {in_source; suffix; emit_dts}
      | s
        when s = Literals.esmodule || s = Literals.es6
             || s = Literals.es6_global ->
        Bsb_spec_set.Esmodule {in_source; suffix; emit_dts}
      | s when s = Literals.typescript ->
        Bsb_spec_set.Typescript {in_source; suffix}
      | _ -> bad_module_format_message_exn ~loc:format_loc format)
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

let from_json suffix (x : Ext_json_types.t) : Bsb_spec_set.t =
  match x with
  | Arr {content; _} -> from_array suffix content
  | _ -> Bsb_spec_set.singleton (from_json_single suffix x)

let bs_package_output = "-bs-package-output"

let package_flag (spec : Bsb_spec_set.spec) dir =
  let module_system = Bsb_spec_set.module_system spec in
  let in_source = Bsb_spec_set.in_source spec in
  let suffix = Bsb_spec_set.suffix spec in
  let base =
    Ext_string.inter2 bs_package_output
      (Ext_string.concat5
         (Bsb_spec_set.format_name spec)
         Ext_string.single_colon
         (if in_source then dir
          else Bsb_config.top_prefix_of_format module_system // dir)
         Ext_string.single_colon suffix)
  in
  let base =
    if Bsb_spec_set.is_typescript spec then
      Ext_string.inter2 base "-bs-typescript"
    else base
  in
  if Bsb_spec_set.emit_dts spec then Ext_string.inter2 base "-bs-emit-dts"
  else base

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
      Bsb_spec_set.fold
        (fun format acc -> Ext_string.inter2 acc (package_flag format dirname))
        package_specs.modules Ext_string.empty
  in
  match package_specs.runtime with
  | None -> res
  | Some x -> Ext_string.inter3 res "-runtime" x

let default_package_specs suffix =
  (* TODO: swap default to Esmodule in v12 *)
  Bsb_spec_set.singleton
    (Bsb_spec_set.Commonjs {in_source = false; suffix; emit_dts = false})

(**
    [get_list_of_output_js specs "src/hi/hello"]

*)
let get_list_of_output_js (package_specs : t)
    (output_file_sans_extension : string) =
  Bsb_spec_set.fold
    (fun (spec : Bsb_spec_set.spec) acc ->
      let basename =
        Ext_namespace.change_ext_ns_suffix output_file_sans_extension
          (Bsb_spec_set.suffix spec)
      in
      (if Bsb_spec_set.in_source spec then Bsb_config.rev_lib_bs_prefix basename
       else
         Bsb_config.lib_bs_prefix_of_format (Bsb_spec_set.module_system spec)
         // basename)
      :: acc)
    package_specs.modules []

let list_dirs_by (package_specs : t) (f : string -> unit) =
  Bsb_spec_set.iter
    (fun (spec : Bsb_spec_set.spec) ->
      if not (Bsb_spec_set.in_source spec) then
        f (Bsb_config.top_prefix_of_format (Bsb_spec_set.module_system spec)))
    package_specs.modules

(** Check if any spec uses TypeScript module format *)
let has_typescript_module (package_specs : t) : bool =
  Bsb_spec_set.fold
    (fun (spec : Bsb_spec_set.spec) acc ->
      acc || Bsb_spec_set.is_typescript spec)
    package_specs.modules false

(** Check if any spec has dts: true *)
let has_dts_output (package_specs : t) : bool =
  Bsb_spec_set.fold
    (fun (spec : Bsb_spec_set.spec) acc -> acc || Bsb_spec_set.emit_dts spec)
    package_specs.modules false

(** Convert package specs for dependency builds.
    TypeScript specs are converted to Esmodule with dts output,
    since dependencies should produce standard JS + .d.ts files. *)
let for_dependency_build (package_specs : t) : t =
  let modules =
    Bsb_spec_set.fold
      (fun (spec : Bsb_spec_set.spec) acc ->
        let converted =
          match spec with
          | Bsb_spec_set.Typescript {in_source; suffix = _} ->
            (* Convert TypeScript to Esmodule with dts *)
            Bsb_spec_set.Esmodule
              {in_source; suffix = Literals.suffix_js; emit_dts = true}
          | other -> other
        in
        Bsb_spec_set.add converted acc)
      package_specs.modules Bsb_spec_set.empty
  in
  {package_specs with modules}

type json_map = Ext_json_types.t Map_string.t

let extract_suffix_exn (map : json_map) : string =
  match map.?(Bsb_build_schemas.suffix) with
  | None -> Literals.suffix_js
  | Some (Str {str = suffix; _}) when validate_suffix suffix -> suffix
  | Some (Str {str; _} as config) ->
    Bsb_exception.config_error config
      ("invalid suffix \"" ^ str
     ^ "\". The suffix must end with .js, .mjs, .cjs, .ts, .mts, or .cts.")
  | Some config ->
    Bsb_exception.config_error config
      "expected a string extension like \".js\" or \".ts\""

let from_map ~(cwd : string) map =
  let suffix = extract_suffix_exn map in
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
