(* Copyright (C) 2015 - 2016 Bloomberg Finance L.P.
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

(* let get_list_string = Bsb_build_util.get_list_string *)
let ( // ) = Ext_path.combine

let resolve_package cwd package_name =
  let x = Bsb_pkg.resolve_bs_package ~cwd package_name in
  {
    Bsb_config_types.package_name;
    package_install_path = x // Bsb_config.lib_ocaml;
  }

type json_map = Ext_json_types.t Map_string.t

(* Key is the path *)
let ( |? ) m (key, cb) = m |> Ext_json.test key cb
let ( .?() ) = Map_string.find_opt

(*TODO: it is a little mess that [cwd] and [project dir] are shared*)

let extract_package_name_and_namespace (map : json_map) : string * string option
    =
  let package_name =
    match map.?(Bsb_build_schemas.name) with
    | Some (Str {str = "_"} as config) ->
      Bsb_exception.config_error config "_ is a reserved package name"
    | Some (Str {str = name}) -> name
    | Some config ->
      Bsb_exception.config_error config "name expect a string field"
    | None -> Bsb_exception.invalid_spec "field name is required"
  in
  let namespace =
    match map.?(Bsb_build_schemas.namespace) with
    | None | Some (False _) -> None
    | Some (True _) ->
      Some (Ext_namespace.namespace_of_package_name package_name)
    | Some (Str {str}) ->
      (*TODO : check the validity of namespace *)
      Some (Ext_namespace.namespace_of_package_name str)
    | Some x ->
      Bsb_exception.config_error x "namespace field expects string or boolean"
  in
  (package_name, namespace)

(**
    There are two things to check:
   - the running bsb and vendoring bsb is the same
   - the running bsb need delete stale build artifacts
      (kinda check npm upgrade)

      Note if the setup is correct:
      the running compiler and node_modules/rescript
      should be the same version,
      The exact check is that the running compiler should have a
      compatible runtime version installed, the location of the
      compiler is actually not relevant.
      We disable the check temporarily
      e.g,
      ```
      bsc -runtime runtime_dir@version
      ```
*)
let extract_gentype_config (map : json_map) : Bsb_config_types.gentype_config =
  match map.?(Bsb_build_schemas.gentypeconfig) with
  | None -> false
  | Some (Obj _) -> true
  | Some config ->
    Bsb_exception.config_error config "gentypeconfig expect an object"

let extract_string (map : json_map) (field : string) cb =
  match map.?(field) with
  | None -> None
  | Some (Str {str}) -> cb str
  | Some config -> Bsb_exception.config_error config (field ^ " expect a string")

let extract_boolean (map : json_map) (field : string) (default : bool) : bool =
  match map.?(field) with
  | None -> default
  | Some (True _) -> true
  | Some (False _) -> false
  | Some config ->
    Bsb_exception.config_error config (field ^ " expect a boolean")

let extract_warning (map : json_map) =
  match map.?(Bsb_build_schemas.warnings) with
  | None -> Bsb_warning.use_default
  | Some (Obj {map}) -> Bsb_warning.from_map map
  | Some config -> Bsb_exception.config_error config "expect an object"

let extract_ignored_dirs (map : json_map) : Set_string.t =
  match map.?(Bsb_build_schemas.ignored_dirs) with
  | None -> Set_string.empty
  | Some (Arr {content}) ->
    Set_string.of_list (Bsb_build_util.get_list_string content)
  | Some config -> Bsb_exception.config_error config "expect an array of string"

let extract_generators (map : json_map) =
  let generators = ref Map_string.empty in
  (match map.?(Bsb_build_schemas.generators) with
  | None -> ()
  | Some (Arr {content = s}) ->
    generators :=
      Ext_array.fold_left s Map_string.empty (fun acc json ->
          match json with
          | Obj {map = m; loc} -> (
            match
              (m.?(Bsb_build_schemas.name), m.?(Bsb_build_schemas.command))
            with
            | Some (Str {str = name}), Some (Str {str = command}) ->
              Map_string.add acc name command
            | _, _ ->
              Bsb_exception.errorf ~loc
                {| generators exepect format like { "name" : "cppo",  "command"  : "cppo $in -o $out"} |}
            )
          | _ -> acc)
  | Some config ->
    Bsb_exception.config_error config
      (Bsb_build_schemas.generators ^ " expect an array field"));
  !generators

let extract_dependencies (map : json_map) cwd (field : string) :
    Bsb_config_types.dependencies =
  match map.?(field) with
  | None -> []
  | Some (Arr {content = s}) ->
    Ext_list.map (Bsb_build_util.get_list_string s) (fun s ->
        resolve_package cwd (Bsb_pkg_types.string_as_package s))
  | Some config -> Bsb_exception.config_error config (field ^ " expect an array")

(* return an empty array if not found *)
let extract_string_list (map : json_map) (field : string) : string list =
  match map.?(field) with
  | None -> []
  | Some (Arr {content = s}) -> Bsb_build_util.get_list_string s
  | Some config -> Bsb_exception.config_error config (field ^ " expect an array")

let extract_ppx (map : json_map) (field : string) ~(cwd : string) :
    Bsb_config_types.ppx list =
  match map.?(field) with
  | None -> []
  | Some (Arr {content}) ->
    let resolve s =
      if s = "" then
        Bsb_exception.invalid_spec "invalid ppx, empty string found"
      else
        (Bsb_build_util.resolve_bsb_magic_file ~cwd
           ~desc:Bsb_build_schemas.ppx_flags s)
          .path
    in
    Ext_array.to_list_f content (fun x ->
        match x with
        | Str x -> {Bsb_config_types.name = resolve x.str; args = []}
        | Arr {content} -> (
          let xs = Bsb_build_util.get_list_string content in
          match xs with
          | [] -> Bsb_exception.config_error x " empty array is not allowed"
          | name :: args -> {Bsb_config_types.name = resolve name; args})
        | config ->
          Bsb_exception.config_error config
            (field ^ "expect each item to be either string or array"))
  | Some config -> Bsb_exception.config_error config (field ^ " expect an array")

let extract_js_post_build (map : json_map) cwd : string option =
  let js_post_build_cmd = ref None in
  map
  |? ( Bsb_build_schemas.js_post_build,
       `Obj
         (fun m ->
           m
           |? ( Bsb_build_schemas.cmd,
                `Str
                  (fun s ->
                    js_post_build_cmd :=
                      Some
                        (Bsb_build_util.resolve_bsb_magic_file ~cwd
                           ~desc:Bsb_build_schemas.js_post_build s)
                          .path) )
           |> ignore) )
  |> ignore;
  !js_post_build_cmd

(** ATT: make sure such function is re-entrant.
    With a given [cwd] it works anywhere*)
let interpret_json ~(filename : string) ~(json : Ext_json_types.t)
    ~(package_kind : Bsb_package_kind.t) ~(per_proj_dir : string) :
    Bsb_config_types.t =
  (* we should not resolve it too early,
      since it is external configuration, no {!Bsb_build_util.convert_and_resolve_path}
  *)

  (* When we plan to add more deps here,
     Make sure check it is consistent that for nested deps, we have a
     quck check by just re-parsing deps
     Make sure it works with [-make-world] [-clean-world]
  *)

  (* Setting ninja is a bit complex
     1. if [build.ninja] does use [ninja] we need set a variable
     2. we need store it so that we can call ninja correctly
  *)
  match json with
  | Obj {map} -> (
    let package_name, namespace = extract_package_name_and_namespace map in
    let gentype_config = extract_gentype_config map in

    (* This line has to be before any calls to Bsb_global_backend.backend, because it'll read the entries
         array from the bsconfig and set the backend_ref to the first entry, if any. *)
    let pp_flags : string option =
      extract_string map Bsb_build_schemas.pp_flags (fun p ->
          if p = "" then
            Bsb_exception.invalid_spec "invalid pp, empty string found"
          else
            Some
              (Bsb_build_util.resolve_bsb_magic_file ~cwd:per_proj_dir
                 ~desc:Bsb_build_schemas.pp_flags p)
                .path)
    in
    let bs_dependencies =
      extract_dependencies map per_proj_dir Bsb_build_schemas.bs_dependencies
    in
    let bs_dev_dependencies =
      match package_kind with
      | Toplevel ->
        extract_dependencies map per_proj_dir
          Bsb_build_schemas.bs_dev_dependencies
      | Dependency _ -> []
    in
    match map.?(Bsb_build_schemas.sources) with
    | Some sources ->
      let cut_generators =
        extract_boolean map Bsb_build_schemas.cut_generators false
      in
      let groups =
        Bsb_parse_sources.scan ~ignored_dirs:(extract_ignored_dirs map)
          ~package_kind ~root:per_proj_dir ~cut_generators
          (* ~namespace *)
          sources
      in
      let bsc_flags = extract_string_list map Bsb_build_schemas.bsc_flags in
      let jsx = Bsb_jsx.from_map map in
      let jsx, bsc_flags =
        match package_kind with
        | Dependency x -> ({jsx with version = x.jsx.version}, bsc_flags)
        | Toplevel -> (jsx, bsc_flags)
      in
      {
        gentype_config;
        package_name;
        namespace;
        warning = extract_warning map;
        external_includes =
          extract_string_list map Bsb_build_schemas.bs_external_includes;
        bsc_flags;
        ppx_files =
          extract_ppx map ~cwd:per_proj_dir Bsb_build_schemas.ppx_flags;
        pp_file = pp_flags;
        bs_dependencies;
        bs_dev_dependencies;
        (*
            reference for quoting
             {[
               let tmpfile = Filename.temp_file "ocamlpp" "" in
               let comm = Printf.sprintf "%s %s > %s"
                   pp (Filename.quote sourcefile) tmpfile
               in
             ]}
          *)
        js_post_build_cmd = extract_js_post_build map per_proj_dir;
        package_specs =
          (match package_kind with
          | Toplevel -> Bsb_package_specs.from_map ~cwd:per_proj_dir map
          | Dependency x -> x.package_specs);
        file_groups = groups;
        files_to_install = Queue.create ();
        jsx;
        generators = extract_generators map;
        cut_generators;
        filename;
      }
    | None -> Bsb_exception.invalid_spec ("no sources specified in " ^ filename)
    )
  | _ -> Bsb_exception.invalid_spec (filename ^ " expect a json object {}")

let deps_from_bsconfig () =
  let cwd = Bsb_global_paths.cwd in
  match
    Bsb_config_load.load_json ~per_proj_dir:cwd ~warn_legacy_config:false
  with
  | _, Obj {map} -> (Bsb_package_specs.from_map ~cwd map, Bsb_jsx.from_map map)
  | _, _ -> assert false
