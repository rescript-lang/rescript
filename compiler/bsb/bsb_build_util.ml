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

let flag_concat flag xs =
  String.concat Ext_string.single_space
    (Ext_list.flat_map xs (fun x -> [flag; x]))

let ( // ) = Ext_path.combine

let ppx_flags (xs : Bsb_config_types.ppx list) =
  flag_concat "-ppx"
    (Ext_list.map xs (fun x ->
         if x.args = [] then Ext_filename.maybe_quote x.name
         else
           let fmt : _ format =
             if Ext_sys.is_windows_or_cygwin then "\"%s %s\"" else "'%s %s'"
           in
           Printf.sprintf fmt x.name (String.concat " " x.args)))

let pp_flag (xs : string) = "-pp " ^ Ext_filename.maybe_quote xs

let include_dirs dirs =
  String.concat Ext_string.single_space
    (Ext_list.flat_map dirs (fun x -> ["-I"; Ext_filename.maybe_quote x]))

let include_dirs_by dirs fn =
  String.concat Ext_string.single_space
    (Ext_list.flat_map dirs (fun x -> ["-I"; Ext_filename.maybe_quote (fn x)]))

(* we use lazy $src_root_dir *)

(* It does several conversion:
   First, it will convert unix path to windows backward on windows platform.
   Then if it is absolute path, it will do thing
   Else if it is relative path, it will be rebased on project's root directory *)

let convert_and_resolve_path : string -> string -> string =
  if Sys.unix then ( // )
  else fun cwd path ->
    if Ext_sys.is_windows_or_cygwin then
      let p = Ext_string.replace_slash_backward path in
      cwd // p
    else failwith ("Unknown OS :" ^ Sys.os_type)
(* we only need convert the path in the beginning *)

type result = {path: string; checked: bool}

(* Magic path resolution:
   foo => foo
   foo/ => /absolute/path/to/projectRoot/node_modules/foo
   foo/bar => /absolute/path/to/projectRoot/node_modules/foo/bar
   /foo/bar => /foo/bar
   ./foo/bar => /absolute/path/to/projectRoot/./foo/bar
   Input is node path, output is OS dependent (normalized) path
*)
let resolve_bsb_magic_file ~cwd ~desc p : result =
  let no_slash = Ext_string.no_slash_idx p in
  if no_slash < 0 then
    (* Single file FIXME: better error message for "" input *)
    {path = p; checked = false}
  else
    let first_char = String.unsafe_get p 0 in
    if Filename.is_relative p && first_char <> '.' then
      let package_name, rest = Bsb_pkg_types.extract_pkg_name_and_file p in
      let relative_path =
        if Ext_sys.is_windows_or_cygwin then
          Ext_string.replace_slash_backward rest
        else rest
      in
      (* let p = if Ext_sys.is_windows_or_cygwin then Ext_string.replace_slash_backward p else p in *)
      let package_dir = Bsb_pkg.resolve_bs_package ~cwd package_name in
      let path = package_dir // relative_path in
      if Sys.file_exists path then {path; checked = true}
      else (
        Bsb_log.error "@{<error>Could not resolve @} %s in %s@." p cwd;
        failwith (p ^ " not found when resolving " ^ desc))
    else
      (* relative path [./x/y]*)
      {path = convert_and_resolve_path cwd p; checked = true}

(** converting a file from Linux path format to Windows *)

(**
   {[
     mkp "a/b/c/d";;
     mkp "/a/b/c/d"
   ]}
*)
let rec mkp dir =
  if not (Sys.file_exists dir) then
    let parent_dir = Filename.dirname dir in
    if parent_dir = Filename.current_dir_name then Unix.mkdir dir 0o777
      (* leaf node *)
    else (
      mkp parent_dir;
      Unix.mkdir dir 0o777)
  else if not @@ Sys.is_directory dir then
    failwith (dir ^ " exists but it is not a directory, plz remove it first")
  else ()

let get_list_string_acc (s : Ext_json_types.t array) acc =
  Ext_array.to_list_map_acc s acc (fun x ->
      match x with
      | Str x -> Some x.str
      | _ -> None)

let get_list_string s = get_list_string_acc s []

(* Key is the path *)
let ( |? ) m (key, cb) = m |> Ext_json.test key cb

type top = Expect_none | Expect_name of string

type package_context = {proj_dir: string; top: top; is_pinned: bool}

(**
   TODO: check duplicate package name
   ?use path as identity?

   Basic requirements
     1. cycle detection
     2. avoid duplication
     3. deterministic, since -make-world will also comes with -clean-world

*)

let pp_packages_rev ppf lst =
  Ext_list.rev_iter lst (fun s -> Format.fprintf ppf "%s " s)

let extract_pinned_dependencies (map : Ext_json_types.t Map_string.t) :
    Set_string.t =
  match Map_string.find_opt map Bsb_build_schemas.pinned_dependencies with
  | None -> Set_string.empty
  | Some (Arr {content}) -> Set_string.of_list (get_list_string content)
  | Some config -> Bsb_exception.config_error config "expect an array of string"

let rec walk_all_deps_aux (visited : string Hash_string.t) (paths : string list)
    ~(top : top) (dir : string) (queue : _ Queue.t) ~pinned_dependencies =
  match
    Bsb_config_load.load_json ~per_proj_dir:dir ~warn_legacy_config:false
  with
  | _, Obj {map; loc} ->
    let cur_package_name =
      match Map_string.find_opt map Bsb_build_schemas.name with
      | Some (Str {str; loc}) ->
        (match top with
        | Expect_none -> ()
        | Expect_name s ->
          if s <> str then
            Bsb_exception.errorf ~loc
              "package name is expected to be %s but got %s" s str);
        str
      | Some _ | None ->
        Bsb_exception.errorf ~loc "package name missing in %s/rescript.json" dir
    in
    if Ext_list.mem_string paths cur_package_name then (
      Bsb_log.error "@{<error>Cyclic dependencies in package stack@}@.";
      exit 2);
    let package_stacks = cur_package_name :: paths in
    Bsb_log.info "@{<info>Package stack:@} %a @." pp_packages_rev package_stacks;
    if Hash_string.mem visited cur_package_name then
      Bsb_log.info "@{<info>Visited before@} %s@." cur_package_name
    else
      let explore_deps (deps : string) pinned_dependencies =
        map
        |? ( deps,
             `Arr
               (fun (new_packages : Ext_json_types.t array) ->
                 Ext_array.iter new_packages (fun js ->
                     match js with
                     | Str {str = new_package} ->
                       let package_dir =
                         Bsb_pkg.resolve_bs_package ~cwd:dir
                           (Bsb_pkg_types.string_as_package new_package)
                       in
                       walk_all_deps_aux visited package_stacks
                         ~top:(Expect_name new_package) package_dir queue
                         ~pinned_dependencies
                     | _ -> Bsb_exception.errorf ~loc "%s expect an array" deps))
           )
        |> ignore
      in
      let is_pinned =
        match top with
        | Expect_name n when Set_string.mem pinned_dependencies n -> true
        | _ -> false
      in
      let pinned_dependencies =
        match is_pinned with
        | true ->
          let transitive_pinned_dependencies =
            extract_pinned_dependencies map
          in
          Set_string.union transitive_pinned_dependencies pinned_dependencies
        | false -> pinned_dependencies
      in
      explore_deps Bsb_build_schemas.bs_dependencies pinned_dependencies;
      (match top with
      | Expect_none ->
        explore_deps Bsb_build_schemas.bs_dev_dependencies pinned_dependencies
      | Expect_name _ when is_pinned ->
        explore_deps Bsb_build_schemas.bs_dev_dependencies pinned_dependencies
      | Expect_name _ -> ());
      Queue.add {top; proj_dir = dir; is_pinned} queue;
      Hash_string.add visited cur_package_name dir
  | _ -> ()

let walk_all_deps dir ~pinned_dependencies : package_context Queue.t =
  let visited = Hash_string.create 0 in
  let cb = Queue.create () in
  walk_all_deps_aux visited [] ~top:Expect_none dir cb ~pinned_dependencies;
  cb
