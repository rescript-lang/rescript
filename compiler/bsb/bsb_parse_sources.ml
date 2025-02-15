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

type build_generator = Bsb_file_groups.build_generator

let ( .?() ) = Map_string.find_opt

(* type file_group = Bsb_file_groups.file_group *)

type t = Bsb_file_groups.t

let is_input_or_output (xs : build_generator list) (x : string) =
  Ext_list.exists xs (fun {input; output} ->
      let it_is y = y = x in
      Ext_list.exists input it_is || Ext_list.exists output it_is)

let errorf x fmt = Bsb_exception.errorf ~loc:(Ext_json.loc_of x) fmt

type cxt = {
  package_kind: Bsb_package_kind.t;
  is_dev: bool;
  cwd: string;
  root: string;
  cut_generators: bool;
  traverse: bool;
  (* namespace : string option; *)
  ignored_dirs: Set_string.t;
}

(** [public] has a list of modules, we do a sanity check to see if all the listed 
    modules are indeed valid module components
*)
let collect_pub_modules (xs : Ext_json_types.t array) (cache : Bsb_db.map) :
    Set_string.t =
  let set = ref Set_string.empty in
  for i = 0 to Array.length xs - 1 do
    let v = Array.unsafe_get xs i in
    match v with
    | Str {str; loc} ->
      if Map_string.mem cache str then set := Set_string.add !set str
      else
        Bsb_exception.errorf ~loc "%S in public is not an existing module" str
    | _ ->
      Bsb_exception.errorf ~loc:(Ext_json.loc_of v)
        "public expects a list of strings"
  done;
  !set

let extract_pub (input : Ext_json_types.t Map_string.t)
    (cur_sources : Bsb_db.map) : Bsb_file_groups.public =
  match input.?(Bsb_build_schemas.public) with
  | Some (Str {str = s} as x) ->
    if s = Bsb_build_schemas.export_all then Export_all
    else if s = Bsb_build_schemas.export_none then Export_none
    else errorf x "invalid str for %s " s
  | Some (Arr {content}) -> Export_set (collect_pub_modules content cur_sources)
  | Some config -> Bsb_exception.config_error config "expect array or string"
  | None -> Export_all

let extract_resources (input : Ext_json_types.t Map_string.t) : string list =
  match input.?(Bsb_build_schemas.resources) with
  | Some (Arr x) -> Bsb_build_util.get_list_string x.content
  | Some config -> Bsb_exception.config_error config "expect array "
  | None -> []

let extract_input_output (edge : Ext_json_types.t) : string list * string list =
  let error () =
    errorf edge {| invalid edge format, expect  ["output" , ":", "input" ]|}
  in
  match edge with
  | Arr {content} -> (
    match
      Ext_array.find_and_split content
        (fun x () ->
          match x with
          | Str {str = ":"} -> true
          | _ -> false)
        ()
    with
    | No_split -> error ()
    | Split (output, input) ->
      ( Ext_array.to_list_map output (fun x ->
            match x with
            | Str {str = ":"} -> error ()
            | Str {str} -> Some str
            | _ -> None),
        Ext_array.to_list_map input (fun x ->
            match x with
            | Str {str = ":"} -> error ()
            | Str {str} ->
              Some str
              (* More rigirous error checking: It would trigger a ninja syntax error *)
            | _ -> None) ))
  | _ -> error ()

type json_map = Ext_json_types.t Map_string.t

let extract_generators (input : json_map) : build_generator list =
  match input.?(Bsb_build_schemas.generators) with
  | Some (Arr {content; loc_start = _}) ->
    (* Need check is dev build or not *)
    Ext_array.fold_left content [] (fun acc x ->
        match x with
        | Obj {map} -> (
          match
            (map.?(Bsb_build_schemas.name), map.?(Bsb_build_schemas.edge))
          with
          | Some (Str command), Some edge ->
            let output, input = extract_input_output edge in
            {Bsb_file_groups.input; output; command = command.str} :: acc
          | _ -> errorf x "Invalid generator format")
        | _ -> errorf x "Invalid generator format")
  | Some x -> errorf x "Invalid generator format"
  | None -> []

let extract_predicate (m : json_map) : string -> bool =
  let excludes =
    match m.?(Bsb_build_schemas.excludes) with
    | None -> []
    | Some (Arr {content = arr}) -> Bsb_build_util.get_list_string arr
    | Some x -> Bsb_exception.config_error x "excludes expect array "
  in
  let slow_re = m.?(Bsb_build_schemas.slow_re) in
  match (slow_re, excludes) with
  | Some (Str {str = s}), [] ->
    let re = Str.regexp s in
    fun name -> Str.string_match re name 0
  | Some (Str {str = s}), _ :: _ ->
    let re = Str.regexp s in
    fun name ->
      Str.string_match re name 0 && not (Ext_list.mem_string excludes name)
  | Some config, _ ->
    Bsb_exception.config_error config
      (Bsb_build_schemas.slow_re ^ " expect a string literal")
  | None, _ -> fun name -> not (Ext_list.mem_string excludes name)

(** [parsing_source_dir_map cxt input]
    Major work done in this function, 
    assume [not toplevel && not (Bsb_dir_index.is_lib_dir dir_index)]      
    is already checked, so we don't need check it again    
*)

(** This is the only place where we do some removal during scanning,
    configurabl
*)

(********************************************************************)
(* starts parsing *)
let rec parsing_source_dir_map ({cwd = dir} as cxt)
    (input : Ext_json_types.t Map_string.t) : Bsb_file_groups.t =
  if Set_string.mem cxt.ignored_dirs dir then Bsb_file_groups.empty
  else
    let cur_globbed_dirs = ref false in
    let has_generators =
      match cxt with
      | {cut_generators = false; package_kind = Toplevel | Pinned_dependency _}
        ->
        true
      | {cut_generators = false; package_kind = Dependency _}
      | {cut_generators = true; _} ->
        false
    in
    let scanned_generators = extract_generators input in
    let sub_dirs_field = input.?(Bsb_build_schemas.subdirs) in
    let base_name_array =
      lazy
        (cur_globbed_dirs := true;
         Sys.readdir (Filename.concat cxt.root dir))
    in
    let output_sources =
      Ext_list.fold_left
        (Ext_list.flat_map scanned_generators (fun x -> x.output))
        Map_string.empty
        (fun acc o -> Bsb_db_util.add_basename ~dir acc o)
    in
    let sources =
      match input.?(Bsb_build_schemas.files) with
      | None ->
        (* We should avoid temporary files *)
        Ext_array.fold_left (Lazy.force base_name_array) output_sources
          (fun acc basename ->
            if is_input_or_output scanned_generators basename then acc
            else Bsb_db_util.add_basename ~dir acc basename)
      | Some (Arr basenames) ->
        Ext_array.fold_left basenames.content output_sources
          (fun acc basename ->
            match basename with
            | Str {str = basename; loc} ->
              Bsb_db_util.add_basename ~dir acc basename
                ~error_on_invalid_suffix:loc
            | _ -> acc)
      | Some (Obj {map; loc = _}) ->
        (* { excludes : [], slow_re : "" }*)
        let predicate = extract_predicate map in
        Ext_array.fold_left (Lazy.force base_name_array) output_sources
          (fun acc basename ->
            if
              is_input_or_output scanned_generators basename
              || not (predicate basename)
            then acc
            else Bsb_db_util.add_basename ~dir acc basename)
      | Some x ->
        Bsb_exception.config_error x "files field expect array or object "
    in
    let resources = extract_resources input in
    let public = extract_pub input sources in
    (* Doing recursive stuff *)
    let children =
      match (sub_dirs_field, cxt.traverse) with
      | None, true | Some (True _), _ ->
        let root = cxt.root in
        let parent = Filename.concat root dir in
        Ext_array.fold_left (Lazy.force base_name_array) Bsb_file_groups.empty
          (fun origin x ->
            if
              (not (Set_string.mem cxt.ignored_dirs x))
              && Ext_sys.is_directory_no_exn (Filename.concat parent x)
            then
              Bsb_file_groups.merge
                (parsing_source_dir_map
                   {
                     cxt with
                     cwd =
                       Ext_path.concat cxt.cwd
                         (Ext_path.simple_convert_node_path_to_os_path x);
                     traverse = true;
                   }
                   Map_string.empty)
                origin
            else origin)
      (* readdir parent avoiding scanning twice *)
      | None, false | Some (False _), _ -> Bsb_file_groups.empty
      | Some s, _ -> parse_sources cxt s
    in
    (* Do some clean up *)
    (* prune_staled_bs_js_files cxt sources ; *)
    Bsb_file_groups.cons
      ~file_group:
        {
          dir;
          sources;
          resources;
          public;
          is_dev = cxt.is_dev;
          generators = (if has_generators then scanned_generators else []);
        }
      ?globbed_dir:(if !cur_globbed_dirs then Some dir else None)
      children

and parsing_single_source ({package_kind; is_dev; cwd} as cxt)
    (x : Ext_json_types.t) : t =
  match x with
  | Str {str = dir} -> (
    match (package_kind, is_dev) with
    | Dependency _, true -> Bsb_file_groups.empty
    | Dependency _, false | (Toplevel | Pinned_dependency _), _ ->
      parsing_source_dir_map
        {
          cxt with
          cwd =
            Ext_path.concat cwd
              (Ext_path.simple_convert_node_path_to_os_path dir);
        }
        Map_string.empty)
  | Obj {map} -> (
    let current_dir_index =
      match map.?(Bsb_build_schemas.type_) with
      | Some (Str {str = "dev"}) -> true
      | Some _ ->
        Bsb_exception.config_error x {|type field expect "dev" literal |}
      | None -> is_dev
    in
    match (package_kind, current_dir_index) with
    | Dependency _, true -> Bsb_file_groups.empty
    | Dependency _, false | (Toplevel | Pinned_dependency _), _ ->
      let dir =
        match map.?(Bsb_build_schemas.dir) with
        | Some (Str {str}) ->
          if str = Literals.library_file then
            Bsb_exception.config_error x
              (Printf.sprintf "dir field should be different from `%s`"
                 Literals.library_file)
          else Ext_path.simple_convert_node_path_to_os_path str
        | Some x -> Bsb_exception.config_error x "dir expected to be a string"
        | None ->
          Bsb_exception.config_error x
            ("required field :" ^ Bsb_build_schemas.dir ^ " missing")
      in

      parsing_source_dir_map
        {cxt with is_dev = current_dir_index; cwd = Ext_path.concat cwd dir}
        map)
  | _ -> Bsb_file_groups.empty

and parsing_arr_sources cxt (file_groups : Ext_json_types.t array) =
  Ext_array.fold_left file_groups Bsb_file_groups.empty (fun origin x ->
      Bsb_file_groups.merge (parsing_single_source cxt x) origin)

and parse_sources (cxt : cxt) (sources : Ext_json_types.t) =
  match sources with
  | Arr file_groups -> parsing_arr_sources cxt file_groups.content
  | _ -> parsing_single_source cxt sources

let scan ~package_kind ~root ~cut_generators
    ~(* ~namespace  *)
     ignored_dirs x : t =
  parse_sources
    {
      ignored_dirs;
      package_kind;
      is_dev = false;
      cwd = Filename.current_dir_name;
      root;
      cut_generators;
      (* namespace; *)
      traverse = false;
    }
    x

(* Walk through to do some work *)
type walk_cxt = {
  cwd: string;
  root: string;
  traverse: bool;
  ignored_dirs: Set_string.t;
  gentype_language: string;
}

let rec walk_sources (cxt : walk_cxt) (sources : Ext_json_types.t) =
  match sources with
  | Arr {content} -> Ext_array.iter content (fun x -> walk_single_source cxt x)
  | x -> walk_single_source cxt x

and walk_single_source cxt (x : Ext_json_types.t) =
  match x with
  | Str {str = dir} ->
    let dir = Ext_path.simple_convert_node_path_to_os_path dir in
    walk_source_dir_map {cxt with cwd = Ext_path.concat cxt.cwd dir} None
  | Obj {map} -> (
    match map.?(Bsb_build_schemas.dir) with
    | Some (Str {str}) ->
      let dir = Ext_path.simple_convert_node_path_to_os_path str in
      walk_source_dir_map
        {cxt with cwd = Ext_path.concat cxt.cwd dir}
        map.?(Bsb_build_schemas.subdirs)
    | _ -> ())
  | _ -> ()

and walk_source_dir_map (cxt : walk_cxt) sub_dirs_field =
  let working_dir = Filename.concat cxt.root cxt.cwd in
  if not (Set_string.mem cxt.ignored_dirs cxt.cwd) then (
    let file_array = Sys.readdir working_dir in
    (* Remove .gen.js/.gen.tsx during clean up *)
    Ext_array.iter file_array (fun file ->
        let is_typescript = cxt.gentype_language = "typescript" in
        if
          (not is_typescript)
          && Ext_string.ends_with file Literals.suffix_gen_js
          || (is_typescript && Ext_string.ends_with file Literals.suffix_gen_tsx)
        then Sys.remove (Filename.concat working_dir file));
    let cxt_traverse = cxt.traverse in
    match (sub_dirs_field, cxt_traverse) with
    | None, true | Some (True _), _ ->
      Ext_array.iter file_array (fun f ->
          if
            (not (Set_string.mem cxt.ignored_dirs f))
            && Ext_sys.is_directory_no_exn (Filename.concat working_dir f)
          then
            walk_source_dir_map
              {
                cxt with
                cwd =
                  Ext_path.concat cxt.cwd
                    (Ext_path.simple_convert_node_path_to_os_path f);
                traverse = true;
              }
              None)
    | None, _ | Some (False _), _ -> ()
    | Some s, _ -> walk_sources cxt s)

(* It makes use of the side effect when [walk_sources], removing suffix_re_js,
   TODO: make it configurable
*)
let clean_re_js root =
  match
    Ext_json_parse.parse_json_from_file
      (Filename.concat root Literals.bsconfig_json)
  with
  | Obj {map} ->
    let ignored_dirs =
      match map.?(Bsb_build_schemas.ignored_dirs) with
      | Some (Arr {content = x}) ->
        Set_string.of_list (Bsb_build_util.get_list_string x)
      | Some _ | None -> Set_string.empty
    in
    let gentype_language =
      match map.?(Bsb_build_schemas.gentypeconfig) with
      | None -> ""
      | Some (Obj {map}) -> (
        match map.?(Bsb_build_schemas.language) with
        | None -> ""
        | Some (Str {str}) -> str
        | Some _ -> "")
      | Some _ -> ""
    in
    Ext_option.iter map.?(Bsb_build_schemas.sources) (fun config ->
        try
          walk_sources
            {
              root;
              traverse = true;
              cwd = Filename.current_dir_name;
              ignored_dirs;
              gentype_language;
            }
            config
        with _ -> ())
  | _ -> ()
  | exception _ -> ()
