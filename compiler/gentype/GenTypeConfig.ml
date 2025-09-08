module ModuleNameMap = Map.Make (ModuleName)

type module_ = CommonJS | ESModule

(** Compatibility for `compilerOptions.moduleResolution` in TypeScript projects. *)
type module_resolution =
  | Node  (** should drop extension on import statements *)
  | Node16
      (** should use TS output's extension (e.g. `.gen.js`) on import statements *)
  | Bundler
      (** should use TS input's extension (e.g. `.gen.tsx`) on import statements *)

type bs_version = int * int * int

type t = {
  mutable bsb_project_root: string;
  bs_dependencies: string list;
  mutable emit_import_curry: bool;
  mutable emit_import_react: bool;
  mutable emit_type_prop_done: bool;
  mutable everything: bool;
  export_interfaces: bool;
  generated_file_extension: string option;
  module_: module_;
  module_resolution: module_resolution;
  namespace: string option;
  platform_lib: string;
  mutable project_root: string;
  shims_map: ModuleName.t ModuleNameMap.t;
  sources: Ext_json_types.t option;
  suffix: string;
}

let default =
  {
    bsb_project_root = "";
    bs_dependencies = [];
    emit_import_curry = false;
    emit_import_react = false;
    emit_type_prop_done = false;
    everything = false;
    export_interfaces = false;
    generated_file_extension = None;
    module_ = ESModule;
    module_resolution = Node;
    namespace = None;
    platform_lib = "";
    project_root = "";
    shims_map = ModuleNameMap.empty;
    sources = None;
    suffix = ".bs.js";
  }

let bs_platform_lib ~config =
  match config.module_ with
  | ESModule -> config.platform_lib ^ "/lib/es6"
  | CommonJS -> config.platform_lib ^ "/lib/js"

let get_bs_curry_path ~config =
  Filename.concat (bs_platform_lib ~config) "curry.js"

type map = Ext_json_types.t Map_string.t

let get_opt s (map : map) = Map_string.find_opt map s

let get_bool s map =
  match map |> get_opt s with
  | Some (True _) -> Some true
  | Some (False _) -> Some false
  | _ -> None

let get_string_option s map =
  match map |> get_opt s with
  | Some (Str {str}) -> Some str
  | _ -> None

let get_shims map =
  let shims = ref [] in
  (match map |> get_opt "shims" with
  | Some (Obj {map = shims_map}) ->
    Map_string.iter shims_map (fun from_module to_module ->
        match to_module with
        | Ext_json_types.Str {str} -> shims := (from_module, str) :: !shims
        | _ -> ())
  | Some (Arr {content}) ->
    (* To be deprecated: array of strings *)
    content
    |> Array.iter (fun x ->
           match x with
           | Ext_json_types.Str {str} ->
             let from_to = str |> String.split_on_char '=' |> Array.of_list in
             assert (Array.length from_to == 2);
             shims :=
               ((from_to.(0) [@doesNotRaise]), (from_to.(1) [@doesNotRaise]))
               :: !shims
           | _ -> ())
  | _ -> ());
  !shims

let set_debug ~gtconf =
  match gtconf |> get_opt "debug" with
  | Some (Obj {map}) -> Map_string.iter map Debug.set_item
  | _ -> ()

let compiler_config_file = "rescript.json"
let legacy_compiler_config_file = "bsconfig.json"

let rec find_project_root ~dir =
  if
    Sys.file_exists (Filename.concat dir compiler_config_file)
    || Sys.file_exists (Filename.concat dir legacy_compiler_config_file)
  then dir
  else
    let parent = dir |> Filename.dirname in
    if parent = dir then (
      prerr_endline
        ("Error: cannot find project root containing " ^ compiler_config_file
       ^ ".");
      assert false)
    else find_project_root ~dir:parent

let read_config ~get_config_file ~namespace =
  let project_root = find_project_root ~dir:(Sys.getcwd ()) in
  let bsb_project_root =
    match Sys.getenv_opt "BSB_PROJECT_ROOT" with
    | None -> project_root
    | Some s -> s
  in
  let parse_config ~bsconf ~gtconf =
    let module_string = gtconf |> get_string_option "module" in
    let module_resolution_string =
      gtconf |> get_string_option "moduleResolution"
    in
    let export_interfaces_bool = gtconf |> get_bool "exportInterfaces" in
    let generated_file_extension_string_option =
      gtconf |> get_string_option "generatedFileExtension"
    in
    let shims_map =
      gtconf |> get_shims
      |> List.fold_left
           (fun map (from_module, to_module) ->
             let module_name =
               (from_module |> ModuleName.from_string_unsafe : ModuleName.t)
             in
             let shim_module_name =
               to_module |> ModuleName.from_string_unsafe
             in
             ModuleNameMap.add module_name shim_module_name map)
           ModuleNameMap.empty
    in
    set_debug ~gtconf;
    let module_ =
      let package_specs_module_string =
        match bsconf |> get_opt "package-specs" with
        | Some (Obj {map = package_specs}) ->
          package_specs |> get_string_option "module"
        | _ -> None
      in
      (* Give priority to gentypeconfig, followed by package-specs *)
      match (module_string, package_specs_module_string) with
      | Some "commonjs", _ -> CommonJS
      | Some ("esmodule" | "es6"), _ -> ESModule
      | None, Some "commonjs" -> CommonJS
      | None, Some ("esmodule" | "es6" | "es6-global") -> ESModule
      | _ -> default.module_
    in
    let module_resolution =
      match module_resolution_string with
      | Some "node" -> Node
      | Some "node16" -> Node16
      | Some "bundler" -> Bundler
      | _ -> default.module_resolution
    in
    let export_interfaces =
      match export_interfaces_bool with
      | None -> default.export_interfaces
      | Some b -> b
    in
    let generated_file_extension = generated_file_extension_string_option in
    let external_stdlib = bsconf |> get_string_option "external-stdlib" in
    let platform_lib =
      match external_stdlib with
      | None -> "rescript"
      | Some external_stdlib -> external_stdlib
    in
    if !Debug.config then (
      Log_.item "Project roLiterals.bsconfig_jsonot: %s\n" project_root;
      if bsb_project_root <> project_root then
        Log_.item "bsb project root: %s\n" bsb_project_root;
      Log_.item "Config module:%s shims:%d entries \n"
        (match module_string with
        | None -> ""
        | Some s -> s)
        (shims_map |> ModuleNameMap.cardinal));
    let namespace =
      match bsconf |> get_opt "namespace" with
      | Some (True _) -> namespace
      | _ -> default.namespace
    in
    let suffix =
      match bsconf |> get_string_option "suffix" with
      | Some s -> s
      | _ -> default.suffix
    in
    let bs_dependencies =
      match
        (bsconf |> get_opt "dependencies", bsconf |> get_opt "bs-dependencies")
      with
      | Some (Arr {content}), None | None, Some (Arr {content}) ->
        let strings = ref [] in
        content
        |> Array.iter (fun x ->
               match x with
               | Ext_json_types.Str {str} -> strings := str :: !strings
               | _ -> ());
        !strings
      | _ -> default.bs_dependencies
    in
    let sources =
      match bsconf |> get_opt "sources" with
      | Some source_item -> Some source_item
      | _ -> default.sources
    in
    let everything = false in
    {
      bsb_project_root;
      bs_dependencies;
      suffix;
      emit_import_curry = false;
      emit_import_react = false;
      emit_type_prop_done = false;
      everything;
      export_interfaces;
      generated_file_extension;
      module_;
      module_resolution;
      namespace;
      platform_lib;
      project_root;
      shims_map;
      sources;
    }
  in
  let default_config = {default with project_root; bsb_project_root} in
  match get_config_file ~project_root with
  | Some bs_config_file -> (
    try
      let json = bs_config_file |> Ext_json_parse.parse_json_from_file in
      match json with
      | Obj {map = bsconf} -> (
        match bsconf |> get_opt "gentypeconfig" with
        | Some (Obj {map = gtconf}) -> parse_config ~bsconf ~gtconf
        | _ -> default_config)
      | _ -> default_config
    with _ -> default_config)
  | None -> default_config
