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
  bsb_project_root: string;
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
  project_root: string;
  shims_map: ModuleName.t ModuleNameMap.t;
  sources: string list;
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
    sources = [];
    suffix = ".bs.js";
  }

let bs_platform_lib ~config =
  match config.module_ with
  | ESModule -> config.platform_lib ^ "/lib/es6"
  | CommonJS -> config.platform_lib ^ "/lib/js"

let get_bs_curry_path ~config =
  Filename.concat (bs_platform_lib ~config) "curry.js"

(* ----- CLI-flag backing state ----------------------------------------- *)

(** The following refs are populated by bsc's CLI flags (registered in
    [rescript_compiler_main.ml]). Everything the gentype config used to read
    from [rescript.json] now comes through here instead. *)

let project_root = ref ""
let bsb_project_root = ref ""
let module_flag : module_ option ref = ref None
let module_resolution_flag : module_resolution option ref = ref None
let export_interfaces_flag = ref false
let generated_file_extension_flag : string option ref = ref None
let suffix_flag : string option ref = ref None
let shims : (string * string) list ref = ref []
let bs_dependencies_flag : string list ref = ref []
let source_dirs_flag : string list ref = ref []

let module_of_string = function
  | "commonjs" -> Some CommonJS
  | "esmodule" -> Some ESModule
  | _ -> None

let module_resolution_of_string = function
  | "node" -> Some Node
  | "node16" -> Some Node16
  | "bundler" -> Some Bundler
  | _ -> None

let add_shim raw =
  match String.split_on_char '=' raw with
  | [from_module; to_module] -> shims := (from_module, to_module) :: !shims
  | _ -> ()

let add_bs_dependency name =
  bs_dependencies_flag := name :: !bs_dependencies_flag

let add_source_dir dir = source_dirs_flag := dir :: !source_dirs_flag

(* ----- Build the Config.t from flags ---------------------------------- *)

let build_config ~namespace =
  let shims_map =
    !shims
    |> List.fold_left
         (fun map (from_module, to_module) ->
           let module_name =
             (from_module |> ModuleName.from_string_unsafe : ModuleName.t)
           in
           let shim_module_name = to_module |> ModuleName.from_string_unsafe in
           ModuleNameMap.add module_name shim_module_name map)
         ModuleNameMap.empty
  in
  let project_root =
    match !project_root with
    | "" -> Sys.getcwd ()
    | dir -> dir
  in
  let bsb_project_root =
    match !bsb_project_root with
    | "" -> project_root
    | dir -> dir
  in
  let module_ =
    match !module_flag with
    | Some m -> m
    | None -> default.module_
  in
  let module_resolution =
    match !module_resolution_flag with
    | Some r -> r
    | None -> default.module_resolution
  in
  let suffix =
    match !suffix_flag with
    | Some s -> s
    | None -> default.suffix
  in
  if !Debug.config then (
    Log_.item "Project root: %s\n" project_root;
    if bsb_project_root <> project_root then
      Log_.item "bsb project root: %s\n" bsb_project_root;
    Log_.item "Config shims:%d entries \n" (shims_map |> ModuleNameMap.cardinal));
  {
    bsb_project_root;
    bs_dependencies = List.rev !bs_dependencies_flag;
    emit_import_curry = false;
    emit_import_react = false;
    emit_type_prop_done = false;
    everything = false;
    export_interfaces = !export_interfaces_flag;
    generated_file_extension = !generated_file_extension_flag;
    module_;
    module_resolution;
    namespace;
    platform_lib = "rescript";
    project_root;
    shims_map;
    sources = List.rev !source_dirs_flag;
    suffix;
  }
