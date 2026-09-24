module Module_name_map = Map.Make (Module_name)

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
  dep_paths: (string, string) Hashtbl.t;
      (** Map from package name to its install path, used to locate
          [.cmt]/[.cmti] files of cross-package references. *)
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
  shims_map: Module_name.t Module_name_map.t;
  sources: string list;
  suffix: string;
}

let default =
  {
    bsb_project_root = "";
    bs_dependencies = [];
    dep_paths = Hashtbl.create 0;
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
    shims_map = Module_name_map.empty;
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

type flag_refs = {
  project_root_ref: string ref;
  bsb_project_root_ref: string ref;
  module_ref: module_ option ref;
  module_resolution_ref: module_resolution option ref;
  export_interfaces_ref: bool ref;
  generated_file_extension_ref: string option ref;
  suffix_ref: string option ref;
  shims_ref: (string * string) list ref;
  bs_dependencies_ref: string list ref;
  source_dirs_ref: string list ref;
  dep_paths_ref: (string * string) list ref;
}

(* A request builds its GenType config from these flags. Another request must
   not reset them while its CMT or declaration output is being generated. *)
let flag_refs =
  Domain.DLS.new_key (fun () ->
      {
        project_root_ref = ref "";
        bsb_project_root_ref = ref "";
        module_ref = ref None;
        module_resolution_ref = ref None;
        export_interfaces_ref = ref false;
        generated_file_extension_ref = ref None;
        suffix_ref = ref None;
        shims_ref = ref [];
        bs_dependencies_ref = ref [];
        source_dirs_ref = ref [];
        dep_paths_ref = ref [];
      })

let project_root () = (Domain.DLS.get flag_refs).project_root_ref
let bsb_project_root () = (Domain.DLS.get flag_refs).bsb_project_root_ref
let module_flag () = (Domain.DLS.get flag_refs).module_ref
let module_resolution_flag () = (Domain.DLS.get flag_refs).module_resolution_ref
let export_interfaces_flag () = (Domain.DLS.get flag_refs).export_interfaces_ref
let generated_file_extension_flag () =
  (Domain.DLS.get flag_refs).generated_file_extension_ref
let suffix_flag () = (Domain.DLS.get flag_refs).suffix_ref
let shims () = (Domain.DLS.get flag_refs).shims_ref
let bs_dependencies_flag () = (Domain.DLS.get flag_refs).bs_dependencies_ref
let source_dirs_flag () = (Domain.DLS.get flag_refs).source_dirs_ref
let dep_paths_flag () = (Domain.DLS.get flag_refs).dep_paths_ref

let reset_flags () =
  project_root () := "";
  bsb_project_root () := "";
  module_flag () := None;
  module_resolution_flag () := None;
  export_interfaces_flag () := false;
  generated_file_extension_flag () := None;
  suffix_flag () := None;
  shims () := [];
  bs_dependencies_flag () := [];
  source_dirs_flag () := [];
  dep_paths_flag () := []

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
  | [from_module; to_module] ->
    shims () := (from_module, to_module) :: !(shims ())
  | _ -> ()

let add_bs_dependency name =
  bs_dependencies_flag () := name :: !(bs_dependencies_flag ())

let add_source_dir dir = source_dirs_flag () := dir :: !(source_dirs_flag ())

let add_dep_path raw =
  match String.split_on_char '=' raw with
  | [name; path] -> dep_paths_flag () := (name, path) :: !(dep_paths_flag ())
  | _ -> ()

(* ----- Build the Config.t from flags ---------------------------------- *)

let build_config ~namespace =
  let shims_map =
    !(shims ())
    |> List.fold_left
         (fun map (from_module, to_module) ->
           let module_name =
             (from_module |> Module_name.from_string_unsafe : Module_name.t)
           in
           let shim_module_name = to_module |> Module_name.from_string_unsafe in
           Module_name_map.add module_name shim_module_name map)
         Module_name_map.empty
  in
  let project_root =
    match !(project_root ()) with
    | "" -> Compiler_request_state.cwd ()
    | dir -> dir
  in
  let bsb_project_root =
    match !(bsb_project_root ()) with
    | "" -> project_root
    | dir -> dir
  in
  let module_ =
    match !(module_flag ()) with
    | Some m -> m
    | None -> default.module_
  in
  let module_resolution =
    match !(module_resolution_flag ()) with
    | Some r -> r
    | None -> default.module_resolution
  in
  let suffix =
    match !(suffix_flag ()) with
    | Some s -> s
    | None -> default.suffix
  in
  if !(Debug.config ()) then (
    Log_.item "Project root: %s\n" project_root;
    if bsb_project_root <> project_root then
      Log_.item "bsb project root: %s\n" bsb_project_root;
    Log_.item "Config shims:%d entries \n"
      (shims_map |> Module_name_map.cardinal));
  let dep_paths =
    let tbl = Hashtbl.create (List.length !(dep_paths_flag ())) in
    List.iter
      (fun (name, path) -> Hashtbl.add tbl name path)
      !(dep_paths_flag ());
    tbl
  in
  {
    bsb_project_root;
    bs_dependencies = !(bs_dependencies_flag ());
    dep_paths;
    emit_import_curry = false;
    emit_import_react = false;
    emit_type_prop_done = false;
    everything = false;
    export_interfaces = !(export_interfaces_flag ());
    generated_file_extension = !(generated_file_extension_flag ());
    module_;
    module_resolution;
    namespace;
    platform_lib = "rescript";
    project_root;
    shims_map;
    sources = !(source_dirs_flag ());
    suffix;
  }
