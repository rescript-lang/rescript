type prepared = {
  compiler_context: Compiler_info.context;
  compile_assets: Compile_assets.t;
  build_state: Build_state.t;
  packages: (string, Package_plan.compilation) Hashtbl.t;
}

type source_reference = {
  package_root: string;
  module_: Source.module_;
  relative_path: string;
  absolute_path: string;
}

type readiness =
  | Not_prepared
  | Freshness_pending of prepared
  | Ready of prepared

type compiler_info_state = Needs_publication | Published

type t = {
  global_modules: (string, Module_graph.module_node) Hashtbl.t;
  namespace_maps: (string, Module_graph.namespace_map) Hashtbl.t;
  namespace_maps_by_name: (string, Module_graph.namespace_map list) Hashtbl.t;
  mutable graph_cycle: Module_graph.cycle_info option;
  package_plans: (string, Package_plan.t) Hashtbl.t;
  source_index: (string, source_reference) Hashtbl.t;
  pending_parse_paths: (string, unit) Hashtbl.t;
  public_outputs: (string, (string, unit) Hashtbl.t) Hashtbl.t;
  mutable readiness: readiness;
  mutable compiler_info_state: compiler_info_state;
  warning_state: Warning_state.t;
}

let create ~warning_state =
  {
    global_modules = Hashtbl.create 64;
    namespace_maps = Hashtbl.create 16;
    namespace_maps_by_name = Hashtbl.create 16;
    graph_cycle = None;
    package_plans = Hashtbl.create 32;
    source_index = Hashtbl.create 64;
    pending_parse_paths = Hashtbl.create 16;
    public_outputs = Hashtbl.create 32;
    readiness = Not_prepared;
    compiler_info_state = Needs_publication;
    warning_state;
  }

let is_ready session =
  match session.readiness with
  | Ready _ -> true
  | _ -> false

let prepared session =
  match session.readiness with
  | Freshness_pending prepared | Ready prepared -> Some prepared
  | Not_prepared -> None

let install_prepared session prepared =
  session.readiness <- Freshness_pending prepared

let mark_freshness_initialized session =
  match session.readiness with
  | Freshness_pending prepared -> session.readiness <- Ready prepared
  | Ready _ -> ()
  | Not_prepared -> invalid_arg "build state has not been prepared"

let find_global_module session key = Hashtbl.find_opt session.global_modules key

let add_global_module session key module_ =
  Hashtbl.add session.global_modules key module_

let global_module_values session =
  Hashtbl.to_seq_values session.global_modules |> List.of_seq

let find_namespace_maps session name =
  Hashtbl.find_opt session.namespace_maps_by_name name

let add_namespace_map session namespace_map =
  Hashtbl.add session.namespace_maps namespace_map.Module_graph.key
    namespace_map;
  let existing =
    find_namespace_maps session namespace_map.namespace
    |> Option.value ~default:[]
  in
  Hashtbl.replace session.namespace_maps_by_name namespace_map.namespace
    (namespace_map :: existing)

let find_namespace_map session key = Hashtbl.find session.namespace_maps key

let namespace_map_values session =
  Hashtbl.to_seq_values session.namespace_maps |> List.of_seq

let graph_cycle session = session.graph_cycle
let set_graph_cycle session cycle = session.graph_cycle <- cycle

let add_package_plan session package =
  Hashtbl.replace session.package_plans package.Package_plan.root package

let find_package_plan session root = Hashtbl.find_opt session.package_plans root

let iter_package_plans session f = Hashtbl.iter f session.package_plans
let package_plan_values session = Hashtbl.to_seq_values session.package_plans

let publish_compiler_info session write =
  match session.compiler_info_state with
  | Published -> ()
  | Needs_publication ->
    Hashtbl.iter (fun _ package -> write package) session.package_plans;
    session.compiler_info_state <- Published

let add_source_reference session normalized_path source =
  Hashtbl.replace session.source_index normalized_path source

let find_source_reference session normalized_path =
  Hashtbl.find_opt session.source_index normalized_path

let pending_parse_paths session =
  session.pending_parse_paths |> Hashtbl.to_seq_keys |> List.of_seq

let mark_parse_pending session path =
  Hashtbl.replace session.pending_parse_paths path ()

let clear_parse_pending session path =
  Hashtbl.remove session.pending_parse_paths path

let set_public_outputs session root outputs =
  Hashtbl.replace session.public_outputs root outputs

let iter_public_outputs session f = Hashtbl.iter f session.public_outputs

let warning_state session = session.warning_state
