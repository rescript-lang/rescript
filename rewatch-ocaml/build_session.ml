type readiness =
  | Not_prepared
  | Freshness_pending of Build_types.prepared
  | Ready of Build_types.prepared

type t = {
  active_features: (string, string list option) Hashtbl.t;
  global_modules: (string, Build_types.global_module) Hashtbl.t;
  namespace_maps: (string, Build_types.namespace_map) Hashtbl.t;
  namespace_maps_by_name: (string, Build_types.namespace_map list) Hashtbl.t;
  mutable graph_has_cycle: bool;
  graph_packages: (string, Build_types.graph_package) Hashtbl.t;
  source_index: (string, Build_types.source_reference) Hashtbl.t;
  pending_parse_paths: (string, unit) Hashtbl.t;
  public_outputs: (string, (string, unit) Hashtbl.t) Hashtbl.t;
  mutable readiness: readiness;
  warning_state: Warning_state.t;
}

let create ~warning_state =
  {
    active_features = Hashtbl.create 16;
    global_modules = Hashtbl.create 64;
    namespace_maps = Hashtbl.create 16;
    namespace_maps_by_name = Hashtbl.create 16;
    graph_has_cycle = false;
    graph_packages = Hashtbl.create 32;
    source_index = Hashtbl.create 64;
    pending_parse_paths = Hashtbl.create 16;
    public_outputs = Hashtbl.create 32;
    readiness = Not_prepared;
    warning_state;
  }

let is_ready session =
  match session.readiness with
  | Ready _ -> true
  | _ -> false

let prepared_exn session =
  match session.readiness with
  | Freshness_pending prepared | Ready prepared -> prepared
  | Not_prepared -> invalid_arg "build state has not been prepared"

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

let prepared_package_exn session root =
  let prepared = prepared_exn session in
  match Hashtbl.find_opt prepared.packages root with
  | Some package -> package
  | None -> invalid_arg ("package has not been prepared: " ^ root)

let find_active_features session root =
  Hashtbl.find_opt session.active_features root

let set_active_features session root features =
  Hashtbl.replace session.active_features root features

let find_global_module session key = Hashtbl.find_opt session.global_modules key

let add_global_module session key module_ =
  Hashtbl.add session.global_modules key module_

let global_module_values session =
  Hashtbl.to_seq_values session.global_modules |> List.of_seq

let find_namespace_maps session name =
  Hashtbl.find_opt session.namespace_maps_by_name name

let add_namespace_map session namespace_map =
  Hashtbl.add session.namespace_maps namespace_map.Build_types.key namespace_map;
  let existing =
    find_namespace_maps session namespace_map.namespace
    |> Option.value ~default:[]
  in
  Hashtbl.replace session.namespace_maps_by_name namespace_map.namespace
    (namespace_map :: existing)

let find_namespace_map session key = Hashtbl.find session.namespace_maps key

let namespace_map_values session =
  Hashtbl.to_seq_values session.namespace_maps |> List.of_seq

let graph_has_cycle session = session.graph_has_cycle
let set_graph_has_cycle session value = session.graph_has_cycle <- value

let add_graph_package session package =
  Hashtbl.replace session.graph_packages package.Build_types.graph_root package

let find_graph_package session root =
  Hashtbl.find_opt session.graph_packages root

let iter_graph_packages session f = Hashtbl.iter f session.graph_packages
let graph_package_values session = Hashtbl.to_seq_values session.graph_packages

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
