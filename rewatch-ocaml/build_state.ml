module String_set = Set.Make (String)

type module_kind = Source_module | Namespace_map
type cmi_change = Cmi_changed | Cmi_unchanged | Cmi_change_unknown

type module_ = {
  key: string;
  kind: module_kind;
  mutable dependencies: string list;
  mutable dependents: String_set.t;
  mutable compile_dirty: bool;
  mutable last_compiled_cmi: float option;
  mutable last_compiled_cmt: float option;
}

type t = {modules: (string, module_) Hashtbl.t}

let create capacity = {modules = Hashtbl.create capacity}

let add state ~key ~kind ~last_compiled_cmi ~last_compiled_cmt =
  Hashtbl.add state.modules key
    {
      key;
      kind;
      dependencies = [];
      dependents = String_set.empty;
      compile_dirty = false;
      last_compiled_cmi;
      last_compiled_cmt;
    }

let find state key = Hashtbl.find_opt state.modules key

let find_exn state key =
  match find state key with
  | Some module_ -> module_
  | None -> raise (Invalid_argument ("unknown build module " ^ key))

let has_complete_compile_assets module_ =
  Option.is_some module_.last_compiled_cmi
  && Option.is_some module_.last_compiled_cmt

let dependency_tree_compiled_after ?(namespace_freshness = Hashtbl.create 4)
    state module_ dependency =
  let rec latest_cmi dependency =
    match dependency.kind with
    | Source_module -> dependency.last_compiled_cmi
    | Namespace_map -> (
      match Hashtbl.find_opt namespace_freshness dependency.key with
      | Some modified -> modified
      | None ->
        let modified =
          dependency.dependencies
          |> List.filter_map (fun key -> latest_cmi (find_exn state key))
          |> List.fold_left max neg_infinity
          |> fun modified ->
          if modified = neg_infinity then None else Some modified
        in
        Hashtbl.add namespace_freshness dependency.key modified;
        modified)
  in
  match (latest_cmi dependency, module_.last_compiled_cmt) with
  | Some dependency_time, Some module_time -> dependency_time > module_time
  | None, _ | _, None -> false

let set_dependencies state ~key dependencies =
  let module_ = find_exn state key in
  List.iter
    (fun dependency ->
      let dependency_module = find_exn state dependency in
      dependency_module.dependents <-
        String_set.remove key dependency_module.dependents)
    module_.dependencies;
  module_.dependencies <- dependencies;
  List.iter
    (fun dependency ->
      let dependency_module = find_exn state dependency in
      dependency_module.dependents <-
        String_set.add key dependency_module.dependents)
    dependencies

let mark_dependents_compile_dirty state module_ =
  let visited = Hashtbl.create 8 in
  let rec mark dependent =
    if not (Hashtbl.mem visited dependent) then (
      Hashtbl.add visited dependent ();
      let dependent_module = find_exn state dependent in
      dependent_module.compile_dirty <- true;
      match dependent_module.kind with
      | Source_module -> ()
      | Namespace_map -> String_set.iter mark dependent_module.dependents)
  in
  String_set.iter mark module_.dependents

let record_published_cmi state ~compile_assets module_ ~path change =
  Compile_assets.refresh_cmi compile_assets ~key:module_.key ~path;
  module_.last_compiled_cmi <-
    Compile_assets.cmi compile_assets module_.key
    |> Option.map (fun entry -> entry.Compile_assets.modified);
  match change with
  | Cmi_unchanged -> ()
  | Cmi_changed | Cmi_change_unknown ->
    mark_dependents_compile_dirty state module_

let record_successful_compile ~compile_assets module_ ~cmt_path =
  Compile_assets.refresh_cmt compile_assets ~key:module_.key ~path:cmt_path;
  module_.last_compiled_cmt <-
    Compile_assets.cmt compile_assets module_.key
    |> Option.map (fun entry -> entry.Compile_assets.modified);
  module_.compile_dirty <- false
