type module_ = {
  key: string;
  package_name: string;
  package_root: string;
  source: Source.module_;
  mutable raw_dependencies: string list;
  mutable dependencies: string list;
  mutable dependents: string list;
  mutable compile_dirty: bool;
  mutable deps_dirty: bool;
  mutable last_compiled_cmi: float option;
  mutable last_compiled_cmt: float option;
}

type t = {modules: (string, module_) Hashtbl.t}

let create capacity = {modules = Hashtbl.create capacity}

let add state ~key ~package_name ~package_root ~source ~raw_dependencies
    ~last_compiled_cmi ~last_compiled_cmt =
  Hashtbl.add state.modules key
    {
      key;
      package_name;
      package_root;
      source;
      raw_dependencies;
      dependencies = [];
      dependents = [];
      compile_dirty = false;
      deps_dirty = true;
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

let dependency_compiled_after module_ dependency =
  match dependency.last_compiled_cmi, module_.last_compiled_cmt with
  | Some dependency_time, Some module_time -> dependency_time > module_time
  | None, _ | _, None -> false

let set_dependencies state ~key dependencies =
  let module_ = find_exn state key in
  List.iter
    (fun dependency ->
      let dependency_module = find_exn state dependency in
      dependency_module.dependents <-
        List.filter (fun dependent -> dependent <> key) dependency_module.dependents)
    module_.dependencies;
  module_.dependencies <- dependencies;
  module_.deps_dirty <- false;
  List.iter
    (fun dependency ->
      let dependency_module = find_exn state dependency in
      if not (List.mem key dependency_module.dependents) then
        dependency_module.dependents <- key :: dependency_module.dependents)
    dependencies

let mark_dependents_compile_dirty state module_ ~is_blocked =
  List.iter
    (fun dependent ->
      if not (is_blocked dependent) then
        (find_exn state dependent).compile_dirty <- true)
    module_.dependents
