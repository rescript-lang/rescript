module String_set = Set.Make (String)

type module_ = {
  key: string;
  package_name: string;
  package_root: string;
  source: Source.module_;
  mutable dependencies: string list;
  mutable dependents: String_set.t;
  mutable compile_dirty: bool;
  mutable last_compiled_cmi: float option;
  mutable last_compiled_cmt: float option;
}

type t = {modules: (string, module_) Hashtbl.t}

let create capacity = {modules = Hashtbl.create capacity}

let add state ~key ~package_name ~package_root ~source ~last_compiled_cmi
    ~last_compiled_cmt =
  Hashtbl.add state.modules key
    {
      key;
      package_name;
      package_root;
      source;
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
        String_set.remove key dependency_module.dependents)
    module_.dependencies;
  module_.dependencies <- dependencies;
  List.iter
    (fun dependency ->
      let dependency_module = find_exn state dependency in
      dependency_module.dependents <-
        String_set.add key dependency_module.dependents)
    dependencies

let mark_dependents_compile_dirty state module_ ~is_blocked =
  String_set.iter
    (fun dependent ->
      if not (is_blocked dependent) then
        (find_exn state dependent).compile_dirty <- true)
    module_.dependents
