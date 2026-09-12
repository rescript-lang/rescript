open OUnit2

let check condition message = assert_bool message condition

let tests =
  "build_state_tests" >:: fun _context ->
  let state = Build_state.create 2 in
  Build_state.add state ~key:"A" ~kind:Build_state.Source_module
    ~last_compiled_cmi:(Some 1.) ~last_compiled_cmt:(Some 2.);
  Build_state.add state ~key:"B" ~kind:Build_state.Source_module
    ~last_compiled_cmi:None ~last_compiled_cmt:None;
  Build_state.set_dependencies state ~key:"A" [];
  Build_state.set_dependencies state ~key:"B" ["A"];
  let a = Build_state.find_exn state "A" in
  let b = Build_state.find_exn state "B" in
  check
    (a.last_compiled_cmi = Some 1. && a.last_compiled_cmt = Some 2.)
    "compile asset timestamps initialize module state";
  check
    (Build_state.has_complete_compile_assets a)
    "both compile artifacts form a complete cached compile";
  check
    (not (Build_state.has_complete_compile_assets b))
    "a missing compile artifact requires compilation";
  check
    (not (Build_state.dependency_tree_compiled_after state b a))
    "a module without a prior CMT relies on its own dirty state";
  check
    (Build_state.String_set.equal a.dependents
       (Build_state.String_set.singleton "B")
    && b.dependencies = ["A"])
    "setting dependencies creates the reverse edge";
  Build_state.mark_dependents_compile_dirty state a;
  check b.compile_dirty "CMI changes preserve pending dependent work";
  b.last_compiled_cmt <- Some 0.5;
  check
    (Build_state.dependency_tree_compiled_after state b a)
    "dependency CMI timestamps invalidate older dependents";
  Build_state.set_dependencies state ~key:"B" [];
  check
    (Build_state.String_set.is_empty a.dependents && b.dependencies = [])
    "updating dependencies removes obsolete reverse edges";
  Build_state.set_dependencies state ~key:"B" ["A"];
  Build_state.set_dependencies state ~key:"B" ["A"];
  check
    (Build_state.String_set.equal a.dependents
       (Build_state.String_set.singleton "B")
    && b.dependencies = ["A"])
    "updating dependencies does not duplicate reverse edges";
  let namespace_state = Build_state.create 3 in
  Build_state.add namespace_state ~key:"A" ~kind:Build_state.Source_module
    ~last_compiled_cmi:None ~last_compiled_cmt:None;
  Build_state.add namespace_state ~key:"namespace"
    ~kind:Build_state.Namespace_map ~last_compiled_cmi:None
    ~last_compiled_cmt:None;
  Build_state.add namespace_state ~key:"Consumer"
    ~kind:Build_state.Source_module ~last_compiled_cmi:None
    ~last_compiled_cmt:None;
  Build_state.set_dependencies namespace_state ~key:"A" [];
  Build_state.set_dependencies namespace_state ~key:"namespace" ["A"];
  Build_state.set_dependencies namespace_state ~key:"Consumer" ["namespace"];
  let namespace = Build_state.find_exn namespace_state "namespace" in
  let consumer = Build_state.find_exn namespace_state "Consumer" in
  Build_state.mark_dependents_compile_dirty namespace_state
    (Build_state.find_exn namespace_state "A");
  check
    (namespace.compile_dirty && consumer.compile_dirty)
    "namespace-map invalidation propagates to namespace consumers";
  consumer.compile_dirty <- false;
  consumer.last_compiled_cmt <- Some 2.;
  (Build_state.find_exn namespace_state "A").last_compiled_cmi <- Some 3.;
  check
    (Build_state.dependency_tree_compiled_after namespace_state consumer
       namespace)
    "restart freshness traverses namespace-map members";
  let entry_source : Source.module_ =
    {
      name = "Entry";
      implementation = "src/Entry.res";
      interface = None;
      is_dev = false;
    }
  in
  let entry : Build_types.global_module =
    {
      key = "Entry";
      package_name = "package";
      package_root = "root";
      source_path = entry_source.implementation;
      namespace =
        Config.Namespace_with_entry {name = "Namespace"; entry = "Entry"};
      allowed_dependencies = [];
      raw_dependencies = [];
    }
  in
  let namespace_map : Build_types.namespace_map =
    {
      key = Build_types.namespace_map_key "root";
      compiler_name = "@Namespace";
      namespace = "Namespace";
      package_name = "package";
      package_root = "root";
      members = [];
    }
  in
  let modules = Hashtbl.create 1 in
  Hashtbl.add modules entry.key entry;
  let namespace_maps = Hashtbl.create 1 in
  Hashtbl.add namespace_maps namespace_map.namespace [namespace_map];
  check
    (Build_preparation.resolved_dependencies
       ~find_module:(Hashtbl.find_opt modules)
       ~find_namespace_maps:(Hashtbl.find_opt namespace_maps)
       entry
    = [namespace_map.key])
    "namespace entry implicitly depends on its namespace map"
