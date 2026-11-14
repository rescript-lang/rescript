open OUnit2

let check condition message = assert_bool message condition

let dependency_state_tests _context =
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
    ((not namespace.compile_dirty) && consumer.compile_dirty)
    "namespace-map barriers propagate invalidation without scheduling map work";
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
  let entry : Module_graph.module_node =
    {
      key = "Entry";
      package_name = "package";
      package_root = "root";
      source_path = entry_source.implementation;
      namespace =
        Config.Namespace_with_entry {name = "Namespace"; entry = "Entry"};
      is_dev = false;
      visible_packages = Hashtbl.create 0;
      raw_dependencies = [];
    }
  in
  let namespace_map : Module_graph.namespace_map =
    {
      key = Module_graph.namespace_map_key "root";
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
    (Module_graph.resolved_dependencies ~find_module:(Hashtbl.find_opt modules)
       ~find_namespace_maps:(Hashtbl.find_opt namespace_maps)
       entry
    = [namespace_map.key])
    "namespace entry implicitly depends on its namespace map"

let dev_module_visibility_tests _context =
  let visible packages =
    let table = Hashtbl.create 2 in
    List.iter (fun package -> Hashtbl.replace table package ()) packages;
    table
  in
  let node ~package ~namespace ~is_dev key raw_dependencies :
      Module_graph.module_node =
    {
      key;
      package_name = package;
      package_root = package;
      source_path = "src/" ^ key ^ ".res";
      namespace;
      is_dev;
      visible_packages = visible ["provider"; "plain"; package];
      raw_dependencies;
    }
  in
  let provider =
    node ~package:"provider" ~namespace:(Config.Namespace "Provider")
  in
  let consumer =
    node ~package:"consumer" ~namespace:(Config.Namespace "Consumer")
  in
  let plain = node ~package:"plain" ~namespace:Config.No_namespace in
  let provider_map : Module_graph.namespace_map =
    {
      key = Module_graph.namespace_map_key "provider";
      compiler_name = "Provider";
      namespace = "Provider";
      package_name = "provider";
      package_root = "provider";
      members = ["Public-Provider"];
    }
  in
  let modules = Hashtbl.create 4 in
  List.iter
    (fun (node : Module_graph.module_node) -> Hashtbl.add modules node.key node)
    [
      provider ~is_dev:false "Public-Provider" [];
      provider ~is_dev:true "Config-Provider" [];
      plain ~is_dev:true "Helper" [];
    ];
  let namespace_maps = Hashtbl.create 1 in
  Hashtbl.add namespace_maps "Provider" [provider_map];
  let resolved (node : Module_graph.module_node) =
    Module_graph.resolved_dependencies ~find_module:(Hashtbl.find_opt modules)
      ~find_namespace_maps:(Hashtbl.find_opt namespace_maps)
      node
  in
  assert_equal ["Config-Provider"]
    (resolved (provider ~is_dev:true "Local-Provider" ["Config"]))
    ~msg:"a dev module depends on a dev module of its package";
  assert_equal []
    (resolved (provider ~is_dev:false "Bad-Provider" ["Config"]))
    ~msg:"a non-dev module does not see the dev modules of its package";
  assert_equal [provider_map.key]
    (resolved (consumer ~is_dev:true "Leak-Consumer" ["Provider.Config"]))
    ~msg:"another package does not see a namespaced package's dev modules";
  assert_equal ["Public-Provider"]
    (resolved (consumer ~is_dev:true "Use-Consumer" ["Provider.Public"]))
    ~msg:"another package sees a namespaced package's other modules";
  assert_equal ["Helper"]
    (resolved (consumer ~is_dev:false "Plain-Consumer" ["Helper"]))
    ~msg:"dev modules of a package without a namespace stay global"

let carries_forward_dirty_state_only_for_matching_sources _context =
  let previous = Build_state.create 2 in
  let next = Build_state.create 2 in
  List.iter
    (fun state ->
      List.iter
        (fun key ->
          Build_state.add state ~key ~kind:Build_state.Source_module
            ~last_compiled_cmi:(Some 1.) ~last_compiled_cmt:(Some 2.))
        ["Same"; "Renamed"])
    [previous; next];
  (Build_state.find_exn previous "Same").compile_dirty <- true;
  (Build_state.find_exn previous "Renamed").compile_dirty <- true;
  Build_state.carry_forward_compile_dirty ~previous ~next
    ~same_source:(fun key -> key = "Same");
  check (Build_state.find_exn next "Same").compile_dirty
    "blocked work survives a full rebuild for the same source";
  check
    (not (Build_state.find_exn next "Renamed").compile_dirty)
    "blocked work is not transferred to another source with the same key"

let tests =
  "build_state_tests"
  >::: [
         "dependency state" >:: dependency_state_tests;
         "dev module visibility" >:: dev_module_visibility_tests;
         "carry forward blocked work"
         >:: carries_forward_dirty_state_only_for_matching_sources;
       ]
