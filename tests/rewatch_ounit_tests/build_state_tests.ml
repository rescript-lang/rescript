open OUnit2

let check condition message = assert_bool message condition

let source name =
  Source.
    {
      name;
      implementation = "src/" ^ name ^ ".res";
      interface = None;
      is_dev = false;
      feature = None;
      deps = [];
    }

let tests =
  "build_state_tests" >:: fun _context ->
  let state = Build_state.create 2 in
  Build_state.add state ~key:"A" ~package_name:"package" ~package_root:"root"
    ~source:(source "A") ~raw_dependencies:[] ~last_compiled_cmi:(Some 1.)
    ~last_compiled_cmt:(Some 2.);
  Build_state.add state ~key:"B" ~package_name:"package" ~package_root:"root"
    ~source:(source "B") ~raw_dependencies:["A"] ~last_compiled_cmi:None
    ~last_compiled_cmt:None;
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
    (not (Build_state.dependency_compiled_after b a))
    "a module without a prior CMT relies on its own dirty state";
  check
    (a.dependents = ["B"] && b.dependencies = ["A"])
    "setting dependencies creates the reverse edge";
  check (not b.deps_dirty) "stored dependency state is marked initialized";
  Build_state.mark_dependents_compile_dirty state a ~is_blocked:(fun _ -> true);
  check (not b.compile_dirty) "CMI changes do not unblock cycle members";
  Build_state.mark_dependents_compile_dirty state a ~is_blocked:(fun _ -> false);
  check b.compile_dirty "CMI changes propagate through reverse edges";
  b.last_compiled_cmt <- Some 0.5;
  check
    (Build_state.dependency_compiled_after b a)
    "dependency CMI timestamps invalidate older dependents"
