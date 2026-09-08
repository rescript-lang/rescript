let check condition message = if not condition then failwith message

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

let () =
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
  check (a.last_compiled_cmi = Some 1. && a.last_compiled_cmt = Some 2.)
    "compile asset timestamps initialize module state";
  check (a.dependents = ["B"] && b.dependencies = ["A"])
    "setting dependencies creates the reverse edge";
  check (not b.deps_dirty) "stored dependency state is marked initialized"
