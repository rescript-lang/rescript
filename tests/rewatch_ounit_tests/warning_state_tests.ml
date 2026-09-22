open OUnit2

let check condition message = assert_bool message condition

let outputs state =
  Warning_state.entries state
  |> List.map (fun entry -> entry.Warning_state.output)

let tests =
  "warning_state_tests" >:: fun _context ->
  let state = Warning_state.create () in
  Warning_state.set state ~module_name:"Zed" ~package_root:"root"
    ~path:"src/Zed.res" ~output:"warning: zed\n";
  Warning_state.set state ~module_name:"Alpha" ~package_root:"root"
    ~path:"src/Alpha.res" ~output:"warning: alpha\n";
  check
    (outputs state = ["warning: alpha\n"; "warning: zed\n"])
    "stored warnings replay in module-name order";
  Warning_state.set state ~module_name:"Middle" ~package_root:"root"
    ~path:"src/Middle.res" ~output:"warning: fresh\n";
  check
    (outputs state = ["warning: alpha\n"; "warning: fresh\n"; "warning: zed\n"])
    "fresh and stored warnings share one deterministic order";
  Warning_state.remove state ~package_root:"root" ~path:"src/Alpha.res";
  check
    (outputs state = ["warning: fresh\n"; "warning: zed\n"])
    "a successfully recompiled path loses its stored warning";
  Warning_state.set state ~module_name:"Module" ~package_root:"root"
    ~path:"src/Module.res" ~output:"warning: implementation\n";
  Warning_state.set state ~module_name:"Module" ~package_root:"root"
    ~path:"src/Module.resi" ~output:"warning: interface\n";
  Warning_state.retain_paths state [Filename.concat "root" "src/Module.resi"];
  check
    (outputs state = ["warning: interface\n"])
    "renamed implementations are discarded while matching interfaces remain"
