let check condition message = if not condition then failwith message

let () =
  let node name deps = (name, deps) in
  let nodes = [node "C" ["B"]; node "A" []; node "B" ["A"]] in
  let sorted =
    Graph.topological_sort nodes ~name:fst ~deps:snd |> List.map fst
  in
  check (sorted = ["A"; "B"; "C"]) "topological ordering";
  let cycle_detected =
    try
      ignore
        (Graph.topological_sort
           [node "A" ["B"]; node "B" ["A"]]
           ~name:fst ~deps:snd);
      false
    with Graph.Cycle _ -> true
  in
  check cycle_detected "cycle detection";
  let blocked =
    Build.blocked_dependents
      [
        ("A", ["B"]);
        ("B", ["A"]);
        ("C", ["A"]);
        ("D", ["C"]);
        ("Unrelated", []);
      ]
      ["A"; "B"]
  in
  check
    (List.for_all (fun name -> List.mem name blocked) ["A"; "B"; "C"; "D"])
    "cycle transitive dependents are blocked";
  check (not (List.mem "Unrelated" blocked))
    "cycle-unrelated modules remain schedulable";
  let temporary = Filename.temp_file "rewatch-ocaml-package-path-" "" in
  Sys.remove temporary;
  Unix.mkdir temporary 0o755;
  let package = Filename.concat temporary "package" in
  let node_modules = Filename.concat temporary "node_modules" in
  Unix.mkdir package 0o755;
  Unix.mkdir node_modules 0o755;
  Unix.symlink package (Filename.concat node_modules "dependency");
  Fun.protect
    ~finally:(fun () ->
      Sys.remove (Filename.concat node_modules "dependency");
      Unix.rmdir node_modules;
      Unix.rmdir package;
      Unix.rmdir temporary)
    (fun () ->
      match Build.dependency_path temporary "dependency" with
      | Some resolved ->
        check (resolved = Unix.realpath package)
          "dependency paths are canonicalized"
      | None -> failwith "dependency symlink was not resolved");
  check
    (Config.namespace_from_package_name "@testrepo/deprecated-config"
    = "TestrepoDeprecatedConfig")
    "scoped package namespace normalization";
  check
    (Config.namespace_from_package_name "some.namespace/name_here"
    = "SomenamespaceName_here")
    "namespace punctuation normalization";
  check
    (Build.strip_ansi "plain \027[1;31mred\027[0m text" = "plain red text")
    "compiler log ANSI stripping";
  check
    (match Cli.parse [|"rescript-ocaml"; "-vvvv"; "watch"|] with
    | Cli.Watch _ -> true
    | _ -> false)
    "leading verbosity before watch";
  let watch_no_timing_rejected =
    try
      ignore (Cli.parse [|"rescript-ocaml"; "watch"; "--no-timing"|]);
      false
    with Cli.Error _ -> true
  in
  check watch_no_timing_rejected "watch rejects build-only --no-timing";
  let lock_root = Filename.temp_file "rewatch-ocaml-stale-lock-" "" in
  Sys.remove lock_root;
  Unix.mkdir lock_root 0o755;
  let lock_dir = Filename.concat lock_root "lib" in
  Unix.mkdir lock_dir 0o755;
  let lock = Filename.concat lock_dir "build.lock" in
  let takeover = lock ^ ".takeover" in
  let write_owner path owner =
    let channel = open_out path in
    Fun.protect ~finally:(fun () -> close_out_noerr channel) (fun () ->
      output_string channel owner)
  in
  write_owner lock "999999999";
  write_owner takeover "999999999";
  Fun.protect
    ~finally:(fun () ->
      Build.remove_file takeover;
      Build.remove_file lock;
      Unix.rmdir lock_dir;
      Unix.rmdir lock_root)
    (fun () ->
      let release = Build.acquire_build_lock lock_root in
      check
        (Build.read_lock_owner lock = Some (string_of_int (Unix.getpid ())))
        "stale build lock is replaced";
      check (not (Sys.file_exists takeover)) "stale takeover marker is removed";
      release ())
