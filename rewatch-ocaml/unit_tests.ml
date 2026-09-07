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
    "namespace punctuation normalization"
