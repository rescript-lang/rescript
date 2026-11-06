(* A package's artifact directory is flat, so its scan is the build root
   alone: no subdirectories, and also_scan_build_root. *)
let scan_json build_root =
  `Assoc
    [
      ("also_scan_build_root", `Bool true);
      ("build_root", `String build_root);
      ("scan_dirs", `List []);
    ]

let write ~root ~dirs ~packages ~build_roots =
  let path =
    Filename.concat (Build_artifacts.artifact_dir root) ".sourcedirs.json"
  in
  File_util.ensure_dir (Filename.dirname path);
  let json =
    `Assoc
      [
        ("cmt_scan", `List (List.map scan_json build_roots));
        ("dirs", `List (List.map (fun path -> `String path) dirs));
        ("generated", `List []);
        ( "pkgs",
          `List
            (List.map
               (fun (name, path) -> `List [`String name; `String path])
               packages) );
        ("version", `Int 2);
      ]
  in
  File_util.write_file_atomic ~ensure_parent:false path
    (Yojson.Safe.to_string json)

let write_build ~(root_config : Config.t) session =
  let packages =
    Build_session.package_plan_values session
    |> List.of_seq
    |> List.sort (fun (left : Package_plan.t) right ->
        String.compare left.root right.root)
  in
  packages
  |> List.iter (fun package ->
      if package.Package_plan.root <> root_config.root then
        File_util.remove_file
          (Filename.concat
             (Build_artifacts.artifact_dir package.root)
             ".sourcedirs.json"));
  let local_packages =
    List.filter (fun package -> package.Package_plan.is_local) packages
  in
  let source_directories package =
    package.Package_plan.modules
    |> List.map (fun module_ -> Filename.dirname module_.Source.implementation)
    |> List.sort_uniq String.compare
  in
  let relative_package_root package =
    if package.Package_plan.root = root_config.root then ""
    else Project_context.relative_to root_config.root package.root
  in
  let dirs =
    local_packages
    |> List.concat_map (fun package ->
        let relative_root = relative_package_root package in
        source_directories package
        |> List.map (fun directory ->
            if relative_root = "" then directory
            else Filename.concat relative_root directory))
    |> List.sort_uniq String.compare
  in
  let package_roots = Hashtbl.create 16 in
  local_packages
  |> List.iter (fun package ->
      package.Package_plan.dependencies
      |> List.iter (fun dependency ->
          Hashtbl.replace package_roots dependency.Package_plan.declaration.name
            dependency.directory));
  let package_roots =
    Hashtbl.to_seq package_roots
    |> List.of_seq
    |> List.sort (fun (left, _) (right, _) -> String.compare left right)
  in
  let build_roots =
    local_packages
    |> List.map (fun package ->
        Build_artifacts.artifact_dir (relative_package_root package))
    |> List.sort String.compare
  in
  write ~root:root_config.root ~dirs ~packages:package_roots ~build_roots
