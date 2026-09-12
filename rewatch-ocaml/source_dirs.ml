type scan = {
  build_root: string;
  scan_dirs: string list;
  also_scan_build_root: bool;
}

let scan_json scan =
  `Assoc
    [
      ("also_scan_build_root", `Bool scan.also_scan_build_root);
      ("build_root", `String scan.build_root);
      ("scan_dirs", `List (List.map (fun path -> `String path) scan.scan_dirs));
    ]

let write ~root ~dirs ~packages ~scans =
  let path = File_util.path_of_parts root ["lib"; "bs"; ".sourcedirs.json"] in
  File_util.ensure_dir (Filename.dirname path);
  let json =
    `Assoc
      [
        ("cmt_scan", `List (List.map scan_json scans));
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
  File_util.write_file_atomic ~ensure_parent:false ~perm:0o644 path
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
          (File_util.path_of_parts package.root
             ["lib"; "bs"; ".sourcedirs.json"]));
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
  let scans =
    local_packages
    |> List.map (fun package ->
        let relative_root = relative_package_root package in
        let build_root =
          if relative_root = "" then File_util.path_of_parts "" ["lib"; "bs"]
          else File_util.path_of_parts relative_root ["lib"; "bs"]
        in
        {
          build_root;
          scan_dirs = source_directories package;
          also_scan_build_root = true;
        })
    |> List.sort (fun (left : scan) right ->
        String.compare left.build_root right.build_root)
  in
  write ~root:root_config.root ~dirs ~packages:package_roots ~scans
