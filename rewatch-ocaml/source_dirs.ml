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
  let path =
    File_util.path_of_parts root ["lib"; "bs"; ".sourcedirs.json"]
  in
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
