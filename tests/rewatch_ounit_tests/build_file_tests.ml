open OUnit2

let check condition message = assert_bool message condition
let with_temp_dir = Test_support.with_temp_dir "rewatch-build-files-"

let ast_contents ~dependencies ~source =
  let dependency_block = "\n" ^ String.concat "\n" dependencies ^ "\n" in
  let length = Bytes.create 4 in
  Bytes.set_int32_be length 0 (Int32.of_int (String.length dependency_block));
  Bytes.to_string length ^ dependency_block ^ source ^ "\n"
  ^ Marshal.to_string "the marshalled AST" []

(* An AST held by the build store reads like the same AST on disk, damaged
   ones included, and so does one whose header outgrows the first read. *)
let ast_header_tests _context =
  with_temp_dir (fun root ->
      let cases =
        [
          ( "Plain.ast",
            ast_contents ~dependencies:["Belt"; "Js"] ~source:"/a.res" );
          ( "Relative.iast",
            ast_contents ~dependencies:["Belt_Id"] ~source:"../../src/R.resi" );
          ( "Long.ast",
            ast_contents
              ~dependencies:(List.init 600 (Printf.sprintf "Dependency%04d"))
              ~source:"/long.res" );
          ("Huge.ast", "\x3f\xff\xff\xffshort");
          ("Negative.ast", "\xff\xff\xff\xffshort");
          ("Short.ast", "\x00\x00");
          ("Unterminated.ast", "\x00\x00\x00\x02\nA/no/newline");
        ]
      in
      List.iter
        (fun (name, contents) ->
          let path = Filename.concat root name in
          Test_support.write_file path contents;
          let from_disk = Ast_header.read path in
          let from_store =
            Build_file_store.with_build (fun () ->
                Build_file_store.write path contents;
                Ast_header.read path)
          in
          check (from_disk = from_store) (name ^ " reads alike from memory"))
        cases;
      let long = Ast_header.read (Filename.concat root "Long.ast") in
      check
        (List.length long.dependencies = 600 && long.source = Some "/long.res")
        "a header longer than the first read is read whole")

(* The first read takes 4096 bytes. A header that ends within them is read
   from them, and a longer one from the whole file. *)
let ast_header_boundary_tests _context =
  with_temp_dir (fun root ->
      List.iter
        (fun header_length ->
          let dependency_block = "\nBelt\n" in
          let source =
            "/"
            ^ String.make
                (header_length - 4 - String.length dependency_block - 2)
                's'
          in
          let path =
            Filename.concat root (Printf.sprintf "A%d.ast" header_length)
          in
          Test_support.write_file path
            (ast_contents ~dependencies:["Belt"] ~source);
          let header = Ast_header.read path in
          check
            (header.source = Some source && header.dependencies = ["Belt"])
            (Printf.sprintf "a header of %d bytes is read" header_length))
        [4095; 4096; 4097])

(* The published headers are read before this build parses, so a module
   parsed in this build takes its dependencies from its new AST, whether or
   not the build store still holds it. *)
let parsed_dependencies_tests _context =
  with_temp_dir (fun root ->
      let build_dir = Filename.concat root "lib/bs" in
      let ocaml_dir = Filename.concat root "lib/ocaml" in
      let path = "src/A.res" in
      let source = Filename.concat root path in
      Test_support.write_file
        (Filename.concat root "rescript.json")
        {|{"name":"graph","sources":{"dir":"src"}}|};
      Test_support.write_file source "let a = New.a\n";
      Test_support.write_file
        (Build_artifacts.published_ast_path ~ocaml_dir path)
        (ast_contents ~dependencies:["Old"] ~source);
      Test_support.write_file
        (Filename.concat build_dir (Source.ast_path path))
        (ast_contents ~dependencies:["New"] ~source);
      let config = Config.load_root root in
      let package : Package_plan.t =
        {
          name = "graph";
          root;
          is_local = true;
          config;
          compile_config = config;
          build_dir;
          ocaml_dir;
          dependencies = [];
          gentype_dependency_args = [];
          modules =
            [
              {
                Source.name = "A";
                implementation = path;
                interface = None;
                is_dev = false;
              };
            ];
          source_mtimes = Hashtbl.create 1;
          source_files = [source];
          present_source_files = [source];
        }
      in
      let dependencies parsed_paths =
        let graph =
          Module_graph.initialize ~root_config:config ~package_plans:[package]
            ~compile_assets:(Compile_assets.create [ocaml_dir])
            ~parsed_paths ~failed_parse_paths:(Hashtbl.create 1)
        in
        List.concat_map
          (fun (node : Module_graph.module_node) -> node.raw_dependencies)
          graph.nodes
      in
      check
        (dependencies (Hashtbl.create 1) = ["Old"])
        "a module not parsed in this build has its published dependencies";
      let parsed_paths = Hashtbl.create 1 in
      Hashtbl.replace parsed_paths source ();
      check
        (dependencies parsed_paths = ["New"])
        "a module parsed in this build has its new AST's dependencies")

let copy_tests _context =
  with_temp_dir (fun root ->
      let source = Filename.concat root "A.cmi" in
      let destination = Filename.concat root "published.cmi" in
      let missing = Filename.concat root "Missing.map" in
      Build_file_store.with_build (fun () ->
          Build_file_store.write source "interface";
          check
            (File_util.copy_build_file_if_different source destination)
            "a new destination is copied";
          check
            (not (File_util.copy_build_file_if_different source destination))
            "an equal destination is kept";
          Build_file_store.write source "changed";
          check
            (File_util.copy_build_file_if_different source destination)
            "a changed source is copied";
          File_util.copy_optional_build_file source missing;
          File_util.copy_optional_build_file
            (Filename.concat root "Absent.map")
            missing;
          check
            (not (Sys.file_exists missing))
            "a missing optional source removes the destination");
      check
        (File_util.read_file destination = "changed")
        "copies are written to disk")

let save_interface path ?(name = "A") ?(flags = []) crcs =
  File_util.ensure_dir (Filename.dirname path);
  ignore
    (Cmi_format.create_cmi path
       {cmi_name = name; cmi_sign = []; cmi_crcs = crcs; cmi_flags = flags})

let change_printer = function
  | Build_state.Cmi_changed -> "changed"
  | Cmi_unchanged -> "unchanged"
  | Cmi_change_unknown -> "unknown"

(* An interface is changed for its dependents when its own CRC or its flags
   change. One that only records other CRCs for its imports is published with
   its previous modification time. *)
let publish_interface_tests _context =
  with_temp_dir (fun root ->
      let source = Filename.concat root "bs/A.cmi" in
      let destination = Filename.concat root "ocaml/A.cmi" in
      let publish () = Published_interface.publish ~source ~destination in
      let check_change expected message =
        assert_equal ~printer:change_printer ~msg:message expected (publish ())
      in
      save_interface source [("B", Some (Digest.string "first"))];
      File_util.ensure_dir (Filename.dirname destination);
      check_change Cmi_changed "a first interface is new";
      check_change Cmi_unchanged "the same interface is unchanged";
      Unix.utimes destination 1000. 1000.;
      save_interface source [("B", Some (Digest.string "second"))];
      check_change Cmi_unchanged "other CRCs for imports leave it unchanged";
      check
        (File_util.read_file destination = File_util.read_file source)
        "an interface with other imports is still published";
      check
        ((Unix.stat destination).st_mtime = 1000.)
        "it keeps its modification time";
      save_interface source ~flags:[Deprecated "use B"] [];
      check_change Cmi_changed "new flags change it";
      save_interface source ~name:"Other" [];
      check_change Cmi_changed "a new CRC changes it";
      check
        ((Unix.stat destination).st_mtime > 1000.)
        "a changed interface has a new modification time")

(* A module is out of date when an interface of this build that it read has
   another CRC than the one it recorded. *)
let imports_changed_tests _context =
  with_temp_dir (fun root ->
      let interface name = Filename.concat root (name ^ ".cmi") in
      save_interface (interface "B") ~name:"B" [];
      let current =
        match Cmi_format.read_cmi (interface "B") with
        | {cmi_crcs = (_, crc) :: _} -> crc
        | {cmi_crcs = []} -> assert_failure "an interface records its own CRC"
      in
      let published = function
        | "B" -> Some (interface "B")
        | _ -> None
      in
      let imports_changed ?(has_interface = false) recorded =
        save_interface (interface "A") recorded;
        Published_interface.imports_changed ~published ~cmi_path:(interface "A")
          ~has_interface
      in
      check
        (not
           (imports_changed
              [("B", current); ("Outside", Some (Digest.string "any"))]))
        "current CRCs and imports from outside the build are unchanged";
      check
        (imports_changed [("B", Some (Digest.string "old"))])
        "another recorded CRC is a change";
      check
        (imports_changed ~has_interface:true [("B", current)])
        "an implementation whose typed tree is missing counts as changed";
      Sys.remove (interface "A");
      check
        (Published_interface.imports_changed ~published
           ~cmi_path:(interface "A") ~has_interface:false)
        "a missing interface counts as changed")

let parallel_map_tests _context =
  let items = List.init 1000 Fun.id in
  check
    (Parallel.map (fun item -> item * 2) items
    = List.map (fun item -> item * 2) items)
    "results keep the order of the items";
  match
    Parallel.map
      (fun item -> if item mod 300 = 299 then failwith (string_of_int item))
      items
  with
  | _ -> check false "a failing item raises"
  | exception Failure item ->
    check (item = "299") "the first failing item's exception is raised"

let tests =
  "build_file_tests"
  >::: [
         "ast_header" >:: ast_header_tests;
         "ast_header_boundary" >:: ast_header_boundary_tests;
         "parsed_dependencies" >:: parsed_dependencies_tests;
         "copy" >:: copy_tests;
         "publish_interface" >:: publish_interface_tests;
         "imports_changed" >:: imports_changed_tests;
         "parallel_map" >:: parallel_map_tests;
       ]
