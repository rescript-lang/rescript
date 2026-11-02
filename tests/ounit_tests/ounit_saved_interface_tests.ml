open OUnit

(* A saved interface depends on its own signature: an import that grows
   changes the CRC the interface records for it, not the interface's own. *)

let rec remove_tree path =
  match (Unix.lstat path).st_kind with
  | Unix.S_DIR ->
    Array.iter
      (fun name -> remove_tree (Filename.concat path name))
      (Sys.readdir path);
    Unix.rmdir path
  | _ -> Sys.remove path
  | exception Unix.Unix_error (Unix.ENOENT, _, _) -> ()

let with_temp_dir action =
  let path = Filename.temp_file "rescript-saved-interface-" "" in
  Sys.remove path;
  Unix.mkdir path 0o755;
  let path = Unix.realpath path in
  Fun.protect ~finally:(fun () -> remove_tree path) (fun () -> action path)

let write_on_disk path contents =
  Out_channel.with_open_bin path (fun channel ->
      Out_channel.output_string channel contents)

let compile ?(flags = []) root input =
  let result =
    Rescript_compiler_driver.run_request ~run_external:None ~cwd:root
      ~argv:
        (flags
        @ [
            "-nostdlib";
            "-nopervasives";
            "-I";
            root;
            "-bs-project-root";
            root;
            "-bs-package-name";
            "saved-interface-test";
            "-bs-package-output";
            "commonjs:.:.js";
          ])
      ~input
  in
  assert_equal
    ~printer:(fun code -> string_of_int code ^ ": " ^ result.stderr)
    0 result.exit_code

let own_crc cmi =
  match cmi.Cmi_format.cmi_crcs with
  | (name, Some crc) :: _ when name = cmi.cmi_name -> crc
  | _ -> assert_failure "an interface records its own CRC first"

let imported_crc cmi name =
  List.assoc_opt name cmi.Cmi_format.cmi_crcs |> Option.join

let growing_import_tests _ =
  with_temp_dir (fun root ->
      let interface name = Filename.concat root (name ^ ".cmi") in
      let compile_user () =
        compile root "User.res";
        Cmi_format.read_cmi (interface "User")
      in
      (* Matching a constructor with an inline record reads the import's
         signature, which creates identifiers before User's are saved. *)
      let shape = "type t = A | B({x: int})\nlet make = () => A\n" in
      write_on_disk (Filename.concat root "Shape.res") shape;
      write_on_disk
        (Filename.concat root "User.res")
        "type t = {value: int}\n\
         let value = switch Shape.make() {\n\
         | A => 1\n\
         | B({x}) => x\n\
         }\n\
         let make = value => {value: value}\n";
      compile root "Shape.res";
      let before = compile_user () in
      write_on_disk
        (Filename.concat root "Shape.res")
        (shape ^ "let extra = 2\n");
      compile root "Shape.res";
      let after = compile_user () in
      assert_bool "the import's CRC changed"
        (imported_crc before "Shape" <> imported_crc after "Shape");
      assert_equal ~printer:Digest.to_hex (own_crc before) (own_crc after))

(* By default a module records the interfaces it read and those they
   recorded; with -bs-record-read-imports, only those it read. *)
let read_imports_tests _ =
  with_temp_dir (fun root ->
      let write name contents =
        write_on_disk (Filename.concat root (name ^ ".res")) contents
      in
      write "Base" "type t = {value: int}\nlet make = value => {value: value}\n";
      write "Middle" "let base = Base.make(1)\nlet count = 2\n";
      write "Top" "let count = Middle.count\n";
      let recorded flags =
        List.iter
          (fun name -> compile ~flags root (name ^ ".res"))
          ["Base"; "Middle"; "Top"];
        let crcs name =
          (Cmi_format.read_cmi (Filename.concat root (name ^ ".cmi"))).cmi_crcs
          |> List.map fst
        in
        assert_bool "Middle records Base, which it read"
          (List.mem "Base" (crcs "Middle"));
        crcs "Top"
      in
      assert_bool "Top records Base by default" (List.mem "Base" (recorded []));
      assert_equal ~printer:(String.concat ", ") ["Top"; "Middle"]
        (recorded ["-bs-record-read-imports"]))

let suites =
  __FILE__
  >::: [
         "growing_import" >:: growing_import_tests;
         "read_imports" >:: read_imports_tests;
       ]
