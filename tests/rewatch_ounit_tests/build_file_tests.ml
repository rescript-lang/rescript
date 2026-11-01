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
         "copy" >:: copy_tests;
         "parallel_map" >:: parallel_map_tests;
       ]
