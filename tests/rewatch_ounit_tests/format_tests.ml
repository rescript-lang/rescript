open OUnit2

let check condition message = assert_bool message condition

let contains text fragment =
  let text_length = String.length text in
  let fragment_length = String.length fragment in
  let rec loop index =
    if index + fragment_length > text_length then false
    else if String.sub text index fragment_length = fragment then true
    else loop (index + 1)
  in
  fragment_length = 0 || loop 0

let write_file = Test_support.write_file

let with_temp_dir = Test_support.with_temp_dir "rewatch-ocaml-format-"

let tests =
  "format_tests" >:: fun _context ->
  check
    (Format.formatting_error "stdin" "invalid source"
    = "Error formatting stdin: invalid source")
    "stdin formatting failures do not expose a temporary filename";
  check
    (Format.formatting_error "src/A.res" "invalid source"
    = "Error formatting src/A.res: invalid source")
    "file formatting failures retain the source path";
  check
    (Format.format_check_summary 1 = "The file listed above needs formatting")
    "format check uses Rust's singular summary";
  check
    (Format.format_check_summary 2 = "The 2 files listed above need formatting")
    "format check uses Rust's plural summary";
  with_temp_dir (fun root ->
      let parent = Filename.concat root "not-a-directory" in
      let path = Filename.concat parent "A.res" in
      write_file parent "file\n";
      check
        (try
           Format.write_file path "let value = 1\n";
           false
         with Format.Error message ->
           contains message "Could not write formatted file"
           && contains message path)
        "formatter write failures retain their operation and source path");
  with_temp_dir (fun root ->
      write_file (Filename.concat root "rescript.json") "{";
      let previous_directory = Sys.getcwd () in
      let previous_bsc = Sys.getenv_opt "RESCRIPT_BSC_EXE" in
      Unix.chdir root;
      Unix.putenv "RESCRIPT_BSC_EXE" (Filename.concat root "missing-bsc");
      let message =
        Fun.protect
          ~finally:(fun () ->
            Unix.chdir previous_directory;
            match previous_bsc with
            | Some value -> Unix.putenv "RESCRIPT_BSC_EXE" value
            | None -> Test_support.unsetenv "RESCRIPT_BSC_EXE")
          (fun () ->
            try
              Format.run_files ~check:false [];
              None
            with Format.Error message -> Some message)
      in
      check
        (match message with
        | Some message ->
          contains message "RESCRIPT_BSC_EXE points to missing path"
        | None -> false)
        "format resolves the compiler before discovering implicit project files");
  if not Sys.win32 then
    with_temp_dir (fun root ->
        let target = Filename.concat root "target.res" in
        let symlink = Filename.concat root "symlink.res" in
        let hard_link = Filename.concat root "hard-link.res" in
        write_file target "before";
        Unix.symlink target symlink;
        Unix.link target hard_link;
        let inode = (Unix.stat target).Unix.st_ino in
        Format.write_file symlink "through symlink";
        check
          (File_util.read_file target = "through symlink")
          "formatting a symlink should update its target";
        Format.write_file hard_link "through hard link";
        check
          ((Unix.stat target).Unix.st_ino = inode
          && File_util.read_file target = "through hard link")
          "formatting should preserve hard-link identity");
  with_temp_dir (fun root ->
      let first = Filename.concat root "First.res" in
      let second = Filename.concat root "Second.res" in
      write_file first "let first = 1\n";
      write_file second "let second = 2\n";
      let previous_root = Sys.getenv_opt "REWATCH_FORMAT_TEST_ROOT" in
      Unix.putenv "REWATCH_FORMAT_TEST_ROOT" root;
      Fun.protect
        ~finally:(fun () ->
          match previous_root with
          | Some value -> Unix.putenv "REWATCH_FORMAT_TEST_ROOT" value
          | None -> Test_support.unsetenv "REWATCH_FORMAT_TEST_ROOT")
        (fun () ->
          Format.format_files_with_bsc ~max_jobs:2
            ~bsc:(Unix.realpath Sys.executable_name)
            ~check:true [first; second]);
      check
        (Sys.file_exists (first ^ ".started")
        && Sys.file_exists (second ^ ".started"))
        "formatter subprocesses overlap rather than running serially");
  with_temp_dir (fun root ->
      let root_source = Test_support.path root "src/App.res" in
      let orphan_interface = Test_support.path root "src/Orphan.resi" in
      let duplicate_one = Test_support.path root "src/one/Duplicate.res" in
      let duplicate_two = Test_support.path root "src/two/Duplicate.res" in
      let installed_source =
        Test_support.path root "node_modules/installed/src/Installed.res"
      in
      write_file
        (Filename.concat root "rescript.json")
        {|{"name":"app","sources":[{"dir":"src","subdirs":true}],"dependencies":["installed"]}|};
      write_file root_source "let value = 1\n";
      write_file orphan_interface "let value: int\n";
      write_file duplicate_one "let value = 1\n";
      write_file duplicate_two "let value = 2\n";
      write_file
        (Test_support.path root "node_modules/installed/rescript.json")
        {|{"name":"installed","sources":["src"]}|};
      write_file installed_source "let value = 2\n";
      let previous = Sys.getcwd () in
      let previous_bsc = Sys.getenv_opt "RESCRIPT_BSC_EXE" in
      let previous_inventory_root =
        Sys.getenv_opt "REWATCH_FORMAT_INVENTORY_TEST_ROOT"
      in
      let marker path =
        Filename.concat root (Digest.string path |> Digest.to_hex)
      in
      Unix.putenv "RESCRIPT_BSC_EXE" (Unix.realpath Sys.executable_name);
      Unix.putenv "REWATCH_FORMAT_INVENTORY_TEST_ROOT" root;
      Fun.protect
        ~finally:(fun () ->
          Unix.chdir previous;
          (match previous_bsc with
          | Some value -> Unix.putenv "RESCRIPT_BSC_EXE" value
          | None -> Test_support.unsetenv "RESCRIPT_BSC_EXE");
          match previous_inventory_root with
          | Some value -> Unix.putenv "REWATCH_FORMAT_INVENTORY_TEST_ROOT" value
          | None -> Test_support.unsetenv "REWATCH_FORMAT_INVENTORY_TEST_ROOT")
        (fun () ->
          Unix.chdir root;
          Format.run_files ~check:true []);
      check
        (Sys.file_exists (marker root_source))
        "implicit format includes the current package";
      check
        (Sys.file_exists (marker orphan_interface))
        "implicit format includes an orphan interface";
      check
        (Sys.file_exists (marker duplicate_one)
        && Sys.file_exists (marker duplicate_two))
        "implicit format does not impose compilation module uniqueness";
      check
        (not (Sys.file_exists (marker installed_source)))
        "implicit format does not rewrite installed node_modules dependencies")
