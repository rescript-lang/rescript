open OUnit2

let check_equal expected actual message =
  assert_equal ~msg:message expected actual

let write_file = Test_support.write_file

let tests =
  "package_metadata_tests" >:: fun _context ->
  let root = Filename.temp_file "rewatch-ocaml-package-" "" in
  Sys.remove root;
  Unix.mkdir root 0o755;
  let root = Unix.realpath root in
  Fun.protect
    ~finally:(fun () -> File_util.remove_tree root)
    (fun () ->
      let package_json = Filename.concat root "package.json" in
      let check contents expected message =
        write_file package_json contents;
        check_equal expected
          (Package_diagnostics.For_test.issue_tracker_url root)
          message
      in
      check {|{"bugs":"https://bugs.example/pkg","repository":"owner/repo"}|}
        (Some "https://bugs.example/pkg") "a bugs string takes precedence";
      check
        {|{"bugs":{"url":"https://bugs.example/object"},"repository":"owner/repo"}|}
        (Some "https://bugs.example/object") "a bugs object takes precedence";
      check {|{"repository":"git+https://github.com/owner/repo.git"}|}
        (Some "https://github.com/owner/repo/issues")
        "a Git repository URL becomes an issues URL";
      check {|{"repository":{"url":"git@github.com:owner/repo.git"}}|}
        (Some "git@github.com:owner/repo/issues")
        "a repository object is accepted";
      check {|{"repository":"github:owner/repo"}|}
        (Some "https://github.com/owner/repo/issues")
        "a GitHub shorthand is expanded";
      check {|{"name":"no-metadata"}|} None
        "missing issue tracker metadata returns none";
      write_file package_json {|{"name":"first","name":"last"}|};
      check_equal (Ok (Some "last"))
        (Package_diagnostics.For_test.package_name root)
        "package identity uses JSON map last-key semantics";
      write_file package_json {|{"name":false}|};
      check_equal (Ok None)
        (Package_diagnostics.For_test.package_name root)
        "a non-string package name falls back to the ReScript config";
      write_file package_json "{invalid";
      check_equal true
        (match Package_diagnostics.For_test.package_name root with
        | Error message ->
          String.starts_with ~prefix:"Could not parse package.json:" message
        | Ok _ -> false)
        "malformed package metadata is rejected")
