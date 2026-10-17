open OUnit2

let check condition message = assert_bool message condition

let write_file = Test_support.write_file

let tests =
  "toolchain_tests" >:: fun _context ->
  let root = Filename.temp_file "rewatch-ocaml-toolchain-" "" in
  Sys.remove root;
  Unix.mkdir root 0o755;
  let root = Unix.realpath root in
  Fun.protect
    ~finally:(fun () -> File_util.remove_tree root)
    (fun () ->
      let bin = Filename.concat root "bin" in
      Unix.mkdir bin 0o755;
      let executable = Filename.concat bin "rescript.exe" in
      write_file executable "test executable";
      let project = Filename.concat root "project" in
      Unix.mkdir project 0o755;
      let no_packages _ _ = None in
      let bundled ?(executable = executable) find_package =
        Toolchain.bundled_bsc ~cwd:root ~executable ~root:project ~find_package
      in
      let sibling = Filename.concat bin "bsc.exe" in
      check
        (bundled no_packages = sibling)
        "without any bsc.exe, the sibling path is recorded";
      (match
         Toolchain.platform_package ~system:Build_platform.system
           ~architecture:Build_platform.architecture
       with
      | None -> ()
      | Some platform ->
        let package = Filename.concat root "platform" in
        let package_bsc =
          Filename.concat (Filename.concat package "bin") "bsc.exe"
        in
        Unix.mkdir package 0o755;
        Unix.mkdir (Filename.concat package "bin") 0o755;
        write_file package_bsc "test bsc";
        check
          (bundled (fun directory name ->
               if directory = project && name = platform then Some package
               else None)
          = package_bsc)
          "a development build records the platform package's bsc.exe";
        let rescript = Filename.concat root "rescript" in
        check
          (bundled (fun directory name ->
               if directory = project && name = "rescript" then Some rescript
               else if directory = rescript && name = platform then Some package
               else None)
          = package_bsc)
          "the platform package is also found from the rescript package";
        check
          (bundled (fun _ name -> if name = platform then Some root else None)
          = sibling)
          "a platform package without bsc.exe is ignored");
      write_file sibling "test bsc";
      check
        (bundled (fun _ _ -> Some root) = sibling)
        "absolute executable paths locate sibling bsc.exe";
      check
        (bundled ~executable:(Filename.concat "bin" "rescript.exe") (fun _ _ ->
             Some root)
        = sibling)
        "relative executable paths locate sibling bsc.exe");
  List.iter
    (fun (system, architecture, expected) ->
      check
        (Toolchain.platform_package ~system ~architecture = expected)
        (Printf.sprintf "%s/%s names its platform package" system architecture))
    [
      ("linux", "amd64", Some "@rescript/linux-x64");
      ("linux", "arm64", Some "@rescript/linux-arm64");
      ("macosx", "amd64", Some "@rescript/darwin-x64");
      ("macosx", "arm64", Some "@rescript/darwin-arm64");
      ("mingw64", "amd64", Some "@rescript/win32-x64");
      ("win64", "amd64", Some "@rescript/win32-x64");
      ("linux", "riscv", None);
      ("freebsd", "amd64", None);
    ];
  check
    (Platform_windows.strip_verbatim_prefix "\\\\?\\C:\\ReScript\\bin\\bsc.exe"
    = "C:\\ReScript\\bin\\bsc.exe")
    "Windows drive paths drop the verbatim prefix";
  check
    (Platform_windows.strip_verbatim_prefix "\\\\?\\UNC\\server\\share\\bsc.exe"
    = "\\\\server\\share\\bsc.exe")
    "Windows UNC paths preserve their network root";
  check
    (Platform_windows.strip_verbatim_prefix "C:\\ReScript\\bin\\bsc.exe"
    = "C:\\ReScript\\bin\\bsc.exe")
    "ordinary Windows paths are unchanged"
