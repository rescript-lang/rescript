let check condition message = if not condition then failwith message

let write_file path contents =
  let channel = open_out_bin path in
  Fun.protect ~finally:(fun () -> close_out_noerr channel) (fun () ->
    output_string channel contents)

let () =
  let root = Filename.temp_file "rewatch-ocaml-toolchain-" "" in
  Sys.remove root;
  Unix.mkdir root 0o755;
  Fun.protect
    ~finally:(fun () -> Build_artifacts.remove_tree root)
    (fun () ->
      let bin = Filename.concat root "bin" in
      Unix.mkdir bin 0o755;
      let executable = Filename.concat bin "rescript-ocaml.exe" in
      write_file executable "test executable";
      check
        (Toolchain.sibling_bsc_candidate ~cwd:root ~executable
        = Filename.concat bin "bsc.exe")
        "absolute executable paths locate sibling bsc.exe";
      check
        (Toolchain.sibling_bsc_candidate ~cwd:root
           ~executable:(Filename.concat "bin" "rescript-ocaml.exe")
        = Filename.concat bin "bsc.exe")
        "relative executable paths locate sibling bsc.exe");
  check
    (Platform_windows.strip_verbatim_prefix
       "\\\\?\\C:\\ReScript\\bin\\bsc.exe"
    = "C:\\ReScript\\bin\\bsc.exe")
    "Windows drive paths drop the verbatim prefix";
  check
    (Platform_windows.strip_verbatim_prefix
       "\\\\?\\UNC\\server\\share\\bsc.exe"
    = "\\\\server\\share\\bsc.exe")
    "Windows UNC paths preserve their network root";
  check
    (Platform_windows.strip_verbatim_prefix "C:\\ReScript\\bin\\bsc.exe"
    = "C:\\ReScript\\bin\\bsc.exe")
    "ordinary Windows paths are unchanged"
