open OUnit

(* While a build runs, the store reads a file from disk once and keeps what
   is written through it, under every spelling of its path. Outside a build,
   and in [bsc], it reads the disk every time. *)

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
  let path = Filename.temp_file "rescript-build-file-store-" "" in
  Sys.remove path;
  Unix.mkdir path 0o755;
  let path = Unix.realpath path in
  Fun.protect ~finally:(fun () -> remove_tree path) (fun () -> action path)

let write_on_disk path contents =
  let channel = open_out_bin path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr channel)
    (fun () -> output_string channel contents)

let printer = function
  | Some contents -> "Some " ^ String.escaped contents
  | None -> "None"

let disabled_reads_disk_tests _ =
  with_temp_dir (fun root ->
      let path = Filename.concat root "A.cmi" in
      write_on_disk path "first";
      assert_equal ~printer (Some "first") (Build_file_store.read path);
      write_on_disk path "second";
      assert_equal ~printer (Some "second") (Build_file_store.read path);
      assert_equal ~printer None (Build_file_store.cached path))

let build_reads_once_tests _ =
  with_temp_dir (fun root ->
      let path = Filename.concat root "A.cmi" in
      write_on_disk path "first";
      Build_file_store.with_build (fun () ->
          assert_equal ~printer (Some "first") (Build_file_store.read path);
          write_on_disk path "changed outside the store";
          assert_equal ~printer (Some "first") (Build_file_store.read path);
          Build_file_store.forget path;
          assert_equal ~printer (Some "changed outside the store")
            (Build_file_store.read path));
      write_on_disk path "between builds";
      Build_file_store.with_build (fun () ->
          assert_equal ~printer (Some "between builds")
            (Build_file_store.read path)))

let spellings_share_an_entry_tests _ =
  with_temp_dir (fun root ->
      Unix.mkdir (Filename.concat root "bs") 0o755;
      Unix.mkdir (Filename.concat root "ocaml") 0o755;
      let request_spelling = Filename.concat root "bs/../ocaml/./A.cmi" in
      let published = Filename.concat root "ocaml/A.cmi" in
      write_on_disk published "old";
      Build_file_store.with_build (fun () ->
          assert_equal ~printer (Some "old")
            (Build_file_store.read request_spelling);
          Build_file_store.write published "new";
          assert_equal ~printer (Some "new")
            (Build_file_store.read request_spelling)))

let kept_and_evicted_tests _ =
  with_temp_dir (fun root ->
      let missing = Filename.concat root "Missing.cmj" in
      let transit = Filename.concat root "A.cmt" in
      let kept = Filename.concat root "A.cmi" in
      Build_file_store.with_build (fun () ->
          assert_equal ~printer None (Build_file_store.read missing);
          assert_bool "a missing file does not exist"
            (not (Build_file_store.exists missing));
          Build_file_store.write missing "now present";
          assert_bool "a written file exists" (Build_file_store.exists missing);
          Build_file_store.write ~keep:false transit "typed tree";
          assert_equal ~printer None (Build_file_store.cached transit);
          assert_bool "an output that is not kept exists"
            (Build_file_store.exists transit);
          assert_equal ~printer (Some "typed tree")
            (Build_file_store.read transit);
          Build_file_store.write kept "interface";
          assert_equal ~printer (Some "interface")
            (Build_file_store.cached kept);
          Build_file_store.evict kept;
          assert_equal ~printer None (Build_file_store.cached kept);
          assert_bool "an evicted file exists" (Build_file_store.exists kept));
      assert_equal ~printer (Some "interface") (Build_file_store.read kept))

let interface ~name =
  {Cmi_format.cmi_name = name; cmi_sign = []; cmi_crcs = []; cmi_flags = []}

(* Within a build, a domain reuses the interfaces it has read, until one is
   written again. *)
let interface_cache_tests _ =
  with_temp_dir (fun root ->
      let path = Filename.concat root "Abc.cmi" in
      Build_file_store.with_build (fun () ->
          Compiler_request_state.with_fresh ~cwd:root (fun () ->
              Config.set_load_path [root];
              let load () =
                (Option.get (Bs_cmi_load.load_cmi ~unit_name:"Abc")).cmi
              in
              ignore (Cmi_format.create_cmi path (interface ~name:"Abc"));
              let first = load () in
              assert_bool "an unchanged interface is read once"
                (first == load ());
              ignore
                (Cmi_format.create_cmi path
                   {
                     (interface ~name:"Abc") with
                     cmi_flags = [Cmi_format.Deprecated "replaced"];
                   });
              let second = load () in
              assert_bool "a rewritten interface is read again"
                (second != first
                && second.cmi_flags = [Cmi_format.Deprecated "replaced"]))))

let suites =
  __FILE__
  >::: [
         "disabled_reads_disk" >:: disabled_reads_disk_tests;
         "build_reads_once" >:: build_reads_once_tests;
         "spellings_share_an_entry" >:: spellings_share_an_entry_tests;
         "kept_and_evicted" >:: kept_and_evicted_tests;
         "interface_cache" >:: interface_cache_tests;
       ]
