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

(* A domain caches interfaces up to a budget of their size, and then starts
   over. *)
let interface_cache_budget_tests _ =
  with_temp_dir (fun root ->
      Build_file_store.with_build (fun () ->
          Compiler_request_state.with_fresh ~cwd:root (fun () ->
              Config.set_load_path [root];
              let load name =
                (Option.get (Bs_cmi_load.load_cmi ~unit_name:name)).cmi
              in
              List.iter
                (fun name ->
                  ignore
                    (Cmi_format.create_cmi
                       (Filename.concat root (name ^ ".cmi"))
                       {
                         (interface ~name) with
                         cmi_flags =
                           [
                             Cmi_format.Deprecated
                               (String.make
                                  (Bs_cmi_load.cmi_cache_budget / 2)
                                  'x');
                           ];
                       }))
                ["Abc"; "Def"];
              let first = load "Abc" in
              assert_bool "an interface within the budget is kept"
                (first == load "Abc");
              ignore (load "Def");
              assert_bool "an interface beyond the budget empties the cache"
                (first != load "Abc"))))

let corrupted_interface =
  Cmi_format.Error (Cmi_format.Corrupted_interface "Short.cmi")

(* An interface cut short reads as corrupted from memory, as it does from
   disk. *)
let corrupted_interface_tests _ =
  with_temp_dir (fun root ->
      let path = Filename.concat root "Short.cmi" in
      Build_file_store.with_build (fun () ->
          Build_file_store.write path
            (Config.cmi_magic_number ^ "not a marshalled signature");
          assert_raises corrupted_interface (fun () ->
              Cmi_format.read_cmi_contents "Short.cmi"
                (Option.get (Build_file_store.read path)));
          Build_file_store.write path "short";
          assert_raises corrupted_interface (fun () ->
              Cmi_format.read_cmi_contents "Short.cmi"
                (Option.get (Build_file_store.read path)))))

(* A write that fails keeps nothing for its path, so a later read sees what
   the disk has. *)
let failed_write_tests _ =
  with_temp_dir (fun root ->
      let path = Filename.concat root "missing/A.cmi" in
      Build_file_store.with_build (fun () ->
          assert_equal ~printer None (Build_file_store.read path);
          (match Build_file_store.write path "interface" with
          | () -> assert_failure "a write into a missing directory succeeded"
          | exception Sys_error _ -> ());
          assert_equal ~printer None (Build_file_store.cached path);
          assert_bool "a failed write leaves no file"
            (not (Build_file_store.exists path))))

(* A request that fails may stop with types of cached interfaces still marked,
   so the domain reads its interfaces again; one that succeeds keeps them. *)
let failed_request_tests _ =
  with_temp_dir (fun root ->
      let run input =
        Rescript_compiler_driver.run_request ~run_external:None ~cwd:root
          ~argv:
            [
              "-nostdlib";
              "-nopervasives";
              "-bs-project-root";
              root;
              "-bs-package-name";
              "store-test";
              "-bs-package-output";
              "commonjs:.:.js";
            ]
          ~input
      in
      write_on_disk (Filename.concat root "Good.res") "let value = 1\n";
      write_on_disk (Filename.concat root "Bad.res") "let value: string = 1\n";
      Build_file_store.with_build (fun () ->
          Compiler_request_state.with_fresh ~cwd:root (fun () ->
              Config.set_load_path [root];
              let load () =
                (Option.get (Bs_cmi_load.load_cmi ~unit_name:"Abc")).cmi
              in
              ignore
                (Cmi_format.create_cmi
                   (Filename.concat root "Abc.cmi")
                   (interface ~name:"Abc"));
              let first = load () in
              assert_equal ~printer:string_of_int 0
                (run "Good.res").Rescript_compiler_driver.exit_code;
              assert_bool "a successful request keeps the interfaces"
                (first == load ());
              assert_bool "the failing request fails"
                ((run "Bad.res").Rescript_compiler_driver.exit_code <> 0);
              assert_bool "a failed request drops the interfaces"
                (first != load ()))))

let suites =
  __FILE__
  >::: [
         "disabled_reads_disk" >:: disabled_reads_disk_tests;
         "build_reads_once" >:: build_reads_once_tests;
         "spellings_share_an_entry" >:: spellings_share_an_entry_tests;
         "kept_and_evicted" >:: kept_and_evicted_tests;
         "interface_cache" >:: interface_cache_tests;
         "interface_cache_budget" >:: interface_cache_budget_tests;
         "corrupted_interface" >:: corrupted_interface_tests;
         "failed_write" >:: failed_write_tests;
         "failed_request" >:: failed_request_tests;
       ]
