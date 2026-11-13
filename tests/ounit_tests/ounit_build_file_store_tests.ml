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
      List.iter
        (fun directory -> Unix.mkdir (Filename.concat root directory) 0o755)
        ["bs"; "bs/src"; "ocaml"];
      let published = Filename.concat root "ocaml/A.cmi" in
      let spellings =
        [
          Filename.concat root "bs/../ocaml/./A.cmi";
          Filename.concat root "bs/src/../../ocaml/A.cmi";
          Filename.concat root "./ocaml//A.cmi";
        ]
        @ if Sys.win32 then [root ^ "\\bs\\src\\..\\..\\ocaml\\A.cmi"] else []
      in
      let read_all expected =
        List.iter
          (fun spelling ->
            assert_equal ~msg:spelling ~printer (Some expected)
              (Build_file_store.read spelling))
          spellings
      in
      write_on_disk published "old";
      Build_file_store.with_build (fun () ->
          read_all "old";
          Build_file_store.write published "new";
          read_all "new"))

(* [key] returns a path that has no [.], [..] or empty segment, nor a
   backslash on Windows, as it is, without normalizing it. This is the
   normalization it skips, as [key] did it for every path, so the two must
   agree on every path. *)
let normalized_key path =
  let is_separator c = c = '/' || (Sys.win32 && c = '\\') in
  let length = String.length path in
  let rec segments start index acc =
    if index = length then
      List.rev (String.sub path start (index - start) :: acc)
    else if is_separator path.[index] then
      segments (index + 1) (index + 1)
        (String.sub path start (index - start) :: acc)
    else segments start (index + 1) acc
  in
  match segments 0 0 [] with
  | [] -> path
  | first :: rest ->
    let rec normalize acc = function
      | [] -> List.rev acc
      | ("" | ".") :: rest -> normalize acc rest
      | ".." :: rest -> (
        match acc with
        | previous :: acc when previous <> ".." -> normalize acc rest
        | _ -> normalize (".." :: acc) rest)
      | segment :: rest -> normalize (segment :: acc) rest
    in
    String.concat "/" (first :: normalize [] rest)

let assert_key path =
  assert_equal ~msg:(String.escaped path) ~printer:String.escaped
    (normalized_key path)
    (Build_file_store.key path)

let key_edge_cases_tests _ =
  List.iter assert_key
    [
      "";
      "/";
      "//";
      "///";
      ".";
      "..";
      "...";
      ".a";
      "a.";
      "..a";
      "a..";
      "a";
      "A.cmi";
      "/A.cmi";
      "//A.cmi";
      "lib/rescript/A.cmi";
      "/home/user/project/lib/rescript/A.cmi";
      "lib/rescript/";
      "/lib/rescript/";
      "lib//rescript/A.cmi";
      "lib/rescript//";
      "./A.cmi";
      "./lib/rescript/A.cmi";
      "../A.cmi";
      "../../src/A.res";
      "lib/rescript/./A.cmi";
      "lib/rescript/../../src/A.res";
      "lib/rescript/..";
      "lib/rescript/.";
      "a/..";
      "a/../..";
      "/..";
      "/../a";
      "/./a";
      "/a/./b/../c";
      "lib/.../A.cmi";
      "lib/.rescript/A.cmi";
      "lib/rescript./A.cmi";
      "lib/..rescript/A.cmi";
      "lib/rescript../A.cmi";
      ".../a";
      ".a/b";
      "a/...";
      "a/.b";
      "a\\b";
      "a\\.\\b";
      "a\\..\\b";
      "lib\\rescript\\A.cmi";
      "lib/rescript\\A.cmi";
      "lib\\";
      "\\";
      "\\a";
      "C:";
      "C:/";
      "C:/a/b";
      "C:/a/./b";
      "C:/a/../b";
      "C:\\a\\b";
      "C:\\a\\..\\b";
      "c:/Users/A.cmi";
      "C:a";
      "//server/share/a";
      "\\\\server\\share\\a";
      "\\\\server\\share\\..\\a";
      "\\\\?\\C:\\a";
      "//?/C:/a";
    ]

(* Every path of up to 8 characters from the characters that matter, then
   longer paths made of segments, which the first set cannot reach. *)
let key_generated_paths_tests _ =
  let alphabet = "/.a\\" in
  let rec all_paths prefix length =
    assert_key prefix;
    if length > 0 then
      String.iter
        (fun c -> all_paths (prefix ^ String.make 1 c) (length - 1))
        alphabet
  in
  all_paths "" 8;
  let segments =
    [|
      ""; "."; ".."; "..."; ".a"; "a."; "a"; "src"; "A.cmi"; "C:"; "\\"; "a\\b";
    |]
  in
  let separators = [|"/"; "/"; "/"; "\\"; "//"|] in
  let random = Random.State.make [|9|] in
  for _ = 1 to 20_000 do
    let count = Random.State.int random 9 in
    let path = Buffer.create 64 in
    for index = 0 to count do
      if index > 0 then
        Buffer.add_string path
          separators.(Random.State.int random (Array.length separators));
      Buffer.add_string path
        segments.(Random.State.int random (Array.length segments))
    done;
    assert_key (Buffer.contents path)
  done

(* A path that is already normal is its own key, not a copy of it. *)
let key_of_normal_path_tests _ =
  List.iter
    (fun path -> assert_bool path (Build_file_store.key path == path))
    [
      "A.cmi";
      "lib/rescript/A.cmi";
      "/home/user/project/lib/rescript/A.cmi";
      "C:/Users/project/lib/rescript/A.cmi";
      "lib/.../.a/a./A.cmi";
    ]

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

(* A held interface stays off the disk until it is published, and publishing
   writes it only when it changed, so an unchanged interface keeps its
   modification time. A forgotten one never reaches the disk. *)
let held_writes_tests _ =
  with_temp_dir (fun root ->
      let interface = Filename.concat root "A.cmi" in
      let unchanged = Filename.concat root "B.cmi" in
      let failed = Filename.concat root "C.cmi" in
      let implementation = Filename.concat root "A.cmj" in
      List.iter
        (fun path -> write_on_disk path "old")
        [interface; unchanged; failed];
      let on_disk path =
        let channel = open_in_bin path in
        Fun.protect
          ~finally:(fun () -> close_in_noerr channel)
          (fun () -> really_input_string channel (in_channel_length channel))
      in
      let written = ref [] in
      let write path contents =
        written := path :: !written;
        write_on_disk path contents
      in
      Build_file_store.set_held_writes (fun path ->
          Filename.check_suffix path ".cmi");
      Fun.protect
        ~finally:(fun () -> Build_file_store.set_held_writes (fun _ -> false))
        (fun () ->
          Build_file_store.with_build (fun () ->
              Build_file_store.write interface "new";
              Build_file_store.write unchanged "old";
              Build_file_store.write failed "new";
              Build_file_store.write implementation "code";
              assert_equal ~printer (Some "new")
                (Build_file_store.read interface);
              assert_equal ~printer (Some "old") (Some (on_disk interface));
              assert_equal ~printer (Some "code")
                (Some (on_disk implementation));
              assert_bool "a changed interface is published"
                (Build_file_store.publish_held ~if_changed:true ~write interface);
              assert_bool "an unchanged interface is not rewritten"
                (not
                   (Build_file_store.publish_held ~if_changed:true ~write
                      unchanged));
              assert_equal ~printer:(String.concat ",") [interface] !written;
              assert_raises Not_found (fun () ->
                  Build_file_store.publish_held ~if_changed:true ~write
                    interface);
              Build_file_store.forget failed;
              assert_equal ~printer (Some "old") (Build_file_store.read failed));
          assert_equal ~printer (Some "new") (Some (on_disk interface));
          assert_equal ~printer (Some "old") (Some (on_disk failed))))

(* Publishing compares a held interface with the one the build read before,
   without reading the disk again. *)
let held_write_compares_read_contents_tests _ =
  with_temp_dir (fun root ->
      let path = Filename.concat root "A.cmi" in
      write_on_disk path "interface";
      let reads = ref 0 in
      Build_file_store.set_held_writes (fun _ -> true);
      Fun.protect
        ~finally:(fun () ->
          Build_file_store.set_held_writes (fun _ -> false);
          Build_file_store.set_disk_reader Bs_cmi_load.read_file)
        (fun () ->
          Build_file_store.with_build (fun () ->
              ignore (Build_file_store.read path);
              Build_file_store.set_disk_reader (fun path ->
                  incr reads;
                  Bs_cmi_load.read_file path);
              Build_file_store.write path "interface";
              assert_bool "an identical interface is not published"
                (not
                   (Build_file_store.publish_held ~if_changed:true
                      ~write:(fun _ _ -> assert_failure "written")
                      path));
              assert_equal ~printer:string_of_int 0 !reads)))

(* Large files that one request reads, such as typed trees, are not kept, but
   held contents are seen. *)
let read_without_keeping_tests _ =
  with_temp_dir (fun root ->
      let read = Filename.concat root "B.cmt" in
      let held = Filename.concat root "A.cmt" in
      write_on_disk read "typed tree";
      Build_file_store.set_held_writes (fun path ->
          Filename.check_suffix path ".cmt");
      Fun.protect
        ~finally:(fun () -> Build_file_store.set_held_writes (fun _ -> false))
        (fun () ->
          Build_file_store.with_build (fun () ->
              assert_equal ~printer (Some "typed tree")
                (Build_file_store.read ~keep:false read);
              assert_equal ~printer None (Build_file_store.cached read);
              assert_bool "a file read once exists"
                (Build_file_store.exists read);
              Build_file_store.write held "held typed tree";
              assert_equal ~printer (Some "held typed tree")
                (Build_file_store.read ~keep:false held))))

(* An existing interface cut short, as by a build killed while it wrote it in
   place, is replaced instead of failing the compile that checks it. *)
let truncated_existing_interface_tests _ =
  with_temp_dir (fun root ->
      let path = Filename.concat root "Abc.cmi" in
      write_on_disk path (Config.cmi_magic_number ^ "cut");
      Compiler_request_state.with_fresh ~cwd:root (fun () ->
          ignore
            (Cmi_format.create_cmi ~check_exists:() path
               {
                 Cmi_format.cmi_name = "Abc";
                 cmi_sign = [];
                 cmi_crcs = [];
                 cmi_flags = [];
               }));
      let contents =
        let channel = open_in_bin path in
        Fun.protect
          ~finally:(fun () -> close_in_noerr channel)
          (fun () -> really_input_string channel (in_channel_length channel))
      in
      assert_bool "a truncated interface is replaced"
        (String.length contents > String.length Config.cmi_magic_number + 3))

(* A file that the compiler removes, such as a stale source map, is missing
   for the rest of the build. *)
let removed_file_tests _ =
  with_temp_dir (fun root ->
      let path = Filename.concat root "A.js.map" in
      Build_file_store.with_build (fun () ->
          Build_file_store.write path "source map";
          Misc.remove_file path;
          assert_equal ~printer None (Build_file_store.read path)))

(* A file that exists but can't be read raises, every time it is read, and
   is not taken for a missing one. *)
let unreadable_file_tests _ =
  with_temp_dir (fun root ->
      let path = Filename.concat root "A.cmi" in
      let error = Sys_error (path ^ ": Permission denied") in
      let attempts = ref 0 in
      write_on_disk path "interface";
      Build_file_store.set_disk_reader (fun _ ->
          incr attempts;
          raise error);
      Fun.protect
        ~finally:(fun () ->
          Build_file_store.set_disk_reader Bs_cmi_load.read_file)
        (fun () ->
          Build_file_store.with_build (fun () ->
              assert_raises error (fun () -> Build_file_store.read path);
              assert_raises error (fun () -> Build_file_store.read path);
              assert_equal ~printer:string_of_int 2 !attempts;
              assert_bool "an unreadable file exists"
                (Build_file_store.exists path))))

(* Compiler workers read, write and evict entries at the same time. *)
let concurrent_domains_tests _ =
  with_temp_dir (fun root ->
      let shared = Filename.concat root "Shared.cmi" in
      write_on_disk shared "shared";
      Build_file_store.with_build (fun () ->
          let work index () =
            let own = Filename.concat root (Printf.sprintf "A%d.cmj" index) in
            for round = 1 to 200 do
              let contents = Printf.sprintf "%d:%d" index round in
              Build_file_store.write own contents;
              if Build_file_store.read own <> Some contents then
                failwith (own ^ " lost a write");
              if Build_file_store.read shared <> Some "shared" then
                failwith "the shared file changed";
              Build_file_store.evict own;
              Build_file_store.evict shared
            done
          in
          List.init 4 (fun index -> Domain.spawn (work index))
          |> List.iter Domain.join;
          List.iter
            (fun index ->
              assert_equal ~printer
                (Some (Printf.sprintf "%d:200" index))
                (Build_file_store.read
                   (Filename.concat root (Printf.sprintf "A%d.cmj" index))))
            [0; 1; 2; 3]))

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
         "key_edge_cases" >:: key_edge_cases_tests;
         "key_generated_paths" >:: key_generated_paths_tests;
         "key_of_normal_path" >:: key_of_normal_path_tests;
         "kept_and_evicted" >:: kept_and_evicted_tests;
         "held_writes" >:: held_writes_tests;
         "held_write_compares_read_contents"
         >:: held_write_compares_read_contents_tests;
         "read_without_keeping" >:: read_without_keeping_tests;
         "truncated_existing_interface" >:: truncated_existing_interface_tests;
         "removed_file" >:: removed_file_tests;
         "unreadable_file" >:: unreadable_file_tests;
         "concurrent_domains" >:: concurrent_domains_tests;
         "interface_cache" >:: interface_cache_tests;
         "interface_cache_budget" >:: interface_cache_budget_tests;
         "corrupted_interface" >:: corrupted_interface_tests;
         "failed_write" >:: failed_write_tests;
         "failed_request" >:: failed_request_tests;
       ]
