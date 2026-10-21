open OUnit

(* Tests that each compiler request starts from fresh state and leaves the
   state of the domain that runs it unchanged, for consecutive requests and
   for requests on several domains at once. *)

let check condition message = assert_bool message condition

let rec ensure_dir path =
  if not (Sys.file_exists path) then (
    ensure_dir (Filename.dirname path);
    try Unix.mkdir path 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ())

let write_file path contents =
  ensure_dir (Filename.dirname path);
  let channel = open_out_bin path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr channel)
    (fun () -> output_string channel contents)

let read_file path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let copy_existing_file source destination =
  write_file destination (read_file source)

let contains_text text fragment =
  let text_length = String.length text in
  let fragment_length = String.length fragment in
  let rec from index =
    index + fragment_length <= text_length
    && (String.sub text index fragment_length = fragment || from (index + 1))
  in
  from 0

let rec remove_tree path =
  match (Unix.lstat path).st_kind with
  | Unix.S_DIR ->
    Array.iter
      (fun name -> remove_tree (Filename.concat path name))
      (Sys.readdir path);
    Unix.rmdir path
  | _ -> Sys.remove path
  | exception Unix.Unix_error (Unix.ENOENT, _, _) -> ()

(* The source path recorded in a binary AST written by -bs-ast: it follows the
   length-prefixed dependency block. *)
let ast_source path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () ->
      let dependencies = input_binary_int channel in
      seek_in channel (pos_in channel + dependencies);
      try Some (input_line channel) with End_of_file -> None)

let with_temp_dir prefix action =
  let path = Filename.temp_file prefix "" in
  Sys.remove path;
  Unix.mkdir path 0o755;
  let path = Unix.realpath path in
  Fun.protect ~finally:(fun () -> remove_tree path) (fun () -> action path)

let request_local_counter = Request_local.new_key (fun () -> ref 0)

(* Keys created in this order; their initializers record when they run. *)
let initialized = ref []
let first_ordered_key =
  Request_local.new_key (fun () -> initialized := "first" :: !initialized)
let second_ordered_key =
  Request_local.new_key (fun () -> initialized := "second" :: !initialized)

(* A fresh request initializes keys in the order they were created, which is
   module initialization order. *)
let request_local_order_tests _ =
  ignore first_ordered_key;
  ignore second_ordered_key;
  initialized := [];
  Request_local.with_fresh (fun () -> ());
  assert_equal ~printer:(String.concat ", ") ["first"; "second"]
    (List.rev !initialized)

(* A request sees fresh request-local state and leaves the previous state in
   place afterwards, also when it fails. *)
let request_local_tests _context =
  let counter () = Request_local.get request_local_counter in
  counter () := 5;
  Request_local.with_fresh (fun () ->
      assert_equal 0 !(counter ());
      counter () := 7);
  assert_equal 5 !(counter ());
  (try
     Request_local.with_fresh (fun () ->
         counter () := 9;
         failwith "request failed")
   with Failure _ -> ());
  assert_equal 5 !(counter ())

let ctype_level_isolation_tests _context =
  let original = Ctype.save_levels () in
  Fun.protect
    ~finally:(fun () -> Ctype.set_levels original)
    (fun () ->
      Ctype.init_def 4;
      Ctype.begin_def ();
      let child =
        Domain.spawn (fun () ->
            Ctype.init_def 20;
            Ctype.begin_def ();
            Ctype.end_def ();
            Ctype.get_current_level ())
      in
      assert_equal 20 (Domain.join child);
      assert_equal 5 (Ctype.get_current_level ());
      Ctype.end_def ();
      assert_equal 4 (Ctype.get_current_level ()))

(* Js_shake keeps a toplevel binding only when the free variables of a live
   statement mention it. Free variables computed on two domains at once must
   not mix, or a used function definition is dropped from the output. *)
let free_variable_isolation_tests _context =
  let free_variables name =
    let ident = Ident.create name in
    let expression =
      Js_exp_make.array (List.init 64 (fun _ -> Js_exp_make.var ident))
    in
    Domain.spawn (fun () ->
        let rec loop remaining =
          remaining = 0
          || Set_ident.elements
               (Js_analyzer.free_variables_of_expression expression)
             = [ident]
             && loop (remaining - 1)
        in
        loop 20_000)
  in
  let left = free_variables "left" in
  let right = free_variables "right" in
  check (Domain.join left) "free variables of the left domain are its own";
  check (Domain.join right) "free variables of the right domain are its own"

let shared_type_graph_isolation_tests _context =
  let host_type = Predef.type_int () in
  let host_env = Env.initial_safe_string () in
  let child_type, child_env =
    Domain.spawn (fun () -> (Predef.type_int (), Env.initial_safe_string ()))
    |> Domain.join
  in
  check (host_type != child_type) "predefined type graphs belong to a domain";
  check (host_env != child_env) "initial environments belong to a domain"

let dependency_extraction_isolation_tests _context =
  let module Set = Depend.String_set in
  let wait flag =
    let deadline = Unix.gettimeofday () +. 5. in
    while (not (Atomic.get flag)) && Unix.gettimeofday () < deadline do
      Domain.cpu_relax ()
    done;
    check (Atomic.get flag) "dependency extraction test timed out"
  in
  Depend.free_structure_names () := Set.singleton "Host";
  let child_ready = Atomic.make false in
  let host_ready = Atomic.make false in
  let child =
    Domain.spawn (fun () ->
        Depend.free_structure_names () := Set.singleton "Child";
        Atomic.set child_ready true;
        wait host_ready;
        !(Depend.free_structure_names ()))
  in
  wait child_ready;
  let host_dependencies = !(Depend.free_structure_names ()) in
  Atomic.set host_ready true;
  let child_dependencies = Domain.join child in
  assert_equal (Set.singleton "Host") host_dependencies;
  assert_equal (Set.singleton "Child") child_dependencies

let gentype_output_capture_tests _context =
  let (), stdout, stderr =
    Compiler_request_output.with_capture (fun () ->
        Log_.Color.setup ();
        Log_.info ~loc:Location.none ~name:"Warning genType" (fun ppf () ->
            Stdlib.Format.fprintf ppf "captured warning"))
  in
  check
    (contains_text stdout "captured warning")
    "GenType warnings use the request-owned stdout formatter";
  assert_equal "" stderr

(* Output written before an exception escapes a request is not lost. *)
let failed_capture_tests _context =
  match
    Compiler_request_output.with_capture (fun () ->
        Compiler_request_output.write_stderr "earlier warning";
        failwith "request failed")
  with
  | _ -> assert_failure "the failure must propagate"
  | exception Compiler_request_output.Failed {error; stdout; stderr} ->
    assert_equal (Failure "request failed") error;
    assert_equal "" stdout;
    assert_equal ~printer:Fun.id "earlier warning" stderr;
    check
      (not (Compiler_request_output.is_active ()))
      "the previous streams are restored"

(* Captured text has a bsc process's line endings: CRLF on Windows, without
   doubling the carriage return of output that already ends lines in CRLF, such
   as a PPX's. *)
let captured_line_endings_tests _context =
  let (), stdout, stderr =
    Compiler_request_output.with_capture (fun () ->
        Compiler_request_output.write_stdout "one\ntwo\n";
        Compiler_request_output.write_stderr "ppx line\r\nwarning\n")
  in
  if Sys.win32 then (
    assert_equal ~printer:String.escaped "one\r\ntwo\r\n" stdout;
    assert_equal ~printer:String.escaped "ppx line\r\nwarning\r\n" stderr)
  else (
    assert_equal ~printer:String.escaped "one\ntwo\n" stdout;
    assert_equal ~printer:String.escaped "ppx line\r\nwarning\n" stderr);
  (* The same holds once stdout is backed by a file, both for output written
     before the switch and for output written after it. *)
  let (), stdout, _ =
    Compiler_request_output.with_capture (fun () ->
        Compiler_request_output.write_stdout "before\r\nplain\n";
        ignore (Compiler_request_output.stdout_channel () : out_channel);
        Compiler_request_output.write_stdout "after\r\n")
  in
  let newline = if Sys.win32 then "\r\n" else "\n" in
  assert_equal ~printer:String.escaped
    ("before\r\nplain" ^ newline ^ "after\r\n")
    stdout

(* Readers of compiler artifacts close their file when decoding fails, so a
   long-lived build process does not keep it open. *)
let failed_decoding_closes_files_tests _context =
  if Sys.file_exists "/proc/self/fd" then
    with_temp_dir "rewatch-driver-decoding-" (fun root ->
        let truncated = Filename.concat root "Truncated" in
        write_file truncated "abc";
        let open_files () = Array.length (Sys.readdir "/proc/self/fd") in
        let before = open_files () in
        for _ = 1 to 20 do
          (try ignore (Js_cmj_format.from_file truncated) with _ -> ());
          try
            ignore
              (Binary_ast.read_ast_exn ~fname:truncated Binary_ast.Ml
                : Parsetree.structure)
          with _ -> ()
        done;
        assert_equal ~printer:string_of_int before (open_files ()))

(* Writers of compiler artifacts close their file when writing fails, as on a
   full disk. *)
let failed_writing_closes_files_tests _context =
  if Sys.file_exists "/proc/self/fd" && Sys.file_exists "/dev/full" then (
    let open_files () = Array.length (Sys.readdir "/proc/self/fd") in
    let before = open_files () in
    let cmj =
      Js_cmj_format.make ~values:Map_string.empty ~hoisted_exports:[]
        ~effect_:None ~package_spec:Js_packages_info.empty
        ~case:Ext_js_file_kind.Little
    in
    let cmi =
      {
        Cmi_format.cmi_name = "Full";
        cmi_sign = [];
        cmi_crcs = [];
        cmi_flags = [];
      }
    in
    for _ = 1 to 20 do
      (try Js_cmj_format.to_file "/dev/full" ~check_exists:false cmj
       with Sys_error _ -> ());
      (try
         Binary_ast.write_ast ~sourcefile:"Full.res" ~output:"/dev/full"
           Binary_ast.Ml []
       with Sys_error _ -> ());
      try ignore (Cmi_format.create_cmi "/dev/full" cmi : Digest.t)
      with Sys_error _ -> ()
    done;
    assert_equal ~printer:string_of_int before (open_files ()))

(* GenType finds a module's source relative to the request's working
   directory, not the host process's. *)
let gentype_source_lookup_tests _context =
  with_temp_dir "rewatch-driver-gentype-source-" (fun root ->
      write_file (Filename.concat root "src/A.res") "let a = 1\n";
      let position = {Lexing.dummy_pos with pos_fname = "src/A.res"} in
      let item : Typedtree.structure_item =
        {
          str_desc =
            Tstr_attribute (Location.mknoloc "attribute", Parsetree.PStr []);
          str_loc = {Location.none with loc_start = position};
          str_env = Env.empty;
        }
      in
      let found =
        Compiler_request_state.with_fresh ~cwd:root (fun () ->
            Find_source_file.cmt
              (Cmt_format.Implementation
                 {str_items = [item]; str_type = []; str_final_env = Env.empty}))
      in
      assert_equal
        ~printer:(Option.value ~default:"None")
        (Some (Filename.concat root "src/A.res"))
        found)

let used_attributes_isolation_tests _context =
  let loc = {Location.none with loc_ghost = false} in
  let name = Asttypes.{txt = "as"; loc} in
  let attribute = (name, Parsetree.PStr []) in
  Used_attributes.mark_used_attribute attribute;
  let child_before, child_after =
    Domain.spawn (fun () ->
        let before = Used_attributes.is_used_attribute name in
        Used_attributes.mark_used_attribute attribute;
        (before, Used_attributes.is_used_attribute name))
    |> Domain.join
  in
  check ((not child_before) && child_after) "used attributes are domain local";
  check
    (Used_attributes.is_used_attribute name)
    "another domain does not clear the caller's used attributes"

let delayed_checks_isolation_tests _context =
  Delayed_checks.reset_delayed_checks ();
  let host_runs = ref 0 in
  Delayed_checks.add_delayed_check (fun () -> incr host_runs);
  let child_runs =
    Domain.spawn (fun () ->
        let runs = ref 0 in
        Delayed_checks.reset_delayed_checks ();
        Delayed_checks.add_delayed_check (fun () -> incr runs);
        Delayed_checks.force_delayed_checks ();
        !runs)
    |> Domain.join
  in
  assert_equal 1 child_runs;
  assert_equal 0 !host_runs;
  Delayed_checks.force_delayed_checks ();
  assert_equal 1 !host_runs

let gentype_flags_isolation_tests _context =
  Request_local.with_fresh (fun () ->
      Gentype_config.add_source_dir "host-source";
      let child_sources =
        Domain.spawn (fun () ->
            Gentype_config.add_source_dir "child-source";
            !(Gentype_config.source_dirs_flag ()))
        |> Domain.join
      in
      assert_equal ["child-source"] child_sources;
      assert_equal ["host-source"] !(Gentype_config.source_dirs_flag ()))

let diagnostic_state_isolation_tests _context =
  Request_local.with_fresh (fun () ->
      Error_message_utils.configured_jsx_module () := Some "HostJsx";
      Debug.set_item "basic";
      let first = Ctype.newvar () in
      let second = Ctype.newvar () in
      let first_text = Stdlib.Format.asprintf "%a" Printtyp.type_expr first in
      let child_jsx, child_debug, child_type =
        Domain.spawn (fun () ->
            let initial_jsx = !(Error_message_utils.configured_jsx_module ()) in
            Error_message_utils.configured_jsx_module () := Some "ChildJsx";
            Debug.set_item "translation";
            ( initial_jsx,
              (!(Debug.basic ()), !(Debug.translation ())),
              Stdlib.Format.asprintf "%a" Printtyp.type_expr (Ctype.newvar ())
            ))
        |> Domain.join
      in
      let second_text = Stdlib.Format.asprintf "%a" Printtyp.type_expr second in
      assert_equal None child_jsx;
      assert_equal (false, true) child_debug;
      assert_equal (Some "HostJsx")
        !(Error_message_utils.configured_jsx_module ());
      check
        (!(Debug.basic ()) && not !(Debug.translation ()))
        "GenType debug flags remain with their request domain";
      check
        (first_text <> second_text)
        "type variable names survive another domain's diagnostic reset";
      check (child_type = first_text)
        "a child domain starts its own type-printing names")

let wait_for_atomic flag =
  let deadline = Unix.gettimeofday () +. 5. in
  while (not (Atomic.get flag)) && Unix.gettimeofday () < deadline do
    Domain.cpu_relax ()
  done;
  check (Atomic.get flag) "concurrent test domain did not reach its checkpoint"

let warning_and_feature_isolation_tests _context =
  let left_ready = Atomic.make false in
  let right_ready = Atomic.make false in
  let left =
    Domain.spawn (fun () ->
        Request_local.with_fresh (fun () ->
            Request_local.with_fresh (fun () ->
                Warnings.parse_options false "-8";
                Experimental_features.enable_from_string "LetUnwrap";
                Atomic.set left_ready true;
                wait_for_atomic right_ready;
                ( Warnings.is_active (Warnings.Partial_match ""),
                  Experimental_features.is_enabled
                    Experimental_features.LetUnwrap ))))
  in
  let right =
    Domain.spawn (fun () ->
        Request_local.with_fresh (fun () ->
            Request_local.with_fresh (fun () ->
                wait_for_atomic left_ready;
                Warnings.parse_options false "+8";
                Atomic.set right_ready true;
                ( Warnings.is_active (Warnings.Partial_match ""),
                  Experimental_features.is_enabled
                    Experimental_features.LetUnwrap ))))
  in
  assert_equal (false, true) (Domain.join left);
  assert_equal (true, false) (Domain.join right);
  check
    (not (Experimental_features.is_enabled Experimental_features.LetUnwrap))
    "feature scope leaves host domain unchanged"

let js_config_isolation_tests _context =
  let left_ready = Atomic.make false in
  let right_ready = Atomic.make false in
  let left =
    Domain.spawn (fun () ->
        Request_local.with_fresh (fun () ->
            let config = Js_config.current () in
            config.jsx_module := Js_config.Generic {module_name = "Left"};
            config.source_map := Js_config.Linked;
            Stack.push "left" config.self_stack;
            Atomic.set left_ready true;
            wait_for_atomic right_ready;
            ( !(config.jsx_module),
              !(config.source_map),
              Stack.top config.self_stack )))
  in
  let right =
    Domain.spawn (fun () ->
        Request_local.with_fresh (fun () ->
            wait_for_atomic left_ready;
            let config = Js_config.current () in
            config.jsx_module := Js_config.Generic {module_name = "Right"};
            config.source_map := Js_config.Inline;
            Stack.push "right" config.self_stack;
            Atomic.set right_ready true;
            ( !(config.jsx_module),
              !(config.source_map),
              Stack.top config.self_stack )))
  in
  assert_equal
    (Js_config.Generic {module_name = "Left"}, Js_config.Linked, "left")
    (Domain.join left);
  assert_equal
    (Js_config.Generic {module_name = "Right"}, Js_config.Inline, "right")
    (Domain.join right);
  let outer = Js_config.current () in
  Request_local.with_fresh (fun () ->
      let nested = Js_config.current () in
      nested.source_map := Js_config.Hidden;
      Stack.push "nested" nested.self_stack);
  check
    (outer == Js_config.current ())
    "nested scope restores host configuration";
  check (Stack.is_empty outer.self_stack) "nested scope restores host stack"

let clflags_isolation_tests _context =
  let left_ready = Atomic.make false in
  let right_ready = Atomic.make false in
  let left =
    Domain.spawn (fun () ->
        Request_local.with_fresh (fun () ->
            Clflags.reset ();
            let flags = Clflags.current () in
            flags.include_dirs := ["left"];
            flags.color := Some Misc.Color.Never;
            Atomic.set left_ready true;
            wait_for_atomic right_ready;
            ( !(flags.include_dirs),
              !(flags.color),
              !(flags.debug),
              !(flags.binary_annotations) )))
  in
  let right =
    Domain.spawn (fun () ->
        Request_local.with_fresh (fun () ->
            wait_for_atomic left_ready;
            Clflags.reset ();
            let flags = Clflags.current () in
            flags.include_dirs := ["right"];
            flags.color := Some Misc.Color.Auto;
            Atomic.set right_ready true;
            ( !(flags.include_dirs),
              !(flags.color),
              !(flags.debug),
              !(flags.binary_annotations) )))
  in
  assert_equal (["left"], Some Misc.Color.Never, true, true) (Domain.join left);
  assert_equal (["right"], Some Misc.Color.Auto, true, true) (Domain.join right);
  check
    (!((Clflags.current ()).include_dirs) = [])
    "flag scopes leave host domain unchanged"

let env_cache_isolation_tests _context =
  let left_ready = Atomic.make false in
  let right_ready = Atomic.make false in
  let run unit_name ready other_ready =
    Domain.spawn (fun () ->
        Request_local.with_fresh (fun () ->
            Env.set_unit_name unit_name;
            Env.add_import unit_name;
            Consistbl.set (Env.crc_units ()) unit_name (Digest.string unit_name)
              unit_name;
            Atomic.set ready true;
            wait_for_atomic other_ready;
            (Env.get_unit_name (), Env.imports ())))
  in
  let left = run "Left" left_ready right_ready in
  let right = run "Right" right_ready left_ready in
  assert_equal
    ("Left", [("Left", Some (Digest.string "Left"))])
    (Domain.join left);
  assert_equal
    ("Right", [("Right", Some (Digest.string "Right"))])
    (Domain.join right);
  assert_equal "" (Env.get_unit_name ())

let identifier_stamp_isolation_tests _context =
  let left_ready = Atomic.make false in
  let right_ready = Atomic.make false in
  let starting_time = Ident.current_time () in
  let run ready other_ready =
    Domain.spawn (fun () ->
        Ident.with_fresh (fun () ->
            let first = Ident.binding_time (Ident.create "local") in
            Atomic.set ready true;
            wait_for_atomic other_ready;
            let second = Ident.binding_time (Ident.create "local") in
            (first, second)))
  in
  let left = run left_ready right_ready in
  let right = run right_ready left_ready in
  let left_first, left_second = Domain.join left in
  let right_first, right_second = Domain.join right in
  assert_equal (left_first + 1) left_second;
  assert_equal (right_first + 1) right_second;
  assert_equal left_first right_first;
  assert_equal starting_time (Ident.current_time ())

let output_capture_isolation_tests _context =
  let left_ready = Atomic.make false in
  let right_ready = Atomic.make false in
  let capture label ready other_ready =
    Domain.spawn (fun () ->
        Compiler_request_output.with_capture (fun () ->
            Compiler_request_output.write_stdout (label ^ "\000");
            Atomic.set ready true;
            wait_for_atomic other_ready;
            let formatter = Compiler_request_output.stderr_formatter () in
            Stdlib.Format.pp_print_string formatter label;
            Stdlib.Format.pp_print_newline formatter ();
            label))
  in
  let left = capture "left" left_ready right_ready in
  let right = capture "right" right_ready left_ready in
  (* Captured output is in text mode, as a bsc process's would be. *)
  let newline = if Sys.win32 then "\r\n" else "\n" in
  assert_equal ("left", "left\000", "left" ^ newline) (Domain.join left);
  assert_equal ("right", "right\000", "right" ^ newline) (Domain.join right);
  check
    (Compiler_request_output.stdout_channel () == Stdlib.stdout)
    "capture scopes restore the host stream"

let backend_module_cache_isolation_tests _context =
  let left_ready = Atomic.make false in
  let right_done = Atomic.make false in
  let add_module () =
    Lam_compile_env.add_js_module External_ffi_types.Phint_nothing
      "shared-backend-module" false ~dynamic_import:false
  in
  let left =
    Domain.spawn (fun () ->
        Request_local.with_fresh (fun () ->
            let first = add_module () in
            Atomic.set left_ready true;
            wait_for_atomic right_done;
            first == add_module ()))
  in
  let right =
    Domain.spawn (fun () ->
        wait_for_atomic left_ready;
        Request_local.with_fresh (fun () ->
            Lam_compile_env.reset ();
            ignore (add_module ());
            Atomic.set right_done true))
  in
  Domain.join right;
  check (Domain.join left) "another request cleared this module cache"

let command_runner_isolation_tests _context =
  let left_ready = Atomic.make false in
  let right_ready = Atomic.make false in
  let left_done = Atomic.make false in
  let left =
    Domain.spawn (fun () ->
        Ccomp.with_command_runner
          (fun _ -> 11)
          (fun () ->
            Atomic.set left_ready true;
            wait_for_atomic right_ready;
            let result = Ccomp.command "left" in
            Atomic.set left_done true;
            result))
  in
  let right =
    Domain.spawn (fun () ->
        wait_for_atomic left_ready;
        Ccomp.with_command_runner
          (fun _ -> 22)
          (fun () ->
            Atomic.set right_ready true;
            wait_for_atomic left_done;
            Ccomp.command "right"))
  in
  assert_equal 11 (Domain.join left);
  assert_equal 22 (Domain.join right)

let input_name_isolation_tests _context =
  let process_cwd = Sys.getcwd () in
  let first_root = Filename.concat process_cwd "first" in
  let second_root = Filename.concat process_cwd "second" in
  let left_ready = Atomic.make false in
  let right_ready = Atomic.make false in
  let left =
    Domain.spawn (fun () ->
        Compiler_request_state.with_fresh ~cwd:first_root (fun () ->
            Location.set_input_name "First.res";
            Atomic.set left_ready true;
            wait_for_atomic right_ready;
            (Location.get_input_name (), Ext_path.absolute_cwd_path "First.res")))
  in
  let right =
    Domain.spawn (fun () ->
        Compiler_request_state.with_fresh ~cwd:second_root (fun () ->
            wait_for_atomic left_ready;
            Location.set_input_name "Second.res";
            Atomic.set right_ready true;
            (Location.get_input_name (), Ext_path.absolute_cwd_path "Second.res")))
  in
  assert_equal
    ("First.res", Filename.concat first_root "First.res")
    (Domain.join left);
  assert_equal
    ("Second.res", Filename.concat second_root "Second.res")
    (Domain.join right);
  assert_equal "_none_" (Location.get_input_name ());
  assert_equal process_cwd (Sys.getcwd ())

let request_root_io_isolation_tests _context =
  with_temp_dir "rewatch-request-root-io-" (fun root ->
      let first = Filename.concat root "first" in
      let second = Filename.concat root "second" in
      ensure_dir first;
      ensure_dir second;
      write_file (Filename.concat first "Input.res") "first";
      write_file (Filename.concat second "Input.res") "second";
      let start = Atomic.make false in
      let access cwd =
        Domain.spawn (fun () ->
            Compiler_request_state.with_fresh ~cwd (fun () ->
                wait_for_atomic start;
                let rescript_source =
                  (Res_driver.parsing_engine.parse_implementation
                     ~filename:"Input.res")
                    .source
                in
                let compiler_source = Ext_io.load_file "Input.res" in
                Ext_io.write_file "Output.txt" rescript_source;
                (rescript_source, compiler_source)))
      in
      let left = access first in
      let right = access second in
      Atomic.set start true;
      assert_equal ("first", "first") (Domain.join left);
      assert_equal ("second", "second") (Domain.join right);
      assert_equal "first" (read_file (Filename.concat first "Output.txt"));
      assert_equal "second" (read_file (Filename.concat second "Output.txt")))

let saved_part =
  Cmt_format.Partial_signature
    {sig_items = []; sig_type = []; sig_final_env = Env.empty}

let cmt_accumulator_isolation_tests _context =
  let left_ready = Atomic.make false in
  let right_ready = Atomic.make false in
  let left =
    Domain.spawn (fun () ->
        Cmt_format.clear ();
        Cmt_format.set_args [|"bsc"; "First.res"|];
        Cmt_format.add_saved_type saved_part;
        Atomic.set left_ready true;
        wait_for_atomic right_ready;
        ( List.length (Cmt_format.get_saved_types ()),
          (Compiler_request_state.current ()).cmt_args ))
  in
  let right =
    Domain.spawn (fun () ->
        wait_for_atomic left_ready;
        Cmt_format.clear ();
        Cmt_format.set_args [|"bsc"; "Second.res"|];
        Cmt_format.add_saved_type saved_part;
        Cmt_format.add_saved_type saved_part;
        Atomic.set right_ready true;
        ( List.length (Cmt_format.get_saved_types ()),
          (Compiler_request_state.current ()).cmt_args ))
  in
  assert_equal (1, [|"bsc"; "First.res"|]) (Domain.join left);
  assert_equal (2, [|"bsc"; "Second.res"|]) (Domain.join right)

let package_output_isolation_tests _context =
  let left_ready = Atomic.make false in
  let right_ready = Atomic.make false in
  let configure name output =
    Js_packages_state.set_package_name name;
    Js_packages_state.update_npm_package_path output;
    match
      Js_packages_info.query_package_infos
        (Js_packages_state.get_packages_info ())
        Ext_module_system.Commonjs
    with
    | Js_packages_info.Package_found {rel_path; pkg_rel_path; suffix} ->
      (rel_path, pkg_rel_path, suffix)
    | Js_packages_info.Package_script | Js_packages_info.Package_not_found ->
      assert_failure "missing output spec"
  in
  let left =
    Domain.spawn (fun () ->
        Request_local.with_fresh (fun () ->
            let output = configure "first-package" "commonjs:lib/first:.js" in
            Atomic.set left_ready true;
            wait_for_atomic right_ready;
            output))
  in
  let right =
    Domain.spawn (fun () ->
        Request_local.with_fresh (fun () ->
            wait_for_atomic left_ready;
            let output =
              configure "second-package" "commonjs:lib/second:.mjs"
            in
            Atomic.set right_ready true;
            output))
  in
  assert_equal
    ("lib/first", Filename.concat "first-package" "lib/first", ".js")
    (Domain.join left);
  assert_equal
    ("lib/second", Filename.concat "second-package" "lib/second", ".mjs")
    (Domain.join right);
  check
    (Js_packages_info.is_empty (Js_packages_state.get_packages_info ()))
    "domain package scopes leave host state unchanged"

let path_configuration_isolation_tests _context =
  let first_root = Filename.concat (Sys.getcwd ()) "first" in
  let second_root = Filename.concat (Sys.getcwd ()) "second" in
  let initial_runtime = Runtime_package.get_path () in
  let initial_root = Ext_path.get_project_root () in
  let initial_load_path = (Compiler_request_state.current ()).load_path in
  let left_ready = Atomic.make false in
  let right_ready = Atomic.make false in
  let configure root =
    Runtime_package.set_path (Filename.concat root "runtime");
    Ext_path.set_project_root root;
    (Compiler_request_state.current ()).load_path <-
      [Filename.concat root "ocaml"];
    ( Runtime_package.get_path (),
      Ext_path.package_dir (),
      (Compiler_request_state.current ()).load_path )
  in
  let left =
    Domain.spawn (fun () ->
        Compiler_request_state.with_fresh (fun () ->
            let paths = configure first_root in
            Atomic.set left_ready true;
            wait_for_atomic right_ready;
            paths))
  in
  let right =
    Domain.spawn (fun () ->
        Compiler_request_state.with_fresh (fun () ->
            wait_for_atomic left_ready;
            let paths = configure second_root in
            Atomic.set right_ready true;
            paths))
  in
  assert_equal
    ( Filename.concat first_root "runtime",
      first_root,
      [Filename.concat first_root "ocaml"] )
    (Domain.join left);
  assert_equal
    ( Filename.concat second_root "runtime",
      second_root,
      [Filename.concat second_root "ocaml"] )
    (Domain.join right);
  assert_equal initial_runtime (Runtime_package.get_path ());
  assert_equal initial_root (Ext_path.get_project_root ());
  assert_equal initial_load_path (Compiler_request_state.current ()).load_path

let lambda_exit_isolation_tests _context =
  let left_ready = Atomic.make false in
  let right_ready = Atomic.make false in
  let left =
    Domain.spawn (fun () ->
        Compiler_request_state.with_fresh (fun () ->
            let first = Lambda_exits.next_raise_count () in
            Atomic.set left_ready true;
            wait_for_atomic right_ready;
            ( first,
              Lambda_exits.next_raise_count (),
              Lambda_exits.next_negative_raise_count () )))
  in
  let right =
    Domain.spawn (fun () ->
        Compiler_request_state.with_fresh (fun () ->
            wait_for_atomic left_ready;
            let first = Lambda_exits.next_raise_count () in
            let second = Lambda_exits.next_raise_count () in
            Atomic.set right_ready true;
            (first, second, Lambda_exits.next_negative_raise_count ())))
  in
  assert_equal (1, 2, -1) (Domain.join left);
  assert_equal (1, 2, -1) (Domain.join right)

let type_node_id_isolation_tests _context =
  let left_ready = Atomic.make false in
  let right_ready = Atomic.make false in
  let left_done = Atomic.make false in
  let left =
    Domain.spawn (fun () ->
        Compiler_request_state.with_fresh (fun () ->
            Request_local.with_fresh (fun () ->
                let ty = Btype.newgenvar () in
                let first = ty.Types.id in
                let snapshot = Btype.snapshot () in
                Btype.set_level ty 3;
                Atomic.set left_ready true;
                wait_for_atomic right_ready;
                Btype.backtrack snapshot;
                Atomic.set left_done true;
                (first, (Btype.newgenvar ()).Types.id, ty.Types.level))))
  in
  let right =
    Domain.spawn (fun () ->
        Compiler_request_state.with_fresh (fun () ->
            Request_local.with_fresh (fun () ->
                wait_for_atomic left_ready;
                let ty = Btype.newgenvar () in
                let first = ty.Types.id in
                let snapshot = Btype.snapshot () in
                Btype.set_level ty 7;
                let second = (Btype.newgenvar ()).Types.id in
                Atomic.set right_ready true;
                wait_for_atomic left_done;
                Btype.backtrack snapshot;
                (first, second, ty.Types.level))))
  in
  assert_equal (0, 1, Btype.generic_level) (Domain.join left);
  assert_equal (0, 1, Btype.generic_level) (Domain.join right)

let write root name contents = write_file (Filename.concat root name) contents

let run root ?(package = "driver-test") ?(extra = []) input =
  let argv =
    [
      "-nostdlib";
      "-nopervasives";
      "-bs-project-root";
      root;
      "-bs-package-name";
      package;
      "-bs-package-output";
      "commonjs:.:.js";
      "-bs-no-version-header";
    ]
    @ extra
  in
  ( argv,
    Rescript_compiler_driver.run_request ~run_external:None ~cwd:root ~argv
      ~input )

let expect_code expected result =
  assert_equal ~printer:string_of_int expected
    result.Rescript_compiler_driver.exit_code

(* The standalone bsc writes to its own stdout and stderr as it goes, without
   capturing output in temporary files: it compiles even when it can't create
   any. *)
let standalone_output_tests _context =
  with_temp_dir "rewatch-driver-standalone-" (fun root ->
      let temp = Filename.concat root "tmp" in
      Unix.mkdir temp 0o555;
      write root "Plain.res" "let plain = 1\n";
      let previous_temp = Filename.get_temp_dir_name () in
      Filename.set_temp_dir_name temp;
      let exit_code =
        Fun.protect
          ~finally:(fun () ->
            Filename.set_temp_dir_name previous_temp;
            Unix.chmod temp 0o755)
          (fun () ->
            Rescript_compiler_driver.run
              [|
                "bsc";
                "-nostdlib";
                "-nopervasives";
                "-bs-project-root";
                root;
                "-bs-package-name";
                "driver-test";
                "-bs-package-output";
                "commonjs:.:.js";
                "-bs-no-version-header";
                Filename.concat root "Plain.res";
              |])
      in
      assert_equal ~printer:string_of_int 0 exit_code)

(* A relative project root names a directory under the request's working
   directory, not the host process's, for the JavaScript writer too. *)
let relative_project_root_tests _context =
  with_temp_dir "rewatch-driver-relative-root-" (fun root ->
      write root "Relative.res" "let relative = 1\n";
      let argv =
        [
          "-nostdlib";
          "-nopervasives";
          "-bs-project-root";
          ".";
          "-bs-package-name";
          "driver-test";
          "-bs-package-output";
          "commonjs:.:.js";
          "-bs-no-version-header";
        ]
      in
      let result =
        Rescript_compiler_driver.run_request ~run_external:None ~cwd:root ~argv
          ~input:"Relative.res"
      in
      expect_code 0 result;
      check
        (Sys.file_exists (Filename.concat root "Relative.js"))
        "JavaScript is written under the request's directory";
      check
        (not (Sys.file_exists "Relative.js"))
        "JavaScript is not written under the host's directory")

(* -e writes its source to a temporary file. With a relative temporary
   directory, which names a directory under the host's working directory, the
   file is still found and removed when the request runs elsewhere. *)
let eval_relative_temp_dir_tests _context =
  with_temp_dir "rewatch-driver-eval-root-" (fun root ->
      with_temp_dir "rewatch-driver-eval-host-" (fun host ->
          let temp = Filename.concat host "tmp" in
          Unix.mkdir temp 0o755;
          let previous_cwd = Sys.getcwd () in
          let previous_temp = Filename.get_temp_dir_name () in
          Sys.chdir host;
          Filename.set_temp_dir_name "tmp";
          let result =
            Fun.protect
              ~finally:(fun () ->
                Filename.set_temp_dir_name previous_temp;
                Sys.chdir previous_cwd)
              (fun () ->
                Rescript_compiler_driver.run_request ~run_external:None
                  ~cwd:root
                  ~argv:
                    [
                      "-nostdlib";
                      "-nopervasives";
                      "-bs-project-root";
                      root;
                      "-bs-package-name";
                      "driver-test";
                      "-bs-no-version-header";
                      "-e";
                    ]
                  ~input:"let value = 1")
          in
          if result.exit_code <> 0 then
            assert_failure (result.stdout ^ result.stderr);
          check
            (not
               (Array.exists
                  (fun name -> Filename.check_suffix name ".res")
                  (Sys.readdir temp)))
            "the -e source file is removed"))

let recovery_tests _context =
  with_temp_dir "rewatch-driver-recovery-" (fun root ->
      write root "Parse.res" "let value = 1\n";
      let _, initial = run root "Parse.res" in
      expect_code 0 initial;
      write root "Parse.res" "let value =\n";
      let _, failed = run root "Parse.res" in
      expect_code 1 failed;
      check
        (contains_text failed.stderr "Syntax error")
        "parse diagnostics are returned to the host";
      write root "Parse.res" "let value = 2\n";
      let _, repaired = run root "Parse.res" in
      expect_code 0 repaired;
      write root "Typed.res" "let value: string = 1\n";
      let _, failed = run root "Typed.res" in
      expect_code 2 failed;
      check
        (contains_text failed.stderr "int"
        && contains_text failed.stderr "string")
        "type diagnostics are returned to the host";
      write root "Typed.res" {|let value: string = "repaired"|};
      let _, repaired = run root "Typed.res" in
      expect_code 0 repaired)

let ppx_recovery_tests _context =
  with_temp_dir "rewatch-driver-ppx-" (fun root ->
      write root "Ppx.res" "let value = 1\n";
      let argv =
        [
          "-nostdlib";
          "-nopervasives";
          "-bs-project-root";
          root;
          "-bs-package-name";
          "driver-test";
          "-bs-package-output";
          "commonjs:.:.js";
          "-ppx";
          "rewatch-test-ppx";
          "-verbose";
        ]
      in
      let failed =
        Rescript_compiler_driver.run_request ~cwd:root ~argv ~input:"Ppx.res"
          ~run_external:(Some (fun _ -> (7, "", "intentional PPX failure\n")))
      in
      expect_code 2 failed;
      check
        (contains_text failed.stderr "intentional PPX failure")
        "PPX failure diagnostics are returned through the host boundary";
      let repaired =
        Rescript_compiler_driver.run_request ~cwd:root ~argv ~input:"Ppx.res"
          ~run_external:
            (Some
               (fun command ->
                 let quote =
                   if String.contains command '\'' then '\'' else '"'
                 in
                 match String.split_on_char quote command with
                 | _program :: input :: _separator :: output :: _ ->
                   copy_existing_file input output;
                   (0, "", "")
                 | _ -> (2, "", "could not decode test PPX command")))
      in
      expect_code 0 repaired;
      check
        (contains_text repaired.stderr "+ rewatch-test-ppx")
        "direct verbose output is returned through the request boundary")

let control_flow_and_output_tests _context =
  with_temp_dir "rewatch-driver-control-" (fun root ->
      let process_cwd = Sys.getcwd () in
      write root "Good.res" "let value = 1\n";
      write root "Broken.res" "let value =\n";
      let request argv input =
        Rescript_compiler_driver.run_request ~run_external:None ~cwd:root ~argv
          ~input
      in
      let version = request ["-version"] "Good.res" in
      expect_code 0 version;
      check
        (contains_text version.stdout "ReScript")
        "version output is captured without terminating the host";
      let help = request ["-help"] "Good.res" in
      expect_code 0 help;
      check
        (contains_text help.stdout "Usage: bsc")
        "help output is captured without terminating the host";
      let reprint = request ["-reprint-source"; "Broken.res"] "Good.res" in
      expect_code 1 reprint;
      check
        (contains_text reprint.stderr "Syntax error")
        "reprint parse failures return diagnostics instead of exiting";
      let format = request ["-format"; "Broken.res"] "Good.res" in
      expect_code 1 format;
      check
        (contains_text format.stderr "Syntax error")
        "format parse failures return diagnostics instead of exiting";
      let stdout_compile =
        request
          ["-nostdlib"; "-nopervasives"; "-bs-no-version-header"]
          "Good.res"
      in
      expect_code 0 stdout_compile;
      check
        (contains_text stdout_compile.stdout "value")
        "compiler stdout is captured in the structured result";
      let _, repaired = run root "Good.res" in
      expect_code 0 repaired;
      assert_equal process_cwd (Sys.getcwd ()))

let state_boundary_tests _context =
  with_temp_dir "rewatch-driver-state-" (fun root ->
      write root "Warning.res"
        "let rec notActuallyRecursive = () => 42\n\n\
         let _ = notActuallyRecursive()\n";
      let _, failed = run root ~extra:["-warn-error"; "+39"] "Warning.res" in
      expect_code 2 failed;
      let _, warning_only = run root "Warning.res" in
      expect_code 0 warning_only;
      check
        (contains_text warning_only.stderr "Warning number 39")
        "warning configuration is reset between requests";
      write root "SourceFlags.res"
        "@@config({flags: [\"-warn-error\", \"+39\"]})\n\n\
         let rec notActuallyRecursive = () => 42\n\n\
         let _ = notActuallyRecursive()\n";
      let _, source_flags = run root "SourceFlags.res" in
      expect_code 2 source_flags;
      let _, after_source_flags = run root "Warning.res" in
      expect_code 0 after_source_flags;
      check
        (contains_text after_source_flags.stderr "Warning number 39")
        "source-level compiler flags are reset between requests";
      write root "Mapped.res" "let mapped = 1\n";
      let mapped_args, mapped =
        run root ~extra:["-bs-source-map"; "linked"] "Mapped.res"
      in
      expect_code 0 mapped;
      check
        (Sys.file_exists (Filename.concat root "Mapped.js.map"))
        "the requested source map is generated";
      let cmt = Cmt_format.read_cmt (Filename.concat root "Mapped.cmt") in
      check
        (Array.to_list cmt.cmt_args = ("bsc" :: mapped_args) @ ["Mapped.res"])
        "CMT metadata records the logical request argv";
      write root "Plain.res" "let plain = 1\n";
      let _, plain =
        run root
          ~extra:
            [
              "-bs-jsx";
              "4";
              "-bs-jsx-module";
              "CustomJsx";
              "-enable-experimental";
              "LetUnwrap";
              "-bs-gentype-module";
              "esmodule";
            ]
          "Plain.res"
      in
      expect_code 0 plain;
      check
        (not (Sys.file_exists (Filename.concat root "Plain.js.map")))
        "source-map state does not leak from the preceding request";
      check
        (!((Js_config.current ()).jsx_version) = None
        && !((Js_config.current ()).jsx_module) = Js_config.React
        && (not
              (Experimental_features.is_enabled Experimental_features.LetUnwrap))
        && !(Gentype_config.module_flag ()) = None)
        "JSX, experimental, and GenType state is restored after the request")

(* A request that fails with a type error can leave memorized abbreviation
   expansions behind. They are keyed by path, and paths into other compilation
   units compare by name, so a later request must not find this [Lib.t]. *)
let ctype_memo_reset_tests _context =
  with_temp_dir "rewatch-driver-ctype-" (fun root ->
      write root "Lib.res" "type t = int\n";
      expect_code 0 (snd (run root "Lib.res"));
      let memorized = Btype.For_test.memorized_abbrev_count () in
      (* A failing request stops before it would flush the memo itself. *)
      write root "Bad.res" "let value: Lib.t = \"text\"\n";
      expect_code 2 (snd (run root ~extra:["-I"; root] "Bad.res"));
      assert_equal ~printer:string_of_int memorized
        (Btype.For_test.memorized_abbrev_count ())
        ~msg:"abbreviation expansions do not outlive their request";
      check
        (Ctype.For_test.previous_env_is_empty ())
        "the expansion environment does not outlive its request")

let package_and_load_path_isolation_tests _context =
  with_temp_dir "rewatch-driver-load-roots-" (fun root ->
      let first = Filename.concat root "first" in
      let second = Filename.concat root "second" in
      ensure_dir first;
      ensure_dir second;
      write first "Shared.res" "let value = 1\n";
      write first "Consumer.res" "let value: int = Shared.value\n";
      write second "Shared.res" {|let value = "second"|};
      write second "Consumer.res" "let value: string = Shared.value\n";
      let compile directory package ?(extra = []) input =
        run directory ~package ~extra input |> snd
      in
      expect_code 0 (compile first "first-package" "Shared.res");
      expect_code 0
        (compile first "first-package" ~extra:["-I"; first] "Consumer.res");
      expect_code 0 (compile second "second-package" "Shared.res");
      expect_code 0
        (compile second "second-package" ~extra:["-I"; second] "Consumer.res");
      expect_code 0
        (compile first "first-package" ~extra:["-I"; first] "Consumer.res"))

let generated_name_isolation_tests _context =
  with_temp_dir "rewatch-driver-generated-names-" (fun root ->
      write root "First.res" "%%private(let hidden = 1)\nlet value = hidden\n";
      write root "Other.res"
        "%%private(let first = 1)\n%%private(let second = first + 1)\n";
      let parse input =
        let _, result = run root ~extra:["-bs-ast"] input in
        expect_code 0 result;
        let ast =
          read_file
            (Filename.concat root (Filename.remove_extension input ^ ".ast"))
        in
        check (String.length ast > 0) "binary AST output is nonempty";
        ast
      in
      let first_ast = parse "First.res" in
      ignore (parse "Other.res");
      let repeated_ast = parse "First.res" in
      assert_equal ~printer:(fun _ -> "<binary AST>") first_ast repeated_ast;
      Lambda_exits.reset ();
      let type_id = (Btype.newgenvar ()).Types.id in
      let exit_id = Lambda_exits.next_raise_count () in
      ignore (parse "Other.res");
      check
        ((Btype.newgenvar ()).Types.id = type_id + 1)
        "an embedded request preserves the host domain's type-node counter";
      check
        (Lambda_exits.next_raise_count () = exit_id + 1)
        "an embedded request preserves the host domain's static-exit counter";
      write root "Objects.res"
        "type base<'a> = {\"value\": 'a}\n\
         type pair<'a> = {\"left\": base<'a>, \"right\": base<'a>}\n\
         type nested<'a> = {\"first\": pair<'a>, \"second\": pair<'a>}\n";
      let _, first_objects = run root "Objects.res" in
      expect_code 0 first_objects;
      (* Saved types in the CMI get fresh ids, so only the CMT shows type-node
         ids that continue from the host's counter. *)
      let artifacts = ["Objects.cmi"; "Objects.cmt"] in
      let read_artifacts () =
        List.map
          (fun name ->
            let contents = read_file (Filename.concat root name) in
            check (String.length contents > 0) (name ^ " output is nonempty");
            contents)
          artifacts
      in
      let first_artifacts = read_artifacts () in
      for _ = 1 to 1000 do
        ignore (Btype.newgenvar ())
      done;
      List.iter (fun name -> Sys.remove (Filename.concat root name)) artifacts;
      let _, repeated_objects = run root "Objects.res" in
      expect_code 0 repeated_objects;
      List.iter2
        (fun name (first, repeated) ->
          assert_equal
            ~printer:(fun _ -> "<binary " ^ name ^ ">")
            first repeated)
        artifacts
        (List.combine first_artifacts (read_artifacts ()));
      Lambda_exits.reset ())

let interface_namespace_and_load_path_tests _context =
  with_temp_dir "rewatch-driver-artifacts-" (fun root ->
      write root "Api.resi" "let value: int\n";
      let _, intf = run root "Api.resi" in
      expect_code 0 intf;
      check
        (Sys.file_exists (Filename.concat root "Api.cmti"))
        "an explicit interface produces CMTI";
      write root "Api.res" "let value = 1\n";
      let _, impl = run root ~extra:["-I"; root; "-bs-read-cmi"] "Api.res" in
      expect_code 0 impl;
      write root "Consumer.res" "let value = Api.value\n";
      let _, consumer = run root ~extra:["-I"; root] "Consumer.res" in
      expect_code 0 consumer;
      write root "DriverNs.mlmap" "randjbuildsystem\nApi\nConsumer\n";
      let _, namespace =
        run root ~extra:["-I"; root; "-no-alias-deps"] "DriverNs.mlmap"
      in
      expect_code 0 namespace;
      List.iter
        (fun extension ->
          check
            (Sys.file_exists (Filename.concat root ("DriverNs." ^ extension)))
            ("namespace output is retained: " ^ extension))
        ["cmi"; "cmj"; "cmt"])

let concurrent_diagnostic_recovery_tests _context =
  with_temp_dir "rewatch-driver-errors-" (fun root ->
      let first = Filename.concat root "first" in
      let second = Filename.concat root "second" in
      ensure_dir first;
      ensure_dir second;
      write first "Api.resi" "let value: int\n";
      expect_code 0 (snd (run first "Api.resi"));
      write first "Api.res" {|let value = "left"|};
      write second "Bad.res" "let value: int = \"right\"\n";
      let first_request () =
        snd
          (run first
             ~extra:["-I"; first; "-bs-read-cmi"; "-bs-jsx"; "4"]
             "Api.res")
      in
      let second_request () =
        snd (run second ~extra:["-bs-jsx-module"; "OtherJsx"] "Bad.res")
      in
      let expected_first = first_request () in
      let expected_second = second_request () in
      expect_code 2 expected_first;
      expect_code 2 expected_second;
      for _ = 1 to 5 do
        let left = Domain.spawn first_request in
        let right = Domain.spawn second_request in
        let actual_first = Domain.join left in
        let actual_second = Domain.join right in
        assert_equal expected_first.stderr actual_first.stderr;
        assert_equal expected_second.stderr actual_second.stderr
      done;
      write first "Api.res" "let value = 1\n";
      write second "Bad.res" "let value = 1\n";
      expect_code 0 (first_request ());
      expect_code 0 (second_request ()))

let concurrent_jsx_diagnostic_tests _context =
  with_temp_dir "rewatch-driver-jsx-errors-" (fun root ->
      let source module_name =
        "@@config({flags: [\"-bs-jsx\", \"4\"]})\n\nmodule " ^ module_name
        ^ " = {\n\
           type element = Jsx.element\n\
           type componentLike<'props, 'return> = 'props => 'return\n\
           type component<'props> = Jsx.component<'props>\n\
           external component: componentLike<'props, element> => \
           component<'props> = \"%component_identity\"\n\
           @module(\"react/jsx-runtime\")\n\
           external jsx: (component<'props>, 'props) => element = \"jsx\"\n\
           type fragmentProps = {children?: element}\n\
           @module(\"react/jsx-runtime\")\n\
           external jsxFragment: component<fragmentProps> = \"Fragment\"\n\
           }\n\n\
           let x = <> {\"\"} </>\n"
      in
      let setup module_name =
        let directory = Filename.concat root module_name in
        ensure_dir directory;
        write directory "Jsx.res" "type element\ntype component<'props>\n";
        expect_code 0 (snd (run directory "Jsx.res"));
        write directory "Bad.res" (source module_name);
        let request () =
          snd
            (run directory
               ~extra:["-I"; directory; "-bs-jsx-module"; module_name]
               "Bad.res")
        in
        (module_name, request)
      in
      let left_name, left_request = setup "React" in
      let right_name, right_request = setup "OtherJsx" in
      let left_expected = left_request () in
      let right_expected = right_request () in
      expect_code 2 left_expected;
      expect_code 2 right_expected;
      List.iter
        (fun (module_name, result) ->
          check
            (contains_text result.Rescript_compiler_driver.stderr
               (module_name ^ ".string"))
            ("JSX diagnostic names " ^ module_name))
        [(left_name, left_expected); (right_name, right_expected)];
      for _ = 1 to 5 do
        let left = Domain.spawn left_request in
        let right = Domain.spawn right_request in
        assert_equal left_expected.stderr (Domain.join left).stderr;
        assert_equal right_expected.stderr (Domain.join right).stderr
      done)

(* Calls of open JS object methods mark their callee in request-local typing
   state. Another request typing applications at the same time must not
   overwrite that mark, which would report the method's arguments as unused
   (warning 20, off by default and enabled here). *)
let concurrent_open_object_method_tests _context =
  with_temp_dir "rewatch-driver-open-method-" (fun root ->
      let source =
        "@get external obj: string => {..} = \"obj\"\n"
        ^ String.concat ""
            (List.init 200 (fun i ->
                 Printf.sprintf "let v%d = obj(\"e\")[\"m\"](\"a\", \"b\")\n" i))
      in
      let setup name =
        let directory = Filename.concat root name in
        ensure_dir directory;
        write directory "Calls.res" source;
        fun () -> snd (run directory ~extra:["-w"; "+20"] "Calls.res")
      in
      let left_request = setup "left" in
      let right_request = setup "right" in
      let expect_clean result =
        expect_code 0 result;
        assert_equal ~printer:Fun.id "" result.Rescript_compiler_driver.stderr
      in
      expect_clean (left_request ());
      for _ = 1 to 5 do
        let left = Domain.spawn left_request in
        let right = Domain.spawn right_request in
        expect_clean (Domain.join left);
        expect_clean (Domain.join right)
      done)

(* Exercise concurrent compiler requests without the process-worker boundary. *)
let concurrent_request_isolation_tests _context =
  with_temp_dir "rewatch-driver-domains-" (fun root ->
      let first = Filename.concat root "first" in
      let second = Filename.concat root "second" in
      ensure_dir first;
      ensure_dir second;
      write first "First.res" "type box<'a> = array<'a>\nlet first = 1\n";
      write second "Second.res" "type box<'b> = array<'b>\nlet second = 2\n";
      let entered = Atomic.make 0 in
      let runner command =
        ignore (Atomic.fetch_and_add entered 1);
        let deadline = Unix.gettimeofday () +. 5. in
        while Atomic.get entered < 2 && Unix.gettimeofday () < deadline do
          Domain.cpu_relax ()
        done;
        if Atomic.get entered < 2 then (7, "", "PPX overlap timed out")
        else
          let quote = if String.contains command '\'' then '\'' else '"' in
          match String.split_on_char quote command with
          | _program :: input :: _separator :: output :: _ ->
            copy_existing_file input output;
            (0, "", "")
          | _ -> (7, "", "could not decode test PPX command")
      in
      let request directory package extra input =
        let argv =
          [
            "-nostdlib";
            "-nopervasives";
            "-bs-project-root";
            directory;
            "-bs-package-name";
            package;
            "-bs-package-output";
            "commonjs:.:.js";
            "-bs-no-version-header";
            "-ppx";
            "rewatch-test-ppx";
          ]
          @ extra
        in
        Rescript_compiler_driver.run_request ~cwd:directory ~argv ~input
          ~run_external:(Some runner)
      in
      let left =
        Domain.spawn (fun () ->
            request first "first-package"
              ["-w"; "-39"; "-bs-jsx"; "4"; "-bs-jsx-module"; "CustomJsx"]
              "First.res")
      in
      let right =
        Domain.spawn (fun () ->
            request second "second-package"
              ["-w"; "+39"; "-bs-source-map"; "linked"]
              "Second.res")
      in
      let left_result = Domain.join left in
      let right_result = Domain.join right in
      expect_code 0 left_result;
      expect_code 0 right_result;
      check
        (Sys.file_exists (Filename.concat first "First.js"))
        "first request writes its JavaScript into its root";
      check
        (Sys.file_exists (Filename.concat second "Second.js"))
        "second request writes its JavaScript into its root";
      check
        (Sys.file_exists (Filename.concat second "Second.js.map"))
        "second request writes its requested source map";
      check
        (not (Sys.file_exists (Filename.concat first "First.js.map")))
        "source-map configuration does not leak to the first request";
      check
        (left_result.stdout = "" && right_result.stdout = "")
        "concurrent requests keep stdout separate";
      check
        (left_result.stderr = "" && right_result.stderr = "")
        (Printf.sprintf
           "concurrent requests keep stderr separate: left=%S right=%S"
           left_result.stderr right_result.stderr);
      Atomic.set entered 0;
      let absolute_parser =
        Domain.spawn (fun () ->
            request first "first-package"
              ["-absname"; "-bs-ast"; "-o"; "First.ast"]
              "First.res")
      in
      let relative_parser =
        Domain.spawn (fun () ->
            request second "second-package"
              ["-bs-ast"; "-o"; "Second.ast"]
              "Second.res")
      in
      expect_code 0 (Domain.join absolute_parser);
      expect_code 0 (Domain.join relative_parser);
      check
        (ast_source (Filename.concat first "First.ast")
        = Some (Filename.concat first "First.res"))
        "-absname retains an absolute source path during concurrent parsing";
      check
        (ast_source (Filename.concat second "Second.ast") = Some "Second.res")
        "a concurrent parser without -absname retains its relative source path")

let concurrent_async_context_tests _context =
  with_temp_dir "rewatch-driver-async-domains-" (fun root ->
      let runtime = Runtime_package.get_path () in
      let runtime_ocaml = Filename.concat runtime "lib/ocaml" in
      let async_source =
        "external get: unit => promise<int> = \"get\"\n"
        ^ (List.init 80 (fun index ->
               Printf.sprintf
                 "let async%d = async () => { let value = await get(); value }\n"
                 index)
          |> String.concat "")
        ^ "let topLevel = await get()\n"
      in
      let sync_source =
        List.init 80 (fun index ->
            Printf.sprintf "let sync%d = () => %d\n" index index)
        |> String.concat ""
      in
      let ready = Atomic.make 0 in
      let run_domain index =
        let directory = Filename.concat root (string_of_int index) in
        ensure_dir directory;
        write directory "Async.res" async_source;
        write directory "Sync.res" sync_source;
        Domain.spawn (fun () ->
            ignore (Atomic.fetch_and_add ready 1);
            let deadline = Unix.gettimeofday () +. 5. in
            while Atomic.get ready < 8 && Unix.gettimeofday () < deadline do
              Domain.cpu_relax ()
            done;
            check (Atomic.get ready = 8) "async requests did not overlap";
            for _ = 1 to 3 do
              List.iter
                (fun input ->
                  let _, result =
                    run directory ~extra:["-I"; runtime_ocaml] input
                  in
                  if result.exit_code <> 0 then
                    assert_failure (Printf.sprintf "%s: %s" input result.stderr))
                (if index mod 2 = 0 then ["Async.res"; "Sync.res"]
                 else ["Sync.res"; "Async.res"])
            done)
      in
      List.init 8 run_domain |> List.iter Domain.join)

let suites =
  __FILE__
  >::: [
         "request_local" >:: request_local_tests;
         "request_local_order" >:: request_local_order_tests;
         "failed_capture" >:: failed_capture_tests;
         "captured_line_endings" >:: captured_line_endings_tests;
         "failed_decoding_closes_files" >:: failed_decoding_closes_files_tests;
         "gentype_source_lookup" >:: gentype_source_lookup_tests;
         "standalone_output" >:: standalone_output_tests;
         "ctype_level_isolation" >:: ctype_level_isolation_tests;
         "free_variable_isolation" >:: free_variable_isolation_tests;
         "ctype_memo_reset" >:: ctype_memo_reset_tests;
         "shared_type_graph_isolation" >:: shared_type_graph_isolation_tests;
         "dependency_extraction_isolation"
         >:: dependency_extraction_isolation_tests;
         "gentype_output_capture" >:: gentype_output_capture_tests;
         "used_attributes_isolation" >:: used_attributes_isolation_tests;
         "delayed_checks_isolation" >:: delayed_checks_isolation_tests;
         "gentype_flags_isolation" >:: gentype_flags_isolation_tests;
         "diagnostic_state_isolation" >:: diagnostic_state_isolation_tests;
         "command_runner_isolation" >:: command_runner_isolation_tests;
         "warning_and_feature_isolation" >:: warning_and_feature_isolation_tests;
         "js_config_isolation" >:: js_config_isolation_tests;
         "clflags_isolation" >:: clflags_isolation_tests;
         "env_cache_isolation" >:: env_cache_isolation_tests;
         "identifier_stamp_isolation" >:: identifier_stamp_isolation_tests;
         "output_capture_isolation" >:: output_capture_isolation_tests;
         "backend_module_cache_isolation"
         >:: backend_module_cache_isolation_tests;
         "input_name_isolation" >:: input_name_isolation_tests;
         "request_root_io_isolation" >:: request_root_io_isolation_tests;
         "cmt_accumulator_isolation" >:: cmt_accumulator_isolation_tests;
         "package_output_isolation" >:: package_output_isolation_tests;
         "path_configuration_isolation" >:: path_configuration_isolation_tests;
         "lambda_exit_isolation" >:: lambda_exit_isolation_tests;
         "type_node_id_isolation" >:: type_node_id_isolation_tests;
         "relative_project_root" >:: relative_project_root_tests;
         "failed_writing_closes_files" >:: failed_writing_closes_files_tests;
         "eval_relative_temp_dir" >:: eval_relative_temp_dir_tests;
         "recovery" >:: recovery_tests;
         "ppx_recovery" >:: ppx_recovery_tests;
         "control_flow_and_output" >:: control_flow_and_output_tests;
         "state_boundary" >:: state_boundary_tests;
         "package_and_load_path_isolation"
         >:: package_and_load_path_isolation_tests;
         "generated_name_isolation" >:: generated_name_isolation_tests;
         "interfaces_namespaces_load_paths"
         >:: interface_namespace_and_load_path_tests;
         "concurrent_diagnostic_recovery"
         >:: concurrent_diagnostic_recovery_tests;
         "concurrent_jsx_diagnostic" >:: concurrent_jsx_diagnostic_tests;
         "concurrent_open_object_method" >:: concurrent_open_object_method_tests;
         "concurrent_request_isolation" >:: concurrent_request_isolation_tests;
         "concurrent_async_context" >:: concurrent_async_context_tests;
       ]
