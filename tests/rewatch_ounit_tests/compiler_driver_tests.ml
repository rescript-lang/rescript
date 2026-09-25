open OUnit2

let check condition message = assert_bool message condition

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
    (String_util.contains stdout "captured warning")
    "GenType warnings use the request-owned stdout formatter";
  assert_equal "" stderr

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
  Gentype_config.reset_flags ();
  Gentype_config.add_source_dir "host-source";
  let child_sources =
    Domain.spawn (fun () ->
        Gentype_config.reset_flags ();
        Gentype_config.add_source_dir "child-source";
        !(Gentype_config.source_dirs_flag ()))
    |> Domain.join
  in
  assert_equal ["child-source"] child_sources;
  assert_equal ["host-source"] !(Gentype_config.source_dirs_flag ());
  Gentype_config.reset_flags ()

let diagnostic_state_isolation_tests _context =
  let old_jsx = !(Error_message_utils.configured_jsx_module ()) in
  Fun.protect
    ~finally:(fun () ->
      Error_message_utils.configured_jsx_module () := old_jsx;
      Debug.reset ();
      Printtyp.reset_request ())
    (fun () ->
      Error_message_utils.configured_jsx_module () := Some "HostJsx";
      Debug.reset ();
      Debug.set_item "basic";
      Printtyp.reset_request ();
      let first = Ctype.newvar () in
      let second = Ctype.newvar () in
      let first_text = Stdlib.Format.asprintf "%a" Printtyp.type_expr first in
      let child_jsx, child_debug, child_type =
        Domain.spawn (fun () ->
            let initial_jsx = !(Error_message_utils.configured_jsx_module ()) in
            Error_message_utils.configured_jsx_module () := Some "ChildJsx";
            Debug.reset ();
            Debug.set_item "translation";
            Printtyp.reset_request ();
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
        Warnings.with_fresh (fun () ->
            Experimental_features.with_fresh (fun () ->
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
        Warnings.with_fresh (fun () ->
            Experimental_features.with_fresh (fun () ->
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
        Js_config.with_fresh (fun () ->
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
        Js_config.with_fresh (fun () ->
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
  Js_config.with_fresh (fun () ->
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
        Clflags.with_fresh (fun () ->
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
        Clflags.with_fresh (fun () ->
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
        Env.with_fresh (fun () ->
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
  assert_equal ("left", "left\000", "left\n") (Domain.join left);
  assert_equal ("right", "right\000", "right\n") (Domain.join right);
  check
    (Compiler_request_output.stdout_channel () == Stdlib.stdout)
    "capture scopes restore the host stream"

let annotation_isolation_tests _context =
  let left_ready = Atomic.make false in
  let right_ready = Atomic.make false in
  let run label ready other_ready =
    Domain.spawn (fun () ->
        Clflags.with_fresh (fun () ->
            Stypes.with_fresh (fun () ->
                (Clflags.current ()).annotations := true;
                let location =
                  {(Location.in_file label) with Location.loc_ghost = false}
                in
                Stypes.record (Stypes.An_call (location, Annot.Tail));
                Atomic.set ready true;
                wait_for_atomic other_ready;
                match Stypes.get_info () with
                | [Stypes.An_call (loc, Annot.Tail)] ->
                  loc.Location.loc_start.Lexing.pos_fname
                | annotations ->
                  assert_failure
                    (Printf.sprintf "expected one annotation, got %d"
                       (List.length annotations)))))
  in
  let left = run "left.res" left_ready right_ready in
  let right = run "right.res" right_ready left_ready in
  assert_equal "left.res" (Domain.join left);
  assert_equal "right.res" (Domain.join right);
  Clflags.with_fresh (fun () ->
      (Clflags.current ()).annotations := true;
      (try
         Stypes.with_fresh (fun () ->
             let location =
               {(Location.in_file "failed.res") with Location.loc_ghost = false}
             in
             Stypes.record (Stypes.An_call (location, Annot.Tail));
             failwith "failed request")
       with Failure _ -> ());
      assert_equal [] (Stypes.get_info ()))

let backend_module_cache_isolation_tests _context =
  let left_ready = Atomic.make false in
  let right_done = Atomic.make false in
  let add_module () =
    Lam_compile_env.add_js_module External_ffi_types.Phint_nothing
      "shared-backend-module" false ~dynamic_import:false
  in
  let left =
    Domain.spawn (fun () ->
        Lam_compile_env.with_fresh (fun () ->
            let first = add_module () in
            Atomic.set left_ready true;
            wait_for_atomic right_done;
            first == add_module ()))
  in
  let right =
    Domain.spawn (fun () ->
        wait_for_atomic left_ready;
        Lam_compile_env.with_fresh (fun () ->
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
  Test_support.with_temp_dir "rewatch-request-root-io-" (fun root ->
      let first = Filename.concat root "first" in
      let second = Filename.concat root "second" in
      File_util.ensure_dir first;
      File_util.ensure_dir second;
      Test_support.write_file (Filename.concat first "Input.res") "first";
      Test_support.write_file (Filename.concat second "Input.res") "second";
      let start = Atomic.make false in
      let access cwd =
        Domain.spawn (fun () ->
            Compiler_request_state.with_fresh ~cwd (fun () ->
                wait_for_atomic start;
                let rescript_source = Res_io.read_file ~filename:"Input.res" in
                let compiler_source = Ext_io.load_file "Input.res" in
                Ext_io.write_file "Output.txt" rescript_source;
                (rescript_source, compiler_source)))
      in
      let left = access first in
      let right = access second in
      Atomic.set start true;
      assert_equal ("first", "first") (Domain.join left);
      assert_equal ("second", "second") (Domain.join right);
      assert_equal "first"
        (File_util.read_file (Filename.concat first "Output.txt"));
      assert_equal "second"
        (File_util.read_file (Filename.concat second "Output.txt")))

let cmt_accumulator_isolation_tests _context =
  let left_ready = Atomic.make false in
  let right_ready = Atomic.make false in
  let left =
    Domain.spawn (fun () ->
        Cmt_format.clear ();
        Cmt_format.set_args [|"bsc"; "First.res"|];
        Cmt_format.add_saved_type (Cmt_format.Partial_class_expr ());
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
        Cmt_format.add_saved_type (Cmt_format.Partial_class_expr ());
        Cmt_format.add_saved_type (Cmt_format.Partial_class_expr ());
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
        Js_packages_state.with_fresh (fun () ->
            let output = configure "first-package" "commonjs:lib/first:.js" in
            Atomic.set left_ready true;
            wait_for_atomic right_ready;
            output))
  in
  let right =
    Domain.spawn (fun () ->
        Js_packages_state.with_fresh (fun () ->
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
            Btype.with_fresh (fun () ->
                Btype.reinit ();
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
            Btype.with_fresh (fun () ->
                wait_for_atomic left_ready;
                Btype.reinit ();
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

let write root name contents =
  Test_support.write_file (Filename.concat root name) contents

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

let recovery_tests _context =
  Test_support.with_temp_dir "rewatch-driver-recovery-" (fun root ->
      write root "Parse.res" "let value = 1\n";
      let _, initial = run root "Parse.res" in
      expect_code 0 initial;
      write root "Parse.res" "let value =\n";
      let _, failed = run root "Parse.res" in
      expect_code 1 failed;
      check
        (Test_support.contains_text failed.stderr "Syntax error")
        "parse diagnostics are returned to the host";
      write root "Parse.res" "let value = 2\n";
      let _, repaired = run root "Parse.res" in
      expect_code 0 repaired;
      write root "Typed.res" "let value: string = 1\n";
      let _, failed = run root "Typed.res" in
      expect_code 2 failed;
      check
        (Test_support.contains_text failed.stderr "int"
        && Test_support.contains_text failed.stderr "string")
        "type diagnostics are returned to the host";
      write root "Typed.res" {|let value: string = "repaired"|};
      let _, repaired = run root "Typed.res" in
      expect_code 0 repaired)

let ppx_recovery_tests _context =
  Test_support.with_temp_dir "rewatch-driver-ppx-" (fun root ->
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
        (Test_support.contains_text failed.stderr "intentional PPX failure")
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
                   File_util.copy_existing_file ~ensure_parent:true input output;
                   (0, "", "")
                 | _ -> (2, "", "could not decode test PPX command")))
      in
      expect_code 0 repaired;
      check
        (Test_support.contains_text repaired.stderr "+ rewatch-test-ppx")
        "direct verbose output is returned through the request boundary")

let control_flow_and_output_tests _context =
  Test_support.with_temp_dir "rewatch-driver-control-" (fun root ->
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
        (Test_support.contains_text version.stdout "ReScript")
        "version output is captured without terminating the host";
      let help = request ["-help"] "Good.res" in
      expect_code 0 help;
      check
        (Test_support.contains_text help.stdout "Usage: bsc")
        "help output is captured without terminating the host";
      let reprint = request ["-reprint-source"; "Broken.res"] "Good.res" in
      expect_code 1 reprint;
      check
        (Test_support.contains_text reprint.stderr "Syntax error")
        "reprint parse failures return diagnostics instead of exiting";
      let format = request ["-format"; "Broken.res"] "Good.res" in
      expect_code 1 format;
      check
        (Test_support.contains_text format.stderr "Syntax error")
        "format parse failures return diagnostics instead of exiting";
      let stdout_compile =
        request
          ["-nostdlib"; "-nopervasives"; "-bs-no-version-header"]
          "Good.res"
      in
      expect_code 0 stdout_compile;
      check
        (Test_support.contains_text stdout_compile.stdout "value")
        "compiler stdout is captured in the structured result";
      let _, repaired = run root "Good.res" in
      expect_code 0 repaired;
      assert_equal process_cwd (Sys.getcwd ()))

let state_boundary_tests _context =
  Test_support.with_temp_dir "rewatch-driver-state-" (fun root ->
      write root "Warning.res"
        "let rec notActuallyRecursive = () => 42\n\n\
         let _ = notActuallyRecursive()\n";
      let _, failed = run root ~extra:["-warn-error"; "+39"] "Warning.res" in
      expect_code 2 failed;
      let _, warning_only = run root "Warning.res" in
      expect_code 0 warning_only;
      check
        (Test_support.contains_text warning_only.stderr "Warning number 39")
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
        (Test_support.contains_text after_source_flags.stderr
           "Warning number 39")
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

let package_and_load_path_isolation_tests _context =
  Test_support.with_temp_dir "rewatch-driver-load-roots-" (fun root ->
      let first = Filename.concat root "first" in
      let second = Filename.concat root "second" in
      File_util.ensure_dir first;
      File_util.ensure_dir second;
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
  Test_support.with_temp_dir "rewatch-driver-generated-names-" (fun root ->
      write root "First.res" "%%private(let hidden = 1)\nlet value = hidden\n";
      write root "Other.res"
        "%%private(let first = 1)\n%%private(let second = first + 1)\n";
      let parse input =
        let _, result = run root ~extra:["-bs-ast"] input in
        expect_code 0 result;
        let ast =
          File_util.read_file
            (Filename.concat root (Filename.remove_extension input ^ ".ast"))
        in
        check (String.length ast > 0) "binary AST output is nonempty";
        ast
      in
      let first_ast = parse "First.res" in
      ignore (parse "Other.res");
      let repeated_ast = parse "First.res" in
      assert_equal ~printer:(fun _ -> "<binary AST>") first_ast repeated_ast;
      Btype.reinit ();
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
      let cmi_path = Filename.concat root "Objects.cmi" in
      let first_cmi = File_util.read_file cmi_path in
      check (String.length first_cmi > 0) "CMI output is nonempty";
      for _ = 1 to 1000 do
        ignore (Btype.newgenvar ())
      done;
      Sys.remove cmi_path;
      let _, repeated_objects = run root "Objects.res" in
      expect_code 0 repeated_objects;
      let repeated_cmi = File_util.read_file cmi_path in
      assert_equal ~printer:(fun _ -> "<binary CMI>") first_cmi repeated_cmi;
      Btype.reinit ();
      Lambda_exits.reset ())

let interface_namespace_and_load_path_tests _context =
  Test_support.with_temp_dir "rewatch-driver-artifacts-" (fun root ->
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

let combined_dependency_cache_tests _context =
  Test_support.with_temp_dir "rewatch-combined-dependency-" (fun root ->
      let previous_cache = Sys.getenv_opt "REWATCH_COMBINED_SIGNATURE_CACHE" in
      let previous_trace = Sys.getenv_opt "REWATCH_TYPECHECK_TRACE" in
      Fun.protect
        (fun () ->
          Unix.putenv "REWATCH_COMBINED_SIGNATURE_CACHE" "force";
          let trace = Filename.concat root "dependency-trace.tsv" in
          Unix.putenv "REWATCH_TYPECHECK_TRACE" trace;
          let compile ?(extra = []) input =
            let _, result = run root ~extra:(["-I"; root] @ extra) input in
            expect_code 0 result
          in
          let compile_dependency () =
            compile ~extra:["-bs-ns"; "Shapes"] "Circle.resi";
            compile ~extra:["-bs-ns"; "Shapes"; "-bs-read-cmi"] "Circle.res";
            compile ~extra:["-no-alias-deps"] "Shapes.mlmap"
          in
          write root "Shapes.mlmap" "randjbuildsystem\nCircle\n";
          write root "Circle.resi" "let value: int\n";
          write root "Circle.res" "let value = 1\n";
          compile ~extra:["-no-alias-deps"] "Shapes.mlmap";
          compile_dependency ();
          write root "Consumer.res"
            "open Shapes.Circle\nlet result: int = value\n";
          compile "Consumer.res";
          let first_cmi =
            File_util.read_file (Filename.concat root "Consumer.cmi")
          in
          let first_cmt =
            File_util.read_file (Filename.concat root "Consumer.cmt")
          in
          compile "Consumer.res";
          check
            (Test_support.contains_text
               (File_util.read_file trace)
               "dependency.snapshot_restore")
            "a repeated namespace open copies the combined snapshot";
          Unix.putenv "REWATCH_COMBINED_SIGNATURE_CACHE" "0";
          compile "Consumer.res";
          assert_equal
            ~printer:(fun _ -> "<binary CMI>")
            first_cmi
            (File_util.read_file (Filename.concat root "Consumer.cmi"));
          assert_equal
            ~printer:(fun _ -> "<binary CMT>")
            first_cmt
            (File_util.read_file (Filename.concat root "Consumer.cmt"));
          Unix.putenv "REWATCH_COMBINED_SIGNATURE_CACHE" "force";
          let value_path =
            let module_path =
              Path.Pdot
                ( Path.Pident (Ident.create_persistent "Shapes"),
                  "Circle",
                  Path.nopos )
            in
            Path.Pdot (module_path, "value", Path.nopos)
          in
          let with_loaded_type ?(load_path = [root]) action =
            Fun.protect
              (fun () ->
                Compiler_request_state.with_fresh ~cwd:root (fun () ->
                    Env.with_fresh (fun () ->
                        (Compiler_request_state.current ()).load_path <-
                          load_path;
                        action
                          (Env.find_value value_path Env.empty).Types.val_type)))
              ~finally:Env.finalize_expanded_snapshot_cache
          in
          let loaded_type () = with_loaded_type Fun.id in
          let first = loaded_type () in
          let second = loaded_type () in
          check (first != second)
            "cached dependency types belong to each request";
          first.Types.desc <- Types.Tvar (Some "changed");
          check
            (match second.Types.desc with
            | Types.Tvar (Some "changed") -> false
            | _ -> true)
            "mutating one request's dependency graph does not affect another";
          Unix.putenv "REWATCH_COMBINED_SIGNATURE_CACHE" "force_typed";
          compile "Consumer.res";
          compile "Consumer.res";
          let reused_first = loaded_type () in
          let reused_second = loaded_type () in
          check
            (reused_first == reused_second)
            "one compiler domain reuses its verified-clean dependency graph";
          let other_domain_type = Domain.join (Domain.spawn loaded_type) in
          check
            (reused_second != other_domain_type)
            "different compiler domains have separate dependency graphs";
          with_loaded_type (fun ty ->
              ty.Types.desc <- Types.Tvar (Some "changed"));
          let restored = loaded_type () in
          check (restored != reused_first)
            "a changed dependency graph is restored before reuse";
          check
            (match restored.Types.desc with
            | Types.Tvar (Some "changed") -> false
            | _ -> true)
            "the restored dependency graph keeps the original type";
          let extra_load_directory = Filename.concat root "extra-load-path" in
          File_util.ensure_dir extra_load_directory;
          let alternate =
            with_loaded_type ~load_path:[extra_load_directory; root] Fun.id
          in
          check
            (alternate != loaded_type ())
            "a different load path does not reuse the prepared dependency graph";
          write root "Circle.resi" "let value: string\n";
          write root "Circle.res" {|let value = "updated"|};
          compile_dependency ();
          write root "ConsumerString.res"
            "open Shapes.Circle\nlet result: string = value\n";
          compile "ConsumerString.res";
          let _, stale = run root ~extra:["-I"; root] "Consumer.res" in
          expect_code 2 stale;
          let updated_on_another_domain =
            Domain.spawn loaded_type |> Domain.join
          in
          check
            (match updated_on_another_domain.Types.desc with
            | Types.Tconstr (path, _, _) -> Path.name path = "string"
            | _ -> false)
            "a new domain sees the updated dependency interface")
        ~finally:(fun () ->
          (match previous_trace with
          | Some value -> Unix.putenv "REWATCH_TYPECHECK_TRACE" value
          | None -> Unix.unsetenv "REWATCH_TYPECHECK_TRACE");
          match previous_cache with
          | Some value -> Unix.putenv "REWATCH_COMBINED_SIGNATURE_CACHE" value
          | None -> Unix.unsetenv "REWATCH_COMBINED_SIGNATURE_CACHE"))

let runtime_cmi_cache_tests _context =
  Test_support.with_temp_dir "rewatch-runtime-cmi-cache-" (fun root ->
      let first = Filename.concat root "first" in
      let second = Filename.concat root "second" in
      File_util.ensure_dir first;
      File_util.ensure_dir second;
      let install directory kind =
        write directory "Api.resi" ("let value: " ^ kind ^ "\n");
        expect_code 0 (snd (run directory "Api.resi"));
        let cmi = Cmi_format.read_cmi (Filename.concat directory "Api.cmi") in
        ignore
          (Cmi_format.create_cmi
             (Filename.concat directory "Stdlib.cmi")
             {cmi with cmi_name = "Stdlib"; cmi_crcs = []})
      in
      install second "int";
      let path =
        Path.Pdot
          (Path.Pident (Ident.create_persistent "Stdlib"), "value", Path.nopos)
      in
      let load ?(mutate = false) directories =
        Env.with_expanded_snapshot_cache (fun () ->
            Fun.protect
              (fun () ->
                Compiler_request_state.with_fresh ~cwd:root (fun () ->
                    Env.with_fresh (fun () ->
                        (Compiler_request_state.current ()).load_path <-
                          directories;
                        let value = Env.find_value path Env.empty in
                        let type_name =
                          match value.Types.val_type.desc with
                          | Types.Tconstr (type_path, _, _) ->
                            Path.name type_path
                          | _ -> assert_failure "expected a named value type"
                        in
                        if mutate then
                          value.Types.val_type.desc <-
                            Types.Tvar (Some "changed");
                        type_name)))
              ~finally:Env.finalize_expanded_snapshot_cache)
      in
      assert_equal "int" (load [first; second]);
      assert_equal "int" (load [first; second]);
      assert_equal "int" (load ~mutate:true [first; second]);
      assert_equal "int" (load [first; second]);
      assert_equal "int"
        (Domain.spawn (fun () -> load [first; second]) |> Domain.join);
      install first "string";
      assert_equal "string" (load [first; second]);
      install first "int";
      assert_equal "int" (load [first; second]);
      install second "string";
      assert_equal "string" (load [second; first]))

let concurrent_diagnostic_recovery_tests _context =
  Test_support.with_temp_dir "rewatch-driver-errors-" (fun root ->
      let first = Filename.concat root "first" in
      let second = Filename.concat root "second" in
      File_util.ensure_dir first;
      File_util.ensure_dir second;
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
  Test_support.with_temp_dir "rewatch-driver-jsx-errors-" (fun root ->
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
        File_util.ensure_dir directory;
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
            (String_util.contains result.Rescript_compiler_driver.stderr
               (module_name ^ ".string"))
            ("JSX diagnostic names " ^ module_name))
        [(left_name, left_expected); (right_name, right_expected)];
      for _ = 1 to 5 do
        let left = Domain.spawn left_request in
        let right = Domain.spawn right_request in
        assert_equal left_expected.stderr (Domain.join left).stderr;
        assert_equal right_expected.stderr (Domain.join right).stderr
      done)

(* Exercise concurrent compiler requests without the process-worker boundary. *)
let concurrent_request_isolation_tests _context =
  Test_support.with_temp_dir "rewatch-driver-domains-" (fun root ->
      let first = Filename.concat root "first" in
      let second = Filename.concat root "second" in
      File_util.ensure_dir first;
      File_util.ensure_dir second;
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
            File_util.copy_existing_file ~ensure_parent:true input output;
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
        ((Ast_header.read (Filename.concat first "First.ast")).source
        = Some (Filename.concat first "First.res"))
        "-absname retains an absolute source path during concurrent parsing";
      check
        ((Ast_header.read (Filename.concat second "Second.ast")).source
       = Some "Second.res")
        "a concurrent parser without -absname retains its relative source path")

let concurrent_async_context_tests _context =
  Test_support.with_temp_dir "rewatch-driver-async-domains-" (fun root ->
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
        File_util.ensure_dir directory;
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

let tests =
  "compiler_driver_tests"
  >::: [
         "ctype_level_isolation" >:: ctype_level_isolation_tests;
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
         "annotation_isolation" >:: annotation_isolation_tests;
         "backend_module_cache_isolation"
         >:: backend_module_cache_isolation_tests;
         "input_name_isolation" >:: input_name_isolation_tests;
         "request_root_io_isolation" >:: request_root_io_isolation_tests;
         "cmt_accumulator_isolation" >:: cmt_accumulator_isolation_tests;
         "package_output_isolation" >:: package_output_isolation_tests;
         "path_configuration_isolation" >:: path_configuration_isolation_tests;
         "lambda_exit_isolation" >:: lambda_exit_isolation_tests;
         "type_node_id_isolation" >:: type_node_id_isolation_tests;
         "recovery" >:: recovery_tests;
         "ppx_recovery" >:: ppx_recovery_tests;
         "control_flow_and_output" >:: control_flow_and_output_tests;
         "state_boundary" >:: state_boundary_tests;
         "package_and_load_path_isolation"
         >:: package_and_load_path_isolation_tests;
         "generated_name_isolation" >:: generated_name_isolation_tests;
         "interfaces_namespaces_load_paths"
         >:: interface_namespace_and_load_path_tests;
         "combined_dependency_cache" >:: combined_dependency_cache_tests;
         "runtime_cmi_cache" >:: runtime_cmi_cache_tests;
         "concurrent_diagnostic_recovery"
         >:: concurrent_diagnostic_recovery_tests;
         "concurrent_jsx_diagnostic" >:: concurrent_jsx_diagnostic_tests;
         "concurrent_request_isolation" >:: concurrent_request_isolation_tests;
         "concurrent_async_context" >:: concurrent_async_context_tests;
       ]
