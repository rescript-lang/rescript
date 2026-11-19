open OUnit

let with_interfaces action =
  Ounit_find_in_path_tests.with_temp_dir (fun root ->
      Compiler_request_state.with_fresh ~cwd:root (fun () ->
          Env.reset_cache ();
          Config.set_load_path [root];
          let write flags =
            Cmi_format.create_cmi
              (Filename.concat root "Hidden.cmi")
              {
                Cmi_format.cmi_name = "Hidden";
                cmi_sign = [];
                cmi_crcs = [];
                cmi_flags = flags;
              }
          in
          ignore (write [Cmi_format.Internal "provider"]);
          let state = Clflags.current () in
          let owner = !(state.project_owner) in
          let transparent = !(state.transparent_modules) in
          Fun.protect
            ~finally:(fun () ->
              state.project_owner := owner;
              state.transparent_modules := transparent;
              Env.reset_cache ())
            (fun () -> action write)))

let lookup name env = Env.lookup_module ~load:true (Longident.Lident name) env

let rejects name action =
  match action () with
  | _ -> assert_failure "an external project accessed an internal unit"
  | exception Env.Error (Env.Internal_module (_, actual)) ->
    assert_equal name actual

let access_tests _ =
  with_interfaces (fun _write ->
      (Clflags.current ()).project_owner := Some "provider";
      ignore (lookup "Hidden" Env.empty);
      (* Check the already-loaded interface again with a foreign owner. *)
      (Clflags.current ()).project_owner := Some "consumer";
      rejects "Hidden" (fun () -> lookup "Hidden" Env.empty);
      (Clflags.current ()).project_owner := None;
      rejects "Hidden" (fun () -> lookup "Hidden" Env.empty);
      ignore
        (Env.find_module
           (Path.Pident (Ident.create_persistent "Hidden"))
           Env.empty))

let alias_tests _ =
  with_interfaces (fun _write ->
      (Clflags.current ()).project_owner := Some "consumer";
      let _, env =
        Env.enter_module "Alias"
          (Types.Mty_alias
             (Mta_present, Path.Pident (Ident.create_persistent "Hidden")))
          Env.empty
      in
      rejects "Alias" (fun () -> lookup "Alias" env))

let transparent_alias_tests _ =
  with_interfaces (fun _write ->
      (Clflags.current ()).project_owner := Some "provider";
      (Clflags.current ()).transparent_modules := true;
      ignore
        (Env.lookup_module ~load:false (Longident.Lident "Hidden") Env.empty);
      assert_equal [("Hidden", None)] (Env.imports ());
      (Clflags.current ()).project_owner := Some "consumer";
      rejects "Hidden" (fun () ->
          Env.lookup_module ~load:false (Longident.Lident "Hidden") Env.empty))

let crc_tests _ =
  with_interfaces (fun write ->
      let public = write [] in
      let provider = write [Cmi_format.Internal "provider"] in
      let other = write [Cmi_format.Internal "other"] in
      assert_bool "visibility changes invalidate interface readers"
        (public <> provider);
      assert_bool "ownership changes invalidate interface readers"
        (provider <> other))

let suites =
  __FILE__
  >::: [
         "access" >:: access_tests;
         "alias" >:: alias_tests;
         "visibility_crc" >:: crc_tests;
         "transparent_alias" >:: transparent_alias_tests;
       ]
