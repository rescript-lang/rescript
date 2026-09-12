open OUnit2

let check condition message = assert_bool message condition

let tests =
  "compiler_process_tests" >:: fun _context ->
  Test_support.with_temp_dir "rewatch-compiler-process-" (fun root ->
      let build_dir = Filename.concat root "build" in
      let ocaml_dir = Filename.concat root "ocaml" in
      File_util.ensure_dir build_dir;
      File_util.ensure_dir ocaml_dir;
      List.iter
        (fun extension ->
          Test_support.write_file
            (Filename.concat build_dir ("Ns." ^ extension))
            ("new " ^ extension))
        ["cmi"; "cmj"; "cmt"];
      File_util.ensure_dir (Filename.concat ocaml_dir "Ns.cmj");
      let namespace_task =
        Compiler_process.namespace_task ~bsc:"unused" ~runtime:"unused"
          ~build_dir ~ocaml_dir ~entry:None ~package_dirty:true ~force:false
          "Ns" []
        |> Option.get
      in
      let result =
        Process.{status = Unix.WEXITED 0; stdout = ""; stderr = ""}
      in
      match
        Compiler_scheduler.capture_publication (fun () ->
            namespace_task.Compiler_scheduler.publish result)
      with
      | Compiler_scheduler.Failed_after_cmi_publication
          {cmi_change = Compiler_scheduler.Cmi_changed; _} ->
        assert_equal "new cmi"
          (File_util.read_file (Filename.concat ocaml_dir "Ns.cmi"))
          ~msg:
            "namespace publication must retain the changed CMI before a later \
             artifact copy fails"
      | _ ->
        assert_failure
          "namespace publication must report a changed CMI across a later \
           artifact-copy failure");
  Test_support.with_temp_dir "rewatch-namespace-retry-" (fun root ->
      let build_dir = Filename.concat root "build" in
      let ocaml_dir = Filename.concat root "ocaml" in
      File_util.ensure_dir build_dir;
      File_util.ensure_dir ocaml_dir;
      List.iter
        (fun extension ->
          Test_support.write_file
            (Filename.concat ocaml_dir ("Ns." ^ extension))
            (if extension = "mlmap" then "old map\n" else "old artifact"))
        ["cmi"; "cmj"; "cmt"; "mlmap"];
      check
        (Option.is_some
           (Compiler_process.namespace_task ~bsc:"unused" ~runtime:"unused"
              ~build_dir ~ocaml_dir ~entry:None ~package_dirty:false
              ~force:false "Ns" []))
        "a published namespace map that differs from membership is rebuilt";
      Test_support.write_file
        (Filename.concat ocaml_dir "Ns.mlmap")
        "randjbuildsystem\n";
      check
        (Option.is_some
           (Compiler_process.namespace_task ~bsc:"unused" ~runtime:"unused"
              ~build_dir ~ocaml_dir ~entry:None ~package_dirty:false ~force:true
              "Ns" []))
        "retained namespace publication dirtiness forces a retry")
