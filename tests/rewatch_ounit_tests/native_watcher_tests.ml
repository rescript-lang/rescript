open OUnit2

let check condition message = assert_bool message condition

let temporary_directory () =
  let path = Filename.temp_file "rewatch-native-watcher-" "" in
  Sys.remove path;
  Unix.mkdir path 0o700;
  Unix.realpath path

let descriptor_count () =
  try Some (Array.length (Sys.readdir "/proc/self/fd"))
  with Sys_error _ -> None

let tests =
  "native_watcher_tests" >:: fun _context ->
  let root = temporary_directory () in
  let source = Filename.concat root "src" in
  let nested = Filename.concat source "nested" in
  let replaced_nested = root ^ "-replaced-nested" in
  let source_lib = Filename.concat source "lib" in
  let artifact_lib = Filename.concat root "lib" in
  let artifact_bs = Filename.concat artifact_lib "bs" in
  Fun.protect
    (fun () ->
      Unix.mkdir source 0o700;
      Unix.mkdir source_lib 0o700;
      Unix.mkdir artifact_lib 0o700;
      Unix.mkdir artifact_bs 0o700;
      let paths = [Native_watcher.{directory = root; recursive = true}] in
      let identity_failure _ = Error "injected identity failure" in
      (match
         Native_watcher.For_test.create_with_directory_identity
           ~directory_identity:identity_failure ~paths
       with
      | Error "injected identity failure" -> ()
      | Error message ->
        assert_failure ("unexpected identity failure: " ^ message)
      | Ok watcher ->
        Native_watcher.close watcher;
        assert_failure "identity failure did not reject watcher creation");
      let descriptors_before_exception = descriptor_count () in
      for _ = 1 to 16 do
        match
          Native_watcher.For_test.create_with_directory_identity
            ~directory_identity:(fun _ -> raise Exit)
            ~paths
        with
        | Error message when Test_support.contains_text message "Exit" -> ()
        | Error message ->
          assert_failure ("unexpected identity exception: " ^ message)
        | Ok watcher ->
          Native_watcher.close watcher;
          assert_failure "identity exception did not reject watcher creation"
      done;
      (match (descriptors_before_exception, descriptor_count ()) with
      | Some before, Some after ->
        check (before = after)
          "exceptional watcher creation should release acquired descriptors"
      | None, _ | _, None -> ());
      match Native_watcher.create ~paths with
      | Error message -> failwith ("native watcher initialization: " ^ message)
      | Ok watcher ->
        Fun.protect
          (fun () ->
            check
              (Native_watcher.For_test.handle_count watcher = 4)
              "valid lib source directories are watched but lib/bs is not";
            Native_watcher.For_test.queue_change watcher;
            Native_watcher.For_test.queue_error watcher "injected failure";
            (match
               Native_watcher.wait watcher ~keep_running:(fun () -> true)
             with
            | Native_watcher.Failed "injected failure" -> ()
            | Native_watcher.Changed _ | Native_watcher.Stopped
            | Native_watcher.Failed _ ->
              assert_failure "a native failure takes precedence over a change");
            (match Native_watcher.refresh watcher ~paths with
            | Error message -> failwith ("native watcher refresh: " ^ message)
            | Ok () -> ());
            (match
               Native_watcher.wait watcher ~keep_running:(fun () -> true)
             with
            | Native_watcher.Changed [_] -> ()
            | Native_watcher.Changed _ ->
              assert_failure "refresh preserves exactly one queued change"
            | Native_watcher.Stopped | Native_watcher.Failed _ ->
              assert_failure "refresh preserves a previously queued change");
            Native_watcher.For_test.queue_change watcher;
            (match
               Native_watcher.wait watcher ~keep_running:(fun () -> false)
             with
            | Native_watcher.Stopped -> ()
            | Native_watcher.Changed _ | Native_watcher.Failed _ ->
              assert_failure "an external stop takes precedence over a change");
            (match
               Native_watcher.wait watcher ~keep_running:(fun () -> true)
             with
            | Native_watcher.Changed [_] -> ()
            | Native_watcher.Changed _ ->
              assert_failure
                "a stopped wait preserves exactly one queued change"
            | Native_watcher.Stopped | Native_watcher.Failed _ ->
              assert_failure "a stopped wait preserves its queued change");
            check
              (Native_watcher.For_test.handle_count watcher = 4)
              "unchanged refresh retains handle count";
            (match
               Native_watcher.For_test.refresh_with_directory_identity
                 ~directory_identity:identity_failure watcher ~paths
             with
            | Error "injected identity failure" -> ()
            | Error message ->
              assert_failure ("unexpected refresh failure: " ^ message)
            | Ok () ->
              assert_failure "identity failure did not reject watcher refresh");
            check
              (Native_watcher.For_test.handle_count watcher = 4)
              "failed identity refresh preserves existing handles";
            Unix.mkdir nested 0o700;
            (match Native_watcher.refresh watcher ~paths with
            | Error message -> failwith ("native watcher add: " ^ message)
            | Ok () -> ());
            check
              (Native_watcher.For_test.handle_count watcher = 5)
              "new directory adds one handle";
            let original_identity =
              Native_watcher.For_test.directory_identity watcher nested
            in
            Unix.rename nested replaced_nested;
            Unix.mkdir nested 0o700;
            (match Native_watcher.refresh watcher ~paths with
            | Error message ->
              failwith ("native watcher replacement: " ^ message)
            | Ok () -> ());
            check
              (Native_watcher.For_test.handle_count watcher = 5)
              "directory replacement preserves the desired handle count";
            check
              (Native_watcher.For_test.directory_identity watcher nested
              <> original_identity)
              "directory replacement installs a handle for the new identity";
            Unix.rmdir nested;
            (match Native_watcher.refresh watcher ~paths with
            | Error message -> failwith ("native watcher remove: " ^ message)
            | Ok () -> ());
            check
              (Native_watcher.For_test.handle_count watcher = 4)
              "removed directory closes one handle")
          ~finally:(fun () -> Native_watcher.close watcher))
    ~finally:(fun () ->
      (try Unix.rmdir nested with Unix.Unix_error _ -> ());
      (try Unix.rmdir replaced_nested with Unix.Unix_error _ -> ());
      (try Unix.rmdir artifact_bs with Unix.Unix_error _ -> ());
      (try Unix.rmdir artifact_lib with Unix.Unix_error _ -> ());
      (try Unix.rmdir source_lib with Unix.Unix_error _ -> ());
      (try Unix.rmdir source with Unix.Unix_error _ -> ());
      try Unix.rmdir root with Unix.Unix_error _ -> ())
