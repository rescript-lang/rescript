let check condition message = if not condition then failwith message

let temporary_directory () =
  let path = Filename.temp_file "rewatch-native-watcher-" "" in
  Sys.remove path;
  Unix.mkdir path 0o700;
  path

let () =
  let root = temporary_directory () in
  let source = Filename.concat root "src" in
  let nested = Filename.concat source "nested" in
  Fun.protect
    (fun () ->
      Unix.mkdir source 0o700;
      let paths = [Native_watcher.{directory = root; recursive = true}] in
      match Native_watcher.create ~paths with
      | Error message -> failwith ("native watcher initialization: " ^ message)
      | Ok watcher ->
        Fun.protect
          (fun () ->
            check
              (Native_watcher.For_test.handle_count watcher = 2)
              "root and source handles";
            (match Native_watcher.refresh watcher ~paths with
            | Error message -> failwith ("native watcher refresh: " ^ message)
            | Ok () -> ());
            check
              (Native_watcher.For_test.handle_count watcher = 2)
              "unchanged refresh retains handle count";
            Unix.mkdir nested 0o700;
            (match Native_watcher.refresh watcher ~paths with
            | Error message -> failwith ("native watcher add: " ^ message)
            | Ok () -> ());
            check
              (Native_watcher.For_test.handle_count watcher = 3)
              "new directory adds one handle";
            Unix.rmdir nested;
            (match Native_watcher.refresh watcher ~paths with
            | Error message -> failwith ("native watcher remove: " ^ message)
            | Ok () -> ());
            check
              (Native_watcher.For_test.handle_count watcher = 2)
              "removed directory closes one handle")
          ~finally:(fun () -> Native_watcher.close watcher))
    ~finally:(fun () ->
      (try Unix.rmdir nested with Unix.Unix_error _ -> ());
      (try Unix.rmdir source with Unix.Unix_error _ -> ());
      try Unix.rmdir root with Unix.Unix_error _ -> ())
