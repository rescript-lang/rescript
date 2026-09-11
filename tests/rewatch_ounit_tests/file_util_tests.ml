open OUnit2

let check condition message = assert_bool message condition

let write_file path contents =
  File_util.ensure_dir (Filename.dirname path);
  let channel = open_out_bin path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr channel)
    (fun () -> output_string channel contents)

let descriptor_count () =
  try Some (Array.length (Sys.readdir "/proc/self/fd"))
  with Sys_error _ -> None

let check_descriptor_count before message =
  match (before, descriptor_count ()) with
  | Some before, Some after -> check (before = after) message
  | None, _ | _, None -> ()

let tests =
  "file_util_tests" >:: fun _context ->
  let root = Filename.temp_file "rewatch-file-util-" "" in
  Sys.remove root;
  Unix.mkdir root 0o755;
  let root = Unix.realpath root in
  Fun.protect
    ~finally:(fun () -> File_util.remove_tree root)
    (fun () ->
      let first = Filename.concat root "first" in
      let second = Filename.concat root "nested/second" in
      let missing = Filename.concat root "missing" in
      write_file first "same";
      write_file second "same";
      check
        (File_util.files_equal first second)
        "equal file contents should compare equal";
      write_file second "different";
      check
        (not (File_util.files_equal first second))
        "different file contents should not compare equal";
      check
        (not (File_util.files_equal missing first))
        "a missing file should not compare equal";
      let large_source = Filename.concat root "large-source" in
      let large_copy = Filename.concat root "large-copy" in
      let large_contents =
        String.init 150_000 (fun index -> Char.chr (index mod 251))
      in
      write_file large_source large_contents;
      File_util.copy_existing_file large_source large_copy;
      check
        (File_util.files_equal large_source large_copy)
        "streaming copies preserve files spanning multiple buffer reads";
      let changed_contents = Bytes.of_string large_contents in
      Bytes.set changed_contents (Bytes.length changed_contents - 1) 'x';
      write_file large_copy (Bytes.to_string changed_contents);
      check
        (not (File_util.files_equal large_source large_copy))
        "chunk comparison detects a difference after the first buffer";
      File_util.remove_file large_source;
      File_util.remove_file large_copy;
      let concurrent_root = Filename.concat root "concurrent" in
      let start = Atomic.make false in
      let failures = ref [] in
      let failures_lock = Mutex.create () in
      let workers =
        Array.init 16 (fun _ ->
            Thread.create
              (fun () ->
                while not (Atomic.get start) do
                  Thread.yield ()
                done;
                for index = 0 to 63 do
                  Thread.yield ();
                  let path =
                    Filename.concat concurrent_root (string_of_int index)
                  in
                  try File_util.ensure_dir path
                  with error ->
                    Mutex.lock failures_lock;
                    failures := error :: !failures;
                    Mutex.unlock failures_lock
                done)
              ())
      in
      Atomic.set start true;
      Array.iter Thread.join workers;
      check (!failures = [])
        "concurrent directory creation should tolerate another creator";
      check
        (Sys.is_directory (Filename.concat concurrent_root "63"))
        "concurrent directory creation should leave the requested tree";
      let existing_file_is_rejected =
        try
          File_util.ensure_dir first;
          false
        with Unix.Unix_error (Unix.EEXIST, _, _) -> true
      in
      check existing_file_is_rejected
        "directory creation should reject an existing non-directory";
      check
        (Option.is_some (File_util.modification_time first))
        "an existing file should have a modification time";
      check
        (Option.is_none (File_util.modification_time missing))
        "a missing file should not have a modification time";
      let optional_copy = Filename.concat root "optional-copy" in
      File_util.copy_optional_existing_file first optional_copy;
      check
        (File_util.read_file optional_copy = "same")
        "an available optional artifact should be copied";
      File_util.copy_optional_existing_file missing optional_copy;
      check
        (not (Sys.file_exists optional_copy))
        "a stale optional destination should be removed when its source is \
         absent";
      let missing_destination = Filename.concat root "absent/optional-copy" in
      let descriptors_before_failed_copies = descriptor_count () in
      for _ = 1 to 32 do
        let destination_failure_is_reported =
          try
            File_util.copy_optional_existing_file ~ensure_parent:false first
              missing_destination;
            false
          with Sys_error _ | Unix.Unix_error _ -> true
        in
        check destination_failure_is_reported
          "an optional copy must not hide destination failures"
      done;
      check_descriptor_count descriptors_before_failed_copies
        "failed destination acquisition should close the source";
      let atomic = Filename.concat root "atomic" in
      write_file atomic "previous";
      if not Sys.win32 then Unix.chmod atomic 0o640;
      File_util.write_file_atomic ~ensure_parent:false atomic "replacement";
      check
        (File_util.read_file atomic = "replacement")
        "atomic writes should publish the complete replacement";
      if not Sys.win32 then
        check
          ((Unix.stat atomic).Unix.st_perm = 0o640)
          "atomic replacement should preserve existing file permissions";
      check
        (Sys.readdir root |> Array.to_list
        |> List.for_all (fun name ->
            not (String.starts_with ~prefix:".rewatch-write-" name)))
        "successful atomic writes should not leave temporary files";
      if (not Sys.win32) && Sys.file_exists "/dev/full" then
        check
          (try
             File_util.write_file "/dev/full" "buffered output";
             false
           with Sys_error _ | Unix.Unix_error _ -> true)
          "a failure while flushing or closing an output file must be reported";
      Sys.remove first;
      write_file first "same";
      if not Sys.win32 then (
        let unreadable = Filename.concat root "unreadable" in
        write_file unreadable "same";
        Unix.chmod unreadable 0o000;
        Fun.protect
          ~finally:(fun () -> Unix.chmod unreadable 0o600)
          (fun () ->
            let second_open_fails =
              try
                let channel = open_in_bin unreadable in
                close_in channel;
                false
              with Sys_error _ -> true
            in
            if second_open_fails then (
              let descriptors_before_comparisons = descriptor_count () in
              for _ = 1 to 32 do
                let comparison_failure_is_reported =
                  try
                    ignore (File_util.files_equal first unreadable);
                    false
                  with Sys_error _ | Unix.Unix_error _ -> true
                in
                check comparison_failure_is_reported
                  "a comparison must not hide input failures"
              done;
              check_descriptor_count descriptors_before_comparisons
                "failed second-file acquisition should close the first file"));
        File_util.remove_file unreadable);
      check
        (List.sort String.compare (File_util.files_under root)
        = List.sort String.compare [atomic; first; second])
        "recursive inventory should contain files but not directories";
      if not Sys.win32 then (
        let live_link = Filename.concat root "live-link" in
        let dangling_link = Filename.concat root "dangling-link" in
        Unix.symlink first live_link;
        Unix.symlink missing dangling_link;
        check
          (List.sort String.compare (File_util.files_under root)
          = List.sort String.compare [atomic; first; second; live_link])
          "recursive inventory should retain live links and omit dangling links");
      File_util.remove_file first;
      File_util.remove_file first;
      check
        (not (Sys.file_exists first))
        "removing an existing or already-missing file should be idempotent")
