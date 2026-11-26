open OUnit2

let check condition message = assert_bool message condition

let write_file path contents =
  File_util.ensure_dir (Filename.dirname path);
  let channel = open_out_bin path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr channel)
    (fun () -> output_string channel contents)

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
      let second = Test_support.path root "nested/second" in
      let missing = Filename.concat root "missing" in
      write_file first "same";
      write_file second "same";
      check (File_util.exists first) "an existing path should be reported";
      check
        (not (File_util.exists missing))
        "a missing path should be reported as absent";
      (* The watch shutdown protocol and atomic saves remove or rename files
         while the watcher may be reading them. *)
      let held = Filename.concat root "held" in
      let renamed = Filename.concat root "held.renamed" in
      write_file held "held";
      let descriptor = File_util.open_for_reading held in
      Fun.protect
        ~finally:(fun () -> Unix.close descriptor)
        (fun () ->
          Unix.rename held renamed;
          Unix.unlink renamed);
      check
        (not (File_util.exists renamed))
        "a file open for reading can be renamed and removed";
      if not Sys.win32 then (
        let fifo = Filename.concat root "stream" in
        Unix.mkfifo fifo 0o600;
        let writer =
          Thread.create (fun () -> File_util.write_file fifo "streamed") ()
        in
        let contents = File_util.read_file fifo in
        Thread.join writer;
        check (contents = "streamed")
          "whole-file reads consume streams whose initial length is zero";
        File_util.remove_file fifo);
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
      let atomic = Filename.concat root "atomic" in
      write_file atomic "previous";
      if not Sys.win32 then Unix.chmod atomic 0o640;
      File_util.write_file_atomic ~ensure_parent:false atomic "replacement";
      check
        (File_util.read_file atomic = "replacement")
        "atomic writes should publish the complete replacement";
      if not Sys.win32 then
        check
          ((Unix.stat atomic).Unix.st_perm = 0o644)
          "atomic replacement should publish world-readable permissions";
      check
        (Sys.readdir root |> Array.to_list
        |> List.for_all (fun name ->
            not (String.starts_with ~prefix:File_util.atomic_write_prefix name))
        )
        "successful atomic writes should not leave temporary files";
      if (not Sys.win32) && Sys.file_exists "/dev/full" then
        check
          (try
             File_util.write_file "/dev/full" "buffered output";
             false
           with Sys_error _ | Unix.Unix_error _ -> true)
          "a failure while flushing or closing an output file must be reported";
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
