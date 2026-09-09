let check condition message = if not condition then failwith message

let write_file path contents =
  Build_artifacts.ensure_dir (Filename.dirname path);
  let channel = open_out_bin path in
  Fun.protect ~finally:(fun () -> close_out_noerr channel) (fun () ->
    output_string channel contents)

let with_temp_dir run =
  let root = Filename.temp_file "rewatch-build-artifacts-" "" in
  Sys.remove root;
  Unix.mkdir root 0o755;
  Fun.protect ~finally:(fun () -> Build_artifacts.remove_tree root) (fun () ->
    run root)

let () =
  with_temp_dir (fun root ->
    let first = Filename.concat root "first" in
    let second = Filename.concat root "nested/second" in
    let missing = Filename.concat root "missing" in
    write_file first "same";
    write_file second "same";
    check
      (Build_artifacts.files_equal first second)
      "equal file contents should compare equal";
    write_file second "different";
    check
      (not (Build_artifacts.files_equal first second))
      "different file contents should not compare equal";
    check
      (not (Build_artifacts.files_equal missing first))
      "a missing file should not compare equal";
    check
      (Option.is_some (Build_artifacts.modification_time first))
      "an existing file should have a modification time";
    check
      (Option.is_none (Build_artifacts.modification_time missing))
      "a missing file should not have a modification time";
    check
      (List.sort String.compare (Build_artifacts.files_under root)
      = List.sort String.compare [first; second])
      "recursive inventory should contain files but not directories";
    if not Sys.win32 then (
      let live_link = Filename.concat root "live-link" in
      let dangling_link = Filename.concat root "dangling-link" in
      Unix.symlink first live_link;
      Unix.symlink missing dangling_link;
      check
        (List.sort String.compare (Build_artifacts.files_under root)
        = List.sort String.compare [first; second; live_link])
        "recursive inventory should retain live links and omit dangling links");
    Build_artifacts.remove_file first;
    Build_artifacts.remove_file first;
    check
      (not (Sys.file_exists first))
      "removing an existing or already-missing file should be idempotent")
