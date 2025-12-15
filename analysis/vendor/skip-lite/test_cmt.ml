(* Test that Marshal_cache can read CMT files *)

[@@@alert "-unsafe"]

let print_cmt_info (cmt : Cmt_format.cmt_infos) =
  Printf.printf "  Module name: %s\n%!" cmt.cmt_modname;
  Printf.printf "  Build dir: %s\n%!" cmt.cmt_builddir;
  (match cmt.cmt_sourcefile with
   | Some sf -> Printf.printf "  Source file: %s\n%!" sf
   | None -> Printf.printf "  Source file: none\n%!")

let test_cmt_file_standard path =
  Printf.printf "Testing with Cmt_format.read_cmt: %s\n%!" path;
  try
    let cmt = Cmt_format.read_cmt path in
    print_cmt_info cmt;
    Printf.printf "  SUCCESS with standard read_cmt\n%!";
    true
  with e ->
    Printf.printf "  FAILED: %s\n%!" (Printexc.to_string e);
    false

let test_cmt_file_cache path =
  Printf.printf "Testing with Marshal_cache: %s\n%!" path;
  try
    Marshal_cache.with_unmarshalled_file path (fun (cmt : Cmt_format.cmt_infos) ->
      print_cmt_info cmt;
      Printf.printf "  SUCCESS with Marshal_cache!\n%!";
      true
    )
  with
  | Marshal_cache.Cache_error (p, msg) ->
    Printf.printf "  Cache_error: %s: %s\n%!" p msg;
    false
  | e ->
    Printf.printf "  FAILED: %s\n%!" (Printexc.to_string e);
    false

let test_cmt_file path =
  if not (Sys.file_exists path) then begin
    Printf.printf "File not found: %s\n%!" path;
    false
  end else begin
    Printf.printf "\n=== Testing: %s ===\n%!" path;
    let std_ok = test_cmt_file_standard path in
    Printf.printf "\n%!";
    let cache_ok = test_cmt_file_cache path in
    std_ok && cache_ok
  end


let () =
  Printf.printf "=== Marshal_cache CMT Test ===\n\n%!";
  
  (* Get CMT files from command line args or find in lib/bs *)
  let cmt_files =
    if Array.length Sys.argv > 1 then
      Array.to_list (Array.sub Sys.argv 1 (Array.length Sys.argv - 1))
    else begin
      (* Find CMT files in lib/bs *)
      let find_cmt_in_dir dir =
        if Sys.file_exists dir && Sys.is_directory dir then begin
          let rec find acc dir =
            Array.fold_left (fun acc name ->
              let path = Filename.concat dir name in
              if Sys.is_directory path then
                find acc path
              else if Filename.check_suffix path ".cmt" then
                path :: acc
              else
                acc
            ) acc (Sys.readdir dir)
          in
          find [] dir
        end else []
      in
      let lib_bs = "lib/bs" in
      let files = find_cmt_in_dir lib_bs in
      Printf.printf "Found %d CMT files in %s\n\n%!" (List.length files) lib_bs;
      files
    end
  in
  
  (* Test first 3 CMT files *)
  let test_files = 
    cmt_files 
    |> List.sort String.compare 
    |> (fun l -> try List.filteri (fun i _ -> i < 3) l with _ -> l)
  in
  
  List.iter (fun path ->
    let _ = test_cmt_file path in
    Printf.printf "\n%!"
  ) test_files;
  
  (* Test if_changed API *)
  Printf.printf "=== Testing with_unmarshalled_if_changed ===\n\n%!";
  Marshal_cache.clear ();  (* Clear cache to start fresh *)
  (match test_files with
   | path :: _ ->
     Printf.printf "First call (should process):\n%!";
     (match Marshal_cache.with_unmarshalled_if_changed path (fun (cmt : Cmt_format.cmt_infos) ->
       Printf.printf "  Processed: %s\n%!" cmt.cmt_modname;
       cmt.cmt_modname
     ) with
     | Some name -> Printf.printf "  Result: Some(%s) - SUCCESS (file was processed)\n%!" name
     | None -> Printf.printf "  Result: None (unexpected - should have processed!)\n%!");
     
     Printf.printf "Second call (should return None - file unchanged):\n%!";
     (match Marshal_cache.with_unmarshalled_if_changed path (fun (cmt : Cmt_format.cmt_infos) ->
       Printf.printf "  Processed: %s\n%!" cmt.cmt_modname;
       cmt.cmt_modname
     ) with
     | Some name -> Printf.printf "  Result: Some(%s) (unexpected - file should be cached!)\n%!" name
     | None -> Printf.printf "  Result: None - SUCCESS (file was cached!)\n%!")
   | [] -> Printf.printf "No CMT files to test\n%!");
  
  Printf.printf "\n=== Test Complete ===\n%!"

