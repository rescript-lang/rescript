open OUnit

(* Load-path lookups accept a module's file under its name as written and
   uncapitalized. They return the name as written when it exists, because on a
   case-insensitive file system both spellings name the same file and the
   result shows up in error messages and import paths. *)

let rec remove_tree path =
  match (Unix.lstat path).st_kind with
  | Unix.S_DIR ->
    Array.iter
      (fun name -> remove_tree (Filename.concat path name))
      (Sys.readdir path);
    Unix.rmdir path
  | _ -> Sys.remove path
  | exception Unix.Unix_error (Unix.ENOENT, _, _) -> ()

let with_temp_dir action =
  let path = Filename.temp_file "rescript-find-in-path-" "" in
  Sys.remove path;
  Unix.mkdir path 0o755;
  let path = Unix.realpath path in
  Fun.protect ~finally:(fun () -> remove_tree path) (fun () -> action path)

let write_cmi path =
  ignore
    (Cmi_format.create_cmi path
       {
         Cmi_format.cmi_name = "Abc";
         cmi_sign = [];
         cmi_crcs = [];
         cmi_flags = [];
       }
      : Digest.t)

(* [files] are created in a temporary directory, which is the load path while
   [check] runs with that directory. *)
let in_load_path files check =
  with_temp_dir (fun root ->
      List.iter (fun name -> write_cmi (Filename.concat root name)) files;
      Compiler_request_state.with_fresh ~cwd:root (fun () ->
          Config.set_load_path [root];
          check root))

let printer = Fun.id

let lookups =
  [
    ( "misc",
      fun name ->
        Misc.find_in_path_uncap (Config.get_load_path ()) (name ^ ".cmi") );
    ( "config_util",
      fun name -> Option.get (Config_util.find_opt (name ^ ".cmi")) );
    ( "bs_cmi_load",
      fun name ->
        (Option.get (Bs_cmi_load.load_cmi ~unit_name:name))
          .Env.Persistent_signature.filename );
  ]

let as_written_first_tests _ =
  List.iter
    (fun (lookup_name, lookup) ->
      in_load_path ["Abc.cmi"; "abc.cmi"] (fun root ->
          assert_equal ~printer ~msg:lookup_name
            (Filename.concat root "Abc.cmi")
            (lookup "Abc"));
      in_load_path ["Abc.cmi"] (fun root ->
          assert_equal ~printer ~msg:lookup_name
            (Filename.concat root "Abc.cmi")
            (lookup "Abc")))
    lookups

(* Only a case-sensitive file system tells the two spellings apart. *)
let uncapitalized_fallback_tests _ =
  List.iter
    (fun (lookup_name, lookup) ->
      in_load_path ["abc.cmi"] (fun root ->
          let found = lookup "Abc" in
          if not (Sys.file_exists (Filename.concat root "Abc.cmi")) then
            assert_equal ~printer ~msg:lookup_name
              (Filename.concat root "abc.cmi")
              found))
    lookups

let suites =
  __FILE__
  >::: [
         "as_written_first" >:: as_written_first_tests;
         "uncapitalized_fallback" >:: uncapitalized_fallback_tests;
       ]
