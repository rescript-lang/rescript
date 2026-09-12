open OUnit2

let check condition message = assert_bool message condition

let write_file = Test_support.write_file

let arguments field path =
  match Yojson.Safe.from_string (Compiler_args_command.run path) with
  | `Assoc fields -> (
    match List.assoc_opt field fields with
    | Some (`List values) ->
      List.map
        (function
          | `String value -> value
          | _ -> failwith "non-string argument")
        values
    | _ -> failwith ("missing " ^ field ^ " array"))
  | _ -> failwith "compiler-args did not return an object"

let compiler_args = arguments "compiler_args"
let parser_args = arguments "parser_args"

let adjacent_positions flag values =
  let rec loop index positions = function
    | current :: value :: rest when current = flag ->
      loop (index + 2) ((value, index) :: positions) rest
    | _ :: rest -> loop (index + 1) positions rest
    | [] -> List.rev positions
  in
  loop 0 [] values

let position value pairs = List.assoc_opt value pairs

let tests =
  "compiler_args_tests" >:: fun _context ->
  let root = Filename.temp_file "rewatch-ocaml-compiler-args-" "" in
  Sys.remove root;
  Unix.mkdir root 0o755;
  let root = Unix.realpath root in
  Fun.protect
    ~finally:(fun () -> File_util.remove_tree root)
    (fun () ->
      let source = Filename.concat root "src/A.res" in
      let dev_source = Filename.concat root "dev/D.res" in
      let nested_dev_source = Filename.concat root "dev/nested/E.res" in
      let prefixed_source = Filename.concat root "developer/F.res" in
      write_file source "let value = 1\n";
      write_file dev_source "let value = 2\n";
      write_file nested_dev_source "let value = 3\n";
      write_file prefixed_source "let value = 4\n";
      List.iter
        (fun package ->
          File_util.ensure_dir
            (File_util.path_of_parts root ["node_modules"; package]))
        ["@rescript/runtime"; "regular"; "development"];
      write_file
        (Filename.concat root "rescript.json")
        {|{
          "name": "compiler-args-test",
          "sources": ["src", {"dir": "dev", "type": "dev", "subdirs": true}],
          "dependencies": ["regular"],
          "dev-dependencies": ["development"]
        }|};
      let regular =
        File_util.path_of_parts root ["node_modules"; "regular"; "lib"; "ocaml"]
      in
      let development =
        File_util.path_of_parts root
          ["node_modules"; "development"; "lib"; "ocaml"]
      in
      let ordinary_includes = compiler_args source |> adjacent_positions "-I" in
      check
        (Option.is_some (position regular ordinary_includes))
        "regular dependency includes do not require a prebuilt lib/ocaml";
      check
        (Option.is_none (position development ordinary_includes))
        "ordinary sources exclude development dependencies";
      let dev_includes = compiler_args dev_source |> adjacent_positions "-I" in
      check
        (match
           (position development dev_includes, position regular dev_includes)
         with
        | Some development_index, Some regular_index ->
          development_index < regular_index
        | _ -> false)
        "development sources include dev dependencies before regular \
         dependencies";
      check
        (compiler_args nested_dev_source
        |> adjacent_positions "-I" |> position development |> Option.is_some)
        "recursive development sources include dev dependencies";
      check
        (compiler_args prefixed_source
        |> adjacent_positions "-I" |> position development |> Option.is_none)
        "source directory matching respects path-component boundaries";
      let config = Config.load_root root in
      let gentype_config = {config with gentype_args = ["-bs-gentype"]} in
      let selected_regular = Filename.concat root "selected/regular" in
      check
        (Compiler_args.gentype_dependency_args_from_paths gentype_config
           [({Config.name = "regular"; features = None}, selected_regular)]
        = ["-bs-gentype-dep-path"; "regular=" ^ selected_regular])
        "GenType arguments reuse the dependency path selected for the graph";
      (if not Sys.win32 then
         let external_source =
           Filename.temp_file "rewatch-linked-source-" ".res"
         in
         Fun.protect
           ~finally:(fun () -> Sys.remove external_source)
           (fun () ->
             write_file external_source "let linked = 1\n";
             let linked_source = Filename.concat root "src/Linked.res" in
             Unix.symlink external_source linked_source;
             check
               (parser_args linked_source |> List.rev |> List.hd
               = Filename.concat
                   (Filename.concat Filename.parent_dir_name
                      Filename.parent_dir_name)
                   "src/Linked.res")
               "compiler-args keeps a source symlink in its project scope"));
      File_util.remove_tree
        (File_util.path_of_parts root ["node_modules"; "development"]);
      check
        (compiler_args dev_source |> adjacent_positions "-I"
       |> position development |> Option.is_none)
        "missing development dependencies are omitted like Rust";
      File_util.remove_tree
        (File_util.path_of_parts root ["node_modules"; "regular"]);
      let previous_runtime = Sys.getenv_opt "RESCRIPT_RUNTIME" in
      Unix.putenv "RESCRIPT_RUNTIME" (Filename.concat root "missing-runtime");
      let missing_dependency_error =
        Fun.protect
          ~finally:(fun () ->
            match previous_runtime with
            | Some value -> Unix.putenv "RESCRIPT_RUNTIME" value
            | None -> Test_support.unsetenv "RESCRIPT_RUNTIME")
          (fun () ->
            try
              ignore (compiler_args source);
              None
            with Project_context.Error message -> Some message)
      in
      check
        (match missing_dependency_error with
        | Some message ->
          Test_support.contains_text message
            "Expected to find dependent package regular of compiler-args-test"
        | None -> false)
        "dependency resolution precedes runtime resolution";
      let missing_source = Filename.concat root "src/Missing.res" in
      check
        (try
           ignore (Compiler_args_command.run missing_source);
           false
         with Project_context.Error message ->
           Test_support.contains_text message "Could not read source file"
           && Test_support.contains_text message missing_source)
        "missing compiler-args sources produce a contextual error");
  Test_support.with_temp_dir "rewatch-compiler-args-workspace-" (fun outer ->
      let workspace = Filename.concat outer "workspace" in
      let package = Filename.concat workspace "packages/app" in
      let source = Filename.concat package "src/App.res" in
      let outside_dependency = Filename.concat outer "node_modules/outside" in
      write_file source "let value = 1\n";
      write_file
        (Filename.concat workspace "rescript.json")
        {|{"name":"workspace","sources":[],"dependencies":["app"]}|};
      write_file
        (Filename.concat package "rescript.json")
        {|{"name":"app","sources":"src","dependencies":["outside"]}|};
      File_util.ensure_dir (Filename.concat workspace "node_modules");
      if
        Test_support.symlink_if_supported
          (Test_support.path workspace "../workspace/packages/app")
          (Test_support.path workspace "node_modules/app")
      then (
        File_util.ensure_dir outside_dependency;
        check
          (try
             ignore (Compiler_args_command.run source);
             false
           with Project_context.Error message ->
             Test_support.contains_text message
               "Expected to find dependent package outside of app")
          "compiler-args does not search above a workspace dependency boundary"))
