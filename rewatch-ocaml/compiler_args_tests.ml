let check condition message = if not condition then failwith message

let write_file path contents =
  Build_artifacts.ensure_dir (Filename.dirname path);
  let channel = open_out_bin path in
  Fun.protect ~finally:(fun () -> close_out_noerr channel) (fun () ->
    output_string channel contents)

let compiler_args path =
  match Yojson.Safe.from_string (Build.compiler_args path) with
  | `Assoc fields -> (
    match List.assoc_opt "compiler_args" fields with
    | Some (`List values) ->
      List.map
        (function `String value -> value | _ -> failwith "non-string argument")
        values
    | _ -> failwith "missing compiler_args array")
  | _ -> failwith "compiler-args did not return an object"

let adjacent_positions flag values =
  let rec loop index positions = function
    | current :: value :: rest when current = flag ->
      loop (index + 2) ((value, index) :: positions) rest
    | _ :: rest -> loop (index + 1) positions rest
    | [] -> List.rev positions
  in
  loop 0 [] values

let position value pairs = List.assoc_opt value pairs

let () =
  let root = Filename.temp_file "rewatch-ocaml-compiler-args-" "" in
  Sys.remove root;
  Unix.mkdir root 0o755;
  Fun.protect
    ~finally:(fun () -> Build_artifacts.remove_tree root)
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
          Build_artifacts.ensure_dir
            (Build_artifacts.path_of_parts root ["node_modules"; package]))
        ["@rescript/runtime"; "regular"; "development"];
      write_file (Filename.concat root "rescript.json")
        {|{
          "name": "compiler-args-test",
          "sources": ["src", {"dir": "dev", "type": "dev", "subdirs": true}],
          "dependencies": ["regular"],
          "dev-dependencies": ["development"]
        }|};
      let regular =
        Build_artifacts.path_of_parts root
          ["node_modules"; "regular"; "lib"; "ocaml"]
      in
      let development =
        Build_artifacts.path_of_parts root
          ["node_modules"; "development"; "lib"; "ocaml"]
      in
      let ordinary_includes = compiler_args source |> adjacent_positions "-I" in
      check (Option.is_some (position regular ordinary_includes))
        "regular dependency includes do not require a prebuilt lib/ocaml";
      check (Option.is_none (position development ordinary_includes))
        "ordinary sources exclude development dependencies";
      let dev_includes = compiler_args dev_source |> adjacent_positions "-I" in
      check
        (match (position development dev_includes, position regular dev_includes) with
        | Some development_index, Some regular_index ->
          development_index < regular_index
        | _ -> false)
        "development sources include dev dependencies before regular dependencies";
      check
        (compiler_args nested_dev_source |> adjacent_positions "-I"
        |> position development |> Option.is_some)
        "recursive development sources include dev dependencies";
      check
        (compiler_args prefixed_source |> adjacent_positions "-I"
        |> position development |> Option.is_none)
        "source directory matching respects path-component boundaries";
      Build_artifacts.remove_tree
        (Build_artifacts.path_of_parts root ["node_modules"; "development"]);
      check
        (compiler_args dev_source |> adjacent_positions "-I"
        |> position development |> Option.is_none)
        "missing development dependencies are omitted like Rust";
      Build_artifacts.remove_tree
        (Build_artifacts.path_of_parts root ["node_modules"; "regular"]);
      check
        (try
           ignore (compiler_args source);
           false
         with Build.Error message ->
           Build.contains_text message
             "Expected to find dependent package regular of compiler-args-test")
        "missing regular dependencies produce a contextual error";
      let missing_source = Filename.concat root "src/Missing.res" in
      check
        (try
           ignore (Build.compiler_args missing_source);
           false
         with Build.Error message ->
           Build.contains_text message "Could not read source file"
           && Build.contains_text message missing_source)
        "missing compiler-args sources produce a contextual error")
