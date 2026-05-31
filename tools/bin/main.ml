let doc_help =
  {|ReScript Tools

Output documentation to standard output

Usage: rescript-tools doc <FILE>

Example: rescript-tools doc ./path/to/EntryPointLib.res|}

let format_codeblocks_help =
  {|ReScript Tools

Format ReScript code blocks in docstrings or markdown files

Usage: rescript-tools format-codeblocks <FILE> [--stdout] [--transform-assert-equal]

Example: rescript-tools format-codeblocks ./path/to/MyModule.res|}

let extract_codeblocks_help =
  {|ReScript Tools

Extract ReScript code blocks from docstrings or markdown files

Usage: rescript-tools extract-codeblocks <FILE> [--transform-assert-equal]

Example: rescript-tools extract-codeblocks ./path/to/MyModule.res|}

let help =
  {|ReScript Tools

Usage: rescript-tools [command]

Commands:

migrate <file> [--stdout]               Runs the migration tool on the given file
migrate-all <root>                      Runs migrations for all project sources under <root>
doc <file>                              Generate documentation
format-codeblocks <file>                Format ReScript code blocks
  [--stdout]                              Output to stdout
  [--transform-assert-equal]              Transform `assertEqual` to `==`
extract-codeblocks <file>               Extract ReScript code blocks from file
  [--transform-assert-equal]              Transform `==` to `assertEqual`
reanalyze                               Reanalyze
reanalyze-server                        Start reanalyze server
-v, --version                           Print version
-h, --help                              Print help|}

let log_and_exit = function
  | Ok log ->
    Printf.printf "%s\n" log;
    exit 0
  | Error log ->
    Printf.eprintf "%s\n" log;
    exit 1

let version = Version.version

let main () =
  match Sys.argv |> Array.to_list |> List.tl with
  | "doc" :: rest -> (
    match rest with
    | ["-h"] | ["--help"] -> log_and_exit (Ok doc_help)
    | [path] ->
      (* NOTE: Internal use to generate docs from compiler *)
      let () =
        match Sys.getenv_opt "FROM_COMPILER" with
        | Some "true" -> Analysis.Cfg.is_doc_gen_from_compiler := true
        | _ -> ()
      in
      log_and_exit (Tools.extract_docs ~entry_point_file:path ~debug:false)
    | _ -> log_and_exit (Error doc_help))
  | "migrate" :: file :: opts -> (
    let is_stdout = List.mem "--stdout" opts in
    let output_mode = if is_stdout then `Stdout else `File in
    match
      (Tools.Migrate.migrate ~entry_point_file:file ~output_mode, output_mode)
    with
    | Ok content, `Stdout -> print_endline content
    | result, `File -> log_and_exit result
    | Error e, _ -> log_and_exit (Error e))
  | "migrate-all" :: root :: _opts -> (
    let root_path =
      if Filename.is_relative root then Unix.realpath root else root
    in
    match Analysis.Packages.new_bs_package ~root_path with
    | None ->
      log_and_exit
        (Error
           (Printf.sprintf
              "error: failed to load ReScript project at %s (missing \
               rescript.json?)"
              root_path))
    | Some package ->
      let module_names =
        Analysis.SharedTypes.FileSet.elements package.project_files
      in
      let files =
        module_names
        |> List.filter_map (fun mod_name ->
               Hashtbl.find_opt package.paths_for_module mod_name
               |> Option.map Analysis.SharedTypes.get_src)
        |> List.concat
        |> List.filter (fun path ->
               Filename.check_suffix path ".res"
               || Filename.check_suffix path ".resi")
      in
      let total = List.length files in
      if total = 0 then log_and_exit (Ok "No source files found to migrate")
      else
        let process_one file =
          (file, Tools.Migrate.migrate ~entry_point_file:file ~output_mode:`File)
        in
        let results = List.map process_one files in
        let migrated, unchanged, failures =
          results
          |> List.fold_left
               (fun (migrated, unchanged, failures) (file, res) ->
                 match res with
                 | Ok msg ->
                   let base = Filename.basename file in
                   if msg = base ^ ": File migrated successfully" then
                     (migrated + 1, unchanged, failures)
                   else if msg = base ^ ": File did not need migration" then
                     (migrated, unchanged + 1, failures)
                   else
                     (* Unknown OK message, count as unchanged *)
                     (migrated, unchanged + 1, failures)
                 | Error _ -> (migrated, unchanged, failures + 1))
               (0, 0, 0)
        in
        let summary =
          Printf.sprintf
            "Migration summary: migrated %d, unchanged %d, failed %d, total %d"
            migrated unchanged failures total
        in
        if failures > 0 then log_and_exit (Error summary)
        else log_and_exit (Ok summary))
  | "format-codeblocks" :: rest -> (
    match rest with
    | ["-h"] | ["--help"] -> log_and_exit (Ok format_codeblocks_help)
    | path :: args -> (
      let is_stdout = List.mem "--stdout" args in
      let transform_assert_equal = List.mem "--transform-assert-equal" args in
      let output_mode = if is_stdout then `Stdout else `File in
      Clflags.color := Some Misc.Color.Never;
      match
        ( Tools.FormatCodeblocks.format_code_blocks_in_file ~output_mode
            ~transform_assert_equal ~entry_point_file:path,
          output_mode )
      with
      | Ok content, `Stdout -> print_endline content
      | result, `File -> log_and_exit result
      | Error e, _ -> log_and_exit (Error e))
    | _ -> log_and_exit (Error format_codeblocks_help))
  | "extract-codeblocks" :: rest -> (
    match rest with
    | ["-h"] | ["--help"] -> log_and_exit (Ok extract_codeblocks_help)
    | path :: args ->
      let transform_assert_equal = List.mem "--transform-assert-equal" args in
      Clflags.color := Some Misc.Color.Never;

      (* TODO: Add result/JSON mode *)
      Tools.ExtractCodeblocks.extract_codeblocks_from_file
        ~transform_assert_equal ~entry_point_file:path
      |> log_and_exit
    | _ -> log_and_exit (Error extract_codeblocks_help))
  | "reanalyze" :: _ ->
    if Sys.getenv_opt "RESCRIPT_REANALYZE_NO_SERVER" = Some "1" then (
      let len = Array.length Sys.argv in
      for i = 1 to len - 2 do
        Sys.argv.(i) <- Sys.argv.(i + 1)
      done;
      Sys.argv.(len - 1) <- "";
      Reanalyze.cli ())
    else
      (* Transparent delegation is supported only for the editor invocation:
         `reanalyze -json` (and nothing else). *)
      let argv_for_server =
        let args = Array.to_list Sys.argv in
        let rest =
          match args with
          | _ :: "reanalyze" :: rest -> rest
          | _ :: rest -> rest
          | [] -> []
        in
        rest |> List.filter (fun s -> s <> "") |> Array.of_list
      in
      if argv_for_server = [|"-json"|] then (
        match Reanalyze.ReanalyzeServer.try_request_default () with
        | Some resp ->
          output_string stdout resp.stdout;
          output_string stderr resp.stderr;
          flush stdout;
          flush stderr;
          exit resp.exit_code
        | None ->
          let len = Array.length Sys.argv in
          for i = 1 to len - 2 do
            Sys.argv.(i) <- Sys.argv.(i + 1)
          done;
          Sys.argv.(len - 1) <- "";
          Reanalyze.cli ())
      else
        let len = Array.length Sys.argv in
        for i = 1 to len - 2 do
          Sys.argv.(i) <- Sys.argv.(i + 1)
        done;
        Sys.argv.(len - 1) <- "";
        Reanalyze.cli ()
  | "reanalyze-server" :: _ ->
    let len = Array.length Sys.argv in
    for i = 1 to len - 2 do
      Sys.argv.(i) <- Sys.argv.(i + 1)
    done;
    Sys.argv.(len - 1) <- "";
    Reanalyze.ReanalyzeServer.server_cli ~parse_argv:Reanalyze.parse_argv
      ~run_analysis:Reanalyze.run_analysis ()
  | "extract-embedded" :: ext_point_names :: filename :: _ ->
    log_and_exit
      (Ok
         (Tools.extract_embedded
            ~extension_points:(ext_point_names |> String.split_on_char ',')
            ~filename))
  | ["ppx"; file_in; file_out] ->
    let ic = open_in_bin file_in in
    let magic =
      really_input_string ic (String.length Config.ast_impl_magic_number)
    in
    let loc = input_value ic in
    let ast0 : Parsetree0.structure = input_value ic in
    let prefix =
      match ast0 with
      | c1 :: c2 :: _ -> [c1; c2]
      | _ -> []
    in
    let ast = prefix @ ast0 in
    close_in ic;
    let oc = open_out_bin file_out in
    output_string oc magic;
    output_value oc loc;
    output_value oc ast;
    close_out oc;
    exit 0
  | ["-h"] | ["--help"] -> log_and_exit (Ok help)
  | ["-v"] | ["--version"] -> log_and_exit (Ok version)
  | _ -> log_and_exit (Error help)

let () = main ()
