let docHelp =
  {|ReScript Tools

Output documentation to standard output

Usage: rescript-tools doc <FILE> [--build]

Options:
  --build    Build the project before extracting documentation

Example: rescript-tools doc ./path/to/EntryPointLib.res
         rescript-tools doc ./path/to/EntryPointLib.res --build|}

let formatCodeblocksHelp =
  {|ReScript Tools

Format ReScript code blocks in docstrings or markdown files

Usage: rescript-tools format-codeblocks <FILE> [--stdout] [--transform-assert-equal]

Example: rescript-tools format-codeblocks ./path/to/MyModule.res|}

let extractCodeblocksHelp =
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
doc <file> [--build]                    Generate documentation
  [--build]                               Build project before extracting docs
format-codeblocks <file>                Format ReScript code blocks
  [--stdout]                              Output to stdout
  [--transform-assert-equal]              Transform `assertEqual` to `==`
extract-codeblocks <file>               Extract ReScript code blocks from file
  [--transform-assert-equal]              Transform `==` to `assertEqual`
reanalyze                               Reanalyze
-v, --version                           Print version
-h, --help                              Print help|}

let logAndExit = function
  | Ok log ->
    Printf.printf "%s\n" log;
    exit 0
  | Error log ->
    Printf.eprintf "%s\n" log;
    exit 1

let version = Version.version

(* Find the rescript binary relative to the current executable *)
let getRescriptBinary () =
  let toolsExePath = Sys.executable_name in
  let binDir = Filename.dirname toolsExePath in
  (* ReScript binaries use .exe extension on all platforms *)
  Filename.concat binDir "rescript.exe"

(* Build the project using the rescript binary *)
let buildProject ~rootPath =
  let rescriptBin = getRescriptBinary () in
  if not (Sys.file_exists rescriptBin) then
    Error
      (Printf.sprintf
         "Could not find rescript binary at %s. Please ensure rescript is \
          installed."
         rescriptBin)
  else
    let originalDir = Sys.getcwd () in
    try
      (* Change to project root to run build *)
      Sys.chdir rootPath;
      let pid =
        Unix.create_process rescriptBin [|rescriptBin; "build"|] Unix.stdin
          Unix.stdout Unix.stderr
      in
      let _, status = Unix.waitpid [] pid in
      Sys.chdir originalDir;
      match status with
      | Unix.WEXITED 0 -> Ok ()
      | Unix.WEXITED code ->
        Error (Printf.sprintf "Build failed with exit code %d" code)
      | Unix.WSIGNALED signal ->
        Error (Printf.sprintf "Build killed by signal %d" signal)
      | Unix.WSTOPPED signal ->
        Error (Printf.sprintf "Build stopped by signal %d" signal)
    with e ->
      Sys.chdir originalDir;
      Error (Printf.sprintf "Build error: %s" (Printexc.to_string e))

let main () =
  match Sys.argv |> Array.to_list |> List.tl with
  | "doc" :: rest -> (
    match rest with
    | ["-h"] | ["--help"] -> logAndExit (Ok docHelp)
    | [path] ->
      (* NOTE: Internal use to generate docs from compiler *)
      let () =
        match Sys.getenv_opt "FROM_COMPILER" with
        | Some "true" -> Analysis.Cfg.isDocGenFromCompiler := true
        | _ -> ()
      in
      logAndExit (Tools.extractDocs ~entryPointFile:path ~debug:false)
    | ["--build"; path] | [path; "--build"] -> (
      (* Build first, then extract docs *)
      let rootPath =
        let absPath =
          if Filename.is_relative path then Unix.realpath path else path
        in
        (* Find the project root by looking for rescript.json *)
        let rec findProjectRoot dir =
          let rescriptJson = Filename.concat dir "rescript.json" in
          let bsconfigJson = Filename.concat dir "bsconfig.json" in
          if Sys.file_exists rescriptJson || Sys.file_exists bsconfigJson then
            dir
          else
            let parent = Filename.dirname dir in
            if parent = dir then
              (* Reached filesystem root, use current directory *)
              Filename.dirname absPath
            else findProjectRoot parent
        in
        findProjectRoot (Filename.dirname absPath)
      in
      match buildProject ~rootPath with
      | Ok () ->
        let () =
          match Sys.getenv_opt "FROM_COMPILER" with
          | Some "true" -> Analysis.Cfg.isDocGenFromCompiler := true
          | _ -> ()
        in
        logAndExit (Tools.extractDocs ~entryPointFile:path ~debug:false)
      | Error e -> logAndExit (Error e))
    | _ -> logAndExit (Error docHelp))
  | "migrate" :: file :: opts -> (
    let isStdout = List.mem "--stdout" opts in
    let outputMode = if isStdout then `Stdout else `File in
    match
      (Tools.Migrate.migrate ~entryPointFile:file ~outputMode, outputMode)
    with
    | Ok content, `Stdout -> print_endline content
    | result, `File -> logAndExit result
    | Error e, _ -> logAndExit (Error e))
  | "migrate-all" :: root :: _opts -> (
    let rootPath =
      if Filename.is_relative root then Unix.realpath root else root
    in
    match Analysis.Packages.newBsPackage ~rootPath with
    | None ->
      logAndExit
        (Error
           (Printf.sprintf
              "error: failed to load ReScript project at %s (missing \
               bsconfig.json/rescript.json?)"
              rootPath))
    | Some package ->
      let moduleNames =
        Analysis.SharedTypes.FileSet.elements package.projectFiles
      in
      let files =
        moduleNames
        |> List.filter_map (fun modName ->
               Hashtbl.find_opt package.pathsForModule modName
               |> Option.map Analysis.SharedTypes.getSrc)
        |> List.concat
        |> List.filter (fun path ->
               Filename.check_suffix path ".res"
               || Filename.check_suffix path ".resi")
      in
      let total = List.length files in
      if total = 0 then logAndExit (Ok "No source files found to migrate")
      else
        let process_one file =
          (file, Tools.Migrate.migrate ~entryPointFile:file ~outputMode:`File)
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
        if failures > 0 then logAndExit (Error summary)
        else logAndExit (Ok summary))
  | "format-codeblocks" :: rest -> (
    match rest with
    | ["-h"] | ["--help"] -> logAndExit (Ok formatCodeblocksHelp)
    | path :: args -> (
      let isStdout = List.mem "--stdout" args in
      let transformAssertEqual = List.mem "--transform-assert-equal" args in
      let outputMode = if isStdout then `Stdout else `File in
      Clflags.color := Some Misc.Color.Never;
      match
        ( Tools.FormatCodeblocks.formatCodeBlocksInFile ~outputMode
            ~transformAssertEqual ~entryPointFile:path,
          outputMode )
      with
      | Ok content, `Stdout -> print_endline content
      | result, `File -> logAndExit result
      | Error e, _ -> logAndExit (Error e))
    | _ -> logAndExit (Error formatCodeblocksHelp))
  | "extract-codeblocks" :: rest -> (
    match rest with
    | ["-h"] | ["--help"] -> logAndExit (Ok extractCodeblocksHelp)
    | path :: args -> (
      let transformAssertEqual = List.mem "--transform-assert-equal" args in
      Clflags.color := Some Misc.Color.Never;

      (* TODO: Add result/JSON mode *)
      match
        Tools.ExtractCodeblocks.extractCodeblocksFromFile ~transformAssertEqual
          ~entryPointFile:path
      with
      | Ok _ as r ->
        print_endline (Analysis.Protocol.stringifyResult r);
        exit 0
      | Error _ as r ->
        print_endline (Analysis.Protocol.stringifyResult r);
        exit 1)
    | _ -> logAndExit (Error extractCodeblocksHelp))
  | "reanalyze" :: _ ->
    let len = Array.length Sys.argv in
    for i = 1 to len - 2 do
      Sys.argv.(i) <- Sys.argv.(i + 1)
    done;
    Sys.argv.(len - 1) <- "";
    Reanalyze.cli ()
  | "extract-embedded" :: extPointNames :: filename :: _ ->
    logAndExit
      (Ok
         (Tools.extractEmbedded
            ~extensionPoints:(extPointNames |> String.split_on_char ',')
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
  | ["-h"] | ["--help"] -> logAndExit (Ok help)
  | ["-v"] | ["--version"] -> logAndExit (Ok version)
  | _ -> logAndExit (Error help)

let () = main ()
