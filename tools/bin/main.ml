let docHelp =
  {|ReScript Tools

Output documentation to standard output

Usage: rescript-tools doc <FILE>

Example: rescript-tools doc ./path/to/EntryPointLib.res|}

let help =
  {|ReScript Tools

Usage: rescript-tools [command]

Commands:

doc <file>            Generate documentation
reanalyze             Reanalyze
-v, --version         Print version
-h, --help            Print help|}

let logAndExit = function
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
    | ["-h"] | ["--help"] -> logAndExit (Ok docHelp)
    | [path] ->
      (* NOTE: Internal use to generate docs from compiler *)
      let () =
        match Sys.getenv_opt "FROM_COMPILER" with
        | Some "true" -> Analysis.Cfg.isDocGenFromCompiler := true
        | _ -> ()
      in
      logAndExit
        (Analysis.DocGen.extractDocsToJson ~entryPointFile:path ~debug:false)
    | _ -> logAndExit (Error docHelp))
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
