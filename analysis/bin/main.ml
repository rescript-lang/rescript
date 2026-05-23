open Analysis

let help =
  {|
**Private CLI For rescript-vscode usage only**

API examples:
  ./rescript-editor-analysis.exe completion src/MyFile.res 0 4 currentContent.res true
  ./rescript-editor-analysis.exe definition src/MyFile.res 9 3
  ./rescript-editor-analysis.exe typeDefinition src/MyFile.res 9 3
  ./rescript-editor-analysis.exe documentSymbol src/Foo.res
  ./rescript-editor-analysis.exe hover src/MyFile.res 10 2 true
  ./rescript-editor-analysis.exe references src/MyFile.res 10 2
  ./rescript-editor-analysis.exe prepareRename src/MyFile.res 10 2
  ./rescript-editor-analysis.exe rename src/MyFile.res 10 2 foo
  ./rescript-editor-analysis.exe diagnosticSyntax src/MyFile.res
  ./rescript-editor-analysis.exe inlayHint src/MyFile.res 0 3 25
  ./rescript-editor-analysis.exe codeLens src/MyFile.res

Dev-time examples:
  ./rescript-editor-analysis.exe dump src/MyFile.res src/MyFile2.res
  ./rescript-editor-analysis.exe test src/MyFile.res

Note: positions are zero-indexed (start at 0 0), following LSP.
https://microsoft.github.io/language-server-protocol/specification#position

Options:
  completion: compute autocomplete for MyFile.res at line 0 and column 4,
    where MyFile.res is being edited and the editor content is in file current.res.

    ./rescript-editor-analysis.exe completion src/MyFile.res 0 4 current.res

  definition: get definition for item in MyFile.res at line 10 column 2:

    ./rescript-editor-analysis.exe definition src/MyFile.res 10 2

  typeDefinition: get type definition for item in MyFile.res at line 10 column 2:

    ./rescript-editor-analysis.exe typeDefinition src/MyFile.res 10 2

  documentSymbol: get all symbols declared in MyFile.res

    ./rescript-editor-analysis.exe documentSymbol src/MyFile.res

  hover: get inferred type for MyFile.res at line 10 column 2 (supporting markdown links):

    ./rescript-editor-analysis.exe hover src/MyFile.res 10 2 true

  references: get all references to item in MyFile.res at line 10 column 2:

    ./rescript-editor-analysis.exe references src/MyFile.res 10 2

  prepareRename: quickly compute the identifier range (and placeholder) for item at line 10 column 2 without scanning references:

    ./rescript-editor-analysis.exe prepareRename src/MyFile.res 10 2

  rename: rename all appearances of item in MyFile.res at line 10 column 2 with foo:

    ./rescript-editor-analysis.exe rename src/MyFile.res 10 2 foo

  semanticTokens: return token semantic highlighting info for MyFile.res

    ./rescript-editor-analysis.exe semanticTokens src/MyFile.res

  createInterface: print to stdout the interface file for src/MyFile.res

    ./rescript-editor-analysis.exe createInterface src/MyFile.res lib/bs/src/MyFile.cmi

  format: print to stdout the formatted version of the provided file

    ./rescript-editor-analysis.exe format src/MyFile.res

  diagnosticSyntax: print to stdout diagnostic for syntax

    ./rescript-editor-analysis.exe diagnosticSyntax src/MyFile.res

  inlayHint: get all inlay Hint between line 0 and 3 declared in MyFile.res. Last argument is maximum of character length for inlay hints

    ./rescript-editor-analysis.exe inlayHint src/MyFile.res 0 3 25

  codeLens: get all code lens entries for file src/MyFile.res

    ./rescript-editor-analysis.exe codeLens src/MyFile.res

  signatureHelp: get signature help if available for position at line 10 column 2 in src/MyFile.res

    ./rescript-editor-analysis.exe signatureHelp src/MyFile.res 10 2

  test: run tests specified by special comments in file src/MyFile.res

    ./rescript-editor-analysis.exe test src/src/MyFile.res
|}

let main () =
  let args = Array.to_list Sys.argv in
  let debugLevel, args =
    match args with
    | _ :: "debug-dump" :: logLevel :: rest ->
      ( (match logLevel with
        | "verbose" -> Debug.Verbose
        | "regular" -> Regular
        | _ -> Off),
        "dummy" :: rest )
    | args -> (Off, args)
  in
  Debug.debugLevel := debugLevel;
  let debug = debugLevel <> Debug.Off in
  let printHeaderInfo path line col =
    if debug then
      Printf.printf "Debug level: %s\n%s:%s-%s\n\n"
        (match debugLevel with
        | Debug.Verbose -> "verbose"
        | Regular -> "regular"
        | Off -> "off")
        path line col
  in
  match args with
  | [_; "cache-project"; rootPath] -> (
    Cfg.readProjectConfigCache := false;
    let uri = Uri.fromPath rootPath in
    match Packages.getPackage ~uri with
    | Some package -> Cache.cacheProject package
    | None -> print_endline "\"ERR\"")
  | [_; "cache-delete"; rootPath] -> (
    Cfg.readProjectConfigCache := false;
    let uri = Uri.fromPath rootPath in
    match Packages.findRoot ~uri (Hashtbl.create 0) with
    | Some (`Bs rootPath) -> (
      match BuildSystem.getLibBs rootPath with
      | None -> print_endline "\"ERR\""
      | Some libBs ->
        Cache.deleteCache (Cache.targetFileFromLibBs libBs);
        print_endline "\"OK\"")
    | _ -> print_endline "\"ERR: Did not find root \"")
  | [_; "completion"; path; line; col; currentFile] ->
    printHeaderInfo path line col;
    Cli.completion ~debug ~path
      ~pos:(int_of_string line, int_of_string col)
      ~currentFile
  | [_; "completionResolve"; path; modulePath] ->
    Cli.completionResolve ~path ~modulePath
  | [_; "definition"; path; line; col] ->
    Cli.definition ~path ~pos:(int_of_string line, int_of_string col) ~debug
  | [_; "typeDefinition"; path; line; col] ->
    Cli.typeDefinition ~path ~pos:(int_of_string line, int_of_string col) ~debug
  | [_; "documentSymbol"; path] -> DocumentSymbol.command ~path
  | [_; "hover"; path; line; col; currentFile; supportsMarkdownLinks] ->
    Cli.hover ~path
      ~pos:(int_of_string line, int_of_string col)
      ~currentFile ~debug
      ~supportsMarkdownLinks:
        (match supportsMarkdownLinks with
        | "true" -> true
        | _ -> false)
  | [
   _; "signatureHelp"; path; line; col; currentFile; allowForConstructorPayloads;
  ] ->
    Cli.signatureHelp ~path
      ~pos:(int_of_string line, int_of_string col)
      ~currentFile ~debug
      ~allowForConstructorPayloads:
        (match allowForConstructorPayloads with
        | "true" -> true
        | _ -> false)
  | [_; "inlayHint"; path; line_start; line_end; maxLength] ->
    Cli.inlayhint ~path
      ~pos:(int_of_string line_start, int_of_string line_end)
      ~maxLength ~debug
  | [_; "codeLens"; path] -> Cli.codeLens ~path ~debug
  | [_; "codeAction"; path; startLine; startCol; endLine; endCol; currentFile]
    ->
    Cli.codeAction ~path
      ~startPos:(int_of_string startLine, int_of_string startCol)
      ~endPos:(int_of_string endLine, int_of_string endCol)
      ~currentFile ~debug
  | [_; "codemod"; path; line; col; typ; hint] ->
    let typ =
      match typ with
      | "add-missing-cases" -> Codemod.AddMissingCases
      | _ -> raise (Failure "unsupported type")
    in
    let source = Files.readFile path |> Option.value ~default:"" in
    `String
      (Codemod.transform ~source
         ~pos:(int_of_string line, int_of_string col)
         ~debug ~typ ~hint)
    |> Yojson.Safe.pretty_to_string ~std:true
    |> print_endline
  | [_; "diagnosticSyntax"; path] -> Cli.diagnosticSyntax ~path
  | [_; "references"; path; line; col] ->
    Cli.references ~path ~pos:(int_of_string line, int_of_string col) ~debug
  | [_; "prepareRename"; path; line; col] ->
    Cli.prepareRename ~path ~pos:(int_of_string line, int_of_string col) ~debug
  | [_; "rename"; path; line; col; newName] ->
    Cli.rename ~path
      ~pos:(int_of_string line, int_of_string col)
      ~newName ~debug
  | [_; "semanticTokens"; currentFile] -> Cli.semanticTokens ~path:currentFile
  | [_; "createInterface"; path; cmiFile] ->
    `String (CreateInterface.command ~path ~cmiFile)
    |> Yojson.Safe.pretty_to_string ~std:true
    |> print_endline
  | [_; "format"; path] -> Cli.format ~path
  | [_; "test"; path] -> Cli.test ~path
  | [_; "cmt"; rescript_json; cmt_path] -> CmtViewer.dump rescript_json cmt_path
  | args when List.mem "-h" args || List.mem "--help" args -> prerr_endline help
  | _ ->
    prerr_endline help;
    exit 1
;;

main ()
