let completion ~debug ~path ~pos ~currentFile =
  let full = Cmt.loadFullCmtFromPath ~path in
  let kindFile = Files.classifySourceFile currentFile in
  match Files.readFile currentFile with
  | None | Some "" -> Protocol.null |> print_endline
  | Some source ->
    Commands.completion ~debug ~source ~kindFile ~pos ~full
    |> List.map Protocol.stringifyCompletionItem
    |> Protocol.array |> print_endline

let completionResolve ~path ~modulePath =
  let full = Cmt.loadFullCmtFromPath ~path in
  let result =
    match Commands.completionResolve ~full ~modulePath with
    | None -> Protocol.null
    | Some content -> Protocol.wrapInQuotes content
  in
  print_endline result

let inlayhint ~path ~pos ~maxLength ~debug =
  let full = Cmt.loadFullCmtFromPath ~path in
  let kindFile = Files.classifySourceFile path in
  match Files.readFile path with
  | None -> Protocol.null |> print_endline
  | Some source -> (
    match Hint.inlay ~source ~kindFile ~pos ~maxLength ~full ~debug with
    | Some hints ->
      hints
      |> List.map Protocol.stringifyHint
      |> Protocol.array |> print_endline
    | None -> Protocol.null |> print_endline)

let codeLens ~path ~debug =
  let full = Cmt.loadFullCmtFromPath ~path in
  let kindFile = Files.classifySourceFile path in
  match Files.readFile path with
  | None -> Protocol.null |> print_endline
  | Some source -> (
    match Hint.codeLens ~source ~kindFile ~full ~debug with
    | Some lens ->
      lens
      |> List.map Protocol.stringifyCodeLens
      |> Protocol.array |> print_endline
    | None -> Protocol.null |> print_endline)

let hover ~path ~pos ~currentFile ~debug ~supportsMarkdownLinks =
  let full = Cmt.loadFullCmtFromPath ~path in
  let kindFile = Files.classifySourceFile currentFile in
  match Files.readFile currentFile with
  | None -> Protocol.null |> print_endline
  | Some source ->
    let result =
      match
        Commands.hover ~source ~kindFile ~pos ~debug ~supportsMarkdownLinks
          ~full
      with
      | Some value -> Protocol.stringifyHover value
      | None -> Protocol.null
    in
    print_endline result

let signatureHelp ~path ~pos ~currentFile ~debug ~allowForConstructorPayloads =
  let full = Cmt.loadFullCmtFromPath ~path in
  let kindFile = Files.classifySourceFile currentFile in
  match Files.readFile currentFile with
  | None -> Protocol.null |> print_endline
  | Some source ->
    Commands.signatureHelp ~source ~kindFile ~pos ~allowForConstructorPayloads
      ~full ~debug
    |> Protocol.stringifySignatureHelp |> print_endline

let codeAction ~path ~startPos ~endPos ~currentFile ~debug =
  let kindFile = Files.classifySourceFile currentFile in
  match Files.readFile currentFile with
  | None -> Protocol.null |> print_endline
  | Some source ->
    Xform.extractCodeActions ~path ~startPos ~endPos ~source ~kindFile ~debug
    |> CodeActions.stringifyCodeActions |> print_endline

let definition ~path ~pos ~debug =
  let full = Cmt.loadFullCmtFromPath ~path in
  print_endline
    (match Commands.definition ~full ~pos ~debug with
    | None -> Protocol.null
    | Some location -> location |> Protocol.stringifyLocation)

let typeDefinition ~path ~pos ~debug =
  let full = Cmt.loadFullCmtFromPath ~path in
  print_endline
    (match Commands.typeDefinition ~full ~pos ~debug with
    | None -> Protocol.null
    | Some location -> location |> Protocol.stringifyLocation)

let references ~path ~pos ~debug =
  let full = Cmt.loadFullCmtFromPath ~path in
  let allLocs = Commands.references ~full ~pos ~debug in
  print_endline
    (if allLocs = [] then Protocol.null
     else
       "[\n"
       ^ (allLocs |> List.map Protocol.stringifyLocation |> String.concat ",\n")
       ^ "\n]")

let rename ~path ~pos ~newName ~debug =
  let full = Cmt.loadFullCmtFromPath ~path in
  let result =
    match Commands.rename ~full ~pos ~newName ~debug with
    | None -> Protocol.null
    | Some (fileRenames, textDocumentEdits) ->
      let fileRenamesString =
        fileRenames |> List.map Protocol.stringifyRenameFile
      in
      let textDocumentEditsString =
        textDocumentEdits |> List.map Protocol.stringifyTextDocumentEdit
      in
      "[\n"
      ^ (fileRenamesString @ textDocumentEditsString |> String.concat ",\n")
      ^ "\n]"
  in
  print_endline result

let prepareRename ~path ~pos ~debug =
  let full = Cmt.loadFullCmtFromPath ~path in
  let result =
    match Commands.prepareRename ~full ~pos ~debug with
    | None -> Protocol.null
    | Some (Range range) -> Protocol.stringifyRange range
    | Some (Placeholder rangeph) ->
      Protocol.stringifyRangeWithPlaceholder rangeph
  in
  print_endline result

let format ~path =
  match Files.readFile path with
  | None -> Protocol.null |> print_endline
  | Some source -> (
    let kindFile = Files.classifySourceFile path in
    match Commands.format ~source ~kindFile with
    | Ok textEdits -> (
      match textEdits with
      | {newText} :: _ -> Printf.printf "\"%s\"" (Json.escape newText)
      | _ -> Protocol.null |> print_endline)
    | Error _ -> Protocol.null |> print_endline)

let diagnosticSyntax ~path =
  match Files.readFile path with
  | None -> Protocol.array [""] |> print_endline
  | Some source ->
    let kindFile = Files.classifySourceFile path in
    Diagnostics.document_syntax ~source ~kindFile
    |> List.map Protocol.stringifyDiagnostic
    |> Protocol.array |> print_endline

let semanticTokens ~path =
  match Files.readFile path with
  | None -> Protocol.null |> print_endline
  | Some source ->
    let kindFile = Files.classifySourceFile path in
    let tokens = SemanticTokens.semanticTokens ~source ~kindFile in
    let data = SemanticTokens.Token.arrayToJsonString tokens.data in
    Printf.printf "{\"data\":%s}" data

let test ~path =
  Uri.stripPath := true;
  match Files.readFile path with
  | None -> assert false
  | Some text ->
    let lines = text |> String.split_on_char '\n' in
    let processLine i line =
      let createCurrentFile () =
        let currentFile, cout =
          Filename.open_temp_file "def" ("txt." ^ Filename.extension path)
        in
        let removeLineComment l =
          let len = String.length l in
          let rec loop i =
            if i + 2 <= len && l.[i] = '/' && l.[i + 1] = '/' then Some (i + 2)
            else if i + 2 < len && l.[i] = ' ' then loop (i + 1)
            else None
          in
          match loop 0 with
          | None -> l
          | Some indexAfterComment ->
            String.make indexAfterComment ' '
            ^ String.sub l indexAfterComment (len - indexAfterComment)
        in
        lines
        |> List.iteri (fun j l ->
               let lineToOutput =
                 if j == i - 1 then removeLineComment l else l
               in
               Printf.fprintf cout "%s\n" lineToOutput);
        close_out cout;
        currentFile
      in
      if Str.string_match (Str.regexp "^ *//[ ]*\\^") line 0 then
        let matched = Str.matched_string line in
        let len = line |> String.length in
        let mlen = String.length matched in
        let rest = String.sub line mlen (len - mlen) in
        let line = i - 1 in
        let col = mlen - 1 in
        if mlen >= 3 then (
          (match String.sub rest 0 3 with
          | "db+" -> Log.verbose := true
          | "db-" -> Log.verbose := false
          | "dv+" -> Debug.debugLevel := Verbose
          | "dv-" -> Debug.debugLevel := Off
          | "in+" -> Cfg.inIncrementalTypecheckingMode := true
          | "in-" -> Cfg.inIncrementalTypecheckingMode := false
          | "ve+" -> (
            let version = String.sub rest 3 (String.length rest - 3) in
            let version = String.trim version in
            if Debug.verbose () then
              Printf.printf "Setting version: %s\n" version;
            match String.split_on_char '.' version with
            | [majorRaw; minorRaw] ->
              let version = (int_of_string majorRaw, int_of_string minorRaw) in
              Packages.overrideRescriptVersion := Some version
            | _ -> ())
          | "ve-" -> Packages.overrideRescriptVersion := None
          | "def" ->
            print_endline
              ("Definition " ^ path ^ " " ^ string_of_int line ^ ":"
             ^ string_of_int col);
            definition ~path ~pos:(line, col) ~debug:true
          | "com" ->
            print_endline
              ("Complete " ^ path ^ " " ^ string_of_int line ^ ":"
             ^ string_of_int col);
            let currentFile = createCurrentFile () in
            completion ~debug:true ~path ~pos:(line, col) ~currentFile;
            Sys.remove currentFile
          | "cre" ->
            let modulePath = String.sub rest 3 (String.length rest - 3) in
            let modulePath = String.trim modulePath in
            print_endline ("Completion resolve: " ^ modulePath);
            completionResolve ~path ~modulePath
          | "dce" ->
            print_endline ("DCE " ^ path);
            Reanalyze.RunConfig.runConfig.suppress <- ["src"];
            Reanalyze.RunConfig.runConfig.unsuppress <-
              [Filename.concat "src" "dce"];
            DceCommand.command ()
          | "doc" ->
            print_endline ("DocumentSymbol " ^ path);
            DocumentSymbol.command ~path
          | "hig" ->
            print_endline ("Highlight " ^ path);
            let source = Files.readFile path |> Option.get in
            let kindFile = Files.classifySourceFile path in

            SemanticTokens.command ~debug:true
              ~emitter:(SemanticTokens.Token.createEmitter ())
              ~source ~kindFile
          | "hov" ->
            print_endline
              ("Hover " ^ path ^ " " ^ string_of_int line ^ ":"
             ^ string_of_int col);
            let currentFile = createCurrentFile () in
            hover ~supportsMarkdownLinks:true ~path ~pos:(line, col)
              ~currentFile ~debug:true;
            Sys.remove currentFile
          | "she" ->
            print_endline
              ("Signature help " ^ path ^ " " ^ string_of_int line ^ ":"
             ^ string_of_int col);
            let currentFile = createCurrentFile () in
            signatureHelp ~path ~pos:(line, col) ~currentFile ~debug:true
              ~allowForConstructorPayloads:true;
            Sys.remove currentFile
          | "int" ->
            print_endline ("Create Interface " ^ path);
            let cmiFile =
              let open Filename in
              let ( ++ ) = concat in
              let name = chop_extension (basename path) ^ ".cmi" in
              let dir = dirname path in
              dir ++ parent_dir_name ++ "lib" ++ "bs" ++ "src" ++ name
            in
            Printf.printf "%s" (CreateInterface.command ~path ~cmiFile)
          | "ref" ->
            print_endline
              ("References " ^ path ^ " " ^ string_of_int line ^ ":"
             ^ string_of_int col);
            references ~path ~pos:(line, col) ~debug:true
          | "pre" ->
            print_endline
              ("PrepareRename " ^ path ^ " " ^ string_of_int line ^ ":"
             ^ string_of_int col);
            prepareRename ~path ~pos:(line, col) ~debug:true
          | "ren" ->
            let newName = String.sub rest 4 (len - mlen - 4) in
            let () =
              print_endline
                ("Rename " ^ path ^ " " ^ string_of_int line ^ ":"
               ^ string_of_int col ^ " " ^ newName)
            in
            rename ~path ~pos:(line, col) ~newName ~debug:true
          | "typ" ->
            print_endline
              ("TypeDefinition " ^ path ^ " " ^ string_of_int line ^ ":"
             ^ string_of_int col);
            typeDefinition ~path ~pos:(line, col) ~debug:true
          | "xfm" ->
            let currentFile = createCurrentFile () in
            (* +2 is to ensure that the character ^ points to is what's considered the end of the selection. *)
            let endCol = col + try String.index rest '^' + 2 with _ -> 0 in
            let endPos = (line, endCol) in
            let startPos = (line, col) in
            if startPos = endPos then
              print_endline
                ("Xform " ^ path ^ " " ^ string_of_int line ^ ":"
               ^ string_of_int col)
            else
              print_endline
                ("Xform " ^ path ^ " start: " ^ Pos.toString startPos
               ^ ", end: " ^ Pos.toString endPos);

            let source =
              Files.readFile currentFile |> Option.value ~default:""
            in
            let kindFile = Files.classifySourceFile currentFile in
            let codeActions =
              Xform.extractCodeActions ~path ~startPos ~endPos ~source ~kindFile
                ~debug:true
            in
            Sys.remove currentFile;
            codeActions
            |> List.iter (fun {Protocol.title; edit = {documentChanges}} ->
                   Printf.printf "Hit: %s\n" title;
                   documentChanges
                   |> List.iter (fun dc ->
                          match dc with
                          | Protocol.TextDocumentEdit tde ->
                            Printf.printf "\nTextDocumentEdit: %s\n"
                              tde.textDocument.uri;

                            tde.edits
                            |> List.iter (fun {Protocol.range; newText} ->
                                   let indent =
                                     String.make range.start.character ' '
                                   in
                                   Printf.printf
                                     "%s\nnewText:\n%s<--here\n%s%s\n"
                                     (Protocol.stringifyRange range)
                                     indent indent newText)
                          | CreateFile cf ->
                            Printf.printf "\nCreateFile: %s\n" cf.uri))
          | "c-a" ->
            let hint = String.sub rest 3 (String.length rest - 3) in
            print_endline
              ("Codemod AddMissingCases" ^ path ^ " " ^ string_of_int line ^ ":"
             ^ string_of_int col);
            let source = Files.readFile path |> Option.value ~default:"" in
            Codemod.transform ~source ~pos:(line, col) ~debug:true
              ~typ:AddMissingCases ~hint
            |> print_endline
          | "dia" -> diagnosticSyntax ~path
          | "hin" ->
            (* Get all inlay Hint between line 1 and n.
               Don't get the first line = 0.
            *)
            let line_start = 1 in
            let line_end = 34 in
            print_endline
              ("Inlay Hint " ^ path ^ " " ^ string_of_int line_start ^ ":"
             ^ string_of_int line_end);
            inlayhint ~path ~pos:(line_start, line_end) ~maxLength:"25"
              ~debug:false
          | "cle" ->
            print_endline ("Code Lens " ^ path);
            codeLens ~path ~debug:false
          | "ast" ->
            print_endline
              ("Dump AST " ^ path ^ " " ^ string_of_int line ^ ":"
             ^ string_of_int col);
            let currentFile = createCurrentFile () in
            DumpAst.dump ~pos:(line, col) ~currentFile;
            Sys.remove currentFile
          | "sem" -> semanticTokens ~path
          | _ -> ());
          print_newline ())
    in
    lines |> List.iteri processLine
