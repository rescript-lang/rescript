let print_null =
  `Null |> Yojson.Safe.pretty_to_string ~std:true |> print_endline
let print_list l =
  `List l |> Yojson.Safe.pretty_to_string ~std:true |> print_endline
let print_string json =
  Yojson.Safe.pretty_to_string ~std:true json |> print_endline

let completion ~debug ~path ~pos ~currentFile =
  let full = Cmt.loadFullCmtFromPath ~path in
  let kindFile = Files.classifySourceFile currentFile in
  match Files.readFile currentFile with
  | None | Some "" -> print_null
  | Some source ->
    Commands.completion ~debug ~source ~kindFile ~pos ~full
    |> List.map (fun c -> Lsp.Types.CompletionItem.yojson_of_t c)
    |> print_list

let completionResolve ~path ~modulePath =
  let full = Cmt.loadFullCmtFromPath ~path in
  match Commands.completionResolve ~full ~modulePath with
  | None -> print_null
  | Some (`MarkupContent {value}) -> `String value |> print_string

let inlayhint ~path ~pos ~maxLength ~debug =
  let full = Cmt.loadFullCmtFromPath ~path in
  let kindFile = Files.classifySourceFile path in
  match Files.readFile path with
  | None -> print_null
  | Some source -> (
    match Hint.inlay ~source ~kindFile ~pos ~maxLength ~full ~debug with
    | Some hints ->
      hints
      |> List.map (fun h -> Lsp.Types.InlayHint.yojson_of_t h)
      |> print_list
    | None -> print_null)

let codeLens ~path ~debug =
  let full = Cmt.loadFullCmtFromPath ~path in
  let kindFile = Files.classifySourceFile path in
  match Files.readFile path with
  | None -> print_null
  | Some source -> (
    match Hint.codeLens ~source ~kindFile ~full ~debug with
    | Some lens ->
      lens |> List.map (fun l -> Lsp.Types.CodeLens.yojson_of_t l) |> print_list
    | None -> print_null)

let hover ~path ~pos ~currentFile ~debug ~supportsMarkdownLinks =
  let full = Cmt.loadFullCmtFromPath ~path in
  let kindFile = Files.classifySourceFile currentFile in
  match Files.readFile currentFile with
  | None -> print_null
  | Some source -> (
    match
      Commands.hover ~source ~kindFile ~pos ~debug ~supportsMarkdownLinks ~full
    with
    | Some value -> Lsp.Types.Hover.yojson_of_t value |> print_string
    | None -> print_null)

let signatureHelp ~path ~pos ~currentFile ~debug ~allowForConstructorPayloads =
  let full = Cmt.loadFullCmtFromPath ~path in
  let kindFile = Files.classifySourceFile currentFile in
  match Files.readFile currentFile with
  | None -> print_null
  | Some source -> (
    match
      SignatureHelp.signatureHelp ~source ~kindFile ~pos
        ~allowForConstructorPayloads ~full ~debug
    with
    | None -> print_null
    | Some s -> Lsp.Types.SignatureHelp.yojson_of_t s |> print_string)

let codeAction ~path ~startPos ~endPos ~currentFile ~debug =
  let kindFile = Files.classifySourceFile currentFile in
  match Files.readFile currentFile with
  | None -> print_null
  | Some source ->
    Xform.extractCodeActions ~path ~startPos ~endPos ~source ~kindFile ~debug
    |> List.map (fun c -> Lsp.Types.CodeAction.yojson_of_t c)
    |> print_list

let definition ~path ~pos ~debug =
  let full = Cmt.loadFullCmtFromPath ~path in

  match Commands.definition ~full ~pos ~debug with
  | None -> print_null
  | Some location -> location |> Lsp.Types.Location.yojson_of_t |> print_string

let typeDefinition ~path ~pos ~debug =
  let full = Cmt.loadFullCmtFromPath ~path in
  match Commands.typeDefinition ~full ~pos ~debug with
  | None -> print_null
  | Some location -> location |> Lsp.Types.Location.yojson_of_t |> print_string

let references ~path ~pos ~debug =
  let full = Cmt.loadFullCmtFromPath ~path in
  let allLocs = Commands.references ~full ~pos ~debug in
  if allLocs = [] then print_null
  else
    allLocs
    |> List.map (fun l -> Lsp.Types.Location.yojson_of_t l)
    |> print_list

let rename ~path ~pos ~newName ~debug =
  let full = Cmt.loadFullCmtFromPath ~path in
  match Commands.rename ~full ~pos ~newName ~debug with
  | Some {documentChanges = Some documentChanges} ->
    documentChanges
    |> List.map (fun c ->
           match c with
           | `RenameFile r -> Lsp.Types.RenameFile.yojson_of_t r
           | `TextDocumentEdit te -> Lsp.Types.TextDocumentEdit.yojson_of_t te
           | `DeleteFile df -> Lsp.Types.DeleteFile.yojson_of_t df
           | `CreateFile cf -> Lsp.Types.CreateFile.yojson_of_t cf)
    |> print_list
  | _ -> print_null

let prepareRename ~path ~pos ~debug =
  let full = Cmt.loadFullCmtFromPath ~path in
  match Commands.prepareRename ~full ~pos ~debug with
  | None -> print_null
  | Some range -> Lsp.Types.Range.yojson_of_t range |> print_string

let format ~path =
  match Files.readFile path with
  | None -> print_null
  | Some source -> (
    let kindFile = Files.classifySourceFile path in
    match Commands.format ~source ~kindFile with
    | Ok textEdits -> (
      match textEdits with
      | {newText} :: _ -> print_string (`String newText)
      | _ -> print_null)
    | Error _ -> print_null)

let diagnosticSyntax ~path =
  match Files.readFile path with
  | None -> print_list []
  | Some source ->
    let kindFile = Files.classifySourceFile path in
    Diagnostics.document_syntax ~source ~kindFile
    |> List.map Lsp.Types.Diagnostic.yojson_of_t
    |> print_list

let semanticTokens ~path =
  match Files.readFile path with
  | None -> print_null
  | Some source ->
    let kindFile = Files.classifySourceFile path in
    let tokens = SemanticTokens.semanticTokens ~source ~kindFile in
    Lsp.Types.SemanticTokens.yojson_of_t tokens |> print_string

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
            |> List.iter (fun {Lsp.Types.CodeAction.title; edit} ->
                   Printf.printf "Hit: %s\n" title;
                   match edit with
                   | Some {documentChanges} ->
                     documentChanges |> Option.get
                     |> List.iter
                          (fun
                            (dc :
                              [ `CreateFile of Lsp.Types.CreateFile.t
                              | `DeleteFile of Lsp.Types.DeleteFile.t
                              | `RenameFile of Lsp.Types.RenameFile.t
                              | `TextDocumentEdit of
                                Lsp.Types.TextDocumentEdit.t ])
                          ->
                            match dc with
                            | `TextDocumentEdit tde ->
                              Printf.printf "\nTextDocumentEdit: %s\n"
                                (tde.textDocument.uri |> Lsp.Uri.to_string);

                              tde.edits
                              |> List.iter
                                   (fun
                                     (edit :
                                       [ `AnnotatedTextEdit of
                                         Lsp.Types.AnnotatedTextEdit.t
                                       | `TextEdit of Lsp.Types.TextEdit.t ])
                                   ->
                                     let start_char, newText, range =
                                       match edit with
                                       | `TextEdit te ->
                                         ( te.range.start.character,
                                           te.newText,
                                           te.range )
                                       | `AnnotatedTextEdit te ->
                                         ( te.range.start.character,
                                           te.newText,
                                           te.range )
                                     in
                                     let indent = String.make start_char ' ' in
                                     Printf.printf
                                       "%s\nnewText:\n%s<--here\n%s%s\n"
                                       (Lsp.Types.Range.yojson_of_t range
                                       |> Yojson.Safe.to_string)
                                       indent indent newText)
                            | `CreateFile cf ->
                              Printf.printf "\nCreateFile: %s\n"
                                (cf.uri |> Lsp.Uri.to_string)
                            | _ -> assert false)
                   | None -> ())
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
