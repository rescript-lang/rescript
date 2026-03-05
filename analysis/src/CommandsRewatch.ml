let getString key obj =
  match Json.get key obj with
  | Some (Json.String s) -> s
  | _ -> ""

let parsePathsForModule json =
  let pathsForModule = Hashtbl.create 30 in
  (match json with
  | Json.Object items ->
    List.iter
      (fun (moduleName, value) ->
        let paths =
          match Json.get "impl" value with
          | Some impl ->
            Some
              (SharedTypes.Impl
                 {cmt = getString "cmt" impl; res = getString "res" impl})
          | None -> (
            match Json.get "intfAndImpl" value with
            | Some ii ->
              Some
                (SharedTypes.IntfAndImpl
                   {
                     cmti = getString "cmti" ii;
                     resi = getString "resi" ii;
                     cmt = getString "cmt" ii;
                     res = getString "res" ii;
                   })
            | None -> (
              match Json.get "namespace" value with
              | Some ns ->
                Some (SharedTypes.Namespace {cmt = getString "cmt" ns})
              | None -> None))
        in
        match paths with
        | Some p -> Hashtbl.replace pathsForModule moduleName p
        | None -> ())
      items
  | _ -> ());
  pathsForModule

let parseFileSet json =
  match json with
  | Some (Json.Array items) ->
    items
    |> List.filter_map (fun item ->
           match item with
           | Json.String s -> Some s
           | _ -> None)
    |> SharedTypes.FileSet.of_list
  | _ -> SharedTypes.FileSet.empty

let parseOpens json =
  match json with
  | Some (Json.Array items) ->
    items
    |> List.filter_map (fun item ->
           match item with
           | Json.Array strings ->
             Some
               (List.filter_map
                  (fun s ->
                    match s with
                    | Json.String s -> Some s
                    | _ -> None)
                  strings)
           | _ -> None)
  | _ -> []

type rewatch_context = {
  source: string;
  path: string;
  pos: int * int;
  endPos: int * int;
  newName: string;
  modulePath: string;
  maxLength: string;
  package: SharedTypes.package;
}

let parseRewatchContext json =
  let source = getString "source" json in
  let path = getString "path" json in
  let pos =
    match Json.get "pos" json with
    | Some (Json.Array [Json.Number line; Json.Number col]) ->
      (int_of_float line, int_of_float col)
    | _ -> (0, 0)
  in
  let rootPath = getString "rootPath" json in
  let namespace =
    match Json.get "namespace" json with
    | Some (Json.String s) -> Some s
    | _ -> None
  in
  let suffix =
    match Json.get "suffix" json with
    | Some (Json.String s) -> s
    | _ -> ".js"
  in
  let rescriptVersion =
    match Json.get "rescriptVersion" json with
    | Some (Json.Array [Json.Number major; Json.Number minor]) ->
      (int_of_float major, int_of_float minor)
    | _ -> (13, 0)
  in
  let genericJsxModule =
    match Json.get "genericJsxModule" json with
    | Some (Json.String s) -> Some s
    | _ -> None
  in
  let opens = parseOpens (Json.get "opens" json) in
  let pathsForModule =
    match Json.get "pathsForModule" json with
    | Some obj -> parsePathsForModule obj
    | None -> Hashtbl.create 0
  in
  let projectFiles = parseFileSet (Json.get "projectFiles" json) in
  let dependenciesFiles = parseFileSet (Json.get "dependenciesFiles" json) in
  let package : SharedTypes.package =
    {
      genericJsxModule;
      suffix;
      rootPath;
      projectFiles;
      dependenciesFiles;
      pathsForModule;
      namespace;
      opens;
      rescriptVersion;
      autocomplete = Misc.StringMap.empty;
    }
  in
  let endPos =
    match Json.get "endPos" json with
    | Some (Json.Array [Json.Number line; Json.Number col]) ->
      (int_of_float line, int_of_float col)
    | _ -> (0, 0)
  in
  let newName = getString "newName" json in
  let modulePath = getString "modulePath" json in
  let maxLength = getString "maxLength" json in
  {source; path; pos; endPos; newName; modulePath; maxLength; package}

let withRewatchContext ~name ~default f =
  let input = In_channel.input_all In_channel.stdin in
  match Json.parse input with
  | None ->
    prerr_endline (name ^ ": failed to parse JSON from stdin");
    print_endline default
  | Some json ->
    let ctx = parseRewatchContext json in
    print_endline (f ctx)

let withLocItem {path; pos; package; _} f =
  match Cmt.loadFullCmtWithPackage ~path ~package with
  | None -> None
  | Some full -> (
    match References.getLocItem ~full ~pos ~debug:false with
    | None -> None
    | Some locItem -> f full locItem)

let completion () =
  withRewatchContext ~name:"completion" ~default:"[]"
    (fun {source; path; pos; package; _} ->
      let completions =
        match
          Completions.getCompletionsFromSource ~debug:false ~path ~pos ~source
            ~package ()
        with
        | None -> []
        | Some (completions, full, _) ->
          completions
          |> List.map (CompletionBackEnd.completionToItem ~full)
          |> List.map Protocol.stringifyCompletionItem
      in
      completions |> Protocol.array)

let completionResolve () =
  withRewatchContext ~name:"completionResolve" ~default:Protocol.null
    (fun {path; package; modulePath; _} ->
      let moduleName =
        match modulePath |> String.split_on_char '.' with
        | moduleName :: _ -> moduleName
        | [] -> ""
      in
      if moduleName = "" then Protocol.null
      else
        match Cmt.loadFullCmtWithPackage ~path ~package with
        | None -> Protocol.null
        | Some full -> (
          match ProcessCmt.fileForModule ~package:full.package moduleName with
          | None -> Protocol.null
          | Some file ->
            file.structure.docstring |> String.concat "\n\n"
            |> Protocol.wrapInQuotes))

let hover () =
  withRewatchContext ~name:"hover" ~default:Protocol.null
    (fun {source; path; pos; package} ->
      match Cmt.loadFullCmtWithPackage ~path ~package with
      | None -> Protocol.null
      | Some full -> (
        match References.getLocItem ~full ~pos ~debug:false with
        | Some locItem -> (
          match Hover.newHover ~supportsMarkdownLinks:true ~full locItem with
          | Some s -> Protocol.stringifyHover s
          | None -> Protocol.null)
        | None -> (
          (* Fall back to completion-based hover *)
          match
            Completions.getCompletionsFromSource ~debug:false ~path ~pos ~source
              ~package ~forHover:true ()
          with
          | None -> Protocol.null
          | Some (completions, ({file; package} as full), scope) -> (
            let rawOpens = Scope.getRawOpens scope in
            match completions with
            | {kind = Label typString; docstring} :: _ ->
              let parts =
                docstring
                @ if typString = "" then [] else [Markdown.codeBlock typString]
              in
              Protocol.stringifyHover (String.concat "\n\n" parts)
            | {env} :: _ -> (
              let opens =
                CompletionBackEnd.getOpens ~debug:false ~rawOpens ~package ~env
              in
              match
                CompletionBackEnd.completionsGetTypeEnv2 ~debug:false ~full
                  ~rawOpens ~opens ~pos completions
              with
              | Some (typ, _env) ->
                let typeString =
                  Hover.hoverWithExpandedTypes ~file ~package
                    ~supportsMarkdownLinks:true typ
                in
                Protocol.stringifyHover typeString
              | None -> Protocol.null)
            | _ -> Protocol.null))))

let definition () =
  withRewatchContext ~name:"definition" ~default:Protocol.null (fun ctx ->
      let locationOpt =
        withLocItem ctx (fun full locItem ->
            match References.definitionForLocItem ~full locItem with
            | None -> None
            | Some (uri, loc) when not loc.loc_ghost ->
              let isInterface = full.file.uri |> Uri.isInterface in
              let posIsZero {Lexing.pos_lnum; pos_bol; pos_cnum} =
                pos_lnum = 1 && pos_cnum - pos_bol = 0
              in
              let isModule =
                match locItem.locType with
                | LModule _ | TopLevelModule _ -> true
                | TypeDefinition _ | Typed _ | Constant _ -> false
              in
              let skipLoc =
                (not isModule) && (not isInterface) && posIsZero loc.loc_start
                && posIsZero loc.loc_end
              in
              if skipLoc then None
              else
                Some
                  {
                    Protocol.uri = Files.canonicalizeUri uri;
                    range = Utils.cmtLocToRange loc;
                  }
            | Some _ -> None)
      in
      match locationOpt with
      | None -> Protocol.null
      | Some location -> location |> Protocol.stringifyLocation)

let prepareRename () =
  withRewatchContext ~name:"prepareRename" ~default:Protocol.null (fun ctx ->
      match
        withLocItem ctx (fun _full locItem ->
            let range = Utils.cmtLocToRange locItem.loc in
            let placeholderOpt =
              match locItem.locType with
              | Typed (name, _, _)
              | TopLevelModule name
              | TypeDefinition (name, _, _) ->
                Some name
              | _ -> None
            in
            Some (range, placeholderOpt))
      with
      | None -> Protocol.null
      | Some (range, placeholderOpt) ->
        let fields =
          [("range", Some (Protocol.stringifyRange range))]
          @
          match placeholderOpt with
          | None -> []
          | Some s -> [("placeholder", Some (Protocol.wrapInQuotes s))]
        in
        Protocol.stringifyObject fields)

let rename () =
  withRewatchContext ~name:"rename" ~default:Protocol.null
    (fun ({newName; _} as ctx) ->
      match
        withLocItem ctx (fun full locItem ->
            let allReferences =
              References.allReferencesForLocItem ~full locItem
            in
            let module StringMap = Misc.StringMap in
            let fileRenames, textEditsByUri =
              List.fold_left
                (fun (renames, editsMap) {References.uri; locOpt} ->
                  match locOpt with
                  | None ->
                    let path = Uri.toPath uri in
                    let dir = Filename.dirname path in
                    let newPath =
                      Filename.concat dir (newName ^ Filename.extension path)
                    in
                    let newUri = Uri.fromPath newPath in
                    let rename =
                      Protocol.stringifyRenameFile
                        {
                          oldUri = Files.canonicalizeUri uri;
                          newUri = newUri |> Uri.toString;
                        }
                    in
                    (rename :: renames, editsMap)
                  | Some loc ->
                    let canonUri = Files.canonicalizeUri uri in
                    let textEdit =
                      Protocol.
                        {range = Utils.cmtLocToRange loc; newText = newName}
                    in
                    let editsMap =
                      match StringMap.find_opt canonUri editsMap with
                      | None -> StringMap.add canonUri [textEdit] editsMap
                      | Some prev ->
                        StringMap.add canonUri (textEdit :: prev) editsMap
                    in
                    (renames, editsMap))
                ([], StringMap.empty) allReferences
            in
            let textDocumentEdits =
              StringMap.fold
                (fun uri edits acc ->
                  Protocol.stringifyTextDocumentEdit
                    {textDocument = {uri; version = None}; edits}
                  :: acc)
                textEditsByUri []
            in
            let documentChanges =
              fileRenames @ textDocumentEdits |> Protocol.array
            in
            Some (Printf.sprintf {|{"documentChanges": %s}|} documentChanges))
      with
      | None -> Protocol.null
      | Some result -> result)

let documentSymbol () =
  withRewatchContext ~name:"documentSymbol" ~default:"[]"
    (fun {source; path; _} -> DocumentSymbol.command ~path ~source)

let codeLens () =
  withRewatchContext ~name:"codeLens" ~default:"[]"
    (fun {source; path; package; _} ->
      match Hint.codeLensFromSource ~path ~source ~package ~debug:false with
      | Some lenses -> lenses |> Protocol.array
      | None -> Protocol.null)

let inlayHint () =
  withRewatchContext ~name:"inlayHint" ~default:"[]"
    (fun {source; path; pos; maxLength; package; _} ->
      match
        Hint.inlayFromSource ~path ~source ~pos ~maxLength ~package ~debug:false
      with
      | Some hints -> hints |> Protocol.array
      | None -> Protocol.null)

let semanticTokens () =
  withRewatchContext ~name:"semanticTokens" ~default:"{\"data\":[]}"
    (fun {source; path; _} ->
      SemanticTokens.semanticTokensFromSource ~path ~source)

let codeAction () =
  withRewatchContext ~name:"codeAction" ~default:"[]"
    (fun {source; path; pos; endPos; package; _} ->
      Xform.extractCodeActionsFromSource ~path ~startPos:pos ~endPos ~source
        ~package ~debug:false
      |> CodeActions.stringifyCodeActions)

let references () =
  withRewatchContext ~name:"references" ~default:Protocol.null (fun ctx ->
      let locations =
        match
          withLocItem ctx (fun full locItem ->
              let allReferences =
                References.allReferencesForLocItem ~full locItem
              in
              match allReferences with
              | [] -> None
              | refs ->
                Some
                  (refs
                  |> List.map (fun {References.uri; locOpt} ->
                         let loc =
                           match locOpt with
                           | Some loc -> loc
                           | None -> Uri.toTopLevelLoc uri
                         in
                         Protocol.stringifyLocation
                           {
                             Protocol.uri = Files.canonicalizeUri uri;
                             range = Utils.cmtLocToRange loc;
                           })))
        with
        | None | Some [] -> None
        | Some locations -> Some locations
      in
      match locations with
      | None -> Protocol.null
      | Some locs -> Protocol.array locs)

let signatureHelp () =
  withRewatchContext ~name:"signatureHelp" ~default:Protocol.null
    (fun {source; path; pos; package; _} ->
      match
        SignatureHelp.signatureHelp ~path ~pos ~currentFile:path ~text:source
          ~debug:false ~allowForConstructorPayloads:true ~package ()
      with
      | None ->
        Protocol.stringifySignatureHelp
          {signatures = []; activeSignature = None; activeParameter = None}
      | Some res -> Protocol.stringifySignatureHelp res)

let typeDefinition () =
  withRewatchContext ~name:"typeDefinition" ~default:Protocol.null (fun ctx ->
      let locationOpt =
        withLocItem ctx (fun full locItem ->
            match References.typeDefinitionForLocItem ~full locItem with
            | None -> None
            | Some (uri, loc) ->
              Some
                {
                  Protocol.uri = Files.canonicalizeUri uri;
                  range = Utils.cmtLocToRange loc;
                })
      in
      match locationOpt with
      | None -> Protocol.null
      | Some location -> location |> Protocol.stringifyLocation)
