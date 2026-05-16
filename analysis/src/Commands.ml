let completion ~debug ~source ~kindFile ~pos ~full =
  match
    Completions.getCompletions ~debug ~source ~kindFile ~pos ~full
      ~forHover:false
  with
  | None -> []
  | Some (completions, full, _) ->
    completions |> List.map (CompletionBackEnd.completionToItem ~full)

let completionResolve ~(full : SharedTypes.full option) ~modulePath =
  (* We ignore the internal module path as of now because there's currently
     no use case for it. But, if we wanted to move resolving documentation
     for regular modules and not just file modules to the completionResolve
     hook as well, it'd be easy to implement here. *)
  let moduleName, _innerModulePath =
    match modulePath |> String.split_on_char '.' with
    | [moduleName] -> (moduleName, [])
    | moduleName :: rest -> (moduleName, rest)
    | [] -> raise (Failure "Invalid module path.")
  in
  let docstring =
    match full with
    | None ->
      if Debug.verbose () then
        Printf.printf "[completion_resolve] Could not load cmt\n";
      None
    | Some full -> (
      match ProcessCmt.fileForModule ~package:full.package moduleName with
      | None ->
        if Debug.verbose () then
          Printf.printf "[completion_resolve] Did not find file for module %s\n"
            moduleName;
        None
      | Some file -> Some (file.structure.docstring |> String.concat "\n\n"))
  in
  docstring

let hover ~source ~kindFile ~pos ~supportsMarkdownLinks ~full ~debug =
  let result =
    match full with
    | None -> None
    | Some full -> (
      match References.getLocItem ~full ~pos ~debug with
      | None -> (
        if debug then
          Printf.printf
            "Nothing at that position. Now trying to use completion.\n";
        match
          Hover.getHoverViaCompletions ~debug ~source ~kindFile ~pos
            ~forHover:true ~supportsMarkdownLinks ~full:(Some full)
        with
        | None -> None
        | Some hover -> Some hover)
      | Some locItem ->
        let isModule =
          match locItem.locType with
          | LModule _ | TopLevelModule _ -> true
          | TypeDefinition _ | Typed _ | Constant _ -> false
        in
        let uriLocOpt = References.definitionForLocItem ~full locItem in
        let skipZero =
          match uriLocOpt with
          | None -> false
          | Some (_, loc) ->
            let isInterface = full.file.uri |> Uri.isInterface in
            let posIsZero {Lexing.pos_lnum; pos_bol; pos_cnum} =
              (not isInterface) && pos_lnum = 1 && pos_cnum - pos_bol = 0
            in
            (* Skip if range is all zero, unless it's a module *)
            (not isModule) && posIsZero loc.loc_start && posIsZero loc.loc_end
        in
        if skipZero then None
        else Hover.newHover ~supportsMarkdownLinks ~full locItem)
  in
  result

let signatureHelp ~source ~kindFile ~pos ~allowForConstructorPayloads ~full
    ~debug =
  match
    SignatureHelp.signatureHelp ~debug ~source ~kindFile ~pos
      ~allowForConstructorPayloads ~full
  with
  | None ->
    {Protocol.signatures = []; activeSignature = None; activeParameter = None}
  | Some res -> res

let definition ~full ~pos ~debug =
  let locationOpt =
    match full with
    | None -> None
    | Some full -> (
      match References.getLocItem ~full ~pos ~debug with
      | None -> None
      | Some locItem -> (
        match References.definitionForLocItem ~full locItem with
        | None -> None
        | Some (uri, loc) when not loc.loc_ghost ->
          let isInterface = full.file.uri |> Uri.isInterface in
          let posIsZero {Lexing.pos_lnum; pos_bol; pos_cnum} =
            (* range is zero *)
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
        | Some _ -> None))
  in
  locationOpt

let typeDefinition ~full ~pos ~debug =
  let maybeLocation =
    match full with
    | None -> None
    | Some full -> (
      match References.getLocItem ~full ~pos ~debug with
      | None -> None
      | Some locItem -> (
        match References.typeDefinitionForLocItem ~full locItem with
        | None -> None
        | Some (uri, loc) ->
          Some
            {
              Protocol.uri = Files.canonicalizeUri uri;
              range = Utils.cmtLocToRange loc;
            }))
  in
  maybeLocation

let references ~full ~pos ~debug =
  let allLocs =
    match full with
    | None -> []
    | Some full -> (
      match References.getLocItem ~full ~pos ~debug with
      | None -> []
      | Some locItem ->
        let allReferences = References.allReferencesForLocItem ~full locItem in
        allReferences
        |> List.fold_left
             (fun acc {References.uri = uri2; locOpt} ->
               let loc =
                 match locOpt with
                 | Some loc -> loc
                 | None -> Uri.toTopLevelLoc uri2
               in

               {
                 Protocol.uri = Uri.toString uri2;
                 range = Utils.cmtLocToRange loc;
               }
               :: acc)
             [])
  in
  allLocs

let rename ~full ~pos ~newName ~debug =
  let result =
    match full with
    | None -> None
    | Some full -> (
      match References.getLocItem ~full ~pos ~debug with
      | None -> None
      | Some locItem ->
        let allReferences = References.allReferencesForLocItem ~full locItem in
        let referencesToToplevelModules =
          allReferences
          |> Utils.filterMap (fun {References.uri = uri2; locOpt} ->
                 if locOpt = None then Some uri2 else None)
        in
        let referencesToItems =
          allReferences
          |> Utils.filterMap (function
               | {References.uri = uri2; locOpt = Some loc} -> Some (uri2, loc)
               | {locOpt = None} -> None)
        in
        let fileRenames =
          referencesToToplevelModules
          |> List.map (fun uri ->
                 let path = Uri.toPath uri in
                 let dir = Filename.dirname path in
                 let newPath =
                   Filename.concat dir (newName ^ Filename.extension path)
                 in
                 let newUri = Uri.fromPath newPath in
                 Protocol.
                   {
                     oldUri = uri |> Uri.toString;
                     newUri = newUri |> Uri.toString;
                   })
        in
        let textDocumentEdits =
          let module StringMap = Misc.StringMap in
          let textEditsByUri =
            referencesToItems
            |> List.map (fun (uri, loc) -> (Uri.toString uri, loc))
            |> List.fold_left
                 (fun acc (uri, loc) ->
                   let textEdit =
                     Protocol.
                       {range = Utils.cmtLocToRange loc; newText = newName}
                   in
                   match StringMap.find_opt uri acc with
                   | None -> StringMap.add uri [textEdit] acc
                   | Some prevEdits ->
                     StringMap.add uri (textEdit :: prevEdits) acc)
                 StringMap.empty
          in
          StringMap.fold
            (fun uri edits acc ->
              let textDocumentEdit =
                Protocol.{textDocument = {uri; version = None}; edits}
              in
              textDocumentEdit :: acc)
            textEditsByUri []
        in
        Some (fileRenames, textDocumentEdits))
  in
  result

let prepareRename ~full ~pos ~debug =
  match full with
  | None -> None
  | Some full -> (
    match References.getLocItem ~full ~pos ~debug with
    | None -> None
    | Some locItem ->
      let range = Utils.cmtLocToRange locItem.loc in
      let placeholderOpt =
        match locItem.locType with
        | Typed (name, _, _) | TopLevelModule name | TypeDefinition (name, _, _)
          ->
          Some name
        | _ -> None
      in
      Some
        (match placeholderOpt with
        | None -> Protocol.Range range
        | Some placeholder -> Protocol.Placeholder {range; placeholder}))

let format ~source ~kindFile =
  let max = String.length source in
  let range =
    Protocol.
      {start = {line = 0; character = 0}; end_ = {line = max; character = max}}
  in

  let result =
    match kindFile with
    | Files.Res ->
      let {Res_driver.parsetree = structure; comments; diagnostics} =
        Res_driver.parsing_engine.parse_implementation_from_source
          ~for_printer:true ~source
      in
      if List.length diagnostics > 0 then Error "Document has syntax errors"
      else Ok (Res_printer.print_implementation ~comments structure)
    | Resi ->
      let {Res_driver.parsetree = signature; comments; diagnostics} =
        Res_driver.parsing_engine.parse_interface_from_source ~for_printer:true
          ~source
      in
      if List.length diagnostics > 0 then Error "Document has syntax errors"
      else Ok (Res_printer.print_interface ~comments signature)
    | Other -> Error "Failed to format, file not supported"
  in

  match result with
  | Ok newText -> Ok [Protocol.{range; newText}]
  | Error e -> Error e
