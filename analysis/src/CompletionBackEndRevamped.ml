open SharedTypes

let resolveOpens = CompletionBackEnd.resolveOpens
let getOpens = CompletionBackEnd.getOpens

let findFields ~env ~package ~hint ?(seenFields = []) typ =
  match TypeUtils.extractRecordType ~env ~package typ with
  | None -> []
  | Some (_recordEnv, fields, decl) ->
    fields
    |> DotCompletionUtils.filterRecordFields ~env ~prefix:hint ~seenFields
         ~exact:false
         ~recordAsString:(decl.item.decl |> Shared.declToString decl.name.txt)

let findRecordField ~env ~package ~fieldName typ =
  match TypeUtils.extractRecordType ~env ~package typ with
  | None -> None
  | Some (_recordEnv, fields, _decl) ->
    fields |> List.find_opt (fun (field : field) -> field.fname.txt = fieldName)

type patternOrExpr = Pattern | Expr
let completeEmpty ~(mode : patternOrExpr) ~env ~package typ =
  ignore mode;
  match TypeUtils.extractType ~env ~package typ with
  | None ->
    if Debug.verbose () then
      print_endline "⚠️ Could not extract completable type";
    []
  | Some (completionType, typeArgContext) -> (
    (* Fill this out with the different completions *)
    match completionType with
    | Trecord _ ->
      [
        Completion.create ?typeArgContext "{}" ~includesSnippets:true
          ~insertText:"{$0}" ~sortText:"A" ~kind:(Value typ) ~env;
      ]
    | Tarray _ ->
      [
        Completion.create ?typeArgContext "[]" ~includesSnippets:true
          ~insertText:"[$0]" ~sortText:"A" ~kind:(Value typ) ~env;
      ]
    | Tbool _ ->
      [
        Completion.create ?typeArgContext "true" ~includesSnippets:true
          ~insertText:"true" ~sortText:"A" ~kind:(Value typ) ~env;
        Completion.create ?typeArgContext "false" ~includesSnippets:true
          ~insertText:"false" ~sortText:"A" ~kind:(Value typ) ~env;
      ]
    | Tstring _ ->
      [
        Completion.create ?typeArgContext "\"\"" ~includesSnippets:true
          ~insertText:"\"$0\"" ~sortText:"A" ~kind:(Value typ) ~env;
      ]
    | _ -> [])

let processCompletable ~debug ~full ~scope ~env ~pos
    (completable : CompletableRevamped.t) =
  let package = full.package in
  let rawOpens = Scope.getRawOpens scope in
  let opens = getOpens ~debug ~rawOpens ~package ~env in
  let allFiles = allFilesInPackage package in

  ignore pos;
  ignore opens;
  ignore allFiles;

  match completable with
  | Cexpression {kind; typeLoc} -> (
    match TypeUtils.findTypeViaLoc typeLoc ~full ~debug with
    | None ->
      if Debug.verbose () then print_endline "⚠️ No type found for loc";
      []
    | Some typ -> (
      if Debug.verbose () then
        print_endline ("✅ Found type at loc:" ^ Shared.typeToString typ);
      match kind with
      | Empty -> completeEmpty ~mode:Expr ~env ~package typ
      | Field {hint} -> findFields ~env ~package ~hint typ))
  | Cpattern {kind; typeLoc} -> (
    match TypeUtils.findTypeViaLoc typeLoc ~full ~debug with
    | None -> []
    | Some typ -> (
      match kind with
      | Empty -> completeEmpty ~mode:Pattern ~env ~package typ
      | Field {hint; seenFields} ->
        findFields ~env ~package ~hint ~seenFields typ
      | FieldValue {fieldName} -> (
        match findRecordField ~env ~package ~fieldName typ with
        | None -> []
        | Some field -> completeEmpty ~mode:Pattern ~env ~package field.typ)))
  | Cnone -> []
  | CextensionNode _ -> []
  | Cdecorator prefix ->
    let mkDecorator (name, docstring, maybeInsertText) =
      {
        (Completion.create name ~synthetic:true ~includesSnippets:true
           ~kind:(Label "") ~env ?insertText:maybeInsertText)
        with
        docstring;
      }
    in
    let isTopLevel = String.starts_with ~prefix:"@" prefix in
    let prefix =
      if isTopLevel then String.sub prefix 1 (String.length prefix - 1)
      else prefix
    in
    let decorators =
      if isTopLevel then CompletionDecorators.toplevel
      else CompletionDecorators.local
    in
    decorators
    |> List.filter (fun (decorator, _, _) -> Utils.startsWith decorator prefix)
    |> List.map (fun (decorator, maybeInsertText, doc) ->
           let parts = String.split_on_char '.' prefix in
           let len = String.length prefix in
           let dec2 =
             if List.length parts > 1 then
               String.sub decorator len (String.length decorator - len)
             else decorator
           in
           (dec2, doc, maybeInsertText))
    |> List.map mkDecorator
  | CdecoratorPayload _ -> []
  | Ccase _ -> []
