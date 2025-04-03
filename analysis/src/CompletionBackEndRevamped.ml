open SharedTypes

let resolveOpens = CompletionBackEnd.resolveOpens
let getOpens = CompletionBackEnd.getOpens

let findFields ~env ~package ~hint typ =
  match TypeUtils.extractRecordType ~env ~package typ with
  | None -> []
  | Some (_recordEnv, fields, decl) ->
    fields
    |> DotCompletionUtils.filterRecordFields ~env ~prefix:hint ~exact:false
         ~recordAsString:(decl.item.decl |> Shared.declToString decl.name.txt)

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
    | None -> []
    | Some typ -> (
      match kind with
      | Field {hint} -> findFields ~env ~package ~hint typ))
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
