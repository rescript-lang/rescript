let filterRecordFields ~env ~recordAsString ~prefix ~exact fields =
  fields
  |> Utils.filterMap (fun (field : SharedTypes.field) ->
         if Utils.checkName field.fname.txt ~prefix ~exact then
           Some
             (SharedTypes.Completion.create field.fname.txt ~env
                ?deprecated:field.deprecated ~docstring:field.docstring
                ~kind:(SharedTypes.Completion.Field (field, recordAsString)))
         else None)

let fieldCompletionsForDotCompletion ?posOfDot typ ~env ~package ~prefix ~exact
    =
  let asObject = typ |> TypeUtils.extractObjectType ~env ~package in
  match asObject with
  | Some (objEnv, obj) ->
    (* Handle obj completion via dot *)
    if Debug.verbose () then
      Printf.printf "[dot_completion]--> Obj type found:\n";
    obj |> TypeUtils.getObjFields
    |> Utils.filterMap (fun (field, _typ) ->
           if Utils.checkName field ~prefix ~exact then
             let fullObjFieldName = Printf.sprintf "[\"%s\"]" field in
             Some
               (SharedTypes.Completion.create fullObjFieldName ~synthetic:true
                  ~insertText:fullObjFieldName ~env:objEnv
                  ~kind:(SharedTypes.Completion.ObjLabel typ)
                  ?additionalTextEdits:
                    (match posOfDot with
                    | None -> None
                    | Some posOfDot ->
                      Some
                        (TypeUtils.makeAdditionalTextEditsForRemovingDot
                           posOfDot)))
           else None)
  | None -> (
    match typ |> TypeUtils.extractRecordType ~env ~package with
    | Some (env, fields, typDecl) ->
      fields
      |> filterRecordFields ~env ~prefix ~exact
           ~recordAsString:
             (typDecl.item.decl |> Shared.declToString typDecl.name.txt)
    | None -> [])
