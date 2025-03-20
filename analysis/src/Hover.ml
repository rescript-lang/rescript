open SharedTypes

let showModuleTopLevel ~docstring ~isType ~name (topLevel : Module.item list) =
  let contents =
    topLevel
    |> List.map (fun item ->
           match item.Module.kind with
           (* TODO pretty print module contents *)
           | Type ({decl}, recStatus) ->
             "  " ^ (decl |> Shared.declToString ~recStatus item.name)
           | Module _ -> "  module " ^ item.name
           | Value typ ->
             "  let " ^ item.name ^ ": " ^ (typ |> Shared.typeToString))
    (* TODO indent *)
    |> String.concat "\n"
  in
  let name = Utils.cutAfterDash name in
  let full =
    Markdown.codeBlock
      ("module "
      ^ (if isType then "type " ^ name ^ " = " else name ^ ": ")
      ^ "{" ^ "\n" ^ contents ^ "\n}")
  in
  let doc =
    match docstring with
    | [] -> ""
    | _ :: _ -> "\n" ^ (docstring |> String.concat "\n") ^ "\n"
  in
  Some (doc ^ full)

let rec showModule ~docstring ~(file : File.t) ~package ~name
    (declared : Module.t Declared.t option) =
  match declared with
  | None ->
    showModuleTopLevel ~docstring ~isType:false ~name file.structure.items
  | Some {item = Structure {items}; modulePath} ->
    let isType =
      match modulePath with
      | ExportedModule {isType} -> isType
      | _ -> false
    in
    showModuleTopLevel ~docstring ~isType ~name items
  | Some ({item = Constraint (_moduleItem, moduleTypeItem)} as declared) ->
    (* show the interface *)
    showModule ~docstring ~file ~name ~package
      (Some {declared with item = moduleTypeItem})
  | Some ({item = Ident path} as declared) -> (
    match References.resolveModuleReference ~file ~package declared with
    | None -> Some ("Unable to resolve module reference " ^ Path.name path)
    | Some (_, declared) -> showModule ~docstring ~file ~name ~package declared)

type extractedType = {
  name: string;
  path: Path.t;
  decl: Types.type_declaration;
  env: SharedTypes.QueryEnv.t;
  loc: Warnings.loc;
}

let findRelevantTypesFromType ~file ~package typ =
  (* Expand definitions of types mentioned in typ.
     If typ itself is a record or variant, search its body *)
  let env = QueryEnv.fromFile file in
  let envToSearch, typesToSearch =
    match typ |> Shared.digConstructor with
    | Some path -> (
      let labelDeclarationsTypes lds =
        lds |> List.map (fun (ld : Types.label_declaration) -> ld.ld_type)
      in
      match References.digConstructor ~env ~package path with
      | None -> (env, [typ])
      | Some (env1, {item = {decl}}) -> (
        match decl.type_kind with
        | Type_record (lds, _) -> (env1, typ :: (lds |> labelDeclarationsTypes))
        | Type_variant cds ->
          ( env1,
            cds
            |> List.map (fun (cd : Types.constructor_declaration) ->
                   let fromArgs =
                     match cd.cd_args with
                     | Cstr_tuple ts -> ts
                     | Cstr_record lds -> lds |> labelDeclarationsTypes
                   in
                   typ
                   ::
                   (match cd.cd_res with
                   | None -> fromArgs
                   | Some t -> t :: fromArgs))
            |> List.flatten )
        | _ -> (env, [typ])))
    | None -> (env, [typ])
  in
  let fromConstructorPath ~env path =
    match References.digConstructor ~env ~package path with
    | None -> None
    | Some (env, {name = {txt}; extentLoc; item = {decl}}) ->
      if Utils.isUncurriedInternal path then None
      else Some {name = txt; env; loc = extentLoc; decl; path}
  in
  let constructors = Shared.findTypeConstructors typesToSearch in
  constructors |> List.filter_map (fromConstructorPath ~env:envToSearch)

let expandTypes ~file ~package ~supportsMarkdownLinks typ =
  match findRelevantTypesFromType typ ~file ~package with
  | {decl; path} :: _
    when Res_parsetree_viewer.has_inline_record_definition_attribute
           decl.type_attributes ->
    (* We print inline record types just with their definition, not the constr pointing 
    to them, since that doesn't make sense to show the user. *)
    ( [
        Markdown.codeBlock
          (decl
          |> Shared.declToString ~printNameAsIs:true
               (SharedTypes.pathIdentToString path));
      ],
      `InlineType )
  | all ->
    ( all
      |> List.map (fun {decl; env; loc; path} ->
             let linkToTypeDefinitionStr =
               if
                 supportsMarkdownLinks
                 && not
                      (Res_parsetree_viewer
                       .has_inline_record_definition_attribute
                         decl.type_attributes)
               then Markdown.goToDefinitionText ~env ~pos:loc.Warnings.loc_start
               else ""
             in
             Markdown.divider
             ^ (if supportsMarkdownLinks then Markdown.spacing else "")
             ^ Markdown.codeBlock
                 (decl
                 |> Shared.declToString ~printNameAsIs:true
                      (SharedTypes.pathIdentToString path))
             ^ linkToTypeDefinitionStr ^ "\n"),
      `Default )

(* Produces a hover with relevant types expanded in the main type being hovered. *)
let hoverWithExpandedTypes ~file ~package ~supportsMarkdownLinks typ =
  let typeString = Markdown.codeBlock (typ |> Shared.typeToString) in
  let expandedTypes, expansionType =
    expandTypes ~file ~package ~supportsMarkdownLinks typ
  in
  match expansionType with
  | `Default -> typeString :: expandedTypes |> String.concat "\n"
  | `InlineType -> expandedTypes |> String.concat "\n"

(* Leverages autocomplete functionality to produce a hover for a position. This
   makes it (most often) work with unsaved content. *)
let getHoverViaCompletions ~debug ~path ~pos ~currentFile ~forHover
    ~supportsMarkdownLinks =
  match Completions.getCompletions ~debug ~path ~pos ~currentFile ~forHover with
  | None -> None
  | Some (completions, ({file; package} as full), scope) -> (
    let rawOpens = Scope.getRawOpens scope in
    match completions with
    | {kind = Label typString; docstring} :: _ ->
      let parts =
        docstring
        @ if typString = "" then [] else [Markdown.codeBlock typString]
      in

      Some (Protocol.stringifyHover (String.concat "\n\n" parts))
    | {kind = Field _; env; docstring} :: _ -> (
      let opens = CompletionBackEnd.getOpens ~debug ~rawOpens ~package ~env in
      match
        CompletionBackEnd.completionsGetTypeEnv2 ~debug ~full ~rawOpens ~opens
          ~pos completions
      with
      | Some (typ, _env) ->
        let typeString =
          hoverWithExpandedTypes ~file ~package ~supportsMarkdownLinks typ
        in
        let parts = docstring @ [typeString] in
        Some (Protocol.stringifyHover (String.concat "\n\n" parts))
      | None -> None)
    | {env} :: _ -> (
      let opens = CompletionBackEnd.getOpens ~debug ~rawOpens ~package ~env in
      match
        CompletionBackEnd.completionsGetTypeEnv2 ~debug ~full ~rawOpens ~opens
          ~pos completions
      with
      | Some (typ, _env) ->
        let typeString =
          hoverWithExpandedTypes ~file ~package ~supportsMarkdownLinks typ
        in
        Some (Protocol.stringifyHover typeString)
      | None -> None)
    | _ -> None)

let newHover ~full:{file; package} ~supportsMarkdownLinks locItem =
  match locItem.locType with
  | TypeDefinition (name, decl, _stamp) -> (
    let typeDef = Markdown.codeBlock (Shared.declToString name decl) in
    match decl.type_manifest with
    | None -> Some typeDef
    | Some typ -> (
      let expandedTypes, expansionType =
        expandTypes ~file ~package ~supportsMarkdownLinks typ
      in
      match expansionType with
      | `Default -> Some (typeDef :: expandedTypes |> String.concat "\n")
      | `InlineType -> Some (expandedTypes |> String.concat "\n")))
  | LModule (Definition (stamp, _tip)) | LModule (LocalReference (stamp, _tip))
    -> (
    match Stamps.findModule file.stamps stamp with
    | None -> None
    | Some md -> (
      match References.resolveModuleReference ~file ~package md with
      | None -> None
      | Some (file, declared) ->
        let name, docstring =
          match declared with
          | Some d -> (d.name.txt, d.docstring)
          | None -> (file.moduleName, file.structure.docstring)
        in
        showModule ~docstring ~name ~file declared ~package))
  | LModule (GlobalReference (moduleName, path, tip)) -> (
    match ProcessCmt.fileForModule ~package moduleName with
    | None -> None
    | Some file -> (
      let env = QueryEnv.fromFile file in
      match References.exportedForTip ~env ~path ~package ~tip with
      | None -> None
      | Some (_env, _name, stamp) -> (
        match Stamps.findModule file.stamps stamp with
        | None -> None
        | Some md -> (
          match References.resolveModuleReference ~file ~package md with
          | None -> None
          | Some (file, declared) ->
            let name, docstring =
              match declared with
              | Some d -> (d.name.txt, d.docstring)
              | None -> (file.moduleName, file.structure.docstring)
            in
            showModule ~docstring ~name ~file ~package declared))))
  | LModule NotFound -> None
  | TopLevelModule name -> (
    match ProcessCmt.fileForModule ~package name with
    | None -> None
    | Some file ->
      showModule ~docstring:file.structure.docstring ~name:file.moduleName ~file
        ~package None)
  | Typed (_, _, Definition (_, (Field _ | Constructor _))) -> None
  | Constant t ->
    Some
      (Markdown.codeBlock
         (match t with
         | Const_int _ -> "int"
         | Const_char _ -> "char"
         | Const_string _ -> "string"
         | Const_float _ -> "float"
         | Const_int32 _ -> "int32"
         | Const_int64 _ -> "int64"
         | Const_bigint _ -> "bigint"))
  | Typed (_, t, locKind) ->
    let fromType ~docstring typ =
      ( hoverWithExpandedTypes ~file ~package ~supportsMarkdownLinks typ,
        docstring )
    in
    let parts =
      match References.definedForLoc ~file ~package locKind with
      | None ->
        let typeString, docstring = t |> fromType ~docstring:[] in
        typeString :: docstring
      | Some (docstring, res) -> (
        match res with
        | `Declared ->
          let typeString, docstring = t |> fromType ~docstring in
          typeString :: docstring
        | `Constructor {cname = {txt}; args; docstring} ->
          let typeString, docstring = t |> fromType ~docstring in
          let argsString =
            match args with
            | InlineRecord _ | Args [] -> ""
            | Args args ->
              args
              |> List.map (fun (t, _) -> Shared.typeToString t)
              |> String.concat ", " |> Printf.sprintf "(%s)"
          in
          typeString :: Markdown.codeBlock (txt ^ argsString) :: docstring
        | `Field ->
          let typeString, docstring = t |> fromType ~docstring in
          typeString :: docstring)
    in
    Some (String.concat Markdown.divider parts)
