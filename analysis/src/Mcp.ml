open SharedTypes

module StringSet = Set.Make (String)

module Utils = struct
  let wrapInTag content ~tag = Printf.sprintf "<%s>\n%s\n</%s>" tag content tag
  let wrapInTagOpt content ~tag =
    match content with
    | None -> None
    | Some content -> Some (wrapInTag content ~tag)
end

module LocInfo = struct
  let showModule = Hover.showModule

  (* LocInfo thoughts:
      - Check for long variants and/or records, and do not expand them automatically. DomProps being one example. *)

  let locInfo ~path ~pos =
    let debug = false in
    let result =
      match Cmt.loadFullCmtFromPath ~path with
      | None -> None
      | Some full -> (
        match References.getLocItem ~full ~pos ~debug with
        | None -> None
        | Some locItem -> (
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
          else
            let file = full.file in
            let package = full.package in
            match locItem.locType with
            | TypeDefinition
                (name, ({type_manifest = Some tmanifest} as decl), _stamp) ->
              Some
                (Shared.declToString name decl
                ^ "\n\n"
                ^ Shared.typeToString tmanifest)
            | TypeDefinition (name, decl, _stamp) ->
              Some (Shared.declToString name decl)
            | LModule (Definition (stamp, _tip))
            | LModule (LocalReference (stamp, _tip)) -> (
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
                    match
                      References.resolveModuleReference ~file ~package md
                    with
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
                showModule ~docstring:file.structure.docstring
                  ~name:file.moduleName ~file ~package None)
            | Typed (name, t, _) ->
              let {TypeUtils.ExpandType.mainTypes; relatedTypes} =
                TypeUtils.ExpandType.expandTypes
                  (TypeUtils.ExpandType.TypeExpr
                     {
                       typeExpr = t;
                       name = Some (Location.mkloc name locItem.loc);
                       env = QueryEnv.fromFile full.file;
                     })
                  ~full
              in
              Some
                (Printf.sprintf
                   "<main_types>\n\
                    %s\n\
                    </main_types>\n\n\
                    <related_types>\n\
                    %s\n\
                    </related_types>"
                   (mainTypes
                   |> List.map
                        (fun (input : TypeUtils.ExpandType.expandTypeInput) ->
                          match input with
                          | TypeUtils.ExpandType.TypeExpr {typeExpr} ->
                            Shared.typeToString typeExpr
                          | TypeUtils.ExpandType.TypeDecl {name; typeDecl} ->
                            Shared.declToString name.txt typeDecl)
                   |> String.concat "\n\n")
                   (relatedTypes
                   |> List.map
                        (fun (input : TypeUtils.ExpandType.expandTypeInput) ->
                          match input with
                          | TypeUtils.ExpandType.TypeExpr {typeExpr} ->
                            Shared.typeToString typeExpr
                          | TypeUtils.ExpandType.TypeDecl {name; typeDecl} ->
                            Shared.declToString name.txt typeDecl)
                   |> String.concat "\n\n"))
            | Constant t ->
              Some
                (match t with
                | Const_int _ -> "int"
                | Const_char _ -> "char"
                | Const_string _ -> "string"
                | Const_float _ -> "float"
                | Const_int32 _ -> "int32"
                | Const_int64 _ -> "int64"
                | Const_bigint _ -> "bigint")))
    in
    match result with
    | None -> "No result."
    | Some s -> s
end
