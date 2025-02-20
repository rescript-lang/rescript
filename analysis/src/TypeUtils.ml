open SharedTypes

let modulePathFromEnv env = env.QueryEnv.file.moduleName :: List.rev env.pathRev

let fullTypeIdFromDecl ~env ~name ~modulePath =
  env.QueryEnv.file.moduleName :: ModulePath.toPath modulePath name
  |> String.concat "."

let debugLogTypeArgContext {env; typeArgs; typeParams} =
  Printf.sprintf "Type arg context. env: %s, typeArgs: %s, typeParams: %s\n"
    (Debug.debugPrintEnv env)
    (typeArgs |> List.map Shared.typeToString |> String.concat ", ")
    (typeParams |> List.map Shared.typeToString |> String.concat ", ")

(** Checks whether this type has any uninstantiated type parameters. *)
let rec hasTvar (ty : Types.type_expr) : bool =
  match ty.desc with
  | Tvar _ -> true
  | Tarrow (_, ty1, ty2, _, _) -> hasTvar ty1 || hasTvar ty2
  | Ttuple tyl -> List.exists hasTvar tyl
  | Tconstr (_, tyl, _) -> List.exists hasTvar tyl
  | Tobject (ty, _) -> hasTvar ty
  | Tfield (_, _, ty1, ty2) -> hasTvar ty1 || hasTvar ty2
  | Tnil -> false
  | Tlink ty -> hasTvar ty
  | Tsubst ty -> hasTvar ty
  | Tvariant {row_fields; _} ->
    List.exists
      (function
        | _, Types.Rpresent (Some ty) -> hasTvar ty
        | _, Reither (_, tyl, _, _) -> List.exists hasTvar tyl
        | _ -> false)
      row_fields
  | Tunivar _ -> true
  | Tpoly (ty, tyl) -> hasTvar ty || List.exists hasTvar tyl
  | Tpackage (_, _, tyl) -> List.exists hasTvar tyl

let findTypeViaLoc ~full ~debug (loc : Location.t) =
  match References.getLocItem ~full ~pos:(Pos.ofLexing loc.loc_end) ~debug with
  | Some {locType = Typed (_, typExpr, _)} -> Some typExpr
  | _ -> None

let pathFromTypeExpr (t : Types.type_expr) =
  match t.desc with
  | Tconstr (path, _typeArgs, _)
  | Tlink {desc = Tconstr (path, _typeArgs, _)}
  | Tsubst {desc = Tconstr (path, _typeArgs, _)}
  | Tpoly ({desc = Tconstr (path, _typeArgs, _)}, []) ->
    Some path
  | _ -> None

let printRecordFromFields ?name (fields : field list) =
  (match name with
  | None -> ""
  | Some name -> "type " ^ name ^ " = ")
  ^ "{"
  ^ (fields
    |> List.map (fun f -> f.fname.txt ^ ": " ^ Shared.typeToString f.typ)
    |> String.concat ", ")
  ^ "}"

let rec extractedTypeToString ?(nameOnly = false) ?(inner = false) = function
  | Tuple (_, _, typ) | Tpolyvariant {typeExpr = typ} | Tfunction {typ} ->
    if inner then
      try typ |> pathFromTypeExpr |> Option.get |> SharedTypes.pathIdentToString
      with _ -> ""
    else Shared.typeToString typ
  | Trecord {definition; fields} ->
    let name =
      match definition with
      | `TypeExpr typ -> (
        try
          typ |> pathFromTypeExpr |> Option.get |> SharedTypes.pathIdentToString
        with _ -> "")
      | `NameOnly name -> name
    in
    if inner || nameOnly then name else printRecordFromFields ~name fields
  | Tbool _ -> "bool"
  | Tstring _ -> "string"
  | TtypeT _ -> "type t"
  | Tarray (_, TypeExpr innerTyp) ->
    "array<" ^ Shared.typeToString innerTyp ^ ">"
  | Tarray (_, ExtractedType innerTyp) ->
    "array<" ^ extractedTypeToString ~inner:true innerTyp ^ ">"
  | Toption (_, TypeExpr innerTyp) ->
    "option<" ^ Shared.typeToString innerTyp ^ ">"
  | Tresult {okType; errorType} ->
    "result<" ^ Shared.typeToString okType ^ ", "
    ^ Shared.typeToString errorType
    ^ ">"
  | Toption (_, ExtractedType innerTyp) ->
    "option<" ^ extractedTypeToString ~inner:true innerTyp ^ ">"
  | Tpromise (_, innerTyp) -> "promise<" ^ Shared.typeToString innerTyp ^ ">"
  | Tvariant {variantDecl; variantName} ->
    if inner || nameOnly then variantName
    else Shared.declToString variantName variantDecl
  | TinlineRecord {fields} -> printRecordFromFields fields
  | Texn _ -> "exn"

let getExtractedType maybeRes =
  match maybeRes with
  | None -> None
  | Some (extractedType, _) -> Some extractedType

let instantiateType ~typeParams ~typeArgs (t : Types.type_expr) =
  if typeParams = [] || typeArgs = [] then t
  else
    let rec applySub tp ta t =
      match (tp, ta) with
      | t1 :: tRest1, t2 :: tRest2 ->
        if t1 = t then t2 else applySub tRest1 tRest2 t
      | [], _ | _, [] -> t
    in
    let rec loop (t : Types.type_expr) =
      match t.desc with
      | Tlink t -> loop t
      | Tvar _ -> applySub typeParams typeArgs t
      | Tunivar _ -> t
      | Tconstr (path, args, memo) ->
        {t with desc = Tconstr (path, args |> List.map loop, memo)}
      | Tsubst t -> loop t
      | Tvariant rd -> {t with desc = Tvariant (rowDesc rd)}
      | Tnil -> t
      | Tarrow (lbl, t1, t2, c, arity) ->
        {t with desc = Tarrow (lbl, loop t1, loop t2, c, arity)}
      | Ttuple tl -> {t with desc = Ttuple (tl |> List.map loop)}
      | Tobject (t, r) -> {t with desc = Tobject (loop t, r)}
      | Tfield (n, k, t1, t2) -> {t with desc = Tfield (n, k, loop t1, loop t2)}
      | Tpoly (t, []) -> loop t
      | Tpoly (t, tl) -> {t with desc = Tpoly (loop t, tl |> List.map loop)}
      | Tpackage (p, l, tl) ->
        {t with desc = Tpackage (p, l, tl |> List.map loop)}
    and rowDesc (rd : Types.row_desc) =
      let row_fields =
        rd.row_fields |> List.map (fun (l, rf) -> (l, rowField rf))
      in
      let row_more = loop rd.row_more in
      let row_name =
        match rd.row_name with
        | None -> None
        | Some (p, tl) -> Some (p, tl |> List.map loop)
      in
      {rd with row_fields; row_more; row_name}
    and rowField (rf : Types.row_field) =
      match rf with
      | Rpresent None -> rf
      | Rpresent (Some t) -> Rpresent (Some (loop t))
      | Reither (b1, tl, b2, r) -> Reither (b1, tl |> List.map loop, b2, r)
      | Rabsent -> Rabsent
    in
    loop t

let instantiateType2 ?(typeArgContext : typeArgContext option)
    (t : Types.type_expr) =
  match typeArgContext with
  | None | Some {typeArgs = []} | Some {typeParams = []} -> t
  | Some {typeArgs; typeParams} ->
    let rec applySub tp ta name =
      match (tp, ta) with
      | {Types.desc = Tvar (Some varName)} :: tRest1, t2 :: tRest2 ->
        if varName = name then t2 else applySub tRest1 tRest2 name
      | _ :: tRest1, _ :: tRest2 -> applySub tRest1 tRest2 name
      | [], _ | _, [] -> t
    in

    let rec loop (t : Types.type_expr) =
      match t.desc with
      | Tlink t -> loop t
      | Tvar (Some name) -> applySub typeParams typeArgs name
      | Tvar _ -> t
      | Tunivar _ -> t
      | Tconstr (path, args, memo) ->
        {t with desc = Tconstr (path, args |> List.map loop, memo)}
      | Tsubst t -> loop t
      | Tvariant rd -> {t with desc = Tvariant (rowDesc rd)}
      | Tnil -> t
      | Tarrow (lbl, t1, t2, c, arity) ->
        {t with desc = Tarrow (lbl, loop t1, loop t2, c, arity)}
      | Ttuple tl -> {t with desc = Ttuple (tl |> List.map loop)}
      | Tobject (t, r) -> {t with desc = Tobject (loop t, r)}
      | Tfield (n, k, t1, t2) -> {t with desc = Tfield (n, k, loop t1, loop t2)}
      | Tpoly (t, []) -> loop t
      | Tpoly (t, tl) -> {t with desc = Tpoly (loop t, tl |> List.map loop)}
      | Tpackage (p, l, tl) ->
        {t with desc = Tpackage (p, l, tl |> List.map loop)}
    and rowDesc (rd : Types.row_desc) =
      let row_fields =
        rd.row_fields |> List.map (fun (l, rf) -> (l, rowField rf))
      in
      let row_more = loop rd.row_more in
      let row_name =
        match rd.row_name with
        | None -> None
        | Some (p, tl) -> Some (p, tl |> List.map loop)
      in
      {rd with row_fields; row_more; row_name}
    and rowField (rf : Types.row_field) =
      match rf with
      | Rpresent None -> rf
      | Rpresent (Some t) -> Rpresent (Some (loop t))
      | Reither (b1, tl, b2, r) -> Reither (b1, tl |> List.map loop, b2, r)
      | Rabsent -> Rabsent
    in
    loop t

let rec extractRecordType ~env ~package (t : Types.type_expr) =
  match t.desc with
  | Tlink t1 | Tsubst t1 | Tpoly (t1, []) -> extractRecordType ~env ~package t1
  | Tconstr (path, typeArgs, _) -> (
    match References.digConstructor ~env ~package path with
    | Some (env, ({item = {kind = Record fields}} as typ)) ->
      let typeParams = typ.item.decl.type_params in
      let fields =
        fields
        |> List.map (fun field ->
               let fieldTyp =
                 field.typ |> instantiateType ~typeParams ~typeArgs
               in
               {field with typ = fieldTyp})
      in
      Some (env, fields, typ)
    | Some
        ( env,
          {item = {decl = {type_manifest = Some t1; type_params = typeParams}}}
        ) ->
      let t1 = t1 |> instantiateType ~typeParams ~typeArgs in
      extractRecordType ~env ~package t1
    | _ -> None)
  | _ -> None

let rec extractObjectType ~env ~package (t : Types.type_expr) =
  match t.desc with
  | Tlink t1 | Tsubst t1 | Tpoly (t1, []) -> extractObjectType ~env ~package t1
  | Tobject (tObj, _) -> Some (env, tObj)
  | Tconstr (path, typeArgs, _) -> (
    match References.digConstructor ~env ~package path with
    | Some
        ( env,
          {item = {decl = {type_manifest = Some t1; type_params = typeParams}}}
        ) ->
      let t1 = t1 |> instantiateType ~typeParams ~typeArgs in
      extractObjectType ~env ~package t1
    | _ -> None)
  | _ -> None

let extractFunctionType ~env ~package ?(digInto = true) typ =
  let rec loop ~env acc (t : Types.type_expr) =
    match t.desc with
    | Tlink t1 | Tsubst t1 | Tpoly (t1, []) -> loop ~env acc t1
    | Tarrow (label, tArg, tRet, _, _) -> loop ~env ((label, tArg) :: acc) tRet
    | Tconstr (path, typeArgs, _) when digInto -> (
      match References.digConstructor ~env ~package path with
      | Some
          ( env,
            {
              item = {decl = {type_manifest = Some t1; type_params = typeParams}};
            } ) ->
        let t1 = t1 |> instantiateType ~typeParams ~typeArgs in
        loop ~env acc t1
      | _ -> (List.rev acc, t))
    | _ -> (List.rev acc, t)
  in
  loop ~env [] typ

let extractFunctionTypeWithEnv ~env ~package typ =
  let rec loop ~env acc (t : Types.type_expr) =
    match t.desc with
    | Tlink t1 | Tsubst t1 | Tpoly (t1, []) -> loop ~env acc t1
    | Tarrow (label, tArg, tRet, _, _) -> loop ~env ((label, tArg) :: acc) tRet
    | Tconstr (path, typeArgs, _) -> (
      match References.digConstructor ~env ~package path with
      | Some
          ( _env,
            {
              item = {decl = {type_manifest = Some t1; type_params = typeParams}};
            } ) ->
        let t1 = t1 |> instantiateType ~typeParams ~typeArgs in
        loop ~env acc t1
      | Some _ -> (List.rev acc, t, env)
      | _ -> (List.rev acc, t, env))
    | _ -> (List.rev acc, t, env)
  in
  loop ~env [] typ

let maybeSetTypeArgCtx ?typeArgContextFromTypeManifest ~typeParams ~typeArgs env
    =
  match typeArgContextFromTypeManifest with
  | Some typeArgContextFromTypeManifest -> Some typeArgContextFromTypeManifest
  | None ->
    let typeArgContext =
      if List.length typeParams > 0 then Some {env; typeParams; typeArgs}
      else None
    in
    (match typeArgContext with
    | None -> ()
    | Some typeArgContext ->
      if Debug.verbose () then
        Printf.printf "[#type_arg_ctx]--> setting new type arg ctx: %s"
          (debugLogTypeArgContext typeArgContext));
    typeArgContext

(* TODO(env-stuff) Maybe this could be removed entirely if we can guarantee that we don't have to look up functions from in here. *)
let extractFunctionType2 ?typeArgContext ~env ~package typ =
  let rec loop ?typeArgContext ~env acc (t : Types.type_expr) =
    match t.desc with
    | Tlink t1 | Tsubst t1 | Tpoly (t1, []) -> loop ?typeArgContext ~env acc t1
    | Tarrow (label, tArg, tRet, _, _) ->
      loop ?typeArgContext ~env ((label, tArg) :: acc) tRet
    | Tconstr (path, typeArgs, _) -> (
      match References.digConstructor ~env ~package path with
      | Some
          ( env,
            {
              item = {decl = {type_manifest = Some t1; type_params = typeParams}};
            } ) ->
        let typeArgContext = maybeSetTypeArgCtx ~typeParams ~typeArgs env in
        loop ?typeArgContext ~env acc t1
      | _ -> (List.rev acc, t, typeArgContext))
    | _ -> (List.rev acc, t, typeArgContext)
  in
  loop ?typeArgContext ~env [] typ

let rec extractType ?(printOpeningDebug = true)
    ?(typeArgContext : typeArgContext option)
    ?(typeArgContextFromTypeManifest : typeArgContext option) ~env ~package
    (t : Types.type_expr) =
  let maybeSetTypeArgCtx = maybeSetTypeArgCtx ?typeArgContextFromTypeManifest in
  if Debug.verbose () && printOpeningDebug then
    Printf.printf
      "[extract_type]--> starting extraction of type: %s, in env: %s. Has type \
       arg ctx: %b\n"
      (Shared.typeToString t) (Debug.debugPrintEnv env)
      (Option.is_some typeArgContext);
  (match typeArgContext with
  | None -> ()
  | Some typeArgContext ->
    if Debug.verbose () && printOpeningDebug then
      Printf.printf "[extract_type]--> %s"
        (debugLogTypeArgContext typeArgContext));
  let instantiateType = instantiateType2 in
  match t.desc with
  | Tlink t1 | Tsubst t1 | Tpoly (t1, []) ->
    extractType ?typeArgContext ~printOpeningDebug:false ~env ~package t1
  | Tconstr (Path.Pident {name = "option"}, [payloadTypeExpr], _) ->
    Some (Toption (env, TypeExpr payloadTypeExpr), typeArgContext)
  | Tconstr (Path.Pident {name = "promise"}, [payloadTypeExpr], _) ->
    Some (Tpromise (env, payloadTypeExpr), typeArgContext)
  | Tconstr (Path.Pident {name = "array"}, [payloadTypeExpr], _) ->
    Some (Tarray (env, TypeExpr payloadTypeExpr), typeArgContext)
  | Tconstr (Path.Pident {name = "result"}, [okType; errorType], _) ->
    Some (Tresult {env; okType; errorType}, typeArgContext)
  | Tconstr (Path.Pident {name = "bool"}, [], _) ->
    Some (Tbool env, typeArgContext)
  | Tconstr (Path.Pident {name = "string"}, [], _) ->
    Some (Tstring env, typeArgContext)
  | Tconstr (Path.Pident {name = "exn"}, [], _) ->
    Some (Texn env, typeArgContext)
  | Tarrow _ -> (
    match extractFunctionType2 ?typeArgContext t ~env ~package with
    | args, tRet, typeArgContext when args <> [] ->
      Some
        ( Tfunction {env; args; typ = t; uncurried = false; returnType = tRet},
          typeArgContext )
    | _args, _tRet, _typeArgContext -> None)
  | Tconstr (path, typeArgs, _) -> (
    if Debug.verbose () then
      Printf.printf "[extract_type]--> digging for type %s in %s\n"
        (Path.name path) (Debug.debugPrintEnv env);
    match References.digConstructor ~env ~package path with
    | Some
        ( envFromDeclaration,
          {item = {decl = {type_manifest = Some t1; type_params}}} ) ->
      if Debug.verbose () then
        print_endline "[extract_type]--> found type manifest";

      (* Type manifests inherit the last type args ctx that wasn't for a type manifest.
         This is because the manifest itself doesn't have type args and an env that can
         be used to instantiate. *)
      let typeArgContext =
        maybeSetTypeArgCtx ~typeParams:type_params ~typeArgs env
      in
      t1
      |> extractType ?typeArgContextFromTypeManifest:typeArgContext
           ~env:envFromDeclaration ~package
    | Some (envFromItem, {name; item = {decl; kind = Type.Variant constructors}})
      ->
      if Debug.verbose () then print_endline "[extract_type]--> found variant";
      let typeArgContext =
        maybeSetTypeArgCtx ~typeParams:decl.type_params ~typeArgs env
      in
      Some
        ( Tvariant
            {
              env = envFromItem;
              constructors;
              variantName = name.txt;
              variantDecl = decl;
            },
          typeArgContext )
    | Some (envFromDeclaration, {item = {kind = Record fields; decl}}) ->
      if Debug.verbose () then print_endline "[extract_type]--> found record";
      (* Need to create a new type arg context here because we're sending along a type expr that might have type vars. *)
      let typeArgContext =
        maybeSetTypeArgCtx ~typeParams:decl.type_params ~typeArgs env
      in
      Some
        ( Trecord {env = envFromDeclaration; fields; definition = `TypeExpr t},
          typeArgContext )
    | Some (envFromDeclaration, {item = {name = "t"; decl = {type_params}}}) ->
      let typeArgContext =
        maybeSetTypeArgCtx ~typeParams:type_params ~typeArgs env
      in
      Some (TtypeT {env = envFromDeclaration; path}, typeArgContext)
    | None ->
      if Debug.verbose () then
        print_endline "[extract_type]--> found nothing when digging";
      None
    | _ ->
      if Debug.verbose () then
        print_endline "[extract_type]--> found something else when digging";
      None)
  | Ttuple expressions -> Some (Tuple (env, expressions, t), typeArgContext)
  | Tvariant {row_fields} ->
    let constructors =
      row_fields
      |> List.map (fun (label, field) ->
             {
               name = label;
               displayName = Utils.printMaybeExoticIdent ~allowUident:true label;
               args =
                 (* Multiple arguments are represented as a Ttuple, while a single argument is just the type expression itself. *)
                 (match field with
                 | Types.Rpresent (Some typeExpr) -> (
                   match typeExpr.desc with
                   | Ttuple args -> args
                   | _ -> [typeExpr])
                 | _ -> []);
             })
    in
    Some (Tpolyvariant {env; constructors; typeExpr = t}, typeArgContext)
  | Tvar (Some varName) -> (
    if Debug.verbose () then
      Printf.printf
        "[extract_type]--> found type variable: '%s. Trying to instantiate %s"
        varName
        (match typeArgContext with
        | None -> "with no type args ctx\n"
        | Some typeArgContext ->
          Printf.sprintf "with %s" (debugLogTypeArgContext typeArgContext));

    let instantiated = t |> instantiateType ?typeArgContext in
    let rec extractInstantiated t =
      match t.Types.desc with
      | Tlink t1 | Tsubst t1 | Tpoly (t1, []) -> extractInstantiated t1
      | _ -> t
    in
    match extractInstantiated instantiated with
    | {desc = Tvar _} ->
      if Debug.verbose () then
        Printf.printf "[extract_type]--> could not instantiate '%s. Skipping.\n"
          varName;
      None
    | _ ->
      if Debug.verbose () then
        Printf.printf
          "[extract_type]--> SUCCEEDED instantiation, new type is: %s\n"
          (Shared.typeToString instantiated);

      (* Use the env from instantiation if we managed to instantiate the type param *)
      let nextEnv =
        match typeArgContext with
        | Some {env} -> env
        | None -> env
      in
      instantiated |> extractType ?typeArgContext ~env:nextEnv ~package)
  | _ ->
    if Debug.verbose () then print_endline "[extract_type]--> miss";
    None

let isFunctionType ~env ~package t =
  match extractType ~env ~package t with
  | Some (Tfunction _, _) -> true
  | _ -> false

let findReturnTypeOfFunctionAtLoc loc ~(env : QueryEnv.t) ~full ~debug =
  match References.getLocItem ~full ~pos:(loc |> Loc.end_) ~debug with
  | Some {locType = Typed (_, typExpr, _)} -> (
    match extractFunctionType ~env ~package:full.package typExpr with
    | args, tRet when args <> [] -> Some tRet
    | _ -> None)
  | _ -> None

let rec digToRelevantTemplateNameType ~env ~package ?(suffix = "")
    (t : Types.type_expr) =
  match t.desc with
  | Tlink t1 | Tsubst t1 | Tpoly (t1, []) ->
    digToRelevantTemplateNameType ~suffix ~env ~package t1
  | Tconstr (Path.Pident {name = "option"}, [t1], _) ->
    digToRelevantTemplateNameType ~suffix ~env ~package t1
  | Tconstr (Path.Pident {name = "array"}, [t1], _) ->
    digToRelevantTemplateNameType ~suffix:"s" ~env ~package t1
  | Tconstr (path, _, _) -> (
    match References.digConstructor ~env ~package path with
    | Some (env, {item = {decl = {type_manifest = Some typ}}}) ->
      digToRelevantTemplateNameType ~suffix ~env ~package typ
    | _ -> (t, suffix, env))
  | _ -> (t, suffix, env)

let rec resolveTypeForPipeCompletion ~env ~package ~lhsLoc ~full
    (t : Types.type_expr) =
  (* If the type we're completing on is a type parameter, we won't be able to
     do completion unless we know what that type parameter is compiled as.
     This attempts to look up the compiled type for that type parameter by
     looking for compiled information at the loc of that expression. *)
  let typFromLoc =
    match t with
    | {Types.desc = Tvar _} ->
      findReturnTypeOfFunctionAtLoc lhsLoc ~env ~full ~debug:false
    | _ -> None
  in
  match typFromLoc with
  | Some typFromLoc ->
    typFromLoc |> resolveTypeForPipeCompletion ~lhsLoc ~env ~package ~full
  | None ->
    let rec digToRelevantType ~env ~package (t : Types.type_expr) =
      match t.desc with
      | Tlink t1 | Tsubst t1 | Tpoly (t1, []) ->
        digToRelevantType ~env ~package t1
      (* Don't descend into types named "t". Type t is a convention in the ReScript ecosystem. *)
      | Tconstr (path, _, _) when path |> Path.last = "t" -> (env, t)
      | Tconstr (path, _, _) -> (
        match References.digConstructor ~env ~package path with
        | Some (env, {item = {decl = {type_manifest = Some typ}}}) ->
          digToRelevantType ~env ~package typ
        | _ -> (env, t))
      | _ -> (env, t)
    in
    digToRelevantType ~env ~package t

let extractTypeFromResolvedType (typ : Type.t) ~env ~full =
  match typ.kind with
  | Tuple items -> Some (Tuple (env, items, Ctype.newty (Ttuple items)))
  | Record fields ->
    Some (Trecord {env; fields; definition = `NameOnly typ.name})
  | Variant constructors ->
    Some
      (Tvariant
         {env; constructors; variantName = typ.name; variantDecl = typ.decl})
  | Abstract _ | Open -> (
    match typ.decl.type_manifest with
    | None -> None
    | Some t -> t |> extractType ~env ~package:full.package |> getExtractedType)

(** The context we just came from as we resolve the nested structure. *)
type ctx = Rfield of string  (** A record field of name *)

let rec resolveNested ?typeArgContext ~env ~full ~nested ?ctx
    (typ : completionType) =
  let extractType = extractType ?typeArgContext in
  if Debug.verbose () then
    Printf.printf
      "[nested]--> running nested in env: %s. Has type arg ctx: %b\n"
      (Debug.debugPrintEnv env)
      (Option.is_some typeArgContext);
  (match typeArgContext with
  | None -> ()
  | Some typeArgContext ->
    if Debug.verbose () then
      Printf.printf "[nested]--> %s" (debugLogTypeArgContext typeArgContext));
  match nested with
  | [] ->
    if Debug.verbose () then
      print_endline "[nested]--> reached end of pattern, returning type";
    Some
      ( typ,
        env,
        (match ctx with
        | None -> None
        | Some (Rfield fieldName) ->
          Some (Completable.CameFromRecordField fieldName)),
        typeArgContext )
  | patternPath :: nested -> (
    match (patternPath, typ) with
    | Completable.NTupleItem {itemNum}, Tuple (env, tupleItems, _) -> (
      if Debug.verbose () then
        print_endline "[nested]--> trying to move into tuple";
      match List.nth_opt tupleItems itemNum with
      | None ->
        if Debug.verbose () then
          print_endline "[nested]--> tuple element not found";
        None
      | Some typ ->
        typ
        |> extractType ~env ~package:full.package
        |> Utils.Option.flatMap (fun (typ, typeArgContext) ->
               typ |> resolveNested ?typeArgContext ~env ~full ~nested))
    | ( NFollowRecordField {fieldName},
        (TinlineRecord {env; fields} | Trecord {env; fields}) ) -> (
      if Debug.verbose () then
        print_endline "[nested]--> trying to move into record field";
      match
        fields
        |> List.find_opt (fun (field : field) -> field.fname.txt = fieldName)
      with
      | None ->
        if Debug.verbose () then
          print_endline "[nested]--> did not find record field";
        None
      | Some {typ; optional} ->
        if Debug.verbose () then
          print_endline "[nested]--> found record field type";
        let typ = if optional then Utils.unwrapIfOption typ else typ in

        if Debug.verbose () then
          Printf.printf "[nested]--> extracting from type %s in env %s\n"
            (Shared.typeToString typ) (Debug.debugPrintEnv env);
        typ
        |> extractType ~env ~package:full.package
        |> Utils.Option.flatMap (fun (typ, typeArgContext) ->
               typ
               |> resolveNested ?typeArgContext ~ctx:(Rfield fieldName) ~env
                    ~full ~nested))
    | NRecordBody {seenFields}, Trecord {env; definition = `TypeExpr typeExpr}
      ->
      typeExpr
      |> extractType ~env ~package:full.package
      |> Option.map (fun (typ, typeArgContext) ->
             ( typ,
               env,
               Some (Completable.RecordField {seenFields}),
               typeArgContext ))
    | ( NRecordBody {seenFields},
        (Trecord {env; definition = `NameOnly _} as extractedType) ) ->
      Some
        ( extractedType,
          env,
          Some (Completable.RecordField {seenFields}),
          typeArgContext )
    | NRecordBody {seenFields}, TinlineRecord {env; fields} ->
      Some
        ( TinlineRecord {fields; env},
          env,
          Some (Completable.RecordField {seenFields}),
          typeArgContext )
    | ( NVariantPayload {constructorName = "Some"; itemNum = 0},
        Toption (env, ExtractedType typ) ) ->
      if Debug.verbose () then
        print_endline "[nested]--> moving into option Some";
      typ |> resolveNested ?typeArgContext ~env ~full ~nested
    | ( NVariantPayload {constructorName = "Some"; itemNum = 0},
        Toption (env, TypeExpr typ) ) ->
      if Debug.verbose () then
        print_endline "[nested]--> moving into option Some";
      typ
      |> extractType ~env ~package:full.package
      |> Utils.Option.flatMap (fun (t, typeArgContext) ->
             t |> resolveNested ?typeArgContext ~env ~full ~nested)
    | NVariantPayload {constructorName = "Ok"; itemNum = 0}, Tresult {okType} ->
      if Debug.verbose () then print_endline "[nested]--> moving into result Ok";
      okType
      |> extractType ~env ~package:full.package
      |> Utils.Option.flatMap (fun (t, typeArgContext) ->
             t |> resolveNested ?typeArgContext ~env ~full ~nested)
    | ( NVariantPayload {constructorName = "Error"; itemNum = 0},
        Tresult {errorType} ) ->
      if Debug.verbose () then
        print_endline "[nested]--> moving into result Error";
      errorType
      |> extractType ~env ~package:full.package
      |> Utils.Option.flatMap (fun (t, typeArgContext) ->
             t |> resolveNested ?typeArgContext ~env ~full ~nested)
    | NVariantPayload {constructorName; itemNum}, Tvariant {env; constructors}
      -> (
      if Debug.verbose () then
        Printf.printf
          "[nested]--> trying to move into variant payload $%i of constructor \
           '%s'\n"
          itemNum constructorName;
      match
        constructors
        |> List.find_opt (fun (c : Constructor.t) ->
               c.cname.txt = constructorName)
      with
      | Some {args = Args args} -> (
        if Debug.verbose () then
          print_endline "[nested]--> found constructor (Args type)";
        match List.nth_opt args itemNum with
        | None ->
          if Debug.verbose () then
            print_endline "[nested]--> did not find relevant args num";
          None
        | Some (typ, _) ->
          if Debug.verbose () then
            Printf.printf "[nested]--> found arg of type: %s\n"
              (Shared.typeToString typ);

          typ
          |> extractType ~env ~package:full.package
          |> Utils.Option.flatMap (fun (typ, typeArgContext) ->
                 if Debug.verbose () then
                   Printf.printf
                     "[nested]--> extracted %s, continuing descent of %i items\n"
                     (extractedTypeToString typ)
                     (List.length nested);
                 typ |> resolveNested ?typeArgContext ~env ~full ~nested))
      | Some {args = InlineRecord fields} when itemNum = 0 ->
        if Debug.verbose () then
          print_endline "[nested]--> found constructor (inline record)";
        TinlineRecord {env; fields}
        |> resolveNested ?typeArgContext ~env ~full ~nested
      | _ -> None)
    | ( NPolyvariantPayload {constructorName; itemNum},
        Tpolyvariant {env; constructors} ) -> (
      match
        constructors
        |> List.find_opt (fun (c : polyVariantConstructor) ->
               c.name = constructorName)
      with
      | None -> None
      | Some constructor -> (
        match List.nth_opt constructor.args itemNum with
        | None -> None
        | Some typ ->
          typ
          |> extractType ~env ~package:full.package
          |> Utils.Option.flatMap (fun (typ, typeArgContext) ->
                 typ |> resolveNested ?typeArgContext ~env ~full ~nested)))
    | NArray, Tarray (env, ExtractedType typ) ->
      typ |> resolveNested ?typeArgContext ~env ~full ~nested
    | NArray, Tarray (env, TypeExpr typ) ->
      typ
      |> extractType ~env ~package:full.package
      |> Utils.Option.flatMap (fun (typ, typeArgContext) ->
             typ |> resolveNested ?typeArgContext ~env ~full ~nested)
    | _ -> None)

let findTypeOfRecordField fields ~fieldName =
  match
    fields |> List.find_opt (fun (field : field) -> field.fname.txt = fieldName)
  with
  | None -> None
  | Some {typ; optional} ->
    let typ = if optional then Utils.unwrapIfOption typ else typ in
    Some typ

let findTypeOfConstructorArg constructors ~constructorName ~payloadNum ~env =
  match
    constructors
    |> List.find_opt (fun (c : Constructor.t) -> c.cname.txt = constructorName)
  with
  | Some {args = Args args} -> (
    match List.nth_opt args payloadNum with
    | None -> None
    | Some (typ, _) -> Some (TypeExpr typ))
  | Some {args = InlineRecord fields} when payloadNum = 0 ->
    Some (ExtractedType (TinlineRecord {env; fields}))
  | _ -> None

let findTypeOfPolyvariantArg constructors ~constructorName ~payloadNum =
  match
    constructors
    |> List.find_opt (fun (c : polyVariantConstructor) ->
           c.name = constructorName)
  with
  | Some {args} -> (
    match List.nth_opt args payloadNum with
    | None -> None
    | Some typ -> Some typ)
  | None -> None

let rec resolveNestedPatternPath (typ : innerType) ~env ~full ~nested =
  if Debug.verbose () then print_endline "[nested_pattern_path]";
  let t =
    match typ with
    | TypeExpr t ->
      t |> extractType ~env ~package:full.package |> getExtractedType
    | ExtractedType t -> Some t
  in
  match nested with
  | [] -> None
  | [finalPatternPath] -> (
    match t with
    | None -> None
    | Some completionType -> (
      match (finalPatternPath, completionType) with
      | ( Completable.NFollowRecordField {fieldName},
          (TinlineRecord {fields} | Trecord {fields}) ) -> (
        match fields |> findTypeOfRecordField ~fieldName with
        | None -> None
        | Some typ -> Some (TypeExpr typ, env))
      | NTupleItem {itemNum}, Tuple (env, tupleItems, _) -> (
        match List.nth_opt tupleItems itemNum with
        | None -> None
        | Some typ -> Some (TypeExpr typ, env))
      | NVariantPayload {constructorName; itemNum}, Tvariant {env; constructors}
        -> (
        match
          constructors
          |> findTypeOfConstructorArg ~constructorName ~payloadNum:itemNum ~env
        with
        | Some typ -> Some (typ, env)
        | None -> None)
      | ( NPolyvariantPayload {constructorName; itemNum},
          Tpolyvariant {env; constructors} ) -> (
        match
          constructors
          |> findTypeOfPolyvariantArg ~constructorName ~payloadNum:itemNum
        with
        | Some typ -> Some (TypeExpr typ, env)
        | None -> None)
      | ( NVariantPayload {constructorName = "Some"; itemNum = 0},
          Toption (env, typ) ) ->
        Some (typ, env)
      | ( NVariantPayload {constructorName = "Ok"; itemNum = 0},
          Tresult {env; okType} ) ->
        Some (TypeExpr okType, env)
      | ( NVariantPayload {constructorName = "Error"; itemNum = 0},
          Tresult {env; errorType} ) ->
        Some (TypeExpr errorType, env)
      | NArray, Tarray (env, typ) -> Some (typ, env)
      | _ -> None))
  | patternPath :: nested -> (
    match t with
    | None -> None
    | Some completionType -> (
      match (patternPath, completionType) with
      | ( Completable.NFollowRecordField {fieldName},
          (TinlineRecord {env; fields} | Trecord {env; fields}) ) -> (
        match fields |> findTypeOfRecordField ~fieldName with
        | None -> None
        | Some typ ->
          typ
          |> extractType ~env ~package:full.package
          |> getExtractedType
          |> Utils.Option.flatMap (fun typ ->
                 ExtractedType typ
                 |> resolveNestedPatternPath ~env ~full ~nested))
      | NTupleItem {itemNum}, Tuple (env, tupleItems, _) -> (
        match List.nth_opt tupleItems itemNum with
        | None -> None
        | Some typ ->
          typ
          |> extractType ~env ~package:full.package
          |> getExtractedType
          |> Utils.Option.flatMap (fun typ ->
                 ExtractedType typ
                 |> resolveNestedPatternPath ~env ~full ~nested))
      | NVariantPayload {constructorName; itemNum}, Tvariant {env; constructors}
        -> (
        match
          constructors
          |> findTypeOfConstructorArg ~constructorName ~payloadNum:itemNum ~env
        with
        | Some typ -> typ |> resolveNestedPatternPath ~env ~full ~nested
        | None -> None)
      | ( NPolyvariantPayload {constructorName; itemNum},
          Tpolyvariant {env; constructors} ) -> (
        match
          constructors
          |> findTypeOfPolyvariantArg ~constructorName ~payloadNum:itemNum
        with
        | Some typ ->
          TypeExpr typ |> resolveNestedPatternPath ~env ~full ~nested
        | None -> None)
      | ( NVariantPayload {constructorName = "Some"; itemNum = 0},
          Toption (env, typ) ) ->
        typ |> resolveNestedPatternPath ~env ~full ~nested
      | ( NVariantPayload {constructorName = "Ok"; itemNum = 0},
          Tresult {env; okType} ) ->
        TypeExpr okType |> resolveNestedPatternPath ~env ~full ~nested
      | ( NVariantPayload {constructorName = "Error"; itemNum = 0},
          Tresult {env; errorType} ) ->
        TypeExpr errorType |> resolveNestedPatternPath ~env ~full ~nested
      | NArray, Tarray (env, typ) ->
        typ |> resolveNestedPatternPath ~env ~full ~nested
      | _ -> None))

let getArgs ~env (t : Types.type_expr) ~full =
  let rec getArgsLoop ~env (t : Types.type_expr) ~full ~currentArgumentPosition
      =
    match t.desc with
    | Tlink t1 | Tsubst t1 | Tpoly (t1, []) ->
      getArgsLoop ~full ~env ~currentArgumentPosition t1
    | Tarrow (Labelled l, tArg, tRet, _, _) ->
      (SharedTypes.Completable.Labelled l, tArg)
      :: getArgsLoop ~full ~env ~currentArgumentPosition tRet
    | Tarrow (Optional l, tArg, tRet, _, _) ->
      (Optional l, tArg) :: getArgsLoop ~full ~env ~currentArgumentPosition tRet
    | Tarrow (Nolabel, tArg, tRet, _, _) ->
      (Unlabelled {argumentPosition = currentArgumentPosition}, tArg)
      :: getArgsLoop ~full ~env
           ~currentArgumentPosition:(currentArgumentPosition + 1)
           tRet
    | Tconstr (path, typeArgs, _) -> (
      match References.digConstructor ~env ~package:full.package path with
      | Some
          ( env,
            {
              item = {decl = {type_manifest = Some t1; type_params = typeParams}};
            } ) ->
        let t1 = t1 |> instantiateType ~typeParams ~typeArgs in
        getArgsLoop ~full ~env ~currentArgumentPosition t1
      | _ -> [])
    | _ -> []
  in
  t |> getArgsLoop ~env ~full ~currentArgumentPosition:0

let typeIsUnit (typ : Types.type_expr) =
  match typ.desc with
  | Tconstr (Pident id, _typeArgs, _)
  | Tlink {desc = Tconstr (Pident id, _typeArgs, _)}
  | Tsubst {desc = Tconstr (Pident id, _typeArgs, _)}
  | Tpoly ({desc = Tconstr (Pident id, _typeArgs, _)}, [])
    when Ident.name id = "unit" ->
    true
  | _ -> false

let rec contextPathFromCoreType (coreType : Parsetree.core_type) =
  match coreType.ptyp_desc with
  | Ptyp_constr ({txt = Lident "option"}, [innerTyp]) ->
    innerTyp |> contextPathFromCoreType
    |> Option.map (fun innerTyp -> Completable.CPOption innerTyp)
  | Ptyp_constr ({txt = Lident "array"}, [innerTyp]) ->
    Some (Completable.CPArray (innerTyp |> contextPathFromCoreType))
  | Ptyp_constr (lid, _) ->
    Some
      (CPId
         {
           path = lid.txt |> Utils.flattenLongIdent;
           completionContext = Type;
           loc = lid.loc;
         })
  | _ -> None

let unwrapCompletionTypeIfOption (t : SharedTypes.completionType) =
  match t with
  | Toption (_, ExtractedType unwrapped) -> unwrapped
  | _ -> t

module Codegen = struct
  let mkFailWithExp () =
    Ast_helper.Exp.apply
      (Ast_helper.Exp.ident {txt = Lident "failwith"; loc = Location.none})
      [(Nolabel, Ast_helper.Exp.constant (Pconst_string ("TODO", None)))]

  let mkConstructPat ?payload name =
    Ast_helper.Pat.construct
      {Asttypes.txt = Longident.Lident name; loc = Location.none}
      payload

  let mkTagPat ?payload name = Ast_helper.Pat.variant name payload

  let any () = Ast_helper.Pat.any ()

  let rec extractedTypeToExhaustivePatterns ~env ~full extractedType =
    match extractedType with
    | Tvariant v ->
      Some
        (v.constructors
        |> List.map (fun (c : SharedTypes.Constructor.t) ->
               mkConstructPat
                 ?payload:
                   (match c.args with
                   | Args [] -> None
                   | _ -> Some (any ()))
                 c.cname.txt))
    | Tpolyvariant v ->
      Some
        (v.constructors
        |> List.map (fun (c : SharedTypes.polyVariantConstructor) ->
               mkTagPat
                 ?payload:
                   (match c.args with
                   | [] -> None
                   | _ -> Some (any ()))
                 c.displayName))
    | Toption (_, innerType) ->
      let extractedType =
        match innerType with
        | ExtractedType t -> Some t
        | TypeExpr t ->
          extractType t ~env ~package:full.package |> getExtractedType
      in
      let expandedBranches =
        match extractedType with
        | None -> []
        | Some extractedType -> (
          match extractedTypeToExhaustivePatterns ~env ~full extractedType with
          | None -> []
          | Some patterns -> patterns)
      in
      Some
        ([
           mkConstructPat "None";
           mkConstructPat ~payload:(Ast_helper.Pat.any ()) "Some";
         ]
        @ (expandedBranches
          |> List.map (fun (pat : Parsetree.pattern) ->
                 mkConstructPat ~payload:pat "Some")))
    | Tresult {okType; errorType} ->
      let extractedOkType =
        okType |> extractType ~env ~package:full.package |> getExtractedType
      in
      let extractedErrorType =
        errorType |> extractType ~env ~package:full.package |> getExtractedType
      in
      let expandedOkBranches =
        match extractedOkType with
        | None -> []
        | Some extractedType -> (
          match extractedTypeToExhaustivePatterns ~env ~full extractedType with
          | None -> []
          | Some patterns -> patterns)
      in
      let expandedErrorBranches =
        match extractedErrorType with
        | None -> []
        | Some extractedType -> (
          match extractedTypeToExhaustivePatterns ~env ~full extractedType with
          | None -> []
          | Some patterns -> patterns)
      in
      Some
        ((expandedOkBranches
         |> List.map (fun (pat : Parsetree.pattern) ->
                mkConstructPat ~payload:pat "Ok"))
        @ (expandedErrorBranches
          |> List.map (fun (pat : Parsetree.pattern) ->
                 mkConstructPat ~payload:pat "Error")))
    | Tbool _ -> Some [mkConstructPat "true"; mkConstructPat "false"]
    | _ -> None

  let extractedTypeToExhaustiveCases ~env ~full extractedType =
    let patterns = extractedTypeToExhaustivePatterns ~env ~full extractedType in

    match patterns with
    | None -> None
    | Some patterns ->
      Some
        (patterns
        |> List.map (fun (pat : Parsetree.pattern) ->
               Ast_helper.Exp.case pat (mkFailWithExp ())))
end

let getModulePathRelativeToEnv ~debug ~(env : QueryEnv.t) ~envFromItem path =
  match path with
  | _ :: pathRev ->
    (* type path is relative to the completion environment
       express it from the root of the file *)
    let found, pathFromEnv =
      QueryEnv.pathFromEnv envFromItem (List.rev pathRev)
    in
    if debug then
      Printf.printf "CPPipe pathFromEnv:%s found:%b\n"
        (pathFromEnv |> String.concat ".")
        found;
    if pathFromEnv = [] then None
    else if
      env.file.moduleName <> envFromItem.file.moduleName && found
      (* If the module names are different, then one needs to qualify the path.
         But only if the path belongs to the env from completion *)
    then Some (envFromItem.file.moduleName :: pathFromEnv)
    else Some pathFromEnv
  | _ -> None

let removeOpensFromCompletionPath ~rawOpens ~package completionPath =
  let rec removeRawOpen rawOpen modulePath =
    match (rawOpen, modulePath) with
    | [_], _ -> Some modulePath
    | s :: inner, first :: restPath when s = first ->
      removeRawOpen inner restPath
    | _ -> None
  in
  let rec removeRawOpens rawOpens modulePath =
    match rawOpens with
    | rawOpen :: restOpens -> (
      let newModulePath = removeRawOpens restOpens modulePath in
      match removeRawOpen rawOpen newModulePath with
      | None -> newModulePath
      | Some mp -> mp)
    | [] -> modulePath
  in
  let completionPathMinusOpens =
    completionPath |> Utils.flattenAnyNamespaceInPath
    |> removeRawOpens package.opens
    |> removeRawOpens rawOpens
  in
  completionPathMinusOpens

let pathToElementProps package =
  match package.genericJsxModule with
  | None -> ["ReactDOM"; "domProps"]
  | Some g -> (g |> String.split_on_char '.') @ ["Elements"; "props"]

module StringSet = Set.Make (String)

let getExtraModulesToCompleteFromForType ~env ~full (t : Types.type_expr) =
  let foundModulePaths = ref StringSet.empty in
  let addToModulePaths attributes =
    ProcessAttributes.findEditorCompleteFromAttribute attributes
    |> List.iter (fun e ->
           foundModulePaths :=
             StringSet.add (e |> String.concat ".") !foundModulePaths)
  in
  let rec inner ~env ~full (t : Types.type_expr) =
    match t |> Shared.digConstructor with
    | Some path -> (
      match References.digConstructor ~env ~package:full.package path with
      | None -> ()
      | Some (env, {item = {decl = {type_manifest = Some t}; attributes}}) ->
        addToModulePaths attributes;
        inner ~env ~full t
      | Some (_, {item = {attributes}}) -> addToModulePaths attributes)
    | None -> ()
  in
  inner ~env ~full t;
  !foundModulePaths |> StringSet.elements
  |> List.map (fun l -> String.split_on_char '.' l)

let getFirstFnUnlabelledArgType ~env ~full t =
  let labels, _, env =
    extractFunctionTypeWithEnv ~env ~package:full.package t
  in
  let rec findFirstUnlabelledArgType labels =
    match labels with
    | (Asttypes.Noloc.Nolabel, t) :: _ -> Some t
    | _ :: rest -> findFirstUnlabelledArgType rest
    | [] -> None
  in
  match findFirstUnlabelledArgType labels with
  | Some t -> Some (t, env)
  | _ -> None

let makeAdditionalTextEditsForRemovingDot posOfDot =
  [
    {
      Protocol.range =
        {
          start = {line = fst posOfDot; character = snd posOfDot - 1};
          end_ = {line = fst posOfDot; character = snd posOfDot};
        };
      newText = "";
    };
  ]

(** Turns a completion into a pipe completion. *)
let transformCompletionToPipeCompletion ?(synthetic = false) ~env ?posOfDot
    (completion : Completion.t) =
  let name = completion.name in
  let nameWithPipe = "->" ^ name in
  Some
    {
      completion with
      name = nameWithPipe;
      sortText =
        (match completion.sortText with
        | Some _ -> completion.sortText
        | None -> Some (name |> String.split_on_char '.' |> List.rev |> List.hd));
      insertText = Some nameWithPipe;
      env;
      synthetic;
      additionalTextEdits =
        (match posOfDot with
        | None -> None
        | Some posOfDot -> Some (makeAdditionalTextEditsForRemovingDot posOfDot));
    }

(** This takes a type expr and the env that type expr was found in, and produces a globally unique 
    id for that specific type. The globally unique id is the full path to the type as seen from the root
    of the project. Example: type x in module SomeModule in file SomeFile would get the globally 
    unique id `SomeFile.SomeModule.x`.*)
let rec findRootTypeId ~full ~env (t : Types.type_expr) =
  let debug = false in
  match t.desc with
  | Tlink t1 | Tsubst t1 | Tpoly (t1, []) -> findRootTypeId ~full ~env t1
  | Tconstr (path, _, _) -> (
    (* We have a path. Try to dig to its declaration *)
    if debug then
      Printf.printf "[findRootTypeId] path %s, dig\n" (Path.name path);
    match References.digConstructor ~env ~package:full.package path with
    | Some (env, {item = {decl = {type_manifest = Some t1}}}) ->
      if debug then
        Printf.printf "[findRootTypeId] dug up type alias at module path %s \n"
          (modulePathFromEnv env |> String.concat ".");
      findRootTypeId ~full ~env t1
    | Some (env, {item = {name}; modulePath}) ->
      (* if it's a named type, then we know its name will be its module path from the env + its name.*)
      if debug then
        Printf.printf
          "[findRootTypeId] dug up named type at module path %s, from item: %s \n"
          (modulePathFromEnv env |> String.concat ".")
          (ModulePath.toPath modulePath name |> String.concat ".");
      Some (fullTypeIdFromDecl ~env ~name ~modulePath)
    | None ->
      (* If we didn't find anything, then it might be a builtin type. Check it.*)
      if debug then Printf.printf "[findRootTypeId] dug up non-type alias\n";
      if
        Predef.builtin_idents
        |> List.find_opt (fun (_, i) -> Ident.same i (Path.head path))
        |> Option.is_some
      then
        Some
          (if debug then Printf.printf "[findRootTypeId] returning builtin\n";
           Path.name path)
      else None)
  | _ -> None

(** Filters out completions that are not pipeable from a list of completions. *)
let filterPipeableFunctions ~env ~full ?synthetic ?targetTypeId ?posOfDot
    completions =
  match targetTypeId with
  | None -> completions
  | Some targetTypeId ->
    completions
    |> List.filter_map (fun (completion : Completion.t) ->
           let thisCompletionItemTypeId =
             match completion.kind with
             | Value t -> (
               match
                 getFirstFnUnlabelledArgType ~full ~env:completion.env t
               with
               | None -> None
               | Some (t, envFromLabelledArg) ->
                 findRootTypeId ~full ~env:envFromLabelledArg t)
             | _ -> None
           in
           match thisCompletionItemTypeId with
           | Some mainTypeId when mainTypeId = targetTypeId -> (
             match posOfDot with
             | None -> Some completion
             | Some posOfDot ->
               transformCompletionToPipeCompletion ?synthetic ~env ~posOfDot
                 completion)
           | _ -> None)

let removeCurrentModuleIfNeeded ~envCompletionIsMadeFrom completionPath =
  if
    List.length completionPath > 0
    && List.hd completionPath = envCompletionIsMadeFrom.QueryEnv.file.moduleName
  then List.tl completionPath
  else completionPath

let rec getObjFields (texp : Types.type_expr) =
  match texp.desc with
  | Tfield (name, _, t1, t2) ->
    let fields = t2 |> getObjFields in
    (name, t1) :: fields
  | Tlink te | Tsubst te | Tpoly (te, []) -> te |> getObjFields
  | Tvar None -> []
  | _ -> []

let pathToBuiltin path =
  Predef.builtin_idents
  |> List.find_opt (fun (_, i) -> Ident.same i (Path.head path))

let completionPathFromMaybeBuiltin path =
  match pathToBuiltin path with
  | Some ("array", _) -> Some ["Stdlib"; "Array"]
  | Some ("option", _) -> Some ["Stdlib"; "Option"]
  | Some ("string", _) -> Some ["Stdlib"; "String"]
  | Some ("int", _) -> Some ["Stdlib"; "Int"]
  | Some ("float", _) -> Some ["Stdlib"; "Float"]
  | Some ("promise", _) -> Some ["Stdlib"; "Promise"]
  | Some ("list", _) -> Some ["Stdlib"; "List"]
  | Some ("result", _) -> Some ["Stdlib"; "Result"]
  | Some ("dict", _) -> Some ["Stdlib"; "Dict"]
  | Some ("char", _) -> Some ["Stdlib"; "Char"]
  | _ -> None
