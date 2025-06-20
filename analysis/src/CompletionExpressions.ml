open SharedTypes

let isExprHole exp =
  match exp.Parsetree.pexp_desc with
  | Pexp_extension ({txt = "rescript.exprhole"}, _) -> true
  | _ -> false

let isExprTuple expr =
  match expr.Parsetree.pexp_desc with
  | Pexp_tuple _ -> true
  | _ -> false

let rec traverseExpr (exp : Parsetree.expression) ~exprPath ~pos
    ~firstCharBeforeCursorNoWhite =
  let locHasCursor loc = loc |> CursorPosition.locHasCursor ~pos in
  let someIfHasCursor v = if locHasCursor exp.pexp_loc then Some v else None in
  match exp.pexp_desc with
  | Pexp_ident {txt = Lident txt} when Utils.hasBraces exp.pexp_attributes ->
    (* An ident with braces attribute corresponds to for example `{n}`.
       Looks like a record but is parsed as an ident with braces. *)
    someIfHasCursor (txt, [Completable.NRecordBody {seenFields = []}] @ exprPath)
  | Pexp_ident {txt = Lident txt} -> someIfHasCursor (txt, exprPath)
  | Pexp_construct ({txt = Lident "()"}, _) -> someIfHasCursor ("", exprPath)
  | Pexp_construct ({txt = Lident txt}, None) -> someIfHasCursor (txt, exprPath)
  | Pexp_variant (label, None) -> someIfHasCursor ("#" ^ label, exprPath)
  | Pexp_array arrayPatterns -> (
    let nextExprPath = [Completable.NArray] @ exprPath in
    (* No fields but still has cursor = empty completion *)
    if List.length arrayPatterns = 0 && locHasCursor exp.pexp_loc then
      Some ("", nextExprPath)
    else
      let arrayItemWithCursor =
        arrayPatterns
        |> List.find_map (fun e ->
               e
               |> traverseExpr ~exprPath:nextExprPath
                    ~firstCharBeforeCursorNoWhite ~pos)
      in

      match (arrayItemWithCursor, locHasCursor exp.pexp_loc) with
      | Some arrayItemWithCursor, _ -> Some arrayItemWithCursor
      | None, true when firstCharBeforeCursorNoWhite = Some ',' ->
        (* No item had the cursor, but the entire expr still has the cursor (so
           the cursor is in the array somewhere), and the first char before the
           cursor is a comma = interpret as compleing for a new value (example:
           `[None, <com>, None]`) *)
        Some ("", nextExprPath)
      | _ -> None)
  | Pexp_tuple tupleItems when locHasCursor exp.pexp_loc ->
    tupleItems
    |> traverseExprTupleItems ~firstCharBeforeCursorNoWhite ~pos
         ~nextExprPath:(fun itemNum ->
           [Completable.NTupleItem {itemNum}] @ exprPath)
         ~resultFromFoundItemNum:(fun itemNum ->
           [Completable.NTupleItem {itemNum = itemNum + 1}] @ exprPath)
  | Pexp_record ([], _) ->
    (* Empty fields means we're in a record body `{}`. Complete for the fields. *)
    someIfHasCursor ("", [Completable.NRecordBody {seenFields = []}] @ exprPath)
  | Pexp_record (fields, _) -> (
    let fieldWithCursor = ref None in
    let fieldWithExprHole = ref None in
    Ext_list.iter fields (fun {lid = fname; x = exp} ->
        match
          ( fname.Location.txt,
            exp.Parsetree.pexp_loc |> CursorPosition.classifyLoc ~pos )
        with
        | Longident.Lident fname, HasCursor ->
          fieldWithCursor := Some (fname, exp)
        | Lident fname, _ when isExprHole exp ->
          fieldWithExprHole := Some (fname, exp)
        | _ -> ());
    let seenFields =
      Ext_list.filter_map fields (fun {lid = fieldName} ->
          match fieldName with
          | {Location.txt = Longident.Lident fieldName} -> Some fieldName
          | _ -> None)
    in
    match (!fieldWithCursor, !fieldWithExprHole) with
    | Some (fname, f), _ | None, Some (fname, f) -> (
      match f.pexp_desc with
      | Pexp_extension ({txt = "rescript.exprhole"}, _) ->
        (* An expression hole means for example `{someField: <com>}`. We want to complete for the type of `someField`.  *)
        someIfHasCursor
          ("", [Completable.NFollowRecordField {fieldName = fname}] @ exprPath)
      | Pexp_ident {txt = Lident txt} when fname = txt ->
        (* This is a heuristic for catching writing field names. ReScript has punning for record fields, but the AST doesn't,
           so punning is represented as the record field name and identifier being the same: {someField}. *)
        someIfHasCursor (txt, [Completable.NRecordBody {seenFields}] @ exprPath)
      | Pexp_ident {txt = Lident txt} ->
        (* A var means `{someField: s}` or similar. Complete for identifiers or values. *)
        someIfHasCursor (txt, exprPath)
      | _ ->
        f
        |> traverseExpr ~firstCharBeforeCursorNoWhite ~pos
             ~exprPath:
               ([Completable.NFollowRecordField {fieldName = fname}] @ exprPath)
      )
    | None, None -> (
      if Debug.verbose () then (
        Printf.printf "[traverse_expr] No field with cursor and no expr hole.\n";

        match firstCharBeforeCursorNoWhite with
        | None -> ()
        | Some c ->
          Printf.printf "[traverse_expr] firstCharBeforeCursorNoWhite: %c.\n" c);

      (* Figure out if we're completing for a new field.
         If the cursor is inside of the record body, but no field has the cursor,
         and there's no pattern hole. Check the first char to the left of the cursor,
         ignoring white space. If that's a comma or {, we assume you're completing for a new field,
         since you're either between 2 fields (comma to the left) or at the start of the record ({). *)
      match firstCharBeforeCursorNoWhite with
      | Some (',' | '{') ->
        someIfHasCursor ("", [Completable.NRecordBody {seenFields}] @ exprPath)
      | _ -> None))
  | Pexp_construct
      ( {txt},
        Some {pexp_loc; pexp_desc = Pexp_construct ({txt = Lident "()"}, _)} )
    when locHasCursor pexp_loc ->
    (* Empty payload with cursor, like: Test(<com>) *)
    Some
      ( "",
        [
          Completable.NVariantPayload
            {constructorName = Utils.getUnqualifiedName txt; itemNum = 0};
        ]
        @ exprPath )
  | Pexp_construct ({txt}, Some e)
    when pos >= (e.pexp_loc |> Loc.end_)
         && firstCharBeforeCursorNoWhite = Some ','
         && isExprTuple e = false ->
    (* Empty payload with trailing ',', like: Test(true, <com>) *)
    Some
      ( "",
        [
          Completable.NVariantPayload
            {constructorName = Utils.getUnqualifiedName txt; itemNum = 1};
        ]
        @ exprPath )
  | Pexp_construct ({txt}, Some {pexp_loc; pexp_desc = Pexp_tuple tupleItems})
    when locHasCursor pexp_loc ->
    tupleItems
    |> traverseExprTupleItems ~firstCharBeforeCursorNoWhite ~pos
         ~nextExprPath:(fun itemNum ->
           [
             Completable.NVariantPayload
               {constructorName = Utils.getUnqualifiedName txt; itemNum};
           ]
           @ exprPath)
         ~resultFromFoundItemNum:(fun itemNum ->
           [
             Completable.NVariantPayload
               {
                 constructorName = Utils.getUnqualifiedName txt;
                 itemNum = itemNum + 1;
               };
           ]
           @ exprPath)
  | Pexp_construct ({txt}, Some p) when locHasCursor exp.pexp_loc ->
    p
    |> traverseExpr ~firstCharBeforeCursorNoWhite ~pos
         ~exprPath:
           ([
              Completable.NVariantPayload
                {constructorName = Utils.getUnqualifiedName txt; itemNum = 0};
            ]
           @ exprPath)
  | Pexp_variant
      (txt, Some {pexp_loc; pexp_desc = Pexp_construct ({txt = Lident "()"}, _)})
    when locHasCursor pexp_loc ->
    (* Empty payload with cursor, like: #test(<com>) *)
    Some
      ( "",
        [Completable.NPolyvariantPayload {constructorName = txt; itemNum = 0}]
        @ exprPath )
  | Pexp_variant (txt, Some e)
    when pos >= (e.pexp_loc |> Loc.end_)
         && firstCharBeforeCursorNoWhite = Some ','
         && isExprTuple e = false ->
    (* Empty payload with trailing ',', like: #test(true, <com>) *)
    Some
      ( "",
        [Completable.NPolyvariantPayload {constructorName = txt; itemNum = 1}]
        @ exprPath )
  | Pexp_variant (txt, Some {pexp_loc; pexp_desc = Pexp_tuple tupleItems})
    when locHasCursor pexp_loc ->
    tupleItems
    |> traverseExprTupleItems ~firstCharBeforeCursorNoWhite ~pos
         ~nextExprPath:(fun itemNum ->
           [Completable.NPolyvariantPayload {constructorName = txt; itemNum}]
           @ exprPath)
         ~resultFromFoundItemNum:(fun itemNum ->
           [
             Completable.NPolyvariantPayload
               {constructorName = txt; itemNum = itemNum + 1};
           ]
           @ exprPath)
  | Pexp_variant (txt, Some p) when locHasCursor exp.pexp_loc ->
    p
    |> traverseExpr ~firstCharBeforeCursorNoWhite ~pos
         ~exprPath:
           ([
              Completable.NPolyvariantPayload
                {constructorName = txt; itemNum = 0};
            ]
           @ exprPath)
  | _ -> None

and traverseExprTupleItems tupleItems ~nextExprPath ~resultFromFoundItemNum ~pos
    ~firstCharBeforeCursorNoWhite =
  let itemNum = ref (-1) in
  let itemWithCursor =
    tupleItems
    |> List.find_map (fun e ->
           itemNum := !itemNum + 1;
           e
           |> traverseExpr ~exprPath:(nextExprPath !itemNum)
                ~firstCharBeforeCursorNoWhite ~pos)
  in
  match (itemWithCursor, firstCharBeforeCursorNoWhite) with
  | None, Some ',' ->
    (* No tuple item has the cursor, but there's a comma before the cursor.
       Figure out what arg we're trying to complete. Example: (true, <com>, None) *)
    let posNum = ref (-1) in
    tupleItems
    |> List.iteri (fun index e ->
           if pos >= Loc.start e.Parsetree.pexp_loc then posNum := index);
    if !posNum > -1 then Some ("", resultFromFoundItemNum !posNum) else None
  | v, _ -> v

let prettyPrintFnTemplateArgName ?currentIndex ~env ~full
    (argTyp : Types.type_expr) =
  let indexText =
    match currentIndex with
    | None -> ""
    | Some i -> string_of_int i
  in
  let defaultVarName = "v" ^ indexText in
  let argTyp, suffix, _env =
    TypeUtils.digToRelevantTemplateNameType ~env ~package:full.package argTyp
  in
  match argTyp |> TypeUtils.pathFromTypeExpr with
  | None -> defaultVarName
  | Some p -> (
    let trailingElementsOfPath =
      p |> Utils.expandPath |> List.rev |> Utils.lastElements
    in
    match trailingElementsOfPath with
    | [] | ["t"] -> defaultVarName
    | ["unit"] -> "()"
    (* Special treatment for JsxEvent, since that's a common enough thing
       used in event handlers. *)
    | ["JsxEvent"; "synthetic"] -> "event"
    | ["synthetic"] -> "event"
    (* Ignore `t` types, and go for its module name instead. *)
    | [someName; "t"] | [_; someName] | [someName] -> (
      match someName with
      | "string" | "int" | "float" | "array" | "option" | "bool" ->
        defaultVarName
      | someName when String.length someName < 30 ->
        if someName = "synthetic" then
          Printf.printf "synthetic! %s\n"
            (trailingElementsOfPath |> SharedTypes.ident);
        (* We cap how long the name can be, so we don't end up with super
           long type names. *)
        (someName |> Utils.lowercaseFirstChar) ^ suffix
      | _ -> defaultVarName)
    | _ -> defaultVarName)

let completeConstructorPayload ~posBeforeCursor ~firstCharBeforeCursorNoWhite
    (constructorLid : Longident.t Location.loc) expr =
  match
    traverseExpr expr ~exprPath:[] ~pos:posBeforeCursor
      ~firstCharBeforeCursorNoWhite
  with
  | None -> None
  | Some (prefix, nested) ->
    (* The nested path must start with the constructor name found, plus
       the target argument number for the constructor. We translate to
       that here, because we need to account for multi arg constructors
       being represented as tuples. *)
    let nested =
      match List.rev nested with
      | Completable.NTupleItem {itemNum} :: rest ->
        [
          Completable.NVariantPayload
            {constructorName = Longident.last constructorLid.txt; itemNum};
        ]
        @ rest
      | nested ->
        [
          Completable.NVariantPayload
            {constructorName = Longident.last constructorLid.txt; itemNum = 0};
        ]
        @ nested
    in
    let variantCtxPath =
      Completable.CTypeAtPos
        {constructorLid.loc with loc_start = constructorLid.loc.loc_end}
    in
    Some
      (Completable.Cexpression {contextPath = variantCtxPath; prefix; nested})
