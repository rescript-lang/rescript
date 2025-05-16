open SharedTypes

let isPatternHole pat =
  match pat.Parsetree.ppat_desc with
  | Ppat_extension ({txt = "rescript.patternhole"}, _) -> true
  | _ -> false

let isPatternTuple pat =
  match pat.Parsetree.ppat_desc with
  | Ppat_tuple _ -> true
  | _ -> false

let rec traverseTupleItems tupleItems ~nextPatternPath ~resultFromFoundItemNum
    ~locHasCursor ~firstCharBeforeCursorNoWhite ~posBeforeCursor =
  let itemNum = ref (-1) in
  let itemWithCursor =
    tupleItems
    |> List.find_map (fun pat ->
           itemNum := !itemNum + 1;
           pat
           |> traversePattern ~patternPath:(nextPatternPath !itemNum)
                ~locHasCursor ~firstCharBeforeCursorNoWhite ~posBeforeCursor)
  in
  match (itemWithCursor, firstCharBeforeCursorNoWhite) with
  | None, Some ',' ->
    (* No tuple item has the cursor, but there's a comma before the cursor.
       Figure out what arg we're trying to complete. Example: (true, <com>, None) *)
    let posNum = ref (-1) in
    tupleItems
    |> List.iteri (fun index pat ->
           if posBeforeCursor >= Loc.start pat.Parsetree.ppat_loc then
             posNum := index);
    if !posNum > -1 then Some ("", resultFromFoundItemNum !posNum) else None
  | v, _ -> v

and traversePattern (pat : Parsetree.pattern) ~patternPath ~locHasCursor
    ~firstCharBeforeCursorNoWhite ~posBeforeCursor =
  let someIfHasCursor v debugId =
    if locHasCursor pat.Parsetree.ppat_loc then (
      if Debug.verbose () then
        Printf.printf
          "[traversePattern:someIfHasCursor] '%s' has cursor, returning \n"
          debugId;
      Some v)
    else None
  in
  match pat.ppat_desc with
  | Ppat_constant _ | Ppat_interval _ -> None
  | Ppat_constraint (p, _)
  | Ppat_alias (p, _)
  | Ppat_exception p
  | Ppat_open (_, p) ->
    p
    |> traversePattern ~patternPath ~locHasCursor ~firstCharBeforeCursorNoWhite
         ~posBeforeCursor
  | Ppat_or (p1, p2) -> (
    let orPatWithItem =
      [p1; p2]
      |> List.find_map (fun p ->
             p
             |> traversePattern ~patternPath ~locHasCursor
                  ~firstCharBeforeCursorNoWhite ~posBeforeCursor)
    in
    match orPatWithItem with
    | None when isPatternHole p1 || isPatternHole p2 ->
      if Debug.verbose () then
        Printf.printf
          "[traversePattern] found or-pattern that was pattern hole\n";
      Some ("", patternPath)
    | v -> v)
  | Ppat_any ->
    (* We treat any `_` as an empty completion. This is mainly because we're
       inserting `_` in snippets and automatically put the cursor there. So
       letting it trigger an empty completion improves the ergonomics by a
       lot. *)
    someIfHasCursor ("", patternPath) "Ppat_any"
  | Ppat_var {txt} -> someIfHasCursor (txt, patternPath) "Ppat_var"
  | Ppat_construct ({txt = Lident "()"}, None) ->
    (* switch s { | (<com>) }*)
    someIfHasCursor
      ("", patternPath @ [Completable.NTupleItem {itemNum = 0}])
      "Ppat_construct()"
  | Ppat_construct ({txt = Lident prefix}, None) ->
    someIfHasCursor (prefix, patternPath) "Ppat_construct(Lident)"
  | Ppat_variant (prefix, None) ->
    someIfHasCursor ("#" ^ prefix, patternPath) "Ppat_variant"
  | Ppat_array arrayPatterns ->
    let nextPatternPath = [Completable.NArray] @ patternPath in
    if List.length arrayPatterns = 0 && locHasCursor pat.ppat_loc then
      Some ("", nextPatternPath)
    else
      arrayPatterns
      |> List.find_map (fun pat ->
             pat
             |> traversePattern ~patternPath:nextPatternPath ~locHasCursor
                  ~firstCharBeforeCursorNoWhite ~posBeforeCursor)
  | Ppat_tuple tupleItems when locHasCursor pat.ppat_loc ->
    tupleItems
    |> traverseTupleItems ~firstCharBeforeCursorNoWhite ~posBeforeCursor
         ~locHasCursor
         ~nextPatternPath:(fun itemNum ->
           [Completable.NTupleItem {itemNum}] @ patternPath)
         ~resultFromFoundItemNum:(fun itemNum ->
           [Completable.NTupleItem {itemNum = itemNum + 1}] @ patternPath)
  | Ppat_record ([], _) ->
    (* Empty fields means we're in a record body `{}`. Complete for the fields. *)
    someIfHasCursor
      ("", [Completable.NRecordBody {seenFields = []}] @ patternPath)
      "Ppat_record(empty)"
  | Ppat_record (fields, _) -> (
    let fieldWithCursor = ref None in
    let fieldWithPatHole = ref None in
    fields
    |> List.iter (fun (fname, f, _) ->
           match
             ( fname.Location.txt,
               f.Parsetree.ppat_loc
               |> CursorPosition.classifyLoc ~pos:posBeforeCursor )
           with
           | Longident.Lident fname, HasCursor ->
             fieldWithCursor := Some (fname, f)
           | Lident fname, _ when isPatternHole f ->
             fieldWithPatHole := Some (fname, f)
           | _ -> ());
    let seenFields =
      fields
      |> List.filter_map (fun (fieldName, _f, _) ->
             match fieldName with
             | {Location.txt = Longident.Lident fieldName} -> Some fieldName
             | _ -> None)
    in
    match (!fieldWithCursor, !fieldWithPatHole) with
    | Some (fname, f), _ | None, Some (fname, f) -> (
      match f.ppat_desc with
      | Ppat_extension ({txt = "rescript.patternhole"}, _) ->
        (* A pattern hole means for example `{someField: <com>}`. We want to complete for the type of `someField`.  *)
        someIfHasCursor
          ( "",
            [Completable.NFollowRecordField {fieldName = fname}] @ patternPath
          )
          "patternhole"
      | Ppat_var {txt} ->
        (* A var means `{s}` or similar. Complete for fields. *)
        someIfHasCursor
          (txt, [Completable.NRecordBody {seenFields}] @ patternPath)
          "Ppat_var #2"
      | _ ->
        f
        |> traversePattern
             ~patternPath:
               ([Completable.NFollowRecordField {fieldName = fname}]
               @ patternPath)
             ~locHasCursor ~firstCharBeforeCursorNoWhite ~posBeforeCursor)
    | None, None -> (
      (* Figure out if we're completing for a new field.
         If the cursor is inside of the record body, but no field has the cursor,
         and there's no pattern hole. Check the first char to the left of the cursor,
         ignoring white space. If that's a comma, we assume you're completing for a new field. *)
      match firstCharBeforeCursorNoWhite with
      | Some ',' ->
        someIfHasCursor
          ("", [Completable.NRecordBody {seenFields}] @ patternPath)
          "firstCharBeforeCursorNoWhite:,"
      | _ -> None))
  | Ppat_construct
      ( {txt},
        Some {ppat_loc; ppat_desc = Ppat_construct ({txt = Lident "()"}, _)} )
    when locHasCursor ppat_loc ->
    (* Empty payload with cursor, like: Test(<com>) *)
    Some
      ( "",
        [
          Completable.NVariantPayload
            {constructorName = Utils.getUnqualifiedName txt; itemNum = 0};
        ]
        @ patternPath )
  | Ppat_construct ({txt}, Some pat)
    when posBeforeCursor >= (pat.ppat_loc |> Loc.end_)
         && firstCharBeforeCursorNoWhite = Some ','
         && isPatternTuple pat = false ->
    (* Empty payload with trailing ',', like: Test(true, <com>) *)
    Some
      ( "",
        [
          Completable.NVariantPayload
            {constructorName = Utils.getUnqualifiedName txt; itemNum = 1};
        ]
        @ patternPath )
  | Ppat_construct ({txt}, Some {ppat_loc; ppat_desc = Ppat_tuple tupleItems})
    when locHasCursor ppat_loc ->
    tupleItems
    |> traverseTupleItems ~locHasCursor ~firstCharBeforeCursorNoWhite
         ~posBeforeCursor
         ~nextPatternPath:(fun itemNum ->
           [
             Completable.NVariantPayload
               {constructorName = Utils.getUnqualifiedName txt; itemNum};
           ]
           @ patternPath)
         ~resultFromFoundItemNum:(fun itemNum ->
           [
             Completable.NVariantPayload
               {
                 constructorName = Utils.getUnqualifiedName txt;
                 itemNum = itemNum + 1;
               };
           ]
           @ patternPath)
  | Ppat_construct ({txt}, Some p) when locHasCursor pat.ppat_loc ->
    p
    |> traversePattern ~locHasCursor ~firstCharBeforeCursorNoWhite
         ~posBeforeCursor
         ~patternPath:
           ([
              Completable.NVariantPayload
                {constructorName = Utils.getUnqualifiedName txt; itemNum = 0};
            ]
           @ patternPath)
  | Ppat_variant
      (txt, Some {ppat_loc; ppat_desc = Ppat_construct ({txt = Lident "()"}, _)})
    when locHasCursor ppat_loc ->
    (* Empty payload with cursor, like: #test(<com>) *)
    Some
      ( "",
        [Completable.NPolyvariantPayload {constructorName = txt; itemNum = 0}]
        @ patternPath )
  | Ppat_variant (txt, Some pat)
    when posBeforeCursor >= (pat.ppat_loc |> Loc.end_)
         && firstCharBeforeCursorNoWhite = Some ','
         && isPatternTuple pat = false ->
    (* Empty payload with trailing ',', like: #test(true, <com>) *)
    Some
      ( "",
        [Completable.NPolyvariantPayload {constructorName = txt; itemNum = 1}]
        @ patternPath )
  | Ppat_variant (txt, Some {ppat_loc; ppat_desc = Ppat_tuple tupleItems})
    when locHasCursor ppat_loc ->
    tupleItems
    |> traverseTupleItems ~locHasCursor ~firstCharBeforeCursorNoWhite
         ~posBeforeCursor
         ~nextPatternPath:(fun itemNum ->
           [Completable.NPolyvariantPayload {constructorName = txt; itemNum}]
           @ patternPath)
         ~resultFromFoundItemNum:(fun itemNum ->
           [
             Completable.NPolyvariantPayload
               {constructorName = txt; itemNum = itemNum + 1};
           ]
           @ patternPath)
  | Ppat_variant (txt, Some p) when locHasCursor pat.ppat_loc ->
    p
    |> traversePattern ~locHasCursor ~firstCharBeforeCursorNoWhite
         ~posBeforeCursor
         ~patternPath:
           ([
              Completable.NPolyvariantPayload
                {constructorName = txt; itemNum = 0};
            ]
           @ patternPath)
  | _ -> None
