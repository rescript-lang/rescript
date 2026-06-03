open Shared_types

let is_pattern_hole pat =
  match pat.Parsetree.ppat_desc with
  | Ppat_extension ({txt = "rescript.patternhole"}, _) -> true
  | _ -> false

let is_pattern_tuple pat =
  match pat.Parsetree.ppat_desc with
  | Ppat_tuple _ -> true
  | _ -> false

let rec traverse_tuple_items tuple_items ~next_pattern_path
    ~result_from_found_item_num ~loc_has_cursor
    ~first_char_before_cursor_no_white ~pos_before_cursor =
  let item_num = ref (-1) in
  let item_with_cursor =
    tuple_items
    |> List.find_map (fun pat ->
           item_num := !item_num + 1;
           pat
           |> traverse_pattern
                ~pattern_path:(next_pattern_path !item_num)
                ~loc_has_cursor ~first_char_before_cursor_no_white
                ~pos_before_cursor)
  in
  match (item_with_cursor, first_char_before_cursor_no_white) with
  | None, Some ',' ->
    (* No tuple item has the cursor, but there's a comma before the cursor.
       Figure out what arg we're trying to complete. Example: (true, <com>, None) *)
    let pos_num = ref (-1) in
    tuple_items
    |> List.iteri (fun index pat ->
           if pos_before_cursor >= Loc.start pat.Parsetree.ppat_loc then
             pos_num := index);
    if !pos_num > -1 then Some ("", result_from_found_item_num !pos_num)
    else None
  | v, _ -> v

and traverse_pattern (pat : Parsetree.pattern) ~pattern_path ~loc_has_cursor
    ~first_char_before_cursor_no_white ~pos_before_cursor =
  let some_if_has_cursor v debug_id =
    if loc_has_cursor pat.Parsetree.ppat_loc then (
      if Debug.verbose () then
        Printf.printf
          "[traversePattern:someIfHasCursor] '%s' has cursor, returning \n"
          debug_id;
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
    |> traverse_pattern ~pattern_path ~loc_has_cursor
         ~first_char_before_cursor_no_white ~pos_before_cursor
  | Ppat_or (p1, p2) -> (
    let or_pat_with_item =
      [p1; p2]
      |> List.find_map (fun p ->
             p
             |> traverse_pattern ~pattern_path ~loc_has_cursor
                  ~first_char_before_cursor_no_white ~pos_before_cursor)
    in
    match or_pat_with_item with
    | None when is_pattern_hole p1 || is_pattern_hole p2 ->
      if Debug.verbose () then
        Printf.printf
          "[traversePattern] found or-pattern that was pattern hole\n";
      Some ("", pattern_path)
    | v -> v)
  | Ppat_any ->
    (* We treat any `_` as an empty completion. This is mainly because we're
       inserting `_` in snippets and automatically put the cursor there. So
       letting it trigger an empty completion improves the ergonomics by a
       lot. *)
    some_if_has_cursor ("", pattern_path) "Ppat_any"
  | Ppat_var {txt} -> some_if_has_cursor (txt, pattern_path) "Ppat_var"
  | Ppat_construct ({txt = Lident "()"}, None) ->
    (* switch s { | (<com>) }*)
    some_if_has_cursor
      ("", pattern_path @ [Completable.NTupleItem {item_num = 0}])
      "Ppat_construct()"
  | Ppat_construct ({txt = Lident prefix}, None) ->
    some_if_has_cursor (prefix, pattern_path) "Ppat_construct(Lident)"
  | Ppat_variant (prefix, None) ->
    some_if_has_cursor ("#" ^ prefix, pattern_path) "Ppat_variant"
  | Ppat_array array_patterns ->
    let next_pattern_path = [Completable.NArray] @ pattern_path in
    if List.length array_patterns = 0 && loc_has_cursor pat.ppat_loc then
      Some ("", next_pattern_path)
    else
      array_patterns
      |> List.find_map (fun pat ->
             pat
             |> traverse_pattern ~pattern_path:next_pattern_path ~loc_has_cursor
                  ~first_char_before_cursor_no_white ~pos_before_cursor)
  | Ppat_tuple tuple_items when loc_has_cursor pat.ppat_loc ->
    tuple_items
    |> traverse_tuple_items ~first_char_before_cursor_no_white
         ~pos_before_cursor ~loc_has_cursor
         ~next_pattern_path:(fun item_num ->
           [Completable.NTupleItem {item_num}] @ pattern_path)
         ~result_from_found_item_num:(fun item_num ->
           [Completable.NTupleItem {item_num = item_num + 1}] @ pattern_path)
  | Ppat_record ([], _) ->
    (* Empty fields means we're in a record body `{}`. Complete for the fields. *)
    some_if_has_cursor
      ("", [Completable.NRecordBody {seen_fields = []}] @ pattern_path)
      "Ppat_record(empty)"
  | Ppat_record (fields, _) -> (
    let field_with_cursor = ref None in
    let field_with_pat_hole = ref None in
    Ext_list.iter fields (fun {lid = fname; x = f} ->
        match
          ( fname.Location.txt,
            f.Parsetree.ppat_loc
            |> Cursor_position.classify_loc ~pos:pos_before_cursor )
        with
        | Longident.Lident fname, HasCursor ->
          field_with_cursor := Some (fname, f)
        | Lident fname, _ when is_pattern_hole f ->
          field_with_pat_hole := Some (fname, f)
        | _ -> ());
    let seen_fields =
      Ext_list.filter_map fields (fun {lid = field_name} ->
          match field_name with
          | {Location.txt = Longident.Lident field_name} -> Some field_name
          | _ -> None)
    in
    match (!field_with_cursor, !field_with_pat_hole) with
    | Some (fname, f), _ | None, Some (fname, f) -> (
      match f.ppat_desc with
      | Ppat_extension ({txt = "rescript.patternhole"}, _) ->
        (* A pattern hole means for example `{someField: <com>}`. We want to complete for the type of `someField`.  *)
        some_if_has_cursor
          ( "",
            [Completable.NFollowRecordField {field_name = fname}] @ pattern_path
          )
          "patternhole"
      | Ppat_var {txt} ->
        (* A var means `{s}` or similar. Complete for fields. *)
        some_if_has_cursor
          (txt, [Completable.NRecordBody {seen_fields}] @ pattern_path)
          "Ppat_var #2"
      | _ ->
        f
        |> traverse_pattern
             ~pattern_path:
               ([Completable.NFollowRecordField {field_name = fname}]
               @ pattern_path)
             ~loc_has_cursor ~first_char_before_cursor_no_white
             ~pos_before_cursor)
    | None, None -> (
      (* Figure out if we're completing for a new field.
         If the cursor is inside of the record body, but no field has the cursor,
         and there's no pattern hole. Check the first char to the left of the cursor,
         ignoring white space. If that's a comma, we assume you're completing for a new field. *)
      match first_char_before_cursor_no_white with
      | Some ',' ->
        some_if_has_cursor
          ("", [Completable.NRecordBody {seen_fields}] @ pattern_path)
          "firstCharBeforeCursorNoWhite:,"
      | _ -> None))
  | Ppat_construct
      ( {txt},
        Some {ppat_loc; ppat_desc = Ppat_construct ({txt = Lident "()"}, _)} )
    when loc_has_cursor ppat_loc ->
    (* Empty payload with cursor, like: Test(<com>) *)
    Some
      ( "",
        [
          Completable.NVariantPayload
            {constructor_name = Utils.get_unqualified_name txt; item_num = 0};
        ]
        @ pattern_path )
  | Ppat_construct ({txt}, Some pat)
    when pos_before_cursor >= (pat.ppat_loc |> Loc.end_)
         && first_char_before_cursor_no_white = Some ','
         && is_pattern_tuple pat = false ->
    (* Empty payload with trailing ',', like: Test(true, <com>) *)
    Some
      ( "",
        [
          Completable.NVariantPayload
            {constructor_name = Utils.get_unqualified_name txt; item_num = 1};
        ]
        @ pattern_path )
  | Ppat_construct ({txt}, Some {ppat_loc; ppat_desc = Ppat_tuple tuple_items})
    when loc_has_cursor ppat_loc ->
    tuple_items
    |> traverse_tuple_items ~loc_has_cursor ~first_char_before_cursor_no_white
         ~pos_before_cursor
         ~next_pattern_path:(fun item_num ->
           [
             Completable.NVariantPayload
               {constructor_name = Utils.get_unqualified_name txt; item_num};
           ]
           @ pattern_path)
         ~result_from_found_item_num:(fun item_num ->
           [
             Completable.NVariantPayload
               {
                 constructor_name = Utils.get_unqualified_name txt;
                 item_num = item_num + 1;
               };
           ]
           @ pattern_path)
  | Ppat_construct ({txt}, Some p) when loc_has_cursor pat.ppat_loc ->
    p
    |> traverse_pattern ~loc_has_cursor ~first_char_before_cursor_no_white
         ~pos_before_cursor
         ~pattern_path:
           ([
              Completable.NVariantPayload
                {
                  constructor_name = Utils.get_unqualified_name txt;
                  item_num = 0;
                };
            ]
           @ pattern_path)
  | Ppat_variant
      (txt, Some {ppat_loc; ppat_desc = Ppat_construct ({txt = Lident "()"}, _)})
    when loc_has_cursor ppat_loc ->
    (* Empty payload with cursor, like: #test(<com>) *)
    Some
      ( "",
        [Completable.NPolyvariantPayload {constructor_name = txt; item_num = 0}]
        @ pattern_path )
  | Ppat_variant (txt, Some pat)
    when pos_before_cursor >= (pat.ppat_loc |> Loc.end_)
         && first_char_before_cursor_no_white = Some ','
         && is_pattern_tuple pat = false ->
    (* Empty payload with trailing ',', like: #test(true, <com>) *)
    Some
      ( "",
        [Completable.NPolyvariantPayload {constructor_name = txt; item_num = 1}]
        @ pattern_path )
  | Ppat_variant (txt, Some {ppat_loc; ppat_desc = Ppat_tuple tuple_items})
    when loc_has_cursor ppat_loc ->
    tuple_items
    |> traverse_tuple_items ~loc_has_cursor ~first_char_before_cursor_no_white
         ~pos_before_cursor
         ~next_pattern_path:(fun item_num ->
           [Completable.NPolyvariantPayload {constructor_name = txt; item_num}]
           @ pattern_path)
         ~result_from_found_item_num:(fun item_num ->
           [
             Completable.NPolyvariantPayload
               {constructor_name = txt; item_num = item_num + 1};
           ]
           @ pattern_path)
  | Ppat_variant (txt, Some p) when loc_has_cursor pat.ppat_loc ->
    p
    |> traverse_pattern ~loc_has_cursor ~first_char_before_cursor_no_white
         ~pos_before_cursor
         ~pattern_path:
           ([
              Completable.NPolyvariantPayload
                {constructor_name = txt; item_num = 0};
            ]
           @ pattern_path)
  | _ -> None
