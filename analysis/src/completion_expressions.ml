open Shared_types

let is_expr_hole exp =
  match exp.Parsetree.pexp_desc with
  | Pexp_extension ({txt = "rescript.exprhole"}, _) -> true
  | _ -> false

let is_expr_tuple expr =
  match expr.Parsetree.pexp_desc with
  | Pexp_tuple _ -> true
  | _ -> false

let rec traverse_expr (exp : Parsetree.expression) ~expr_path ~pos
    ~first_char_before_cursor_no_white =
  let loc_has_cursor loc = loc |> Cursor_position.loc_has_cursor ~pos in
  let some_if_has_cursor v =
    if loc_has_cursor exp.pexp_loc then Some v else None
  in
  match exp.pexp_desc with
  | Pexp_ident {txt = Lident txt} when Utils.has_braces exp.pexp_attributes ->
    (* An ident with braces attribute corresponds to for example `{n}`.
       Looks like a record but is parsed as an ident with braces. *)
    some_if_has_cursor
      (txt, [Completable.NRecordBody {seen_fields = []}] @ expr_path)
  | Pexp_ident {txt = Lident txt} -> some_if_has_cursor (txt, expr_path)
  | Pexp_construct ({txt = Lident "()"}, _) -> some_if_has_cursor ("", expr_path)
  | Pexp_construct ({txt = Lident txt}, None) ->
    some_if_has_cursor (txt, expr_path)
  | Pexp_variant (label, None) -> some_if_has_cursor ("#" ^ label, expr_path)
  | Pexp_array array_patterns -> (
    let next_expr_path = [Completable.NArray] @ expr_path in
    (* No fields but still has cursor = empty completion *)
    if List.length array_patterns = 0 && loc_has_cursor exp.pexp_loc then
      Some ("", next_expr_path)
    else
      let array_item_with_cursor =
        array_patterns
        |> List.find_map (fun e ->
               e
               |> traverse_expr ~expr_path:next_expr_path
                    ~first_char_before_cursor_no_white ~pos)
      in

      match (array_item_with_cursor, loc_has_cursor exp.pexp_loc) with
      | Some array_item_with_cursor, _ -> Some array_item_with_cursor
      | None, true when first_char_before_cursor_no_white = Some ',' ->
        (* No item had the cursor, but the entire expr still has the cursor (so
           the cursor is in the array somewhere), and the first char before the
           cursor is a comma = interpret as compleing for a new value (example:
           `[None, <com>, None]`) *)
        Some ("", next_expr_path)
      | _ -> None)
  | Pexp_tuple tuple_items when loc_has_cursor exp.pexp_loc ->
    tuple_items
    |> traverse_expr_tuple_items ~first_char_before_cursor_no_white ~pos
         ~next_expr_path:(fun item_num ->
           [Completable.NTupleItem {item_num}] @ expr_path)
         ~result_from_found_item_num:(fun item_num ->
           [Completable.NTupleItem {item_num = item_num + 1}] @ expr_path)
  | Pexp_record ([], _) ->
    (* Empty fields means we're in a record body `{}`. Complete for the fields. *)
    some_if_has_cursor
      ("", [Completable.NRecordBody {seen_fields = []}] @ expr_path)
  | Pexp_record (fields, _) -> (
    let field_with_cursor = ref None in
    let field_with_expr_hole = ref None in
    Ext_list.iter fields (fun {lid = fname; x = exp} ->
        match
          ( fname.Location.txt,
            exp.Parsetree.pexp_loc |> Cursor_position.classify_loc ~pos )
        with
        | Longident.Lident fname, HasCursor ->
          field_with_cursor := Some (fname, exp)
        | Lident fname, _ when is_expr_hole exp ->
          field_with_expr_hole := Some (fname, exp)
        | _ -> ());
    let seen_fields =
      Ext_list.filter_map fields (fun {lid = field_name} ->
          match field_name with
          | {Location.txt = Longident.Lident field_name} -> Some field_name
          | _ -> None)
    in
    match (!field_with_cursor, !field_with_expr_hole) with
    | Some (fname, f), _ | None, Some (fname, f) -> (
      match f.pexp_desc with
      | Pexp_extension ({txt = "rescript.exprhole"}, _) ->
        (* An expression hole means for example `{someField: <com>}`. We want to complete for the type of `someField`.  *)
        some_if_has_cursor
          ("", [Completable.NFollowRecordField {field_name = fname}] @ expr_path)
      | Pexp_ident {txt = Lident txt} when fname = txt ->
        (* This is a heuristic for catching writing field names. ReScript has punning for record fields, but the AST doesn't,
           so punning is represented as the record field name and identifier being the same: {someField}. *)
        some_if_has_cursor
          (txt, [Completable.NRecordBody {seen_fields}] @ expr_path)
      | Pexp_ident {txt = Lident txt} ->
        (* A var means `{someField: s}` or similar. Complete for identifiers or values. *)
        some_if_has_cursor (txt, expr_path)
      | _ ->
        f
        |> traverse_expr ~first_char_before_cursor_no_white ~pos
             ~expr_path:
               ([Completable.NFollowRecordField {field_name = fname}]
               @ expr_path))
    | None, None -> (
      if Debug.verbose () then (
        Printf.printf "[traverse_expr] No field with cursor and no expr hole.\n";

        match first_char_before_cursor_no_white with
        | None -> ()
        | Some c ->
          Printf.printf "[traverse_expr] firstCharBeforeCursorNoWhite: %c.\n" c);

      (* Figure out if we're completing for a new field.
         If the cursor is inside of the record body, but no field has the cursor,
         and there's no pattern hole. Check the first char to the left of the cursor,
         ignoring white space. If that's a comma or {, we assume you're completing for a new field,
         since you're either between 2 fields (comma to the left) or at the start of the record ({). *)
      match first_char_before_cursor_no_white with
      | Some (',' | '{') ->
        some_if_has_cursor
          ("", [Completable.NRecordBody {seen_fields}] @ expr_path)
      | _ -> None))
  | Pexp_construct
      ( {txt},
        Some {pexp_loc; pexp_desc = Pexp_construct ({txt = Lident "()"}, _)} )
    when loc_has_cursor pexp_loc ->
    (* Empty payload with cursor, like: Test(<com>) *)
    Some
      ( "",
        [
          Completable.NVariantPayload
            {constructor_name = Utils.get_unqualified_name txt; item_num = 0};
        ]
        @ expr_path )
  | Pexp_construct ({txt}, Some e)
    when pos >= (e.pexp_loc |> Loc.end_)
         && first_char_before_cursor_no_white = Some ','
         && is_expr_tuple e = false ->
    (* Empty payload with trailing ',', like: Test(true, <com>) *)
    Some
      ( "",
        [
          Completable.NVariantPayload
            {constructor_name = Utils.get_unqualified_name txt; item_num = 1};
        ]
        @ expr_path )
  | Pexp_construct ({txt}, Some {pexp_loc; pexp_desc = Pexp_tuple tuple_items})
    when loc_has_cursor pexp_loc ->
    tuple_items
    |> traverse_expr_tuple_items ~first_char_before_cursor_no_white ~pos
         ~next_expr_path:(fun item_num ->
           [
             Completable.NVariantPayload
               {constructor_name = Utils.get_unqualified_name txt; item_num};
           ]
           @ expr_path)
         ~result_from_found_item_num:(fun item_num ->
           [
             Completable.NVariantPayload
               {
                 constructor_name = Utils.get_unqualified_name txt;
                 item_num = item_num + 1;
               };
           ]
           @ expr_path)
  | Pexp_construct ({txt}, Some p) when loc_has_cursor exp.pexp_loc ->
    p
    |> traverse_expr ~first_char_before_cursor_no_white ~pos
         ~expr_path:
           ([
              Completable.NVariantPayload
                {
                  constructor_name = Utils.get_unqualified_name txt;
                  item_num = 0;
                };
            ]
           @ expr_path)
  | Pexp_variant
      (txt, Some {pexp_loc; pexp_desc = Pexp_construct ({txt = Lident "()"}, _)})
    when loc_has_cursor pexp_loc ->
    (* Empty payload with cursor, like: #test(<com>) *)
    Some
      ( "",
        [Completable.NPolyvariantPayload {constructor_name = txt; item_num = 0}]
        @ expr_path )
  | Pexp_variant (txt, Some e)
    when pos >= (e.pexp_loc |> Loc.end_)
         && first_char_before_cursor_no_white = Some ','
         && is_expr_tuple e = false ->
    (* Empty payload with trailing ',', like: #test(true, <com>) *)
    Some
      ( "",
        [Completable.NPolyvariantPayload {constructor_name = txt; item_num = 1}]
        @ expr_path )
  | Pexp_variant (txt, Some {pexp_loc; pexp_desc = Pexp_tuple tuple_items})
    when loc_has_cursor pexp_loc ->
    tuple_items
    |> traverse_expr_tuple_items ~first_char_before_cursor_no_white ~pos
         ~next_expr_path:(fun item_num ->
           [Completable.NPolyvariantPayload {constructor_name = txt; item_num}]
           @ expr_path)
         ~result_from_found_item_num:(fun item_num ->
           [
             Completable.NPolyvariantPayload
               {constructor_name = txt; item_num = item_num + 1};
           ]
           @ expr_path)
  | Pexp_variant (txt, Some p) when loc_has_cursor exp.pexp_loc ->
    p
    |> traverse_expr ~first_char_before_cursor_no_white ~pos
         ~expr_path:
           ([
              Completable.NPolyvariantPayload
                {constructor_name = txt; item_num = 0};
            ]
           @ expr_path)
  | _ -> None

and traverse_expr_tuple_items tuple_items ~next_expr_path
    ~result_from_found_item_num ~pos ~first_char_before_cursor_no_white =
  let item_num = ref (-1) in
  let item_with_cursor =
    tuple_items
    |> List.find_map (fun e ->
           item_num := !item_num + 1;
           e
           |> traverse_expr ~expr_path:(next_expr_path !item_num)
                ~first_char_before_cursor_no_white ~pos)
  in
  match (item_with_cursor, first_char_before_cursor_no_white) with
  | None, Some ',' ->
    (* No tuple item has the cursor, but there's a comma before the cursor.
       Figure out what arg we're trying to complete. Example: (true, <com>, None) *)
    let pos_num = ref (-1) in
    tuple_items
    |> List.iteri (fun index e ->
           if pos >= Loc.start e.Parsetree.pexp_loc then pos_num := index);
    if !pos_num > -1 then Some ("", result_from_found_item_num !pos_num)
    else None
  | v, _ -> v

let pretty_print_fn_template_arg_name ?current_index ~env ~state ~full
    (arg_typ : Types.type_expr) =
  let index_text =
    match current_index with
    | None -> ""
    | Some i -> string_of_int i
  in
  let default_var_name = "v" ^ index_text in
  let arg_typ, suffix, _env =
    Type_utils.dig_to_relevant_template_name_type ~env ~package:full.package
      ~state arg_typ
  in
  match arg_typ |> Type_utils.path_from_type_expr with
  | None -> default_var_name
  | Some p -> (
    let trailing_elements_of_path =
      p |> Utils.expand_path |> List.rev |> Utils.last_elements
    in
    match trailing_elements_of_path with
    | [] | ["t"] -> default_var_name
    | ["unit"] -> "()"
    (* Special treatment for JsxEvent, since that's a common enough thing
       used in event handlers. *)
    | ["JsxEvent"; "synthetic"] -> "event"
    | ["synthetic"] -> "event"
    (* Ignore `t` types, and go for its module name instead. *)
    | [some_name; "t"] | [_; some_name] | [some_name] -> (
      match some_name with
      | "string" | "int" | "float" | "array" | "option" | "bool" ->
        default_var_name
      | some_name when String.length some_name < 30 ->
        if some_name = "synthetic" then
          Printf.printf "synthetic! %s\n"
            (trailing_elements_of_path |> Shared_types.ident);
        (* We cap how long the name can be, so we don't end up with super
           long type names. *)
        (some_name |> Utils.lowercase_first_char) ^ suffix
      | _ -> default_var_name)
    | _ -> default_var_name)

let complete_constructor_payload ~pos_before_cursor
    ~first_char_before_cursor_no_white
    (constructor_lid : Longident.t Location.loc) expr =
  match
    traverse_expr expr ~expr_path:[] ~pos:pos_before_cursor
      ~first_char_before_cursor_no_white
  with
  | None -> None
  | Some (prefix, nested) ->
    (* The nested path must start with the constructor name found, plus
       the target argument number for the constructor. We translate to
       that here, because we need to account for multi arg constructors
       being represented as tuples. *)
    let nested =
      match List.rev nested with
      | Completable.NTupleItem {item_num} :: rest ->
        [
          Completable.NVariantPayload
            {constructor_name = Longident.last constructor_lid.txt; item_num};
        ]
        @ rest
      | nested ->
        [
          Completable.NVariantPayload
            {
              constructor_name = Longident.last constructor_lid.txt;
              item_num = 0;
            };
        ]
        @ nested
    in
    let variant_ctx_path =
      Completable.CTypeAtPos
        {constructor_lid.loc with loc_start = constructor_lid.loc.loc_end}
    in
    Some
      (Completable.Cexpression {context_path = variant_ctx_path; prefix; nested})
