(* TODO: Improve error messages? Say why we can't coerce. *)

(* Right now we only allow coercing to primitives string/int/float *)
let can_coerce_primitive (path : Path.t) =
  Path.same path Predef.path_string
  || Path.same path Predef.path_int
  || Path.same path Predef.path_float
  || Path.same path Predef.path_bigint

let check_paths_same p1 p2 target_path =
  Path.same p1 target_path && Path.same p2 target_path

let variant_has_catch_all_case
    (constructors : Types.constructor_declaration list) path_is_same =
  let has_catch_all_string_case (c : Types.constructor_declaration) =
    let args = c.cd_args in
    match args with
    | Cstr_tuple [{desc = Tconstr (p, [], _)}] -> path_is_same p
    | _ -> false
  in

  constructors |> List.exists has_catch_all_string_case

(* Checks if every case of the variant has the same runtime representation as the target type. *)
let variant_has_same_runtime_representation_as_target ~(target_path : Path.t)
    ~unboxed (constructors : Types.constructor_declaration list) =
  (* Helper function to check if a constructor has the same runtime representation as the target type *)
  let has_same_runtime_representation (c : Types.constructor_declaration) =
    let args = c.cd_args in
    let as_payload = Ast_untagged_variants.process_tag_type c.cd_attributes in

    match args with
    | Cstr_tuple [{desc = Tconstr (p, [], _)}] when unboxed ->
      let path_same = check_paths_same p target_path in
      (* unboxed String(string) :> string *)
      path_same Predef.path_string
      (* unboxed Number(float) :> float *)
      || path_same Predef.path_float
      ||
      (* unboxed BigInt(bigint) :> bigint *)
      path_same Predef.path_bigint
    | Cstr_tuple [] -> (
      (* Check that @as payloads match with the target path to coerce to.
           No @as means the default encoding, which is string *)
      match as_payload with
      | None | Some (String _) -> Path.same target_path Predef.path_string
      | Some (Int _) -> Path.same target_path Predef.path_int
      | Some (Float _) -> Path.same target_path Predef.path_float
      | Some (BigInt _) -> Path.same target_path Predef.path_bigint
      | Some (Null | Undefined | Bool _ | Untagged _) -> false)
    | _ -> false
  in

  List.for_all has_same_runtime_representation constructors

let can_try_coerce_variant_to_primitive
    ((_, p, typedecl) : Path.t * Path.t * Types.type_declaration) =
  match typedecl with
  | {type_kind = Type_variant constructors; type_params = []; type_attributes}
    when Path.name p <> "bool" ->
    (* bool is represented as a variant internally, so we need to account for that *)
    Some (constructors, type_attributes |> Ast_untagged_variants.has_untagged)
  | _ -> None

let can_try_coerce_variant_to_primitive_opt p =
  match p with
  | None -> None
  | Some p -> can_try_coerce_variant_to_primitive p

let variant_representation_matches (c1_attrs : Parsetree.attributes)
    (c2_attrs : Parsetree.attributes) =
  match
    ( Ast_untagged_variants.process_tag_type c1_attrs,
      Ast_untagged_variants.process_tag_type c2_attrs )
  with
  | None, None -> true
  | Some s1, Some s2 when s1 = s2 -> true
  | _ -> false

type variant_configuration_error =
  | Untagged of {left_is_unboxed: bool}
  | TagName of {left_tag: string option; right_tag: string option}

type variant_error =
  | VariantError of {
      left_loc: Location.t;
      right_loc: Location.t;
      error: variant_configuration_error;
      is_spread_context: bool;
    }

exception VariantConfigurationError of variant_error

let variant_configuration_can_be_coerced (a1 : Parsetree.attributes)
    (a2 : Parsetree.attributes) =
  let unboxed =
    match
      ( Ast_untagged_variants.process_untagged a1,
        Ast_untagged_variants.process_untagged a2 )
    with
    | true, true | false, false -> true
    | _ -> false
  in
  if not unboxed then false
  else
    let tag =
      match
        ( Ast_untagged_variants.process_tag_name a1,
          Ast_untagged_variants.process_tag_name a2 )
      with
      | Some tag1, Some tag2 when tag1 = tag2 -> true
      | None, None -> true
      | _ -> false
    in
    if not tag then false else true

let variant_configuration_can_be_coerced_raises ~is_spread_context ~left_loc
    ~right_loc ~(left_attributes : Parsetree.attributes)
    ~(right_attributes : Parsetree.attributes) =
  (match
     ( Ast_untagged_variants.process_untagged left_attributes,
       Ast_untagged_variants.process_untagged right_attributes )
   with
  | true, true | false, false -> ()
  | left, _right ->
    raise
      (VariantConfigurationError
         (VariantError
            {
              is_spread_context;
              left_loc;
              right_loc;
              error = Untagged {left_is_unboxed = left};
            })));

  match
    ( Ast_untagged_variants.process_tag_name left_attributes,
      Ast_untagged_variants.process_tag_name right_attributes )
  with
  | Some host_tag, Some spread_tag when host_tag = spread_tag -> ()
  | None, None -> ()
  | left_tag, right_tag ->
    raise
      (VariantConfigurationError
         (VariantError
            {
              is_spread_context;
              left_loc;
              right_loc;
              error = TagName {left_tag; right_tag};
            }))

let can_coerce_polyvariant_to_variant ~row_fields ~variant_constructors
    ~type_attributes =
  let polyvariant_runtime_representations =
    row_fields
    |> List.filter_map (fun (label, (field : Types.row_field)) ->
           (* Check that there's no payload in the polyvariant *)
           match field with
           | Rpresent None -> Some label
           | _ -> None)
  in
  if List.length polyvariant_runtime_representations <> List.length row_fields
  then
    (* Error: At least one polyvariant constructor has a payload. Cannot have payloads. *)
    Error `PolyvariantConstructorHasPayload
  else
    let is_unboxed = Ast_untagged_variants.has_untagged type_attributes in
    if
      List.for_all
        (fun polyvariant_value ->
          variant_constructors
          |> List.exists (fun (c : Types.constructor_declaration) ->
                 let constructor_name = Ident.name c.cd_id in
                 match
                   Ast_untagged_variants.process_tag_type c.cd_attributes
                 with
                 | Some (String as_runtime_string) ->
                   (* `@as("")`, does the configured string match the polyvariant value? *)
                   as_runtime_string = polyvariant_value
                 | Some _ ->
                   (* Any other `@as` can't match since it's by definition not a string *)
                   false
                 | None -> (
                   (* No `@as` means the runtime representation will be the constructor
                      name as a string.

                      However, there's a special case with unboxed types where there's a
                      string catch-all case. In that case, any polyvariant will match,
                      since the catch-all case will match any string. *)
                   match (is_unboxed, c.cd_args) with
                   | true, Cstr_tuple [{desc = Tconstr (p, _, _)}] ->
                     Path.same p Predef.path_string
                   | _ -> polyvariant_value = constructor_name)))
        polyvariant_runtime_representations
    then Ok ()
    else Error `Unknown

let type_is_variant (typ : (Path.t * Path.t * Types.type_declaration) option) =
  match typ with
  | Some (_, _, {type_kind = Type_variant _; _}) -> true
  | _ -> false

let has_res_pat_variant_spread_attribute attrs =
  attrs
  |> List.find_opt (fun (({txt}, _) : Parsetree.attribute) ->
         txt = "res.patVariantSpread")
  |> Option.is_some
