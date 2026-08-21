(* Derivation of the canonical runtime layout of a variant declaration.

   This runs exactly once per declaration, in [Typedecl], after the whole
   recursive group has entered the environment; the result is stored on
   [Type_variant] and never re-derived. It lives above [Ctype] because
   classifying untagged payloads requires expanding their types. *)

open Variant_runtime
open Ast_untagged_variants

let get_block_type_from_typ ~env (t : Types.type_expr) : block_type option =
  (* First check the original (unexpanded) type for typed arrays and other instance types *)
  match type_to_instanceof_backed_obj t with
  | Some instance_type -> Some (InstanceType instance_type)
  | None -> (
    (* If original type didn't match, expand and try standard checks *)
    let expanded_t = Ctype.expand_head env t in
    match expanded_t with
    | {desc = Tconstr (path, _, _)} when Path.same path Predef.path_string ->
      Some StringType
    | {desc = Tconstr (path, _, _)} when Path.same path Predef.path_int ->
      Some IntType
    | {desc = Tconstr (path, _, _)} when Path.same path Predef.path_float ->
      Some FloatType
    | {desc = Tconstr (path, _, _)} when Path.same path Predef.path_bigint ->
      Some BigintType
    | {desc = Tconstr (path, _, _)} when Path.same path Predef.path_bool ->
      Some BooleanType
    | {desc = Tarrow _} -> Some FunctionType
    | {desc = Tconstr _} as expanded_t when type_is_builtin_object expanded_t ->
      Some ObjectType
    | {desc = Tconstr _} as expanded_t
      when type_to_instanceof_backed_obj expanded_t |> Option.is_some -> (
      match type_to_instanceof_backed_obj expanded_t with
      | None -> None
      | Some instance_type -> Some (InstanceType instance_type))
    | {desc = Ttuple _} -> Some (InstanceType Array)
    | _ -> None)

let get_block_type ~env (cstr : Types.constructor_declaration) :
    block_type option =
  match (process_untagged cstr.cd_attributes, cstr.cd_args) with
  | false, _ -> None
  | true, Cstr_tuple [t] when get_block_type_from_typ ~env t |> Option.is_some
    ->
    get_block_type_from_typ ~env t
  | true, Cstr_tuple [ty] -> (
    let default = Some UnknownType in
    match Ctype.extract_concrete_typedecl env ty with
    | _, _, {type_kind = Type_record (_, Record_unboxed _)} -> default
    | _, _, {type_kind = Type_record (_, _)} -> Some ObjectType
    | _ -> default
    | exception _ -> default)
  | true, Cstr_tuple (_ :: _ :: _) ->
    (* C(_, _) with at least 2 args is an object *)
    Some ObjectType
  | true, Cstr_record _ ->
    (* inline record is an object *)
    Some ObjectType
  | true, _ -> None (* TODO: add restrictions here *)

let layout_from_type_variant ?(is_untagged_def = false) ~env
    (cstrs : Types.constructor_declaration list) :
    Variant_runtime.variant_layout =
  let get_block (cstr : Types.constructor_declaration) : block =
    {
      runtime = block_runtime ~name:(Ident.name cstr.cd_id) cstr.cd_attributes;
      block_type = get_block_type ~env cstr;
    }
  in
  let located_constructors =
    List.map
      (fun (cstr : Types.constructor_declaration) ->
        if is_nullary_variant cstr.cd_args then
          let loc, tag = get_cstr_loc_tag cstr in
          (loc, Constant tag)
        else (cstr.cd_loc, Block (get_block cstr)))
      cstrs
  in
  let consts, blocks =
    Ext_list.fold_left located_constructors ([], [])
      (fun (consts, blocks) (loc, constructor) ->
        match constructor with
        | Constant tag -> ((loc, tag) :: consts, blocks)
        | Block block -> (consts, (loc, block) :: blocks))
  in
  check_invariant ~is_untagged_def ~consts ~blocks;
  let constructors =
    Array.of_list
      (List.map (fun (_, constructor) -> constructor) located_constructors)
  in
  constructors
