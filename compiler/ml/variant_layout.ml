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

let get_block_type ~env (cstr : Types.constructor_declaration) : block_type =
  match cstr.cd_args with
  | Cstr_tuple [ty] -> (
    match get_block_type_from_typ ~env ty with
    | Some shape -> shape
    | None -> (
      match Ctype.extract_concrete_typedecl env ty with
      | _, _, {type_kind = Type_record (_, Record_unboxed _)} -> UnknownType
      | _, _, {type_kind = Type_record (_, _)} -> ObjectType
      | _ -> UnknownType
      | exception _ -> UnknownType))
  | Cstr_tuple (_ :: _ :: _) ->
    report_constructor_more_than_one_arg ~loc:cstr.cd_loc
      ~name:(Ident.name cstr.cd_id)
  | Cstr_record _ -> ObjectType
  | Cstr_tuple [] ->
    (* Nullary constructors are handled before computing a payload shape. *)
    assert false

let empty_domain : value_domain = {literals = []; shapes = []}

let record_shape fields =
  if
    Lambda.record_fields_are_array
      (Array.of_list
         (List.map
            (fun (field : Types.label_declaration) ->
              (Record_runtime.declaration_name field, field.ld_optional))
            fields))
  then InstanceType Array
  else ObjectType

(* Field domains need actual type identities, not the legacy classifier's
   name hints. In particular, local [dict] types can shadow the builtin, and
   typed-array names are aliases of one shared abstract representation. *)
let field_shape ~env ty =
  let primitive =
    match get_block_type_from_typ ~env ty with
    | Some
        (( IntType | FloatType | StringType | BigintType | BooleanType
         | FunctionType ) as shape) ->
      Some shape
    | Some (InstanceType _ | ObjectType | UnknownType | ObjectDiscriminant _)
    | None ->
      None
  in
  match primitive with
  | Some _ -> primitive
  | None -> (
    match ty.Types.desc with
    | Ttuple _ -> Some (InstanceType Array)
    | Tconstr (path, _, _) when Path.same path Predef.path_dict ->
      Some ObjectType
    | Tconstr (path, _, _) when Path.same path Predef.path_array ->
      Some (InstanceType Array)
    | Tconstr (path, _, _) when Path.same path Predef.path_promise ->
      Some (InstanceType Promise)
    | Tconstr (path, _, _) -> (
      match Path.flatten (Env.normalize_path_prefix None env path) with
      | `Ok (root, ["t"]) when Ident.persistent root ->
        if Ident.name root = "Stdlib_TypedArray" then
          Some (InstanceType TypedArray)
        else
          List.find_map
            (fun instance ->
              if Ident.name root = "Stdlib_" ^ Instance.to_string instance then
                Some (InstanceType instance)
              else None)
            [
              ArrayBuffer;
              Blob;
              DataView;
              Date;
              File;
              RegExp;
              Set;
              Map;
              WeakSet;
              WeakMap;
            ]
      | `Ok _ | `Contains_apply -> None)
    | _ -> None)

(* Inspect declarations rather than layouts: a field's variant may belong to
   the same recursive group and still have a pending layout. Recursive unboxed
   payloads cannot be classified by finitely expanding their representation. *)
let field_domain ~env ty =
  let rec derive fuel seen ty =
    if fuel < 0 then
      Result.Error "exceeds the unboxed runtime domain expansion limit (100)"
    else
      let ty = Ctype.expand_head env ty in
      match field_shape ~env ty with
      | Some shape -> Ok (Variant_domain.singleton_shape shape)
      | None -> (
        match ty.desc with
        | Tconstr (path, _, _) when Path.same path Predef.path_unit ->
          Ok (Variant_domain.singleton_literal Undefined)
        | Tconstr (path, _, _) when Path.same path Predef.path_list ->
          Ok
            (Variant_domain.normalize
               {literals = [Int 0]; shapes = [ObjectType]})
        | Tconstr (path, _, _) when Path.same path Predef.path_option ->
          Result.Error
            "uses the special option representation, which is not supported as \
             a discriminator"
        | Tconstr (path, args, _) -> (
          if
            (* Repeated declarations with different arguments can be finite, as in
             wrapped<wrapped<string>>. Compare instantiated types instead. *)
            List.exists
              (fun previous -> Ctype.equal env false [previous] [ty])
              seen
          then Result.Error "has a recursive unboxed runtime domain"
          else
            match Env.find_type path env with
            | {type_private = Private} ->
              Result.Error "has a private runtime representation"
            | {type_kind = Type_record (fields, Record_regular)} ->
              Ok (Variant_domain.singleton_shape (record_shape fields))
            | {type_kind = Type_variant (constructors, _)}
              when Datarepr.variant_has_optional_shape constructors ->
              Result.Error
                "uses the special option representation, which is not \
                 supported as a discriminator"
            | {type_kind = Type_variant (constructors, _)}
              when List.exists
                     (fun cstr -> cstr.Types.cd_res <> None)
                     constructors ->
              (* Expanding every constructor would include values that cannot
               inhabit this GADT's indices, making literal-first overlap unsafe. *)
              Result.Error
                "has GADT constructors, whose type refinements are not \
                 supported in discriminator domains"
            | {
             type_kind = Type_variant (constructors, _);
             type_params;
             type_attributes;
             type_representation;
            }
              when constructors <> [] ->
              let substitute ty = Ctype.apply env type_params ty args in
              let unboxed = has_untagged type_attributes in
              let rec collect domain = function
                | [] -> Ok domain
                | (cstr : Types.constructor_declaration) :: rest -> (
                  let next =
                    match cstr.cd_args with
                    | Cstr_tuple [] ->
                      let literal =
                        match cstr.cd_runtime_tag with
                        | Some literal -> literal
                        | None -> String (Ident.name cstr.cd_id)
                      in
                      Ok (Variant_domain.singleton_literal literal)
                    | Cstr_tuple [payload]
                      when unboxed || has_untagged cstr.cd_attributes ->
                      derive (fuel - 1) (ty :: seen) (substitute payload)
                    | Cstr_record [{ld_type; ld_mutable = Immutable}]
                      when type_representation = Unboxed ->
                      (* A single immutable inline field is erased together with
                       its constructor, so the field itself is the discriminator. *)
                      derive (fuel - 1) (ty :: seen) (substitute ld_type)
                    | Cstr_record _ ->
                      Ok (Variant_domain.singleton_shape ObjectType)
                    | Cstr_tuple (_ :: _) ->
                      if unboxed || has_untagged cstr.cd_attributes then
                        Result.Error "has an unsupported unboxed payload"
                      else Ok (Variant_domain.singleton_shape ObjectType)
                  in
                  match next with
                  | Result.Error reason -> Result.Error reason
                  | Ok next -> collect (Variant_domain.union domain next) rest)
              in
              collect empty_domain constructors
            | {type_kind = Type_abstract} ->
              Result.Error "has an abstract runtime representation"
            | {type_kind = Type_record _ | Type_variant _ | Type_open} ->
              Result.Error "has an unsupported runtime representation"
            | exception Not_found -> Result.Error "has an abstract type")
        | Tvar _ | Tunivar _ -> Result.Error "has an unresolved type parameter"
        | _ -> Result.Error "has an unsupported runtime representation")
  in
  (* As in [Typedecl.get_unboxed_type_representation], bound expansion even
     when nonregular recursion keeps producing different type arguments. *)
  try derive 100 [] ty with
  | Ctype.Cannot_apply -> Result.Error "has unresolved type parameters"
  | Invalid_argument reason -> Result.Error ("has an invalid literal: " ^ reason)

(* Literal-first dispatch may reinterpret a broad case as a specific case.
   Require equal remaining payload layouts, including mutable field types;
   checking only the discriminator would permit reading missing or mistyped
   properties. Compare runtime names so field renaming remains transparent. *)
let compatible_object_payloads ~env ~field_name fields1 fields2 =
  let remaining fields =
    List.filter
      (fun field -> Record_runtime.declaration_name field <> field_name)
      fields
    |> List.sort (fun left right ->
        String.compare
          (Record_runtime.declaration_name left)
          (Record_runtime.declaration_name right))
  in
  let rec compare_fields left right =
    match (left, right) with
    | [], [] -> Ok ()
    | field1 :: rest1, field2 :: rest2 ->
      let name1 = Record_runtime.declaration_name field1 in
      let name2 = Record_runtime.declaration_name field2 in
      if name1 <> name2 then
        Result.Error "their remaining runtime field names differ"
      else if field1.ld_optional <> field2.ld_optional then
        Result.Error (Printf.sprintf "field %S has different optionality" name1)
      else if field1.ld_mutable <> field2.ld_mutable then
        Result.Error (Printf.sprintf "field %S has different mutability" name1)
      else if not (Ctype.equal env false [field1.ld_type] [field2.ld_type]) then
        Result.Error (Printf.sprintf "field %S has different types" name1)
      else compare_fields rest1 rest2
    | [], _ :: _ | _ :: _, [] ->
      Result.Error "their remaining runtime field names differ"
  in
  compare_fields (remaining fields1) (remaining fields2)

let object_fields ~env (cstr : Types.constructor_declaration) =
  let check_fields fields =
    if record_shape fields = InstanceType Array then
      Result.Error "record has an array runtime representation"
    else Ok fields
  in
  match cstr.cd_args with
  | Cstr_record fields -> Ok fields
  | Cstr_tuple [ty] -> (
    let ty = Ctype.expand_head env ty in
    match ty.desc with
    | Tconstr (path, args, _) -> (
      match Env.find_type path env with
      | {
       type_private = Public;
       type_kind = Type_record (fields, Record_regular);
       type_params;
      } -> (
        try
          check_fields
            (List.map
               (fun (field : Types.label_declaration) ->
                 {
                   field with
                   ld_type = Ctype.apply env type_params field.ld_type args;
                 })
               fields)
        with Ctype.Cannot_apply ->
          Result.Error "record parameters cannot be resolved")
      | _ -> Result.Error "payload is not a public ordinary record"
      | exception Not_found -> Result.Error "payload is abstract")
    | _ -> Result.Error "payload is not a named record")
  | Cstr_tuple [] | Cstr_tuple (_ :: _ :: _) ->
    Result.Error "payload is not a single record"

let derive_object_discriminants ~env located_constructors cstrs =
  let objects =
    List.filter_map
      (fun (cstr, (_, constructor)) ->
        match constructor with
        | Block (Untagged {block_type = ObjectType}) -> Some cstr
        | Constant _ | Block _ -> None)
      (List.combine cstrs located_constructors)
  in
  match objects with
  | [] | [_] -> located_constructors
  | first :: _ ->
    let names = List.map (fun cstr -> Ident.name cstr.Types.cd_id) objects in
    let reject reason =
      raise
        (Ast_untagged_variants.Error
           ( first.cd_loc,
             InvalidUntaggedVariantDefinition
               (InvalidObjectDiscriminant (names, reason)) ))
    in
    if
      List.exists
        (function
          | _, Block (Tagged _) -> true
          | _, (Constant _ | Block (Untagged _)) -> false)
        located_constructors
    then
      reject "Tagged object cases cannot be combined with object discriminants.";
    let fields =
      List.map
        (fun cstr ->
          match object_fields ~env cstr with
          | Ok fields -> (cstr, fields)
          | Result.Error reason ->
            reject ("Case " ^ Ident.name cstr.cd_id ^ ": " ^ reason ^ "."))
        objects
    in
    let candidates =
      match fields with
      | (_, first_fields) :: rest ->
        List.filter_map
          (fun field ->
            let name = Record_runtime.declaration_name field in
            if
              List.for_all
                (fun (_, fields) ->
                  List.exists
                    (fun field -> Record_runtime.declaration_name field = name)
                    fields)
                rest
            then Some name
            else None)
          first_fields
        |> List.sort_uniq String.compare
      | [] -> assert false
    in
    let validate field_name =
      let conflict name other_name detail =
        Result.Error
          ("Runtime field "
          ^ Printf.sprintf "%S" field_name
          ^ " overlaps between cases " ^ other_name ^ " and " ^ name ^ " "
          ^ detail ^ ".")
      in
      let rec check_overlaps (cstr : Types.constructor_declaration) fields
          domain = function
        | [] -> Ok ()
        | (other_cstr, other_fields, other_domain) :: rest ->
          let name = Ident.name cstr.cd_id in
          let other_name = Ident.name other_cstr.Types.cd_id in
          let rec check = function
            | [] -> check_overlaps cstr fields domain rest
            | Variant_domain.Shared_literal literal :: _ ->
              conflict name other_name
                ("at literal " ^ Variant_domain.literal_to_string literal)
            | Shared_shape (left, right) :: _ ->
              conflict name other_name
                ("because "
                ^ Variant_domain.shape_to_string left
                ^ " and "
                ^ Variant_domain.shape_to_string right
                ^ " cannot be distinguished at runtime")
            | Literal_shape (literal, _) :: _
              when cstr.cd_res <> None || other_cstr.cd_res <> None ->
              (* Equal payloads do not establish equal GADT refinements. A
                 literal must not reinterpret a witness of one type as another. *)
              conflict name other_name
                ("at literal "
                ^ Variant_domain.literal_to_string literal
                ^ ". Literal-first overlap involving GADT constructors is not \
                   supported because matching a different constructor can \
                   change its type refinements. Use disjoint discriminator \
                   values or types")
            | Literal_shape (literal, _) :: rest -> (
              match
                compatible_object_payloads ~env ~field_name fields other_fields
              with
              | Ok () -> check rest
              | Result.Error reason ->
                conflict name other_name
                  ("at literal "
                  ^ Variant_domain.literal_to_string literal
                  ^ ", but " ^ reason
                  ^ ". Literal-first matching requires compatible remaining \
                     payload fields"))
          in
          check (Variant_domain.overlaps domain other_domain)
      in
      let rec collect previous = function
        | [] -> Ok (List.rev previous)
        | (cstr, fields) :: rest -> (
          let name = Ident.name cstr.Types.cd_id in
          let field =
            List.find
              (fun field -> Record_runtime.declaration_name field = field_name)
              fields
          in
          let fail reason =
            Result.Error
              ("Runtime field "
              ^ Printf.sprintf "%S" field_name
              ^ " in case " ^ name ^ " " ^ reason ^ ".")
          in
          if field.ld_optional then fail "is optional"
          else if field.ld_mutable = Mutable then fail "is mutable"
          else
            match field_domain ~env field.ld_type with
            | Result.Error reason -> fail reason
            | Ok domain -> (
              match check_overlaps cstr fields domain previous with
              | Result.Error reason -> Result.Error reason
              | Ok () -> collect ((cstr, fields, domain) :: previous) rest))
      in
      collect [] fields
    in
    let rec choose errors = function
      | [] ->
        reject
          (if errors = [] then "No runtime field is shared by all object cases."
           else String.concat " " (List.rev errors))
      | field_name :: rest -> (
        match validate field_name with
        | Result.Error reason -> choose (reason :: errors) rest
        | Ok domains ->
          List.map
            (fun (loc, constructor) ->
              match constructor with
              | Block (Untagged {tag; block_type = ObjectType}) ->
                let _, _, domain =
                  List.find
                    (fun (cstr, _, _) -> Ident.name cstr.Types.cd_id = tag.name)
                    domains
                in
                (* Exclusions belong to the declaration, not a particular
                   match. They are also part of interface/coercion identity:
                   adding a literal must not silently reinterpret a fallback. *)
                let excluded =
                  List.fold_left
                    (fun excluded (cstr, _, other) ->
                      if Ident.name cstr.Types.cd_id = tag.name then excluded
                      else
                        let literals =
                          List.filter
                            (fun literal ->
                              List.exists
                                (Variant_domain.literal_matches_shape literal)
                                domain.shapes)
                            other.literals
                        in
                        let shapes =
                          if List.mem ObjectType domain.shapes then
                            List.filter
                              (function
                                | InstanceType Array -> false
                                | InstanceType _ -> true
                                | _ -> false)
                              other.shapes
                          else []
                        in
                        Variant_domain.union excluded {literals; shapes})
                    empty_domain domains
                in
                ( loc,
                  Block
                    (Untagged
                       {
                         tag;
                         block_type =
                           ObjectDiscriminant {field_name; domain; excluded};
                       }) )
              | Constant _ | Block _ -> (loc, constructor))
            located_constructors)
    in
    choose [] candidates

let layout_from_type_variant ~(configuration : configuration) ~env
    (cstrs : Types.constructor_declaration list) : Variant_runtime.layout =
  let get_block (cstr : Types.constructor_declaration) : block =
    let tag = {name = Ident.name cstr.cd_id; literal = cstr.cd_runtime_tag} in
    let tag_name = process_tag_name cstr.cd_attributes in
    if process_untagged cstr.cd_attributes then
      Untagged {tag; block_type = get_block_type ~env cstr}
    else Tagged {tag; tag_name}
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
  let located_constructors =
    if configuration.unboxed then
      derive_object_discriminants ~env located_constructors cstrs
    else located_constructors
  in
  let consts, blocks =
    Ext_list.fold_left located_constructors ([], [])
      (fun (consts, blocks) (loc, constructor) ->
        match constructor with
        | Constant tag -> ((loc, tag) :: consts, blocks)
        | Block block -> (consts, (loc, block) :: blocks))
  in
  check_invariant ~is_untagged_def:configuration.unboxed ~consts ~blocks;
  let constructors =
    Array.of_list
      (List.map (fun (_, constructor) -> constructor) located_constructors)
  in
  Variant_runtime.make_layout ~configuration constructors
