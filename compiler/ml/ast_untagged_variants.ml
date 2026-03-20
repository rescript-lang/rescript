module Instance = struct
  type t =
    | Array
    | ArrayBuffer
    | BigInt64Array
    | BigUint64Array
    | Blob
    | DataView
    | Date
    | File
    | Float32Array
    | Float64Array
    | Int16Array
    | Int32Array
    | Int8Array
    | Promise
    | RegExp
    | Uint16Array
    | Uint32Array
    | Uint8Array
    | Uint8ClampedArray
    | Set
    | Map
    | WeakSet
    | WeakMap
  let to_string = function
    | Array -> "Array"
    | ArrayBuffer -> "ArrayBuffer"
    | BigInt64Array -> "BigInt64Array"
    | BigUint64Array -> "BigUint64Array"
    | Blob -> "Blob"
    | DataView -> "DataView"
    | Date -> "Date"
    | File -> "File"
    | Float32Array -> "Float32Array"
    | Float64Array -> "Float64Array"
    | Int16Array -> "Int16Array"
    | Int32Array -> "Int32Array"
    | Int8Array -> "Int8Array"
    | Promise -> "Promise"
    | RegExp -> "RegExp"
    | Uint16Array -> "Uint16Array"
    | Uint32Array -> "Uint32Array"
    | Uint8Array -> "Uint8Array"
    | Uint8ClampedArray -> "Uint8ClampedArray"
    | Set -> "Set"
    | Map -> "Map"
    | WeakSet -> "WeakSet"
    | WeakMap -> "WeakMap"
end

type untagged_error =
  | OnlyOneUnknown of string
  | AtMostOneObject
  | AtMostOneInstance of Instance.t
  | AtMostOneFunction
  | AtMostOneString
  | AtMostOneNumber
  | AtMostOneBigint
  | AtMostOneBoolean
  | DuplicateLiteral of string
  | ConstructorMoreThanOneArg of string
type error =
  | InvalidVariantAsAnnotation
  | InvalidVariantCatchAnnotation
  | Duplicated_bs_as
  | DuplicatedVariantConstructorAnnotation
  | InvalidVariantTagAnnotation
  | InvalidUntaggedVariantDefinition of untagged_error
  | TagFieldNameConflict of string * string * string
  | TaggedPrimitiveCatchAll_TaggedVariantRequired
  | TaggedPrimitiveCatchAll_UnboxedVariantUnsupported
  | TaggedPrimitiveCatchAll_AtMostOneNumber
  | TaggedPrimitiveCatchAll_AtMostOneString
  | TaggedPrimitiveCatchAll_OnNullaryConstructor of string
  | TaggedPrimitiveCatchAll_InlineRecordRequired of string
  | TaggedPrimitiveCatchAll_TooManyTagFields of string * string
  | TaggedPrimitiveCatchAll_TagFieldOptional of string * string
  | TaggedPrimitiveCatchAll_TagFieldWrongType of string * string * string
exception Error of Location.t * error

let report_error ppf =
  let open Format in
  function
  | InvalidVariantAsAnnotation ->
    fprintf ppf
      "A variant case annotation @as(...) must be a string or integer, \
       boolean, null, undefined"
  | InvalidVariantCatchAnnotation ->
    fprintf ppf
      "A variant case annotation @catch(...) must be int, float, or string"
  | Duplicated_bs_as -> fprintf ppf "duplicate @as "
  | DuplicatedVariantConstructorAnnotation ->
    fprintf ppf
      "duplicate constructor annotation, use only one of @as or @catch"
  | InvalidVariantTagAnnotation ->
    fprintf ppf "A variant tag annotation @tag(...) must be a string"
  | InvalidUntaggedVariantDefinition untagged_variant ->
    fprintf ppf "This untagged variant definition is invalid: %s"
      (match untagged_variant with
      | OnlyOneUnknown name ->
        "Case " ^ name
        ^ " has a payload that is not of one of the recognized shapes (object, \
           array, etc). Then it must be the only case with payloads."
      | AtMostOneObject -> "At most one case can be an object type."
      | AtMostOneInstance Array ->
        "At most one case can be an array or tuple type."
      | AtMostOneInstance i ->
        "At most one case can be a " ^ Instance.to_string i ^ " type."
      | AtMostOneFunction -> "At most one case can be a function type."
      | AtMostOneString -> "At most one case can be a string type."
      | AtMostOneBoolean -> "At most one case can be a boolean type."
      | AtMostOneNumber ->
        "At most one case can be a number type (int or float)."
      | AtMostOneBigint -> "At most one case can be a bigint type."
      | DuplicateLiteral s -> "Duplicate literal " ^ s ^ "."
      | ConstructorMoreThanOneArg name ->
        "Constructor " ^ name ^ " has more than one argument.")
  | TagFieldNameConflict (constructor_name, field_name, runtime_value) ->
    fprintf ppf
      "Constructor \"%s\": the @tag name \"%s\" conflicts with the runtime \
       value of inline record field \"%s\". Use a different @tag name or \
       rename the field."
      constructor_name runtime_value field_name
  | TaggedPrimitiveCatchAll_TaggedVariantRequired ->
    fprintf ppf
      "Primitive catch-all @catch(int|float|string) requires an explicit \
       @tag(\"...\") annotation on the variant type"
  | TaggedPrimitiveCatchAll_UnboxedVariantUnsupported ->
    fprintf ppf
      "Primitive catch-all @catch(int|float|string) is not allowed on @unboxed \
       variants"
  | TaggedPrimitiveCatchAll_AtMostOneNumber ->
    fprintf ppf
      "At most one number catch-all (@catch(int|float)) is allowed per variant"
  | TaggedPrimitiveCatchAll_AtMostOneString ->
    fprintf ppf
      "At most one string catch-all (@catch(string)) is allowed per variant"
  | TaggedPrimitiveCatchAll_OnNullaryConstructor name ->
    fprintf ppf
      "Constructor \"%s\": primitive catch-all @catch(int|float|string) is not \
       allowed on nullary constructors"
      name
  | TaggedPrimitiveCatchAll_InlineRecordRequired name ->
    fprintf ppf
      "Constructor \"%s\": primitive catch-all requires an inline record \
       payload"
      name
  | TaggedPrimitiveCatchAll_TooManyTagFields (name, tag_name) ->
    fprintf ppf
      "Constructor \"%s\": inline record may expose the discriminant through \
       at most one field named \"%s\" (or @as(\"%s\"))"
      name tag_name tag_name
  | TaggedPrimitiveCatchAll_TagFieldOptional (name, field_name) ->
    fprintf ppf
      "Constructor \"%s\": field \"%s\" must not be optional for primitive \
       catch-all"
      name field_name
  | TaggedPrimitiveCatchAll_TagFieldWrongType (name, field_name, expected) ->
    fprintf ppf
      "Constructor \"%s\": field \"%s\" must have type %s (direct builtin)" name
      field_name expected

(* Type of the runtime representation of an untagged block (case with payoad) *)
type block_type =
  | IntType
  | StringType
  | FloatType
  | BigintType
  | BooleanType
  | InstanceType of Instance.t
  | FunctionType
  | ObjectType
  | UnknownType

type primitive_catchall = PrimitiveInt | PrimitiveFloat | PrimitiveString

let block_type_to_user_visible_string = function
  | IntType -> "int"
  | StringType -> "string"
  | FloatType -> "float"
  | BigintType -> "bigint"
  | BooleanType -> "bool"
  | InstanceType i -> Instance.to_string i
  | FunctionType -> "function"
  | ObjectType -> "object"
  | UnknownType -> "unknown"

let primitive_catchall_to_block_type = function
  | PrimitiveInt -> IntType
  | PrimitiveFloat -> FloatType
  | PrimitiveString -> StringType

let primitive_catchall_to_bucket = function
  | PrimitiveInt | PrimitiveFloat -> `Number
  | PrimitiveString -> `String

(*
  Type of the runtime representation of a tag.
  Can be a literal (case with no payload), or a block (case with payload).
  In the case of block it can be tagged or untagged.
*)
type tag_type =
  | String of string
  | Int of int
  | Float of string
  | BigInt of string
  | Bool of bool
  | Null
  | Undefined (* literal or tagged block *)
  | Untagged of block_type (* untagged block *)
type tag = {name: string; tag_type: tag_type option}
type block_kind =
  | Tagged_block
  | Tagged_primitive_catchall of primitive_catchall
  | Untagged_block of block_type

type block = {tag: tag; tag_name: string option; kind: block_kind}
type switch_names = {consts: tag array; blocks: block array}

type constructor_runtime_representation =
  | Constructor_primitive_catchall of primitive_catchall
  | Constructor_tag of tag_type option

let tag_type_to_user_visible_string = function
  | String _ -> "string"
  | Int _ -> "int"
  | Float _ -> "float"
  | BigInt _ -> "bigint"
  | Bool _ -> "bool"
  | Null -> "null"
  | Undefined -> "undefined"
  | Untagged block_type -> block_type_to_user_visible_string block_type

let block_kind_to_block_type = function
  | Tagged_block -> None
  | Tagged_primitive_catchall primitive_catchall ->
    Some (primitive_catchall_to_block_type primitive_catchall)
  | Untagged_block block_type -> Some block_type

let untagged = "unboxed"

let block_type_can_be_undefined = function
  | IntType | StringType | FloatType | BigintType | BooleanType | InstanceType _
  | FunctionType | ObjectType ->
    false
  | UnknownType -> true

let tag_can_be_undefined tag =
  match tag.tag_type with
  | None -> false
  | Some (String _ | Int _ | Float _ | BigInt _ | Bool _ | Null) -> false
  | Some (Untagged block_type) -> block_type_can_be_undefined block_type
  | Some Undefined -> true

let has_untagged (attrs : Parsetree.attributes) =
  Ext_list.exists attrs (function {txt}, _ -> txt = untagged)

let process_untagged (attrs : Parsetree.attributes) =
  let st = ref false in
  Ext_list.iter attrs (fun ({txt}, _) ->
      match txt with
      | "unboxed" -> st := true
      | _ -> ());
  !st

let extract_concrete_typedecl :
    (Env.t -> Types.type_expr -> Path.t * Path.t * Types.type_declaration) ref =
  ref (Obj.magic ())

let expand_head : (Env.t -> Types.type_expr -> Types.type_expr) ref =
  ref (Obj.magic ())

type constructor_annotation =
  | Tag of tag_type
  | PrimitiveCatchAll of primitive_catchall

let process_constructor_annotation (attrs : Parsetree.attributes) =
  let st : constructor_annotation option ref = ref None in
  Ext_list.iter attrs (fun (({txt; loc}, payload) as attr) ->
      match txt with
      | "as" ->
        if !st = None then (
          (match Ast_payload.is_single_string payload with
          | None -> ()
          | Some (s, _dec) -> st := Some (Tag (String s)));
          (match Ast_payload.is_single_int payload with
          | None -> ()
          | Some i -> st := Some (Tag (Int i)));
          (match Ast_payload.is_single_float payload with
          | None -> ()
          | Some f -> st := Some (Tag (Float f)));
          (match Ast_payload.is_single_bigint payload with
          | None -> ()
          | Some i -> st := Some (Tag (BigInt i)));
          (match Ast_payload.is_single_bool payload with
          | None -> ()
          | Some b -> st := Some (Tag (Bool b)));
          (match Ast_payload.is_single_ident payload with
          | None -> ()
          | Some (Lident "null") -> st := Some (Tag Null)
          | Some (Lident "undefined") -> st := Some (Tag Undefined)
          | Some _ -> raise (Error (loc, InvalidVariantAsAnnotation)));
          if !st = None then raise (Error (loc, InvalidVariantAsAnnotation))
          else Used_attributes.mark_used_attribute attr)
        else raise (Error (loc, DuplicatedVariantConstructorAnnotation))
      | "catch" ->
        if !st = None then (
          (match Ast_payload.is_single_ident payload with
          | Some (Lident "int") -> st := Some (PrimitiveCatchAll PrimitiveInt)
          | Some (Lident "float") ->
            st := Some (PrimitiveCatchAll PrimitiveFloat)
          | Some (Lident "string") ->
            st := Some (PrimitiveCatchAll PrimitiveString)
          | Some _ | None -> raise (Error (loc, InvalidVariantCatchAnnotation)));
          Used_attributes.mark_used_attribute attr)
        else raise (Error (loc, DuplicatedVariantConstructorAnnotation))
      | _ -> ());
  !st

let process_tag_type (attrs : Parsetree.attributes) =
  let st : tag_type option ref = ref None in
  Ext_list.iter attrs (fun (({txt; loc}, payload) as attr) ->
      match txt with
      | "as" ->
        if !st = None then (
          (match Ast_payload.is_single_string payload with
          | None -> ()
          | Some (s, _dec) -> st := Some (String s));
          (match Ast_payload.is_single_int payload with
          | None -> ()
          | Some i -> st := Some (Int i));
          (match Ast_payload.is_single_float payload with
          | None -> ()
          | Some f -> st := Some (Float f));
          (match Ast_payload.is_single_bigint payload with
          | None -> ()
          | Some i -> st := Some (BigInt i));
          (match Ast_payload.is_single_bool payload with
          | None -> ()
          | Some b -> st := Some (Bool b));
          (match Ast_payload.is_single_ident payload with
          | None -> ()
          | Some (Lident "null") -> st := Some Null
          | Some (Lident "undefined") -> st := Some Undefined
          | Some _ -> raise (Error (loc, InvalidVariantAsAnnotation)));
          if !st = None then raise (Error (loc, InvalidVariantAsAnnotation))
          else Used_attributes.mark_used_attribute attr)
        else raise (Error (loc, Duplicated_bs_as))
      | _ -> ());
  !st

let process_constructor_tag_type (attrs : Parsetree.attributes) =
  match process_constructor_annotation attrs with
  | Some (Tag tag_type) -> Some tag_type
  | Some (PrimitiveCatchAll _) | None -> None

let process_primitive_catchall (attrs : Parsetree.attributes) =
  match process_constructor_annotation attrs with
  | Some (PrimitiveCatchAll primitive_catchall) -> Some primitive_catchall
  | Some (Tag _) | None -> None

let has_primitive_catchall (attrs : Parsetree.attributes) =
  process_primitive_catchall attrs <> None

let constructor_runtime_representation (attrs : Parsetree.attributes) =
  match process_primitive_catchall attrs with
  | Some primitive_catchall -> Constructor_primitive_catchall primitive_catchall
  | None -> Constructor_tag (process_constructor_tag_type attrs)

let () =
  Location.register_error_of_exn (function
    | Error (loc, err) -> Some (Location.error_of_printer loc report_error err)
    | _ -> None)

let report_constructor_more_than_one_arg ~loc ~name =
  raise
    (Error
       (loc, InvalidUntaggedVariantDefinition (ConstructorMoreThanOneArg name)))

let type_is_builtin_object (t : Types.type_expr) =
  match t.desc with
  | Tconstr (Path.Pident ident, [_], _) when Ident.name ident = "dict" -> true
  | Tconstr (path, _, _) ->
    let name = Path.name path in
    name = "Js.Dict.t" || name = "Js_dict.t"
  | _ -> false

let type_to_instanceof_backed_obj (t : Types.type_expr) =
  match t.desc with
  | Tconstr (path, _, _) when Path.same path Predef.path_promise ->
    Some Instance.Promise
  | Tconstr (path, _, _) when Path.same path Predef.path_array -> Some Array
  | Tconstr (path, _, _) -> (
    match Path.name path with
    | "Stdlib_ArrayBuffer.t" -> Some ArrayBuffer
    | "Stdlib.BigInt64Array.t" -> Some BigInt64Array
    | "Stdlib.BigUint64Array.t" -> Some BigUint64Array
    | "Stdlib.DataView.t" -> Some DataView
    | "Stdlib_Date.t" -> Some Date
    | "Stdlib.Float32Array.t" -> Some Float32Array
    | "Stdlib.Float64Array.t" -> Some Float64Array
    | "Stdlib.Int16Array.t" -> Some Int16Array
    | "Stdlib.Int32Array.t" -> Some Int32Array
    | "Stdlib.Int8Array.t" -> Some Int8Array
    | "Stdlib_RegExp.t" -> Some RegExp
    | "Stdlib.Uint16Array.t" -> Some Uint16Array
    | "Stdlib.Uint32Array.t" -> Some Uint32Array
    | "Stdlib.Uint8Array.t" -> Some Uint8Array
    | "Stdlib.Uint8ClampedArray.t" -> Some Uint8ClampedArray
    | "Js_file.t" -> Some File
    | "Js_blob.t" -> Some Blob
    | "Stdlib.Set.t" -> Some Set
    | "Stdlib.Map.t" -> Some Map
    | "Stdlib.WeakSet.t" -> Some WeakSet
    | "Stdlib.WeakMap.t" -> Some WeakMap
    | _ -> None)
  | _ -> None

let get_block_type_from_typ ~env (t : Types.type_expr) : block_type option =
  (* First check the original (unexpanded) type for typed arrays and other instance types *)
  match type_to_instanceof_backed_obj t with
  | Some instance_type -> Some (InstanceType instance_type)
  | None -> (
    (* If original type didn't match, expand and try standard checks *)
    let expanded_t = !expand_head env t in
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

let get_block_kind ~env (cstr : Types.constructor_declaration) :
    block_kind option =
  match (process_untagged cstr.cd_attributes, cstr.cd_args) with
  | false, _ -> (
    match process_primitive_catchall cstr.cd_attributes with
    | None -> None
    | Some primitive_catchall ->
      Some (Tagged_primitive_catchall primitive_catchall))
  | true, Cstr_tuple [t] when get_block_type_from_typ ~env t |> Option.is_some
    ->
    Option.map
      (fun block_type -> Untagged_block block_type)
      (get_block_type_from_typ ~env t)
  | true, Cstr_tuple [ty] ->
    let default = Some UnknownType in
    let block_type =
      match !extract_concrete_typedecl env ty with
      | _, _, {type_kind = Type_record (_, Record_unboxed _)} -> default
      | _, _, {type_kind = Type_record (_, _)} -> Some ObjectType
      | _ -> default
      | exception _ -> default
    in
    Option.map (fun block_type -> Untagged_block block_type) block_type
  | true, Cstr_tuple (_ :: _ :: _) ->
    (* C(_, _) with at least 2 args is an object *)
    Some (Untagged_block ObjectType)
  | true, Cstr_record _ ->
    (* inline record is an object *)
    Some (Untagged_block ObjectType)
  | true, _ -> None (* TODO: add restrictions here *)

let get_block_type ~env (cstr : Types.constructor_declaration) :
    block_type option =
  Option.bind (get_block_kind ~env cstr) block_kind_to_block_type

let process_tag_name (attrs : Parsetree.attributes) =
  let st = ref None in
  Ext_list.iter attrs (fun ({txt; loc}, payload) ->
      match txt with
      | "tag" ->
        if !st = None then (
          (match Ast_payload.is_single_string payload with
          | None -> ()
          | Some (s, _dec) -> st := Some s);
          if !st = None then raise (Error (loc, InvalidVariantTagAnnotation)))
        else raise (Error (loc, Duplicated_bs_as))
      | _ -> ());
  !st

let get_tag_name (cstr : Types.constructor_declaration) =
  process_tag_name cstr.cd_attributes

let is_nullary_variant (x : Types.constructor_arguments) =
  match x with
  | Types.Cstr_tuple [] -> true
  | _ -> false

let check_invariant ~is_untagged_def ~(consts : (Location.t * tag) list)
    ~(blocks : (Location.t * block) list) =
  let module StringSet = Set.Make (String) in
  let string_literals_consts = ref StringSet.empty in
  let string_literals_blocks = ref StringSet.empty in
  let nonstring_literals_consts = ref StringSet.empty in
  let nonstring_literals_blocks = ref StringSet.empty in
  let instance_types = Hashtbl.create 1 in
  let function_types = ref 0 in
  let object_types = ref 0 in
  let string_types = ref 0 in
  let number_types = ref 0 in
  let bigint_types = ref 0 in
  let boolean_types = ref 0 in
  let unknown_types = ref 0 in
  let add_string_literal ~is_const ~loc s =
    let set =
      if is_const then string_literals_consts else string_literals_blocks
    in
    if StringSet.mem s !set then
      raise (Error (loc, InvalidUntaggedVariantDefinition (DuplicateLiteral s)));
    set := StringSet.add s !set
  in
  let add_nonstring_literal ~is_const ~loc s =
    let set =
      if is_const then nonstring_literals_consts else nonstring_literals_blocks
    in
    if StringSet.mem s !set then
      raise (Error (loc, InvalidUntaggedVariantDefinition (DuplicateLiteral s)));
    set := StringSet.add s !set
  in
  let invariant loc name =
    if !unknown_types <> 0 && List.length blocks <> 1 then
      raise
        (Error (loc, InvalidUntaggedVariantDefinition (OnlyOneUnknown name)));
    if !object_types > 1 then
      raise (Error (loc, InvalidUntaggedVariantDefinition AtMostOneObject));
    Hashtbl.iter
      (fun i count ->
        if count > 1 then
          raise
            (Error (loc, InvalidUntaggedVariantDefinition (AtMostOneInstance i))))
      instance_types;
    if !function_types > 1 then
      raise (Error (loc, InvalidUntaggedVariantDefinition AtMostOneFunction));
    if !string_types > 1 then
      raise (Error (loc, InvalidUntaggedVariantDefinition AtMostOneString));
    if !number_types > 1 then
      raise (Error (loc, InvalidUntaggedVariantDefinition AtMostOneNumber));
    if !bigint_types > 1 then
      raise (Error (loc, InvalidUntaggedVariantDefinition AtMostOneBigint));
    if !boolean_types > 1 then
      raise (Error (loc, InvalidUntaggedVariantDefinition AtMostOneBoolean));
    if
      !boolean_types > 0
      && (StringSet.mem "true" !nonstring_literals_consts
         || StringSet.mem "false" !nonstring_literals_consts)
    then raise (Error (loc, InvalidUntaggedVariantDefinition AtMostOneBoolean));
    ()
  in
  let check_literal ~is_const ~loc (literal : tag) =
    match literal.tag_type with
    | Some (String s) -> add_string_literal ~is_const ~loc s
    | Some (Int i) -> add_nonstring_literal ~is_const ~loc (string_of_int i)
    | Some (Float f) -> add_nonstring_literal ~is_const ~loc f
    | Some (BigInt i) -> add_nonstring_literal ~is_const ~loc i
    | Some Null -> add_nonstring_literal ~is_const ~loc "null"
    | Some Undefined -> add_nonstring_literal ~is_const ~loc "undefined"
    | Some (Bool b) ->
      add_nonstring_literal ~is_const ~loc (if b then "true" else "false")
    | Some (Untagged _) -> ()
    | None -> add_string_literal ~is_const ~loc literal.name
  in

  Ext_list.rev_iter consts (fun (loc, literal) ->
      check_literal ~is_const:true ~loc literal);
  if is_untagged_def then
    Ext_list.rev_iter blocks (fun (loc, block) ->
        match block_kind_to_block_type block.kind with
        | Some block_type ->
          (match block_type with
          | UnknownType -> incr unknown_types
          | ObjectType -> incr object_types
          | InstanceType i ->
            let count =
              Hashtbl.find_opt instance_types i |> Option.value ~default:0
            in
            Hashtbl.replace instance_types i (count + 1)
          | FunctionType -> incr function_types
          | IntType | FloatType -> incr number_types
          | BigintType -> incr bigint_types
          | BooleanType -> incr boolean_types
          | StringType -> incr string_types);
          invariant loc block.tag.name
        | None -> ())
  else
    Ext_list.rev_iter blocks (fun (loc, block) ->
        match block.kind with
        | Tagged_block -> check_literal ~is_const:false ~loc block.tag
        | Tagged_primitive_catchall _ | Untagged_block _ -> ())

let get_cstr_loc_tag (cstr : Types.constructor_declaration) =
  ( cstr.cd_loc,
    {
      name = Ident.name cstr.cd_id;
      tag_type = process_constructor_tag_type cstr.cd_attributes;
    } )

let constructor_declaration_from_constructor_description ~env
    (cd : Types.constructor_description) : Types.constructor_declaration option
    =
  match cd.cstr_res.desc with
  | Tconstr (path, _, _) -> (
    match Env.find_type path env with
    | {type_kind = Type_variant cstrs} ->
      Ext_list.find_opt cstrs (fun cstr ->
          if cstr.cd_id.name = cd.cstr_name then Some cstr else None)
    | _ -> None)
  | _ -> None

let names_from_type_variant ?(is_untagged_def = false) ~env
    (cstrs : Types.constructor_declaration list) =
  let get_block (cstr : Types.constructor_declaration) : block =
    let tag = snd (get_cstr_loc_tag cstr) in
    let kind =
      match get_block_kind ~env cstr with
      | Some kind -> kind
      | None -> Tagged_block
    in
    {tag; tag_name = get_tag_name cstr; kind}
  in
  let consts, blocks =
    Ext_list.fold_left cstrs ([], []) (fun (consts, blocks) cstr ->
        if is_nullary_variant cstr.cd_args then
          (get_cstr_loc_tag cstr :: consts, blocks)
        else (consts, (cstr.cd_loc, get_block cstr) :: blocks))
  in
  check_invariant ~is_untagged_def ~consts ~blocks;
  let blocks = blocks |> List.map snd in
  let consts = consts |> List.map snd in
  let consts = Ext_array.reverse_of_list consts in
  let blocks = Ext_array.reverse_of_list blocks in
  Some {consts; blocks}

let check_tag_field_conflicts (cstrs : Types.constructor_declaration list) =
  List.iter
    (fun (cstr : Types.constructor_declaration) ->
      let constructor_name = Ident.name cstr.cd_id in
      let primitive_catchall = process_primitive_catchall cstr.cd_attributes in
      let effective_tag_name =
        match primitive_catchall with
        | Some _ -> (
          match process_tag_name cstr.cd_attributes with
          | Some explicit_tag -> explicit_tag
          | None -> assert false)
        | None -> (
          match process_tag_name cstr.cd_attributes with
          | Some explicit_tag -> explicit_tag
          | None -> constructor_name)
      in
      let effective_field_name (field : Types.label_declaration) =
        let field_name = Ident.name field.ld_id in
        match process_tag_type field.ld_attributes with
        | Some (String as_name) -> as_name
        | Some _ | None -> field_name
      in
      match (primitive_catchall, cstr.cd_args) with
      | None, Cstr_record fields ->
        List.iter
          (fun (field : Types.label_declaration) ->
            let field_name = Ident.name field.ld_id in
            let runtime_field_name = effective_field_name field in
            if runtime_field_name = effective_tag_name then
              raise
                (Error
                   ( cstr.cd_loc,
                     TagFieldNameConflict
                       (constructor_name, field_name, runtime_field_name) )))
          fields
      | Some _, Cstr_tuple [] ->
        raise
          (Error
             ( cstr.cd_loc,
               TaggedPrimitiveCatchAll_OnNullaryConstructor constructor_name ))
      | Some _, Cstr_record fields -> (
        let matching_fields =
          List.filter
            (fun field -> effective_field_name field = effective_tag_name)
            fields
        in
        match matching_fields with
        | [] -> ()
        | _ :: _ :: _ ->
          raise
            (Error
               ( cstr.cd_loc,
                 TaggedPrimitiveCatchAll_TooManyTagFields
                   (constructor_name, effective_tag_name) ))
        | [field] ->
          let field_name = Ident.name field.ld_id in
          if field.ld_optional then
            raise
              (Error
                 ( cstr.cd_loc,
                   TaggedPrimitiveCatchAll_TagFieldOptional
                     (constructor_name, field_name) ));
          let expected, ok =
            match primitive_catchall with
            | Some PrimitiveInt -> (
              match field.ld_type.desc with
              | Tconstr (path, _, _) when Path.same path Predef.path_int ->
                ("int", true)
              | _ -> ("int", false))
            | Some PrimitiveFloat -> (
              match field.ld_type.desc with
              | Tconstr (path, _, _) when Path.same path Predef.path_float ->
                ("float", true)
              | _ -> ("float", false))
            | Some PrimitiveString -> (
              match field.ld_type.desc with
              | Tconstr (path, _, _) when Path.same path Predef.path_string ->
                ("string", true)
              | _ -> ("string", false))
            | None -> assert false
          in
          if not ok then
            raise
              (Error
                 ( cstr.cd_loc,
                   TaggedPrimitiveCatchAll_TagFieldWrongType
                     (constructor_name, field_name, expected) )))
      | Some _, _ ->
        raise
          (Error
             ( cstr.cd_loc,
               TaggedPrimitiveCatchAll_InlineRecordRequired constructor_name ))
      | None, _ -> ())
    cstrs

type well_formedness_check = {
  is_untagged_def: bool;
  cstrs: Types.constructor_declaration list;
}

let check_well_formed ~env {is_untagged_def; cstrs} =
  let primitive_catchalls =
    List.filter_map
      (fun (cstr : Types.constructor_declaration) ->
        match process_primitive_catchall cstr.cd_attributes with
        | None -> None
        | Some primitive_catchall ->
          Some
            ( cstr.cd_loc,
              Ident.name cstr.cd_id,
              primitive_catchall,
              process_tag_name cstr.cd_attributes ))
      cstrs
  in
  (match primitive_catchalls with
  | [] -> ()
  | (loc, _, _, tag_name) :: _ ->
    if is_untagged_def then
      raise (Error (loc, TaggedPrimitiveCatchAll_UnboxedVariantUnsupported));
    if tag_name = None then
      raise (Error (loc, TaggedPrimitiveCatchAll_TaggedVariantRequired)));
  check_tag_field_conflicts cstrs;
  let has_number_catchall = ref false in
  let has_string_catchall = ref false in
  List.iter
    (fun (loc, _, primitive_catchall, _) ->
      match primitive_catchall_to_bucket primitive_catchall with
      | `Number ->
        if !has_number_catchall then
          raise (Error (loc, TaggedPrimitiveCatchAll_AtMostOneNumber));
        has_number_catchall := true
      | `String ->
        if !has_string_catchall then
          raise (Error (loc, TaggedPrimitiveCatchAll_AtMostOneString));
        has_string_catchall := true)
    primitive_catchalls;
  ignore (names_from_type_variant ~env ~is_untagged_def cstrs)

let has_undefined_literal attrs =
  process_constructor_tag_type attrs = Some Undefined

let block_is_object ~env attrs = get_block_type ~env attrs = Some ObjectType

module DynamicChecks = struct
  type op = EqEqEq | NotEqEq | Or | And
  type 'a t =
    | BinOp of op * 'a t * 'a t
    | TagType of tag_type
    | TypeOf of 'a t
    | IsInstanceOf of Instance.t * 'a t
    | Not of 'a t
    | Expr of 'a

  let rec size = function
    | BinOp (_, x, y) -> 1 + size x + size y
    | TagType _ -> 1
    | TypeOf x -> 1 + size x
    | IsInstanceOf (_, x) -> 1 + size x
    | Not x -> 1 + size x
    | Expr _ -> 1

  let bin op x y = BinOp (op, x, y)
  let tag_type t = TagType t
  let typeof x = TypeOf x
  let str s = String s |> tag_type
  let is_instance i x = IsInstanceOf (i, x)
  let not x = Not x
  let nil = Null |> tag_type
  let undefined = Undefined |> tag_type
  let object_ = Untagged ObjectType |> tag_type

  let function_ = Untagged FunctionType |> tag_type
  let string = Untagged StringType |> tag_type
  let number = Untagged IntType |> tag_type

  let bigint = Untagged BigintType |> tag_type

  let boolean = Untagged BooleanType |> tag_type

  let ( == ) x y = bin EqEqEq x y
  let ( != ) x y = bin NotEqEq x y
  let ( ||| ) x y = bin Or x y
  let ( &&& ) x y = bin And x y

  let rec is_a_literal_case ~(literal_cases : tag_type list) ~block_cases
      ~list_literal_cases (e : _ t) =
    let literals_overlaps_with_string () =
      Ext_list.exists literal_cases (function
        | String _ -> true
        | _ -> false)
    in
    let literals_overlaps_with_number () =
      Ext_list.exists literal_cases (function
        | Int _ | Float _ -> true
        | _ -> false)
    in
    let literals_overlaps_with_bigint () =
      Ext_list.exists literal_cases (function
        | BigInt _ -> true
        | _ -> false)
    in
    let literals_overlaps_with_boolean () =
      Ext_list.exists literal_cases (function
        | Bool _ -> true
        | _ -> false)
    in
    let literals_overlaps_with_object () =
      Ext_list.exists literal_cases (function
        | Null -> true
        | _ -> false)
    in
    let is_literal_case (t : tag_type) : _ t = e == tag_type t in
    let is_not_block_case (c : block_type) : _ t =
      match c with
      | StringType
        when literals_overlaps_with_string () = false (* No overlap *) ->
        typeof e != string
      | IntType when literals_overlaps_with_number () = false ->
        typeof e != number
      | FloatType when literals_overlaps_with_number () = false ->
        typeof e != number
      | BigintType when literals_overlaps_with_bigint () = false ->
        typeof e != bigint
      | BooleanType when literals_overlaps_with_boolean () = false ->
        typeof e != boolean
      | InstanceType i -> not (is_instance i e)
      | FunctionType -> typeof e != function_
      | ObjectType when literals_overlaps_with_object () = false ->
        typeof e != object_
      | ObjectType (* overlap *) -> e == nil ||| (typeof e != object_)
      | StringType (* overlap *)
      | IntType (* overlap *)
      | FloatType (* overlap *)
      | BigintType (* overlap *)
      | BooleanType (* overlap *)
      | UnknownType -> (
        (* We don't know the type of unknown, so we need to express:
           this is not one of the literals *)
        match literal_cases with
        | [] ->
          (* this should not happen *)
          assert false
        | l1 :: others ->
          let is_literal_1 = is_literal_case l1 in
          Ext_list.fold_right others is_literal_1 (fun literal_n acc ->
              is_literal_case literal_n ||| acc))
    in
    if list_literal_cases then
      let rec mk cases =
        match List.rev cases with
        | [case] -> is_literal_case case
        | case :: rest -> is_literal_case case ||| mk rest
        | [] -> assert false
      in
      mk literal_cases
    else
      match block_cases with
      | [c] -> is_not_block_case c
      | c1 :: (_ :: _ as rest) ->
        is_not_block_case c1
        &&& is_a_literal_case ~literal_cases ~block_cases:rest
              ~list_literal_cases e
      | [] -> assert false

  let is_a_literal_case ~literal_cases ~block_cases e =
    let with_literal_cases =
      is_a_literal_case ~literal_cases ~block_cases ~list_literal_cases:true e
    in
    let without_literal_cases =
      is_a_literal_case ~literal_cases ~block_cases ~list_literal_cases:false e
    in
    if size with_literal_cases <= size without_literal_cases then
      with_literal_cases
    else without_literal_cases

  let is_int_tag ?(has_null_undefined_other = (false, false, false)) (e : _ t) :
      _ t =
    let has_null, has_undefined, has_other = has_null_undefined_other in
    if has_null && has_undefined = false && has_other = false then
      (* null *)
      bin EqEqEq e nil
    else if has_null && has_undefined && has_other = false then
      (* null + undefined *)
      e == nil ||| e == undefined
    else if has_null = false && has_undefined && has_other = false then
      (* undefined *)
      e == undefined
    else if has_null then
      (* (null + undefined + other) || (null + other) *)
      e == nil ||| typeof e != object_
    else (* (undefiled + other) || other *)
      typeof e != object_

  let literal_cases_for_block_type (block_type : block_type)
      (literal_cases : tag_type list) =
    Ext_list.filter literal_cases (function
      | String _ -> block_type = StringType
      | Int _ | Float _ -> block_type = IntType || block_type = FloatType
      | BigInt _ -> block_type = BigintType
      | Bool _ -> block_type = BooleanType
      | _ -> false)

  let literal_case_expr y (literal_case : tag_type) = y == tag_type literal_case

  let not_one_of_the_literals y = function
    | [] -> None
    | literal_1 :: rest ->
      let is_literal_1 = literal_case_expr y literal_1 in
      let is_any_literal =
        Ext_list.fold_right rest is_literal_1 (fun literal_n acc ->
            literal_case_expr y literal_n ||| acc)
      in
      Some (not is_any_literal)

  let add_runtime_type_check ~tag_type ~has_null_case
      ~(block_cases : block_type list) ~(literal_cases : tag_type list) x y =
    let instances =
      Ext_list.filter_map block_cases (function
        | InstanceType i -> Some i
        | _ -> None)
    in
    match tag_type with
    | Untagged
        ((IntType | StringType | FloatType | BigintType | BooleanType) as
         block_type) -> (
      let runtime_type_matches = typeof y == x in
      match
        literal_cases
        |> literal_cases_for_block_type block_type
        |> not_one_of_the_literals y
      with
      | Some not_a_literal -> runtime_type_matches &&& not_a_literal
      | None -> runtime_type_matches)
    | Untagged FunctionType -> typeof y == x
    | Untagged ObjectType ->
      let object_case =
        if has_null_case then typeof y == x &&& (y != nil) else typeof y == x
      in
      if instances <> [] then
        let not_one_of_the_instances =
          Ext_list.fold_right instances object_case (fun i x ->
              x &&& not (is_instance i y))
        in
        not_one_of_the_instances
      else object_case
    | Untagged (InstanceType i) -> is_instance i y
    | Untagged UnknownType ->
      (* This should not happen because unknown must be the only non-literal case *)
      assert false
    | Bool _ | Float _ | Int _ | BigInt _ | String _ | Null | Undefined -> x
end
