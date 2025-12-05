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
  | Duplicated_bs_as
  | InvalidVariantTagAnnotation
  | InvalidUntaggedVariantDefinition of untagged_error
  | TagFieldNameConflict of string * string * string
exception Error of Location.t * error

let report_error ppf =
  let open Format in
  function
  | InvalidVariantAsAnnotation ->
    fprintf ppf
      "A variant case annotation @as(...) must be a string or integer, \
       boolean, null, undefined"
  | Duplicated_bs_as -> fprintf ppf "duplicate @as "
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
type block = {tag: tag; tag_name: string option; block_type: block_type option}
type switch_names = {consts: tag array; blocks: block array}

let tag_type_to_user_visible_string = function
  | String _ -> "string"
  | Int _ -> "int"
  | Float _ -> "float"
  | BigInt _ -> "bigint"
  | Bool _ -> "bool"
  | Null -> "null"
  | Undefined -> "undefined"
  | Untagged block_type -> block_type_to_user_visible_string block_type

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

let get_block_type ~env (cstr : Types.constructor_declaration) :
    block_type option =
  match (process_untagged cstr.cd_attributes, cstr.cd_args) with
  | false, _ -> None
  | true, Cstr_tuple [t] when get_block_type_from_typ ~env t |> Option.is_some
    ->
    get_block_type_from_typ ~env t
  | true, Cstr_tuple [ty] -> (
    let default = Some UnknownType in
    match !extract_concrete_typedecl env ty with
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
        match block.block_type with
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
        check_literal ~is_const:false ~loc block.tag)

let get_cstr_loc_tag (cstr : Types.constructor_declaration) =
  ( cstr.cd_loc,
    {
      name = Ident.name cstr.cd_id;
      tag_type = process_tag_type cstr.cd_attributes;
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
    {tag; tag_name = get_tag_name cstr; block_type = get_block_type ~env cstr}
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
      let effective_tag_name =
        match process_tag_name cstr.cd_attributes with
        | Some explicit_tag -> explicit_tag
        | None -> constructor_name
      in
      match cstr.cd_args with
      | Cstr_record fields ->
        List.iter
          (fun (field : Types.label_declaration) ->
            let field_name = Ident.name field.ld_id in
            let effective_field_name =
              match process_tag_type field.ld_attributes with
              | Some (String as_name) -> as_name
              (* @as payload types other than string have no effect on record fields *)
              | Some _ | None -> field_name
            in
            (* Check if effective field name conflicts with tag *)
            if effective_field_name = effective_tag_name then
              raise
                (Error
                   ( cstr.cd_loc,
                     TagFieldNameConflict
                       (constructor_name, field_name, effective_field_name) )))
          fields
      | _ -> ())
    cstrs

type well_formedness_check = {
  is_untagged_def: bool;
  cstrs: Types.constructor_declaration list;
}

let check_well_formed ~env {is_untagged_def; cstrs} =
  check_tag_field_conflicts cstrs;
  ignore (names_from_type_variant ~env ~is_untagged_def cstrs)

let has_undefined_literal attrs = process_tag_type attrs = Some Undefined

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

  let add_runtime_type_check ~tag_type ~has_null_case
      ~(block_cases : block_type list) x y =
    let instances =
      Ext_list.filter_map block_cases (function
        | InstanceType i -> Some i
        | _ -> None)
    in
    match tag_type with
    | Untagged
        ( IntType | StringType | FloatType | BigintType | BooleanType
        | FunctionType ) ->
      typeof y == x
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
