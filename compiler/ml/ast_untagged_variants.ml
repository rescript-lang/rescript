open Variant_runtime

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
  | VariantAsIntegerOutOfRange of string
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
      "A variant case annotation @as(...) must be a string, integer, boolean, \
       null, or undefined."
  | VariantAsIntegerOutOfRange value ->
    fprintf ppf
      "The integer %s in this variant case's @as annotation is out of range."
      value
  | Duplicated_bs_as ->
    fprintf ppf "Duplicate @as annotation; only one @as is allowed here."
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
let literal_tag_to_user_visible_string = function
  | String _ -> "string"
  | Int _ -> "int"
  | Float _ -> "float"
  | BigInt _ -> "bigint"
  | Bool _ -> "bool"
  | Null -> "null"
  | Undefined -> "undefined"

let untagged = "unboxed"

let block_type_can_be_undefined = function
  | IntType | StringType | FloatType | BigintType | BooleanType | InstanceType _
  | FunctionType | ObjectType ->
    false
  | UnknownType -> true

let has_untagged (attrs : Parsetree.attributes) =
  Ext_list.exists attrs (function {txt}, _ -> txt = untagged)

let process_untagged (attrs : Parsetree.attributes) =
  let st = ref false in
  Ext_list.iter attrs (fun ({txt}, _) ->
      match txt with
      | "unboxed" -> st := true
      | _ -> ());
  !st

let runtime_tag_of_parsetree ~loc = function
  | Parsetree.Pct_string s -> String (String_literal.string_semantic s)
  | Pct_int source -> (
    match int_of_string_opt source with
    | Some i -> Int i
    | None -> raise (Error (loc, VariantAsIntegerOutOfRange source)))
  | Pct_float f -> Float f
  | Pct_bigint i -> BigInt i
  | Pct_bool b -> Bool b
  | Pct_null -> Null
  | Pct_undefined -> Undefined

let parsetree_tag_of_runtime = function
  | String s -> Parsetree.Pct_string (String_literal.string_from_semantic s)
  | Int i -> Pct_int (string_of_int i)
  | Float f -> Pct_float f
  | BigInt i -> Pct_bigint i
  | Bool b -> Pct_bool b
  | Null -> Pct_null
  | Undefined -> Pct_undefined

(* An [@as] left in the attributes did not name the constructor: either its
   payload is not a tag, or an earlier [@as] already named it. *)
let reject_leftover_as (attrs : Parsetree.attributes) err =
  Ext_list.iter attrs (fun (({txt; loc}, _) : Parsetree.attribute) ->
      if txt = "as" then raise (Error (loc, err)))

let process_constructor_tag (cstr : Parsetree.constructor_declaration) =
  match cstr.pcd_runtime_tag with
  | None ->
    reject_leftover_as cstr.pcd_attributes InvalidVariantAsAnnotation;
    None
  | Some {txt; loc} ->
    reject_leftover_as cstr.pcd_attributes Duplicated_bs_as;
    Some (runtime_tag_of_parsetree ~loc txt)

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
    name = "Stdlib.Dict.t" || name = "Stdlib_Dict.t"
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
    | "Stdlib_File.t" -> Some File
    | "Stdlib_Blob.t" -> Some Blob
    | "Stdlib.Set.t" -> Some Set
    | "Stdlib.Map.t" -> Some Map
    | "Stdlib.WeakSet.t" -> Some WeakSet
    | "Stdlib.WeakMap.t" -> Some WeakMap
    | _ -> None)
  | _ -> None

let process_tag_name (attrs : Parsetree.attributes) =
  let st = ref None in
  Ext_list.iter attrs (fun ({txt; loc}, payload) ->
      match txt with
      | "tag" ->
        if !st = None then (
          Ast_payload.reject_json_literal_payload payload;
          (match Ast_payload.semantic_string_of_payload payload with
          | None -> ()
          | Some s -> st := Some s);
          if !st = None then raise (Error (loc, InvalidVariantTagAnnotation)))
        else raise (Error (loc, Duplicated_bs_as))
      | _ -> ());
  !st

(* A constructor the compiler generates itself carries no annotations. *)
let generated_tag ~name = {name; literal = None}

let generated_block_runtime ~name =
  {tag = generated_tag ~name; tag_name = None; untagged = false}

let is_nullary_variant (x : Types.constructor_arguments) =
  match x with
  | Types.Cstr_tuple [] -> true
  | _ -> false

let check_invariant ~is_untagged_def ~(consts : (Location.t * tag) list)
    ~(blocks : (Location.t * block) list) =
  let module String_set = Set.Make (String) in
  let string_literals_consts = ref String_set.empty in
  let string_literals_blocks = ref String_set.empty in
  let nonstring_literals_consts = ref String_set.empty in
  let nonstring_literals_blocks = ref String_set.empty in
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
    if String_set.mem s !set then
      raise (Error (loc, InvalidUntaggedVariantDefinition (DuplicateLiteral s)));
    set := String_set.add s !set
  in
  let add_nonstring_literal ~is_const ~loc s =
    let set =
      if is_const then nonstring_literals_consts else nonstring_literals_blocks
    in
    if String_set.mem s !set then
      raise (Error (loc, InvalidUntaggedVariantDefinition (DuplicateLiteral s)));
    set := String_set.add s !set
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
      && (String_set.mem "true" !nonstring_literals_consts
         || String_set.mem "false" !nonstring_literals_consts)
    then raise (Error (loc, InvalidUntaggedVariantDefinition AtMostOneBoolean));
    ()
  in
  let check_literal ~is_const ~loc (tag : tag) =
    match tag.literal with
    | None -> add_string_literal ~is_const ~loc tag.name
    | Some (String s) -> add_string_literal ~is_const ~loc s
    | Some (Int i) -> add_nonstring_literal ~is_const ~loc (string_of_int i)
    | Some (Float f) -> add_nonstring_literal ~is_const ~loc f
    | Some (BigInt i) -> add_nonstring_literal ~is_const ~loc i
    | Some (Bool b) ->
      add_nonstring_literal ~is_const ~loc (if b then "true" else "false")
    | Some Null -> add_nonstring_literal ~is_const ~loc "null"
    | Some Undefined -> add_nonstring_literal ~is_const ~loc "undefined"
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
          invariant loc block.runtime.tag.name
        | None -> ())
  else
    Ext_list.rev_iter blocks (fun (loc, block) ->
        check_literal ~is_const:false ~loc block.runtime.tag)

let get_cstr_loc_tag (cstr : Types.constructor_declaration) =
  (cstr.cd_loc, {name = Ident.name cstr.cd_id; literal = cstr.cd_runtime_tag})

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
            let effective_field_name = Record_runtime.declaration_name field in
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

module Dynamic_checks = struct
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
  let str s = Literal (String s) |> tag_type
  let is_instance i x = IsInstanceOf (i, x)
  let not x = Not x
  let nil = Literal Null |> tag_type
  let undefined = Literal Undefined |> tag_type
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

  let rec is_a_literal_case ~(literal_cases : literal_tag list) ~block_cases
      ~list_literal_cases (e : _ t) =
    let overlaps p = Ext_list.exists literal_cases p in
    let literals_overlaps_with_string () =
      overlaps (function
        | String _ -> true
        | _ -> false)
    in
    let literals_overlaps_with_number () =
      overlaps (function
        | Int _ | Float _ -> true
        | _ -> false)
    in
    let literals_overlaps_with_bigint () =
      overlaps (function
        | BigInt _ -> true
        | _ -> false)
    in
    let literals_overlaps_with_boolean () =
      overlaps (function
        | Bool _ -> true
        | _ -> false)
    in
    let literals_overlaps_with_object () =
      overlaps (function
        | Null -> true
        | _ -> false)
    in
    let is_literal_case (t : literal_tag) : _ t = e == tag_type (Literal t) in
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
    | Literal _ -> x
end
