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
  | AtMostOneSymbol
  | UnreachableBoolean of string
  | DuplicateLiteral of string
  | SameLiteralValue of string * string
  | ConstructorMoreThanOneArg of string

(* Why a syntactically valid [@instanceOf] path is still not accepted *)
and unsupported_class =
  | Through_global_this
  | Every_value_of of string  (** names the [@typeOf] that says the same *)
  | Wrapper_of of string  (** names the [@typeOf] of the wrapped primitive *)
  | Array_class
  | Not_a_class

type error =
  | InvalidVariantAsAnnotation
  | VariantAsIntegerOutOfRange of string
  | Duplicated_bs_as
  | InvalidVariantTagAnnotation
  | InvalidUntaggedVariantDefinition of untagged_error
  | TagFieldNameConflict of string * string * string
  | InvalidInstanceOf
  | UnsupportedInstanceOf of string * unsupported_class
  | InvalidTypeOf of string
  | ConflictingRuntimeAnnotations
  | DuplicateRuntimeAnnotation of string
  | RuntimeAnnotationOnTypeParameter of string
  | RuntimeAnnotationOnRepresentedType of string
  | RuntimeAnnotationOnUnrelatedType of string
  | RuntimeAnnotationContradictsManifest of string * string
  | SubclassOfPublicAbbreviation of string * string
  | ClassTwiceInChain of string
exception Error of Location.t * error

let report_error ppf =
  let open Format in
  function
  | InvalidVariantAsAnnotation ->
    fprintf ppf
      "A variant case annotation @as(...) must be a string, integer, float, \
       bigint, boolean, null, or undefined."
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
        "At most one case can be an instance of " ^ Instance.to_string i ^ "."
      | AtMostOneFunction -> "At most one case can be a function type."
      | AtMostOneString -> "At most one case can be a string type."
      | AtMostOneBoolean -> "At most one case can be a boolean type."
      | AtMostOneSymbol -> "At most one case can be a symbol type."
      | UnreachableBoolean name ->
        "Case " ^ name
        ^ " can never match: both true and false are already cases of this \
           variant."
      | AtMostOneNumber ->
        "At most one case can be a number type (int or float)."
      | AtMostOneBigint -> "At most one case can be a bigint type."
      | DuplicateLiteral s -> "Duplicate literal " ^ s ^ "."
      | SameLiteralValue (literal, earlier) ->
        "The literal " ^ literal ^ " is the same JavaScript value as " ^ earlier
        ^ "."
      | ConstructorMoreThanOneArg name ->
        "Constructor " ^ name ^ " has more than one argument.")
  | TagFieldNameConflict (constructor_name, field_name, runtime_value) ->
    fprintf ppf
      "Constructor \"%s\": the @tag name \"%s\" conflicts with the runtime \
       value of inline record field \"%s\". Use a different @tag name or \
       rename the field."
      constructor_name runtime_value field_name
  | InvalidInstanceOf ->
    fprintf ppf
      "@instanceOf expects the name of a global class, such as \
       @instanceOf(\"Date\") or @instanceOf(\"Intl.Locale\")."
  | UnsupportedInstanceOf (name, reason) -> (
    match reason with
    | Through_global_this ->
      fprintf ppf
        "Write @instanceOf(\"%s\") without \"globalThis.\": the compiler reads \
         the class through globalThis itself where a local binding shadows it."
        (String.concat "." (List.tl (String.split_on_char '.' name)))
    | Every_value_of type_of ->
      fprintf ppf
        "Every %s is an instance of %s, so @instanceOf(\"%s\") cannot tell \
         cases apart. Use @typeOf(\"%s\") instead."
        type_of name name type_of
    | Wrapper_of type_of ->
      fprintf ppf
        "Values of type %s are primitives, not instances of %s. Use \
         @typeOf(\"%s\") instead."
        type_of name type_of
    | Array_class ->
      fprintf ppf
        "Arrays are recognized by Array.isArray. Use array<_> as the payload \
         type instead of @instanceOf(\"Array\")."
    | Not_a_class ->
      fprintf ppf "%s is not a class, so it cannot be used with @instanceOf."
        name)
  | InvalidTypeOf name ->
    fprintf ppf
      "@typeOf expects one of \"string\", \"number\", \"bigint\", \"boolean\", \
       \"symbol\", \"function\" or \"object\"%s."
      (if name = "" then "" else ", not \"" ^ name ^ "\"")
  | DuplicateRuntimeAnnotation attribute ->
    fprintf ppf "Duplicate @%s annotation; a type can have only one." attribute
  | RuntimeAnnotationOnTypeParameter attribute ->
    fprintf ppf
      "@%s cannot describe the type of a functor parameter, of a recursive \
       module being defined, or a locally abstract type: such a type stands \
       for another one, whose values can be anything."
      attribute
  | ConflictingRuntimeAnnotations ->
    fprintf ppf "A type can have either @instanceOf or @typeOf, not both."
  | RuntimeAnnotationOnRepresentedType attribute ->
    fprintf ppf
      "@%s can only be used on abstract types and type abbreviations, whose \
       runtime representation it describes."
      attribute
  | RuntimeAnnotationOnUnrelatedType attribute ->
    fprintf ppf
      "@%s on a type abbreviation must describe an abstract type, or a type \
       with the same annotation. The abbreviated type already has a runtime \
       representation the compiler knows, or can be undefined."
      attribute
  | RuntimeAnnotationContradictsManifest (outer, inner) ->
    fprintf ppf
      "This annotation, %s, contradicts the abbreviated type, whose values are \
       recognized by %s."
      outer inner
  | SubclassOfPublicAbbreviation (name, ancestor) ->
    fprintf ppf
      "A subclass must be a private abbreviation: write `= private ...` so \
       that instances of %s cannot be used as instances of %s without a \
       coercion."
      ancestor name
  | ClassTwiceInChain name ->
    fprintf ppf
      "%s appears twice in this chain of abbreviations, so it would be its own \
       superclass."
      name

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

let has_untagged (attrs : Parsetree.attributes) =
  Ext_list.exists attrs (function {txt}, _ -> txt = untagged)

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

(* What [@instanceOf] or [@typeOf] on a type declaration states about the
   runtime representation of its values. *)
type runtime_annotation =
  | Instance_of of string  (** instances of the global class at this path *)
  | Type_of of block_type  (** values with this [typeof] *)

let instance_of = "instanceOf"
let type_of = "typeOf"

(* The reason a global that is a valid path is still not a class to recognize
   values by. Classes nested in namespaces, such as [Intl.Locale], are not
   checked. *)
let unsupported_class name =
  match String.split_on_char '.' name with
  | "globalThis" :: _ :: _ -> Some Through_global_this
  | [global] -> (
    match global with
    | "Object" -> Some (Every_value_of "object")
    | "Function" -> Some (Every_value_of "function")
    | "Array" -> Some Array_class
    | "String" -> Some (Wrapper_of "string")
    | "Number" -> Some (Wrapper_of "number")
    | "Boolean" -> Some (Wrapper_of "boolean")
    | "BigInt" -> Some (Wrapper_of "bigint")
    | "Symbol" -> Some (Wrapper_of "symbol")
    | "globalThis" | "undefined" | "NaN" | "Infinity" | "Math" | "JSON"
    | "Reflect" | "Atomics" | "Intl" | "Proxy" ->
      Some Not_a_class
    | _ -> None)
  | _ -> None

let is_class_path name =
  let is_start = function
    | 'A' .. 'Z' | 'a' .. 'z' | '_' | '$' -> true
    | _ -> false
  in
  let is_part = function
    | '0' .. '9' -> true
    | c -> is_start c
  in
  let is_identifier segment =
    segment <> "" && is_start segment.[0] && String.for_all is_part segment
  in
  match String.split_on_char '.' name with
  | global :: properties ->
    (* Properties may be named like keywords, the global may not *)
    is_identifier global
    && (not (Js_reserved_map.is_js_keyword global))
    && List.for_all is_identifier properties
  | [] -> false

let runtime_attribute_name = function
  | Instance_of _ -> instance_of
  | Type_of _ -> type_of

(* A [@typeOf] states one of the kinds [typeof] recognizes *)
let typeof_name block_type =
  Option.value (Variant_runtime.typeof_name block_type) ~default:"object"

let runtime_annotation_to_string = function
  | Instance_of name -> Printf.sprintf "@%s(\"%s\")" instance_of name
  | Type_of block_type ->
    Printf.sprintf "@%s(\"%s\")" type_of (typeof_name block_type)

(* What a declaration's annotations state, for error messages *)
let describe_declared_block_type = function
  | Some (InstanceType (Class {name; ancestors = []})) -> "instances of " ^ name
  | Some (InstanceType (Class {name; ancestors})) ->
    "instances of " ^ name ^ ", a subclass of "
    ^ String.concat " and " ancestors
  | Some (InstanceType Array) -> "arrays"
  | Some block_type ->
    "values whose typeof is \"" ^ typeof_name block_type ^ "\""
  | None -> "no runtime class or typeof"

let has_not_undefined (attrs : Parsetree.attributes) =
  Ext_list.exists attrs (fun ({txt}, _) -> txt = "notUndefined")

(* The [@instanceOf] or [@typeOf] annotation in [attrs], validated. Only
   [Typedecl] passes [~loc]; later readers see attributes it accepted. *)
let process_runtime_annotation ?(loc = Location.none)
    (attrs : Parsetree.attributes) =
  let annotation_of ({Location.txt; loc = attr_loc}, payload) =
    let loc = if attr_loc = Location.none then loc else attr_loc in
    if txt = instance_of then
      match Ast_payload.semantic_string_of_payload payload with
      | Some name when is_class_path name -> (
        match unsupported_class name with
        | None -> Some (Instance_of name)
        | Some reason ->
          raise (Error (loc, UnsupportedInstanceOf (name, reason))))
      | _ -> raise (Error (loc, InvalidInstanceOf))
    else if txt = type_of then
      match Ast_payload.semantic_string_of_payload payload with
      | Some name -> (
        match block_type_of_typeof name with
        | Some block_type -> Some (Type_of block_type)
        | None -> raise (Error (loc, InvalidTypeOf name)))
      | None -> raise (Error (loc, InvalidTypeOf ""))
    else None
  in
  match Ext_list.filter_map attrs annotation_of with
  | [] -> None
  | [annotation] -> Some annotation
  | first :: second :: _ ->
    raise
      (Error
         ( loc,
           if runtime_attribute_name first = runtime_attribute_name second then
             DuplicateRuntimeAnnotation (runtime_attribute_name first)
           else ConflictingRuntimeAnnotations ))

(* Checks the runtime annotations of a declaration whose kind is
   [is_abstract_or_abbreviation]. *)
let check_runtime_annotation ~loc ~is_abstract_or_abbreviation
    (attrs : Parsetree.attributes) =
  match process_runtime_annotation ~loc attrs with
  | None -> ()
  | Some annotation ->
    if not is_abstract_or_abbreviation then
      raise
        (Error
           ( loc,
             RuntimeAnnotationOnRepresentedType
               (runtime_attribute_name annotation) ))

let process_tag_name (attrs : Parsetree.attributes) =
  let st = ref None in
  Ext_list.iter attrs (fun ({txt; loc}, payload) ->
      match txt with
      | "tag" ->
        if !st = None then (
          (match Ast_payload.semantic_string_of_payload payload with
          | None -> ()
          | Some s -> st := Some s);
          if !st = None then raise (Error (loc, InvalidVariantTagAnnotation)))
        else raise (Error (loc, Duplicated_bs_as))
      | _ -> ());
  !st

(* A constructor the compiler generates itself carries no annotations. *)
let generated_tag ~name = {name; literal = String name}

let generated_block_runtime ~name = {tag = generated_tag ~name; tag_name = None}

let is_nullary_variant (x : Types.constructor_arguments) =
  match x with
  | Types.Cstr_tuple [] -> true
  | _ -> false

(* A literal's JavaScript value, which a [switch] compares with [===]. Ints
   are emitted as 32-bit numbers, so [1], [1.0] and [4294967297] are one
   number, while [1n] is a bigint. *)
let check_invariant ~is_untagged_def ~(consts : (Location.t * tag) list)
    ~(blocks : (Location.t * block) list) =
  let module String_set = Set.Make (String) in
  let module String_map = Map.Make (String) in
  let string_literals_consts = ref String_set.empty in
  let string_literals_blocks = ref String_set.empty in
  (* Keyed by [literal_key], with the spelling of the literal *)
  let nonstring_literals_consts = ref String_map.empty in
  let nonstring_literals_blocks = ref String_map.empty in
  let instance_types = Hashtbl.create 1 in
  let function_types = ref 0 in
  let object_types = ref 0 in
  let string_types = ref 0 in
  let number_types = ref 0 in
  let bigint_types = ref 0 in
  let boolean_types = ref 0 in
  let symbol_types = ref 0 in
  let unknown_types = ref 0 in
  let add_string_literal ~is_const ~loc s =
    let set =
      if is_const then string_literals_consts else string_literals_blocks
    in
    if String_set.mem s !set then
      raise (Error (loc, InvalidUntaggedVariantDefinition (DuplicateLiteral s)));
    set := String_set.add s !set
  in
  let add_nonstring_literal ~is_const ~loc literal s =
    let map =
      if is_const then nonstring_literals_consts else nonstring_literals_blocks
    in
    let key = Variant_runtime.literal_value_key literal in
    (match String_map.find_opt key !map with
    | Some earlier when earlier = s ->
      raise (Error (loc, InvalidUntaggedVariantDefinition (DuplicateLiteral s)))
    | Some earlier ->
      raise
        (Error
           ( loc,
             InvalidUntaggedVariantDefinition (SameLiteralValue (s, earlier)) ))
    | None -> ());
    map := String_map.add key s !map
  in
  let invariant loc name =
    if !unknown_types <> 0 && List.length blocks <> 1 then
      raise
        (Error (loc, InvalidUntaggedVariantDefinition (OnlyOneUnknown name)));
    if !object_types > 1 then
      raise (Error (loc, InvalidUntaggedVariantDefinition AtMostOneObject));
    Hashtbl.iter
      (fun _ (i, count) ->
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
    if !symbol_types > 1 then
      raise (Error (loc, InvalidUntaggedVariantDefinition AtMostOneSymbol));
    ()
  in
  let check_literal ~is_const ~loc (tag : tag) =
    match tag.literal with
    | String s -> add_string_literal ~is_const ~loc s
    | Int i as literal ->
      add_nonstring_literal ~is_const ~loc literal (string_of_int i)
    | Float f as literal -> add_nonstring_literal ~is_const ~loc literal f
    | BigInt i as literal ->
      add_nonstring_literal ~is_const ~loc literal (i ^ "n")
    | Bool b as literal ->
      add_nonstring_literal ~is_const ~loc literal
        (if b then "true" else "false")
    | Null as literal -> add_nonstring_literal ~is_const ~loc literal "null"
    | Undefined as literal ->
      add_nonstring_literal ~is_const ~loc literal "undefined"
  in

  Ext_list.rev_iter consts (fun (loc, literal) ->
      check_literal ~is_const:true ~loc literal);
  (* A tagged block is told apart from the others by its tag; an untagged one
     is its payload, whatever its [@as] *)
  Ext_list.rev_iter blocks (fun (loc, block) ->
      match block with
      | Tagged {tag} -> check_literal ~is_const:false ~loc tag
      | Untagged _ -> ());
  (* The payload kinds are checked as soon as one case is untagged, even when
     the type itself is not [@unboxed] *)
  let has_untagged_block =
    List.exists
      (function
        | _, Untagged _ -> true
        | _, Tagged _ -> false)
      blocks
  in
  if is_untagged_def || has_untagged_block then (
    (* Tagged blocks are objects with a tag field: together one object case *)
    if
      List.exists
        (function
          | _, Tagged _ -> true
          | _, Untagged _ -> false)
        blocks
    then incr object_types;
    Ext_list.rev_iter blocks (fun (loc, block) ->
        match block with
        | Untagged {tag; block_type} ->
          (match block_type with
          | UnknownType -> incr unknown_types
          | ObjectType -> incr object_types
          | InstanceType i ->
            (* A class is identified by its name *)
            let name = Instance.to_string i in
            let count =
              match Hashtbl.find_opt instance_types name with
              | Some (_, count) -> count
              | None -> 0
            in
            Hashtbl.replace instance_types name (i, count + 1)
          | SymbolType -> incr symbol_types
          | FunctionType -> incr function_types
          | NumberType -> incr number_types
          | BigintType -> incr bigint_types
          | BooleanType ->
            (* Literal cases are matched before payload cases, so a boolean
               payload only receives the booleans that are not literal cases.
               A partial overlap is allowed, like for strings and numbers. *)
            if
              String_map.mem "true" !nonstring_literals_consts
              && String_map.mem "false" !nonstring_literals_consts
            then
              raise
                (Error
                   ( loc,
                     InvalidUntaggedVariantDefinition
                       (UnreachableBoolean tag.name) ));
            incr boolean_types
          | StringType -> incr string_types);
          invariant loc tag.name
        | Tagged _ -> ()))

(* A constructor without [@as] is represented by its name *)
let declared_tag (cstr : Types.constructor_declaration) =
  let name = Ident.name cstr.cd_id in
  {name; literal = Option.value cstr.cd_runtime_tag ~default:(String name)}

let get_cstr_loc_tag (cstr : Types.constructor_declaration) =
  (cstr.cd_loc, declared_tag cstr)

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

  let bin op x y = BinOp (op, x, y)
  let tag_type t = TagType t
  let typeof x = TypeOf x
  let nil = Literal Null |> tag_type
  let undefined = Literal Undefined |> tag_type
  let object_ = Payload_shape ObjectType |> tag_type

  let ( == ) x y = bin EqEqEq x y
  let ( != ) x y = bin NotEqEq x y
  let ( ||| ) x y = bin Or x y

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
end
