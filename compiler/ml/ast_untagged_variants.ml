open Variant_runtime

type untagged_error =
  | OnlyOneUnknown of string
  | AtMostOneOfKind of block_type  (** never [UnknownType] *)
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

(* Why the properties of a [@scope] cannot lead to an [@instanceOf] class *)
and invalid_scope =
  | Not_properties
  | Keyword_global of string  (** without [@module], the first is a global *)
  | Scope_through_global_this

(* Why a declaration with [@module] or [@scope] has no [@instanceOf] class
   for them to locate *)
and missing_class =
  | No_annotation  (** an abstract type or an abbreviation *)
  | Other_annotation of string  (** names the [@typeOf] or [@notUndefined] *)
  | Represented_type  (** a record, a variant or an extensible type *)

type error =
  | InvalidVariantAsAnnotation
  | VariantAsIntegerOutOfRange of string
  | Duplicated_bs_as
  | InvalidVariantTagAnnotation
  | InvalidUntaggedVariantDefinition of untagged_error
  | TagFieldNameConflict of string * string * string
  | UnboxedOnConstructor of {constructor: string; single_field_record: bool}
  | InvalidInstanceOf
  | InvalidModuleClass of string
  | InvalidClassModule
  | InvalidClassScope of invalid_scope
  | ClassLocationWithoutClass of string * missing_class
  | UnsupportedInstanceOf of string * unsupported_class
  | InvalidTypeOf of string
  | ConflictingRuntimeAnnotations
  | DuplicateRuntimeAnnotation of string
  | RuntimeAnnotationOnTypeParameter of string
  | RuntimeAnnotationOnRepresentedType of string
  | RuntimeAnnotationOnUnrelatedType of string
  | RuntimeAnnotationContradictsManifest of string * string
  | SubclassOfArray of string
  | SubclassOfPublicAbbreviation of string * string
  | ClassOnPublicAbbreviation of string
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
      | AtMostOneOfKind kind ->
        "At most one case can be "
        ^ (match kind with
          | ObjectType -> "an object type"
          | InstanceType Array ->
            "an array, a tuple or a record represented as an array"
          | InstanceType i -> "an instance of " ^ Instance.to_string i
          | FunctionType -> "a function type"
          | StringType -> "a string type"
          | NumberType -> "a number type (int or float)"
          | BigintType -> "a bigint type"
          | BooleanType -> "a boolean type"
          | SymbolType -> "a symbol type"
          | UnknownType -> "of an unknown type")
        ^ "."
      | UnreachableBoolean name ->
        "Case " ^ name
        ^ " can never match: both true and false are already cases of this \
           variant."
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
  | UnboxedOnConstructor {constructor; single_field_record} ->
    fprintf ppf
      "@@unboxed applies to a whole variant: write it on the type, as in \
       `@@unboxed type t = ...`, instead of on constructor %s."
      constructor;
    if single_field_record then
      fprintf ppf
        " For a constructor whose payload is an inline record with one field, \
         `@@unboxed type t = %s({...})` makes the value that field. To keep \
         the record object, give the record type a name: `@@unboxed type t = \
         %s(r)`."
        constructor constructor
  | InvalidInstanceOf ->
    fprintf ppf
      "@instanceOf expects the name of a global class, such as \
       @instanceOf(\"Date\") or @instanceOf(\"Intl.Locale\")."
  | InvalidModuleClass module_ ->
    fprintf ppf
      "@instanceOf expects the name of a class that \"%s\" exports, such as \
       @instanceOf(\"Mesh\"), or @instanceOf(\"default\") for its default \
       export."
      (String_literal.encode_js_string module_)
  | InvalidClassModule ->
    fprintf ppf
      "@module on an @instanceOf type expects the module that exports the \
       class, such as @module(\"three\") or @module(\"./shapes.js\")."
  | InvalidClassScope Not_properties ->
    fprintf ppf
      "@scope on an @instanceOf type expects the properties the class is read \
       through, such as @scope(\"THREE\") or @scope((\"lib\", \"shapes\"))."
  | InvalidClassScope (Keyword_global global) ->
    fprintf ppf
      "Without @module, the first property of @scope is a global, which cannot \
       be named like the keyword \"%s\"."
      global
  | ClassLocationWithoutClass (attribute, missing) -> (
    fprintf ppf "@%s on a type declaration names %s, so it needs @instanceOf"
      attribute
      (if attribute = "module" then "the module a class is imported from"
       else "the properties a class is read through");
    match missing with
    | No_annotation ->
      fprintf ppf
        " too. Add @instanceOf with the name of the class, or remove @%s."
        attribute
    | Other_annotation annotation ->
      fprintf ppf
        ", and this type has @%s instead. Remove @%s, or write @instanceOf \
         instead of @%s if its values are instances of a class."
        annotation attribute annotation
    | Represented_type ->
      fprintf ppf
        ", which only abstract types and type abbreviations can have. Remove \
         @%s."
        attribute)
  | InvalidClassScope Scope_through_global_this ->
    fprintf ppf
      "Remove \"globalThis\" from @scope: the compiler reads the class through \
       globalThis itself where a local binding shadows it."
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
    fprintf ppf
      "A type can have only one of @notUndefined, @instanceOf and @typeOf. \
       @instanceOf and @typeOf already imply @notUndefined."
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
  | SubclassOfArray annotation ->
    fprintf ppf
      "%s cannot describe the values of an array type: they are recognized by \
       Array.isArray, and subclasses of Array are not supported."
      annotation
  | ClassOnPublicAbbreviation name ->
    fprintf ppf
      "A class must be declared on an abstract type or a private abbreviation: \
       write `= private ...`, so that values of the abbreviated type are not \
       instances of %s without a coercion, and another abbreviation cannot \
       declare them instances of another class."
      name
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

(* What [@notUndefined], [@instanceOf] or [@typeOf] on a type declaration
   states about the runtime representation of its values. *)
type runtime_annotation =
  | Not_undefined  (** values that are never [undefined] *)
  | Instance_of of Instance.class_name
      (** instances of this class: a global, or with [@module], an export *)
  | Type_of of block_type  (** values with this [typeof] *)

let not_undefined = "notUndefined"
let instance_of = "instanceOf"
let type_of = "typeOf"

(* Where an [@instanceOf] class is found, as for an external *)
let class_module = "module"
let class_scope = "scope"

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

let is_identifier segment =
  let is_start = function
    | 'A' .. 'Z' | 'a' .. 'z' | '_' | '$' -> true
    | _ -> false
  in
  let is_part = function
    | '0' .. '9' -> true
    | c -> is_start c
  in
  segment <> "" && is_start segment.[0] && String.for_all is_part segment

(* A path of properties of a module's exports, such as "default" or
   "shapes.Mesh": properties may be named like keywords *)
let is_export_path name =
  List.for_all is_identifier (String.split_on_char '.' name)

let is_class_path name =
  match String.split_on_char '.' name with
  | global :: _ ->
    (* Properties may be named like keywords, the global may not *)
    is_export_path name && not (Js_reserved_map.is_js_keyword global)
  | [] -> false

let runtime_attribute_name = function
  | Not_undefined -> not_undefined
  | Instance_of _ -> instance_of
  | Type_of _ -> type_of

(* A [@typeOf] states one of the kinds [typeof] recognizes *)
let typeof_name block_type =
  Option.value (Variant_runtime.typeof_name block_type) ~default:"object"

let runtime_annotation_to_string = function
  | Not_undefined -> "@" ^ not_undefined
  | Instance_of {module_ = None; path} ->
    Printf.sprintf "@%s(\"%s\")" instance_of path
  | Instance_of {module_ = Some module_; path} ->
    Printf.sprintf "@%s(\"%s\") @%s(\"%s\")" class_module
      (String_literal.encode_js_string module_)
      instance_of path
  | Type_of block_type ->
    Printf.sprintf "@%s(\"%s\")" type_of (typeof_name block_type)

(* What a declaration's annotations state about its values, for error
   messages *)
let describe_declared_values = function
  | Some (InstanceType (Class {name; ancestors = []})) ->
    "instances of " ^ Instance.class_name_to_string name
  | Some (InstanceType (Class {name; ancestors})) ->
    "instances of "
    ^ Instance.class_name_to_string name
    ^ ", a subclass of "
    ^ String.concat " and " (List.map Instance.class_name_to_string ancestors)
  | Some (InstanceType Array) -> "arrays"
  | Some block_type -> "typeof \"" ^ typeof_name block_type ^ "\""
  | None -> "no stated class or typeof"

(* The strings of a payload such as ["a"] or [("a", "b")] *)
let strings_of_payload payload =
  let strings expressions =
    let strings =
      List.filter_map Ast_payload.semantic_string_of_expression expressions
    in
    if List.length strings = List.length expressions then Some strings else None
  in
  match Ast_payload.unwrap_payload_expression payload with
  | PStr [{pstr_desc = Pstr_eval ({pexp_desc = Pexp_tuple expressions}, _)}] ->
    strings expressions
  | PStr [{pstr_desc = Pstr_eval (expression, _)}] -> strings [expression]
  | PStr _ | PSig _ | PTyp _ | PPat _ -> None

(* The location of an attribute, or [loc] for one the compiler made *)
let attribute_loc ~loc (attr_loc : Location.t) =
  if attr_loc = Location.none then loc else attr_loc

(* The class at the path [name] of [@instanceOf(name)], read through the
   properties of [@scope] in [attrs], among the exports of the module of
   [@module] or else among the globals. [name] is [None] when the payload of
   [@instanceOf] is not a string. *)
let class_of_attributes ~loc (attrs : Parsetree.attributes) name =
  let attribute txt =
    match
      Ext_list.filter attrs (fun ({Location.txt = txt'}, _) -> txt' = txt)
    with
    | [] -> None
    | [({loc = attr_loc}, payload)] ->
      Some (attribute_loc ~loc attr_loc, payload)
    | _ :: ({loc = attr_loc}, _) :: _ ->
      raise
        (Error (attribute_loc ~loc attr_loc, DuplicateRuntimeAnnotation txt))
  in
  let module_ =
    match attribute class_module with
    | None -> None
    | Some (loc, payload) -> (
      match Ast_payload.semantic_string_of_payload payload with
      | Some module_ when module_ <> "" -> Some module_
      | Some _ | None -> raise (Error (loc, InvalidClassModule)))
  in
  let scopes =
    match attribute class_scope with
    | None -> []
    | Some (scope_loc, payload) -> (
      let invalid reason = Error (scope_loc, InvalidClassScope reason) in
      match strings_of_payload payload with
      | Some (_ :: _ as scopes) when List.for_all is_identifier scopes -> (
        match (module_, scopes) with
        (* Without [@module], the first property is a global *)
        | None, "globalThis" :: _ -> raise (invalid Scope_through_global_this)
        | None, global :: _ when Js_reserved_map.is_js_keyword global ->
          raise (invalid (Keyword_global global))
        | (None | Some _), _ -> scopes)
      | Some _ | None -> raise (invalid Not_properties))
  in
  match (module_, name) with
  | Some module_, Some name when is_export_path name ->
    (* Any export can be a class *)
    {
      Instance.module_ = Some module_;
      path = String.concat "." (scopes @ [name]);
    }
  | Some module_, (Some _ | None) ->
    raise (Error (loc, InvalidModuleClass module_))
  | None, Some name -> (
    let path = String.concat "." (scopes @ [name]) in
    if not (is_class_path path) then raise (Error (loc, InvalidInstanceOf));
    match unsupported_class path with
    | None -> Instance.global path
    | Some reason -> raise (Error (loc, UnsupportedInstanceOf (path, reason))))
  | None, None -> raise (Error (loc, InvalidInstanceOf))

(* The [@notUndefined], [@instanceOf] or [@typeOf] annotation in [attrs],
   validated, with the [@module] and [@scope] of an [@instanceOf]. Only
   [Typedecl] passes [~loc]; later readers see attributes it accepted. *)
let process_runtime_annotation ?(loc = Location.none)
    (attrs : Parsetree.attributes) =
  let annotation_of ({Location.txt; loc = attr_loc}, payload) =
    let loc = attribute_loc ~loc attr_loc in
    if txt = not_undefined then Some Not_undefined
    else if txt = instance_of then
      Some
        (Instance_of
           (class_of_attributes ~loc attrs
              (Ast_payload.semantic_string_of_payload payload)))
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
   [is_abstract_or_abbreviation]. [@module] and [@scope] say where the class
   of an [@instanceOf] is found, so they need one. Only [Typedecl] calls this:
   a declaration read from an interface compiled before keeps its
   attributes. *)
let check_runtime_annotation ~loc ~is_abstract_or_abbreviation
    (attrs : Parsetree.attributes) =
  let annotation = process_runtime_annotation ~loc attrs in
  (match annotation with
  | Some annotation when not is_abstract_or_abbreviation ->
    raise
      (Error
         ( loc,
           RuntimeAnnotationOnRepresentedType
             (runtime_attribute_name annotation) ))
  | Some _ | None -> ());
  let missing =
    match annotation with
    | Some (Instance_of _) -> None
    | Some ((Not_undefined | Type_of _) as annotation) ->
      Some (Other_annotation (runtime_attribute_name annotation))
    | None ->
      Some
        (if is_abstract_or_abbreviation then No_annotation else Represented_type)
  in
  match missing with
  | None -> ()
  | Some missing ->
    Ext_list.iter attrs (fun ({Location.txt; loc = attr_loc}, _) ->
        if txt = class_module || txt = class_scope then
          raise
            (Error
               ( attribute_loc ~loc attr_loc,
                 ClassLocationWithoutClass (txt, missing) )))

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
