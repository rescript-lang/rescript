(* Derivation of the canonical runtime layout of a variant declaration.

   This runs exactly once per declaration, in [Typedecl], after the whole
   recursive group has entered the environment; the result is stored on
   [Type_variant] and never re-derived. It lives above [Ctype] because
   classifying untagged payloads requires expanding their types. *)

open Variant_runtime
open Ast_untagged_variants

(* The shape of [t]: the one stated by the first declaration on its expansion
   chain that states one. Each declaration's shape already includes what the
   types it abbreviates state, such as a class's superclasses. [fuel] bounds
   the walk, since a recursive group's shapes are resolved before its
   abbreviations are checked for cycles. *)
let value_shape ~env (t : Types.type_expr) : value_shape =
  let rec walk fuel (t : Types.type_expr) =
    match (Btype.repr t).desc with
    | Tconstr (path, _, _) when fuel > 0 -> (
      match Env.find_type path env with
      | {type_shape = (Never_undefined | Recognized_as _) as shape} -> shape
      | {type_shape = Unstated} | (exception Not_found) -> (
        match Ctype.try_expand_once_opt env t with
        | expanded -> walk (fuel - 1) expanded
        | exception Ctype.Cannot_expand -> Unstated))
    | _ -> Unstated
  in
  walk 100 t

(* The payload kind [t] is declared to have *)
let recognized_as ~env t =
  match value_shape ~env t with
  | Recognized_as block_type -> Some block_type
  | Unstated | Never_undefined -> None

(* The shape a declaration states, resolved against what the type it
   abbreviates states in [env]: a class's superclasses are the classes
   annotated further down a chain of abbreviations. *)
let declaration_shape ~env (decl : Types.type_declaration) : value_shape =
  match process_runtime_annotation decl.type_attributes with
  | Some (Type_of block_type) -> Recognized_as block_type
  | Some (Instance_of name) ->
    let ancestors =
      match Option.map (recognized_as ~env) decl.type_manifest with
      | Some (Some (InstanceType (Class {name = inner; ancestors}))) ->
        (* An annotation repeating the abbreviated type's, as a module alias
           does, states no new class *)
        if inner = name then ancestors else inner :: ancestors
      | Some _ | None -> []
    in
    Recognized_as (InstanceType (Class {name; ancestors}))
  | None -> (
    if not (has_not_undefined decl.type_attributes) then Unstated
    else
      (* Restating a kind the abbreviated type is recognized by keeps it *)
      match Option.map (value_shape ~env) decl.type_manifest with
      | Some (Recognized_as _ as shape) -> shape
      | Some (Unstated | Never_undefined) | None -> Never_undefined)

(* Resolves the shapes of a group of declarations, which can abbreviate each
   other. Each round resolves one more level of abbreviation within the
   group, so the rounds stop when one changes nothing; a chain inside the
   group has at most one level per declaration. *)
let resolve_shapes ~env (decls : (Ident.t * Types.type_declaration) list) =
  let rec round env previous n =
    let decls =
      List.map
        (fun (id, decl) ->
          (id, {decl with Types.type_shape = declaration_shape ~env decl}))
        previous
    in
    if
      n <= 1
      || List.for_all2
           (fun (_, d1) (_, d2) -> d1.Types.type_shape = d2.Types.type_shape)
           previous decls
    then decls
    else
      let env =
        List.fold_left
          (fun env (id, decl) -> Env.add_type ~check:false id decl env)
          env decls
      in
      round env decls (n - 1)
  in
  let env =
    List.fold_left
      (fun env (id, decl) -> Env.add_type ~check:false id decl env)
      env decls
  in
  round env decls (List.length decls)

(* Whether the abstract type at [path] has a representation of its own, which
   an annotation can refine, or stands for another type: a functor
   parameter's type, a recursive module's type being defined, or a locally
   abstract type. *)
type abstractness = Opaque | Stands_for_another | Not_abstract

let abstractness ~env path =
  match Env.find_type path env with
  | {type_kind = Type_abstract; type_manifest = None; type_newtype_level} ->
    if type_newtype_level <> None || Env.is_functor_arg path env then
      Stands_for_another
    else if Predef.type_is_builtin_path_but_option path <> Predef.NA then
      Not_abstract
    else Opaque
  | _ -> Not_abstract
  | exception Not_found -> Not_abstract

let annotation_of_block_type = function
  | InstanceType (Class {name}) -> Instance_of name
  | block_type -> Type_of block_type

(* An annotation on an abbreviation is a claim about the abbreviated type, so
   it must agree with what the compiler knows about that type. It can refine
   an abstract type, restate the annotation the abbreviated type has, or, on a
   private abbreviation, declare a subclass of the class annotated there.
   Anything else, such as [@typeOf("string") type t = option<string>], would
   let two spellings of one type be recognized differently, and would make
   [Some(None)] of a type that can be undefined indistinguishable from
   [None]. *)
let check_manifest_annotation ~loc ~env (decl : Types.type_declaration) =
  match
    (process_runtime_annotation decl.type_attributes, decl.type_manifest)
  with
  | None, _ | Some _, None -> ()
  | Some outer, Some manifest -> (
    let error err = raise (Error (loc, err)) in
    match (outer, recognized_as ~env manifest) with
    | _, None -> (
      match (Ctype.expand_head_opt env manifest).desc with
      | Tconstr (path, _, _) -> (
        match abstractness ~env path with
        | Opaque -> ()
        | Stands_for_another ->
          error
            (RuntimeAnnotationOnTypeParameter (runtime_attribute_name outer))
        | Not_abstract ->
          error
            (RuntimeAnnotationOnUnrelatedType (runtime_attribute_name outer)))
      | _ ->
        error (RuntimeAnnotationOnUnrelatedType (runtime_attribute_name outer)))
    | Type_of block_type, Some inner when block_type = inner -> ()
    | Instance_of name, Some (InstanceType (Class {name = inner}))
      when name = inner ->
      ()
    | Instance_of name, Some ((InstanceType (Class _) | ObjectType) as inner) ->
      (* A public abbreviation would make every value of the superclass type
         a value of the subclass, without a coercion *)
      let superclass, ancestors =
        match inner with
        | InstanceType (Class {name = ancestor; ancestors}) ->
          (ancestor, ancestor :: ancestors)
        (* Every class instance is an object *)
        | _ -> ("Object", [])
      in
      if decl.type_private = Public then
        error (SubclassOfPublicAbbreviation (name, superclass))
      else if List.mem name ancestors then error (ClassTwiceInChain name)
    | (Instance_of _ | Type_of _), Some inner ->
      error
        (RuntimeAnnotationContradictsManifest
           ( runtime_annotation_to_string outer,
             runtime_annotation_to_string (annotation_of_block_type inner) )))

let get_block_type_from_typ ~env (t : Types.type_expr) : block_type option =
  match recognized_as ~env t with
  | Some _ as declared -> declared
  | None -> (
    (* Builtin types state their shapes in [Predef]; type expressions other
       than constructors have no declaration *)
    match (Ctype.expand_head env t).desc with
    | Ttuple _ -> Some (InstanceType Array)
    | Tarrow _ -> Some FunctionType
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

(* The cases of a variant must be told apart at run time: nullary cases and
   tagged blocks by distinct literals, the untagged payloads of an [@unboxed]
   variant by distinct kinds *)
let check_invariant ~(consts : (Location.t * tag) list)
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
  (* The blocks of a variant are all tagged or, when the type is [@unboxed],
     all untagged. A tagged block is told apart from the others by its tag; an
     untagged one by the kind of its payload, whatever its [@as]. *)
  Ext_list.rev_iter blocks (fun (loc, block) ->
      match block with
      | Tagged {tag} -> check_literal ~is_const:false ~loc tag
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
        invariant loc tag.name)

(* A constructor without [@as] is represented by its name *)

let layout_from_type_variant ~(configuration : configuration) ~env
    (cstrs : Types.constructor_declaration list) : Variant_runtime.layout =
  let get_block (cstr : Types.constructor_declaration) : block =
    let tag = declared_tag cstr in
    let tag_name = process_tag_name cstr.cd_attributes in
    if configuration.unboxed then
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
  let consts, blocks =
    Ext_list.fold_left located_constructors ([], [])
      (fun (consts, blocks) (loc, constructor) ->
        match constructor with
        | Constant tag -> ((loc, tag) :: consts, blocks)
        | Block block -> (consts, (loc, block) :: blocks))
  in
  check_invariant ~consts ~blocks;
  let constructors =
    Array.of_list
      (List.map (fun (_, constructor) -> constructor) located_constructors)
  in
  Variant_runtime.make_layout ~configuration constructors
