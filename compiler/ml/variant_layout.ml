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

(* The payload kind of the values of [t]: the one its declarations state, or
   the one its structure gives. Builtin types state their kinds in [Predef];
   type expressions other than constructors have no declaration. An unboxed
   record is the value of its field, so it has no kind of its own. *)
let payload_kind ~env (t : Types.type_expr) : block_type option =
  match recognized_as ~env t with
  | Some _ as declared -> declared
  | None -> (
    match (Ctype.expand_head env t).desc with
    | Ttuple _ -> Some (InstanceType Array)
    | Tarrow _ -> Some FunctionType
    | Tconstr _ -> (
      match Ctype.extract_concrete_typedecl env t with
      | _, _, {type_kind = Type_record (_, Record_unboxed _)} -> None
      | _, _, {type_kind = Type_record _} -> Some ObjectType
      | _ -> None
      | exception Not_found -> None)
    | _ -> None)

(* The shape a declaration states, resolved against what the type it
   abbreviates states in [env]: a class's superclasses are the classes
   annotated further down a chain of abbreviations. *)
let declaration_shape ~env (decl : Types.type_declaration) : value_shape =
  match process_runtime_annotation decl.type_attributes with
  | None -> Unstated
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
  | Some Not_undefined -> (
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

let get_block_type ~env (cstr : Types.constructor_declaration) : block_type =
  match cstr.cd_args with
  | Cstr_tuple [ty] -> Option.value (payload_kind ~env ty) ~default:UnknownType
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
  (* The kinds of the untagged payloads seen so far. A class is identified by
     its name. *)
  let kinds = Hashtbl.create 1 in
  let kind_key = function
    | InstanceType (Class {name}) -> InstanceType (Class {name; ancestors = []})
    | kind -> kind
  in
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
      | Untagged {tag; block_type = UnknownType} ->
        (* No check tells an unknown payload apart from another payload *)
        if List.length blocks <> 1 then
          raise
            (Error
               (loc, InvalidUntaggedVariantDefinition (OnlyOneUnknown tag.name)))
      | Untagged {tag; block_type} ->
        (* Literal cases are matched before payload cases, so a boolean
           payload only receives the booleans that are not literal cases. A
           partial overlap is allowed, like for strings and numbers. *)
        if
          block_type = BooleanType
          && String_map.mem "true" !nonstring_literals_consts
          && String_map.mem "false" !nonstring_literals_consts
        then
          raise
            (Error
               ( loc,
                 InvalidUntaggedVariantDefinition (UnreachableBoolean tag.name)
               ));
        let key = kind_key block_type in
        if Hashtbl.mem kinds key then
          raise
            (Error
               ( loc,
                 InvalidUntaggedVariantDefinition (AtMostOneOfKind block_type)
               ));
        Hashtbl.add kinds key ())

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
