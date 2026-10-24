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
  | None ->
    if has_not_undefined decl.type_attributes then Never_undefined else Unstated

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
      (* A public abbreviation is the same type as the superclass, unless it
         is a strict instance of it, as in the phantom type hierarchy
         [type element<'a> = node<_element<'a>>]. The arguments are read after
         expansion, where the superclass type is reached. *)
      let same_type =
        match (Ctype.expand_head_opt env manifest).desc with
        | Tconstr (_, args, _) -> List.for_all Btype.is_Tvar args
        | _ -> true
      in
      let superclass, ancestors =
        match inner with
        | InstanceType (Class {name = ancestor; ancestors}) ->
          (ancestor, ancestor :: ancestors)
        (* Every class instance is an object *)
        | _ -> ("Object", [])
      in
      if decl.type_private = Public && same_type then
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
    (* Builtin types have no declaration to annotate *)
    match (Ctype.expand_head env t).desc with
    | Tconstr (path, _, _) when Path.same path Predef.path_string ->
      Some StringType
    | Tconstr (path, _, _)
      when Path.same path Predef.path_int || Path.same path Predef.path_float ->
      Some NumberType
    | Tconstr (path, _, _) when Path.same path Predef.path_bigint ->
      Some BigintType
    | Tconstr (path, _, _) when Path.same path Predef.path_bool ->
      Some BooleanType
    | Tconstr (path, _, _) when Path.same path Predef.path_dict ->
      Some ObjectType
    | Tconstr (path, _, _) when Path.same path Predef.path_array ->
      Some (InstanceType Array)
    | Tconstr (path, _, _) when Path.same path Predef.path_promise ->
      Some (InstanceType (Class {name = "Promise"; ancestors = []}))
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

let layout_from_type_variant ~(configuration : configuration) ~env
    (cstrs : Types.constructor_declaration list) : Variant_runtime.layout =
  let get_block (cstr : Types.constructor_declaration) : block =
    let tag = {name = Ident.name cstr.cd_id; literal = cstr.cd_runtime_tag} in
    let tag_name = process_tag_name cstr.cd_attributes in
    if has_untagged cstr.cd_attributes then
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
  check_invariant ~is_untagged_def:configuration.unboxed ~consts ~blocks;
  let constructors =
    Array.of_list
      (List.map (fun (_, constructor) -> constructor) located_constructors)
  in
  Variant_runtime.make_layout ~configuration constructors
