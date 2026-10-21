(* Derivation of the canonical runtime layout of a variant declaration.

   This runs exactly once per declaration, in [Typedecl], after the whole
   recursive group has entered the environment; the result is stored on
   [Type_variant] and never re-derived. It lives above [Ctype] because
   classifying untagged payloads requires expanding their types. *)

open Variant_runtime
open Ast_untagged_variants

(* The [@instanceOf] and [@typeOf] annotations of [t] and of the types it
   abbreviates, outermost first. The type as written can carry one, and so can
   each level of a chain of private abbreviations that models a class
   hierarchy, such as [@instanceOf("File") type t = private Blob.t]. *)
let runtime_annotations ~env (t : Types.type_expr) =
  let rec collect ~previous (t : Types.type_expr) =
    match (Btype.repr t).desc with
    | Tconstr (path, _, _) ->
      let here =
        match Env.find_type path env with
        | {type_attributes} -> process_runtime_annotation type_attributes
        | exception Not_found -> None
      in
      (* A module alias makes [M.t] an abbreviation of the original [t] with
         the same attributes, so an annotation repeating the one just seen is
         the same declaration again *)
      let here, previous =
        match here with
        | Some _ when here = previous -> (None, previous)
        | Some _ -> (here, here)
        | None -> (None, previous)
      in
      let rest =
        match Ctype.try_expand_once_opt env t with
        | expanded -> collect ~previous expanded
        | exception Ctype.Cannot_expand -> []
      in
      Option.to_list here @ rest
    | _ -> []
  in
  collect ~previous:None t

(* The block type the outermost of [annotations] states. A class's ancestors
   are the classes annotated further down the chain. *)
let block_type_of_annotations = function
  | Instance_of name :: rest ->
    let ancestors =
      List.filter_map
        (function
          | Instance_of ancestor when ancestor <> name -> Some ancestor
          | Instance_of _ | Type_of _ -> None)
        rest
    in
    Some (InstanceType (Class {name; ancestors}))
  | Type_of block_type :: _ -> Some block_type
  | [] -> None

let declared_block_type ~env t =
  block_type_of_annotations (runtime_annotations ~env t)

(* The block type a declaration states, through its own annotation or the
   ones of the type it abbreviates. *)
let declaration_block_type ~env (decl : Types.type_declaration) =
  let own = process_runtime_annotation decl.type_attributes in
  let inner =
    match decl.type_manifest with
    | Some manifest -> runtime_annotations ~env manifest
    | None -> []
  in
  let inner =
    match inner with
    | annotation :: rest when Some annotation = own -> rest
    | _ -> inner
  in
  block_type_of_annotations (Option.to_list own @ inner)

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
    match (outer, runtime_annotations ~env manifest) with
    | _, [] -> (
      let is_abstract path =
        match Env.find_type path env with
        | {type_kind = Type_abstract; type_manifest = None} -> true
        | _ -> false
        | exception Not_found -> false
      in
      match (Ctype.expand_head_opt env manifest).desc with
      | Tconstr (path, _, _)
        when Predef.type_is_builtin_path_but_option path = Predef.NA
             && is_abstract path ->
        ()
      | _ ->
        error (RuntimeAnnotationOnUnrelatedType (runtime_attribute_name outer)))
    | Type_of block_type, Type_of inner :: _ when block_type = inner -> ()
    | Instance_of name, Instance_of inner :: _ when name = inner -> ()
    | Instance_of name, (Instance_of ancestor :: _ as chain) ->
      (* A public abbreviation is the same type as the superclass, unless it
         is a strict instance of it, as in the phantom type hierarchy
         [type element<'a> = node<_element<'a>>] *)
      let same_type =
        match (Btype.repr manifest).desc with
        | Tconstr (_, args, _) -> List.for_all Btype.is_Tvar args
        | _ -> true
      in
      if decl.type_private = Public && same_type then
        error (SubclassOfPublicAbbreviation (name, ancestor))
      else if List.mem outer chain then error (ClassTwiceInChain name)
    | (Instance_of _ | Type_of _), inner :: _ ->
      error
        (RuntimeAnnotationContradictsManifest
           ( runtime_annotation_to_string outer,
             runtime_annotation_to_string inner )))

let get_block_type_from_typ ~env (t : Types.type_expr) : block_type option =
  match declared_block_type ~env t with
  | Some _ as declared -> declared
  | None -> (
    (* Builtin types have no declaration to annotate *)
    match (Ctype.expand_head env t).desc with
    | Tconstr (path, _, _) when Path.same path Predef.path_string ->
      Some StringType
    | Tconstr (path, _, _) when Path.same path Predef.path_int -> Some IntType
    | Tconstr (path, _, _) when Path.same path Predef.path_float ->
      Some FloatType
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
