(* Copyright (C) 2015 - 2016 Bloomberg Finance L.P.
 * Copyright (C) 2017 - Hongbo Zhang, Authors of ReScript
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)

let rec variant_can_unwrap_aux (row_fields : Parsetree.row_field list) : bool =
  match row_fields with
  | [] -> true
  | Rtag (_, _, false, [_]) :: rest -> variant_can_unwrap_aux rest
  | _ :: _ -> false

let variant_unwrap (row_fields : Parsetree.row_field list) : bool =
  match row_fields with
  | [] -> false (* impossible syntax *)
  | xs -> variant_can_unwrap_aux xs

(*
  TODO: [nolabel] is only used once turn Nothing into Unit, refactor later
*)
let spec_of_ptyp (nolabel : bool) (ptyp : Parsetree.core_type) :
    External_arg_spec.attr =
  let ptyp_desc = ptyp.ptyp_desc in
  match
    Ast_attributes.iter_process_bs_string_int_unwrap_uncurry
      ptyp.ptyp_attributes
  with
  | `String -> (
    match ptyp_desc with
    | Ptyp_variant (row_fields, Closed, None) ->
      Ast_polyvar.map_row_fields_into_strings ptyp.ptyp_loc row_fields
    | _ -> Bs_syntaxerr.err ptyp.ptyp_loc Invalid_bs_string_type)
  | `Ignore -> Ignore
  | `Int -> (
    match ptyp_desc with
    | Ptyp_variant (row_fields, Closed, None) ->
      let int_lists =
        Ast_polyvar.map_row_fields_into_ints ptyp.ptyp_loc row_fields
      in
      Int int_lists
    | _ -> Bs_syntaxerr.err ptyp.ptyp_loc Invalid_bs_int_type)
  | `Unwrap -> (
    match ptyp_desc with
    | Ptyp_variant (row_fields, Closed, _) when variant_unwrap row_fields ->
      Unwrap
    (* Unwrap attribute can only be attached to things like `[a of a0 | b of b0]` *)
    | _ -> Bs_syntaxerr.err ptyp.ptyp_loc Invalid_bs_unwrap_type)
  | `Nothing -> (
    match ptyp_desc with
    | Ptyp_constr ({txt = Lident "unit"}, []) ->
      if nolabel then Extern_unit else Nothing
    | _ -> Nothing)

(* is_optional = false 
*)
let refine_arg_type ~(nolabel : bool) (ptyp : Ast_core_type.t) :
    External_arg_spec.attr =
  if ptyp.ptyp_desc = Ptyp_any then
    let ptyp_attrs = ptyp.ptyp_attributes in
    let payload = Ast_attributes.iter_process_bs_string_or_int_as ptyp_attrs in
    match payload with
    | None -> spec_of_ptyp nolabel ptyp
    | Some cst -> (
      (* (_[@as ])*)
      (* when ppx start dropping attributes
         we should warn, there is a trade off whether
         we should warn dropped non bs attribute or not
      *)
      Bs_ast_invariant.warn_discarded_unused_attributes ptyp_attrs;
      match cst with
      | Fixed source -> Arg_cst (External_arg_spec.cst_fixed source))
  else (* ([`a|`b] [@string]) *)
    spec_of_ptyp nolabel ptyp

let refine_obj_arg_type ~(nolabel : bool) (ptyp : Ast_core_type.t) :
    External_arg_spec.attr =
  if ptyp.ptyp_desc = Ptyp_any then (
    let ptyp_attrs = ptyp.ptyp_attributes in
    let payload = Ast_attributes.iter_process_bs_string_or_int_as ptyp_attrs in
    (* when ppx start dropping attributes
       we should warn, there is a trade off whether
       we should warn dropped non bs attribute or not
    *)
    Bs_ast_invariant.warn_discarded_unused_attributes ptyp_attrs;
    match payload with
    | None -> Bs_syntaxerr.err ptyp.ptyp_loc Invalid_underscore_type_in_external
    | Some (Fixed source) -> Arg_cst (External_arg_spec.cst_fixed source))
  else (* ([`a|`b] [@string]) *)
    spec_of_ptyp nolabel ptyp

(** Given the type of argument, process its [bs.] attribute and new type,
    The new type is currently used to reconstruct the external type
    and result type in [@@obj]
    They are not the same though, for example
    {[
      external f : hi:([ `hi | `lo ] [@string]) -> unit -> _ = "" [@@obj]
    ]}
    The result type would be [ hi:string ]
*)
let get_opt_arg_type ~(nolabel : bool) (ptyp : Ast_core_type.t) :
    External_arg_spec.attr =
  if ptyp.ptyp_desc = Ptyp_any then
    (* (_[@as ])*)
    (* extenral f : ?x:_ -> y:int -> _ = "" [@@obj] is not allowed *)
    Bs_syntaxerr.err ptyp.ptyp_loc Invalid_underscore_type_in_external;
  (* ([`a|`b] [@@string]) *)
  spec_of_ptyp nolabel ptyp

(**
   [@@module "react"]
   [@@module "react"]
   ---
   [@@module "@" "react"]
   [@@module "@" "react"]

   They should have the same module name

   TODO: we should emit an warning if we bind
   two external files to the same module name
*)
type source = Payload | External

type bundle_source = {name: string; source: source}

type external_desc = {
  val_name: bundle_source option;
  external_module_name: External_ffi_types.external_module_name option;
  module_as_val: External_ffi_types.external_module_name option;
  val_send: bundle_source option;
  splice: bool;
  (* mutable *)
  scopes: string list;
  set_index: bool;
  (* mutable *)
  get_index: bool;
  new_name: bundle_source option;
  call_name: bundle_source option;
  set_name: bundle_source option;
  get_name: bundle_source option;
  mk_obj: bool;
  return_wrapper: External_ffi_types.return_wrapper;
}

let init_st =
  {
    val_name = None;
    external_module_name = None;
    module_as_val = None;
    val_send = None;
    splice = false;
    scopes = [];
    set_index = false;
    get_index = false;
    new_name = None;
    call_name = None;
    set_name = None;
    get_name = None;
    mk_obj = false;
    return_wrapper = Return_unset;
  }

let return_wrapper loc (txt : string) : External_ffi_types.return_wrapper =
  match txt with
  | "null_to_opt" -> Return_null_to_opt
  | "nullable" | "null_undefined_to_opt" -> Return_null_undefined_to_opt
  | "identity" -> Return_identity
  | _ -> Bs_syntaxerr.err loc Not_supported_directive_in_bs_return

exception Not_handled_external_attribute

(* The processed attributes will be dropped *)
let parse_external_attributes (no_arguments : bool) (prim_name_check : string)
    (prim_name_or_pval_prim : bundle_source)
    (prim_attributes : Ast_attributes.t) : Ast_attributes.t * external_desc =
  (* shared by `[@@val]`, `[@@send]`,
     `[@@set]`, `[@@get]` , `[@@new]`
  *)
  let name_from_payload_or_prim ~loc (payload : Parsetree.payload) :
      bundle_source =
    match payload with
    | PStr [] -> prim_name_or_pval_prim
    (* It is okay to have [@@val] without payload *)
    | _ -> (
      match Ast_payload.semantic_string_of_payload payload with
      | Some val_name -> {name = val_name; source = Payload}
      | None -> Location.raise_errorf ~loc "Invalid payload")
  in

  Ext_list.fold_left prim_attributes ([], init_st)
    (fun (attrs, st) (({txt; loc}, payload) as attr) ->
      if txt = Literals.gentype_import1 || txt = Literals.gentype_import2 then
        let bundle =
          "./"
          ^ Ext_filename.new_extension
              (Filename.basename !Location.input_name)
              ".gen"
        in
        ( attr :: attrs,
          {
            st with
            external_module_name =
              Some
                {
                  bundle;
                  module_bind_name = Phint_nothing;
                  import_attributes = None;
                };
          } )
      else
        let action () =
          match txt with
          | "val" ->
            if no_arguments then
              {st with val_name = Some (name_from_payload_or_prim ~loc payload)}
            else
              {
                st with
                call_name = Some (name_from_payload_or_prim ~loc payload);
              }
          | "module" -> (
            match payload with
            | PStr
                [
                  {
                    pstr_desc =
                      Pstr_eval
                        ({pexp_loc; pexp_desc = Pexp_record (fields, _)}, _);
                    _;
                  };
                ] -> (
              let from_name = ref None in
              let with_ = ref None in
              Ext_list.iter fields (fun {lid = l; x = exp} ->
                  match l with
                  | {txt = Lident "from"} -> (
                    match Ast_payload.semantic_string_of_expression exp with
                    | Some name -> from_name := Some name
                    | None -> ())
                  | {txt = Lident "with"} -> (
                    match exp.pexp_desc with
                    | Pexp_record (fields, _) -> with_ := Some fields
                    | _ -> ())
                  | _ -> ());
              match (!from_name, !with_) with
              | None, _ ->
                Location.raise_errorf ~loc:pexp_loc
                  "@module annotations with import attributes must have a \
                   \"from\" field. This \"from\" field should point to the JS \
                   module to import, just like the string payload to @module \
                   normally does."
              | Some _, None ->
                Location.raise_errorf ~loc:pexp_loc
                  "@module annotations with import attributes must have a \
                   \"with\" field. This \"with\" field should hold a record of \
                   the import attributes you want applied to the import."
              | Some from_name, Some with_fields ->
                let import_attributes_from_record =
                  Ext_list.filter_map with_fields (fun {lid = l; x = exp} ->
                      match Ast_payload.semantic_string_of_expression exp with
                      | Some s -> (
                        match l.txt with
                        | Longident.Lident "type_" -> Some ("type", s)
                        | Longident.Lident txt -> Some (txt, s)
                        | _ ->
                          Location.raise_errorf ~loc:exp.pexp_loc
                            "Field must be a regular key.")
                      | _ ->
                        Location.raise_errorf ~loc:exp.pexp_loc
                          "Only string values are allowed here.")
                in
                {
                  st with
                  external_module_name =
                    Some
                      {
                        bundle = from_name;
                        module_bind_name = Phint_nothing;
                        import_attributes = Some import_attributes_from_record;
                      };
                })
            | _ -> (
              match Ast_payload.assert_strings loc payload with
              | [bundle] ->
                {
                  st with
                  external_module_name =
                    Some
                      {
                        bundle;
                        module_bind_name = Phint_nothing;
                        import_attributes = None;
                      };
                }
              | [bundle; bind_name] ->
                {
                  st with
                  external_module_name =
                    Some
                      {
                        bundle;
                        module_bind_name = Phint_name bind_name;
                        import_attributes = None;
                      };
                }
              | [] ->
                {
                  st with
                  module_as_val =
                    Some
                      {
                        bundle = prim_name_or_pval_prim.name;
                        module_bind_name = Phint_nothing;
                        import_attributes = None;
                      };
                }
              | _ -> Bs_syntaxerr.err loc Illegal_attribute))
          | "scope" -> (
            match Ast_payload.assert_strings loc payload with
            | [] -> Bs_syntaxerr.err loc Illegal_attribute
            (* We need err on empty scope, so we can tell the difference
               between unset/set
            *)
            | scopes -> {st with scopes})
          | "taggedTemplate" ->
            Location.raise_errorf ~loc
              "The @@taggedTemplate decorator has been removed. Bind the \
               external with the builtin taggedTemplate<'param, 'output> type \
               instead, e.g. `@@module(\"x\") external sql: taggedTemplate<'a, \
               string> = \"sql\"`. The tag can then be used with backtick \
               syntax across module boundaries and as a first-class value."
          | "variadic" -> {st with splice = true}
          | "send" ->
            {st with val_send = Some (name_from_payload_or_prim ~loc payload)}
          | "set" ->
            {st with set_name = Some (name_from_payload_or_prim ~loc payload)}
          | "get" ->
            {st with get_name = Some (name_from_payload_or_prim ~loc payload)}
          | "new" ->
            {st with new_name = Some (name_from_payload_or_prim ~loc payload)}
          | "set_index" ->
            if String.length prim_name_check <> 0 then
              Location.raise_errorf ~loc
                "%@set_index this particular external's name needs to be a \
                 placeholder empty string";
            {st with set_index = true}
          | "get_index" ->
            if String.length prim_name_check <> 0 then
              Location.raise_errorf ~loc
                "%@get_index this particular external's name needs to be a \
                 placeholder empty string";
            {st with get_index = true}
          | "obj" -> {st with mk_obj = true}
          | "return" -> (
            let actions = Ast_payload.ident_or_record_as_config loc payload in
            match actions with
            | [({txt}, None)] ->
              {st with return_wrapper = return_wrapper loc txt}
            | _ -> Bs_syntaxerr.err loc Not_supported_directive_in_bs_return)
          | _ -> raise_notrace Not_handled_external_attribute
        in
        try (attrs, action ())
        with Not_handled_external_attribute -> (attr :: attrs, st))

let check_return_wrapper loc (wrapper : External_ffi_types.return_wrapper)
    result_type =
  match wrapper with
  | Return_identity | Return_unset -> wrapper
  | Return_null_to_opt | Return_null_undefined_to_opt ->
    if Ast_core_type.is_user_option result_type then wrapper
    else Bs_syntaxerr.err loc Expect_opt_in_bs_return_to_opt

type response = {
  pval_type: Parsetree.core_type;
  pval_prim: Parsetree.primitive_repr;
  pval_attributes: Parsetree.attributes;
  no_inline_cross_module: bool;
}

let fixed_arg_type ({txt = source; loc} : Parsetree.fixed_value) =
  match Classify_function.classify source with
  | Js_literal _ ->
    External_arg_spec.Arg_cst (External_arg_spec.cst_fixed source)
  | _ ->
    Location.raise_errorf ~loc "The %%raw payload must be a JavaScript literal"

let process_obj (loc : Location.t) (st : external_desc) (prim_name : string)
    (arg_types_ty : Parsetree.arg list) (result_type : Ast_core_type.t) :
    int * Parsetree.core_type * External_ffi_types.t =
  match st with
  | {
   val_name = None;
   external_module_name = None;
   module_as_val = None;
   val_send = None;
   splice = false;
   new_name = None;
   call_name = None;
   set_name = None;
   get_name = None;
   get_index = false;
   return_wrapper = Return_unset;
   set_index = false;
   mk_obj = _;
   scopes =
     [] (* wrapper does not work with @obj
        TODO: better error message *);
  } ->
    if String.length prim_name <> 0 then
      Location.raise_errorf ~loc
        "%@obj expect external names to be empty string";
    let arg_kinds, args, (result_types : Parsetree.object_field list) =
      Ext_list.fold_right arg_types_ty ([], [], [])
        (fun
          param_type
          (arg_labels, (arg_types : Parsetree.arg list), result_types)
        ->
          let arg_label, param_attrs, ty, fixed =
            match param_type with
            | Parsetree.Parg_type {attrs; lbl; typ} -> (lbl, attrs, typ, None)
            | Parsetree.Parg_fixed {attrs; lbl; value} ->
              (lbl, attrs, Ast_helper.Typ.any ~loc:value.loc (), Some value)
          in
          let new_arg_label, new_arg_types, output_tys =
            match arg_label with
            | Nolabel -> (
              match ty.ptyp_desc with
              | Ptyp_constr ({txt = Lident "unit"}, []) ->
                ( External_arg_spec.empty_kind Extern_unit,
                  param_type :: arg_types,
                  result_types )
              | _ ->
                Location.raise_errorf ~loc
                  "expect label, optional, or unit here")
            | Labelled {txt = label} -> (
              let field_name =
                match Ast_attributes.iter_process_bs_string_as param_attrs with
                | Some alias -> alias
                | None -> label
              in
              let obj_arg_type =
                match fixed with
                | Some value -> fixed_arg_type value
                | None -> refine_obj_arg_type ~nolabel:false ty
              in
              match obj_arg_type with
              | Ignore ->
                ( External_arg_spec.empty_kind obj_arg_type,
                  param_type :: arg_types,
                  result_types )
              | Arg_cst _ ->
                ( {
                    obj_arg_label = External_arg_spec.obj_label field_name;
                    obj_arg_type;
                  },
                  arg_types,
                  (* ignored in [arg_types], reserved in [result_types] *)
                  result_types )
              | Nothing ->
                ( {
                    obj_arg_label = External_arg_spec.obj_label field_name;
                    obj_arg_type;
                  },
                  param_type :: arg_types,
                  Parsetree.Otag ({Asttypes.txt = field_name; loc}, [], ty)
                  :: result_types )
              | Int _ ->
                ( {
                    obj_arg_label = External_arg_spec.obj_label field_name;
                    obj_arg_type;
                  },
                  param_type :: arg_types,
                  Otag
                    ( {Asttypes.txt = field_name; loc},
                      [],
                      Ast_literal.type_int ~loc () )
                  :: result_types )
              | Poly_var_string _ ->
                ( {
                    obj_arg_label = External_arg_spec.obj_label field_name;
                    obj_arg_type;
                  },
                  param_type :: arg_types,
                  Otag
                    ( {Asttypes.txt = field_name; loc},
                      [],
                      Ast_literal.type_string ~loc () )
                  :: result_types )
              | Extern_unit -> assert false
              | Poly_var _ ->
                Location.raise_errorf ~loc
                  "%@obj label %s does not support such arg type" label
              | Unwrap ->
                Location.raise_errorf ~loc
                  "%@obj label %s does not support %@unwrap arguments" label)
            | Optional {txt = label} -> (
              let field_name =
                match Ast_attributes.iter_process_bs_string_as param_attrs with
                | Some alias -> alias
                | None -> label
              in
              let obj_arg_type =
                match fixed with
                | Some value -> fixed_arg_type value
                | None -> get_opt_arg_type ~nolabel:false ty
              in
              match obj_arg_type with
              | Ignore ->
                ( External_arg_spec.empty_kind obj_arg_type,
                  param_type :: arg_types,
                  result_types )
              | Nothing ->
                let for_sure_not_nested =
                  match ty.ptyp_desc with
                  | Ptyp_constr ({txt = Lident txt}, []) ->
                    Ast_core_type.is_builtin_rank0_type txt
                  | _ -> false
                in
                ( {
                    obj_arg_label =
                      External_arg_spec.optional for_sure_not_nested field_name;
                    obj_arg_type;
                  },
                  param_type :: arg_types,
                  Parsetree.Otag
                    ( {Asttypes.txt = field_name; loc},
                      [],
                      Ast_comb.to_undefined_type loc ty )
                  :: result_types )
              | Int _ ->
                ( {
                    obj_arg_label = External_arg_spec.optional true field_name;
                    obj_arg_type;
                  },
                  param_type :: arg_types,
                  Otag
                    ( {Asttypes.txt = field_name; loc},
                      [],
                      Ast_comb.to_undefined_type loc
                      @@ Ast_literal.type_int ~loc () )
                  :: result_types )
              | Poly_var_string _ ->
                ( {
                    obj_arg_label = External_arg_spec.optional true field_name;
                    obj_arg_type;
                  },
                  param_type :: arg_types,
                  Otag
                    ( {Asttypes.txt = field_name; loc},
                      [],
                      Ast_comb.to_undefined_type loc
                      @@ Ast_literal.type_string ~loc () )
                  :: result_types )
              | Arg_cst _ ->
                Location.raise_errorf ~loc
                  "%@as is not supported with optional yet"
              | Extern_unit -> assert false
              | Poly_var _ ->
                Location.raise_errorf ~loc
                  "%@obj label %s does not support such arg type" label
              | Unwrap ->
                Location.raise_errorf ~loc
                  "%@obj label %s does not support %@unwrap arguments" label)
          in
          (new_arg_label :: arg_labels, new_arg_types, output_tys))
    in

    let result =
      if result_type.ptyp_desc = Ptyp_any then
        Ast_core_type.make_obj ~loc result_types
      else result_type
      (* TODO: do we need do some error checking here *)
      (* result type can not be labeled *)
    in

    ( List.length args,
      (match args with
      | [] -> result
      | _ -> Ast_helper.Typ.arrow ~loc args result),
      External_ffi_types.ffi_obj_create arg_kinds )
  | _ -> Location.raise_errorf ~loc "Attribute found that conflicts with %@obj"

let external_decl_of_non_obj (loc : Location.t) (st : external_desc)
    (prim_name_or_pval_prim : bundle_source) (arg_type_specs_length : int)
    (arg_type_specs : External_arg_spec.params) :
    External_ffi_types.external_decl =
  let mk ?module_ ?(scopes = []) ?(variadic = false) kind :
      External_ffi_types.external_decl =
    {kind; module_; scopes; variadic; effective_arity = arg_type_specs_length}
  in
  match st with
  | {
   set_index = true;
   val_name = None;
   external_module_name = None;
   module_as_val = None;
   val_send = None;
   splice = false;
   scopes;
   get_index = false;
   new_name = None;
   call_name = None;
   set_name = None;
   get_name = None;
   return_wrapper = _;
   mk_obj = _;
  } ->
    if arg_type_specs_length = 3 then mk ~scopes Decl_set_index
    else
      Location.raise_errorf ~loc
        "Ill defined attribute %@set_index (arity of 3)"
  | {set_index = true} ->
    Bs_syntaxerr.err loc
      (Conflict_ffi_attribute "Attribute found that conflicts with @set_index")
  | {
   get_index = true;
   val_name = None;
   external_module_name = None;
   module_as_val = None;
   val_send = None;
   splice = false;
   scopes;
   new_name = None;
   call_name = None;
   set_name = None;
   get_name = None;
   set_index = false;
   mk_obj = _;
   return_wrapper = _;
  } ->
    if arg_type_specs_length = 2 then mk ~scopes Decl_get_index
    else
      Location.raise_errorf ~loc
        "Ill defined attribute %@get_index (arity expected 2 : while %d)"
        arg_type_specs_length
  | {get_index = true} ->
    Bs_syntaxerr.err loc
      (Conflict_ffi_attribute "Attribute found that conflicts with @get_index")
  | {
   module_as_val = Some _;
   get_index = false;
   val_name;
   new_name;
   external_module_name = None;
   val_send = None;
   scopes = [];
   (* module as var does not need scopes *)
   splice;
   call_name = None;
   set_name = None;
   get_name = None;
   set_index = false;
   return_wrapper = _;
   mk_obj = _;
  } -> (
    let module_ = External_ffi_types.Module_itself in
    match (new_name, val_name) with
    | None, _ ->
      mk ~module_ ~variadic:splice
        (Decl_val {name = prim_name_or_pval_prim.name})
    | Some _, Some _ ->
      Bs_syntaxerr.err loc
        (Conflict_ffi_attribute "Attribute found that conflicts with @module.")
    | Some {source = External; name = _}, None ->
      mk ~module_ ~variadic:splice
        (Decl_new {name = prim_name_or_pval_prim.name})
    | Some {source = Payload; name = _}, None ->
      Location.raise_errorf ~loc
        "Incorrect FFI attribute found: (%@new should not carry a payload here)"
    )
  | {module_as_val = Some _; get_index; val_send} ->
    let reason =
      match (get_index, val_send) with
      | true, _ ->
        "@module is for imports from a module, @get_index does not need import \
         a module "
      | _, Some _ ->
        "@module is for imports from a module, @send does not need import a \
         module "
      | _ -> "Attribute found that conflicts with @module."
    in
    Bs_syntaxerr.err loc (Conflict_ffi_attribute reason)
  | {
   get_name = None;
   val_name = None;
   call_name = None;
   module_as_val = None;
   set_index = false;
   get_index = false;
   val_send = None;
   new_name = None;
   set_name = None;
   external_module_name = None;
   splice;
   scopes;
   mk_obj = _;
   (* mk_obj is always false *)
   return_wrapper = _;
  } ->
    mk ~scopes ~variadic:splice (Decl_val {name = prim_name_or_pval_prim.name})
  | {
   call_name = Some {name; source = _};
   splice;
   scopes;
   external_module_name;
   val_name = None;
   module_as_val = None;
   val_send = None;
   set_index = false;
   get_index = false;
   new_name = None;
   set_name = None;
   get_name = None;
   mk_obj = _;
   return_wrapper = _;
  } ->
    mk
      ?module_:
        (Option.map
           (fun m -> External_ffi_types.Module_named m)
           external_module_name)
      ~scopes ~variadic:splice
      (Decl_val {name})
  | {call_name = Some _} ->
    Bs_syntaxerr.err loc
      (Conflict_ffi_attribute "Attribute found that conflicts with @val")
  | {
   val_name = Some {name; source = _};
   external_module_name;
   call_name = None;
   module_as_val = None;
   val_send = None;
   set_index = false;
   get_index = false;
   new_name = None;
   set_name = None;
   get_name = None;
   mk_obj = _;
   return_wrapper = _;
   splice = false;
   scopes;
  } ->
    mk
      ?module_:
        (Option.map
           (fun m -> External_ffi_types.Module_named m)
           external_module_name)
      ~scopes
      (Decl_val {name})
  | {val_name = Some _} ->
    Bs_syntaxerr.err loc
      (Conflict_ffi_attribute "Attribute found that conflicts with @val")
  | {
   splice;
   scopes;
   external_module_name = Some _ as external_module_name;
   val_name = None;
   call_name = None;
   module_as_val = None;
   val_send = None;
   set_index = false;
   get_index = false;
   new_name = None;
   set_name = None;
   get_name = None;
   mk_obj = _;
   return_wrapper = _;
  } ->
    mk
      ?module_:
        (Option.map
           (fun m -> External_ffi_types.Module_named m)
           external_module_name)
      ~scopes ~variadic:splice
      (Decl_val {name = prim_name_or_pval_prim.name})
  | {
   val_send = Some {name; source = _};
   splice;
   scopes;
   val_name = None;
   call_name = None;
   module_as_val = None;
   set_index = false;
   get_index = false;
   new_name = None;
   set_name = None;
   get_name = None;
   external_module_name = None;
   mk_obj = _;
   return_wrapper = _;
  } -> (
    (* PR #2162 - since when we assemble arguments the first argument in
       [@@send] is ignored
    *)
    match arg_type_specs with
    | [] ->
      Location.raise_errorf ~loc
        "Ill defined attribute %@send(the external needs to be a regular \
         function call with at least one argument)"
    | {arg_type = Arg_cst _; arg_label = _} :: _ ->
      Location.raise_errorf ~loc
        "Ill defined attribute %@send(first argument can't be const)"
    | _ :: _ -> mk ~scopes ~variadic:splice (Decl_send {name}))
  | {val_send = Some _} ->
    Location.raise_errorf ~loc
      "You used a FFI attribute that can't be used with %@send"
  | {
   new_name = Some {name; source = _};
   external_module_name;
   val_name = None;
   call_name = None;
   module_as_val = None;
   set_index = false;
   get_index = false;
   val_send = None;
   set_name = None;
   get_name = None;
   splice;
   scopes;
   mk_obj = _;
   return_wrapper = _;
  } ->
    mk
      ?module_:
        (Option.map
           (fun m -> External_ffi_types.Module_named m)
           external_module_name)
      ~scopes ~variadic:splice
      (Decl_new {name})
  | {new_name = Some _} ->
    Bs_syntaxerr.err loc
      (Conflict_ffi_attribute "Attribute found that conflicts with @new")
  | {
   set_name = Some {name; source = _};
   val_name = None;
   call_name = None;
   module_as_val = None;
   set_index = false;
   get_index = false;
   val_send = None;
   new_name = None;
   get_name = None;
   external_module_name = None;
   splice = false;
   mk_obj = _;
   return_wrapper = _;
   scopes;
  } ->
    if arg_type_specs_length = 2 then mk ~scopes (Decl_set {name})
    else
      Location.raise_errorf ~loc
        "Ill defined attribute %@set (two args required)"
  | {set_name = Some _} ->
    Location.raise_errorf ~loc "conflict attributes found with %@set"
  | {
   get_name = Some {name; source = _};
   val_name = None;
   call_name = None;
   module_as_val = None;
   set_index = false;
   get_index = false;
   val_send = None;
   new_name = None;
   set_name = None;
   external_module_name = None;
   splice = false;
   mk_obj = _;
   return_wrapper = _;
   scopes;
  } ->
    if arg_type_specs_length = 1 then
      (* Check if the first argument is unit, which is invalid for @get *)
      match arg_type_specs with
      | [{arg_type = Extern_unit}] ->
        Location.raise_errorf ~loc
          "Ill defined attribute %@get (unit argument is not allowed)"
      | _ -> mk ~scopes (Decl_get {name})
    else
      Location.raise_errorf ~loc
        "Ill defined attribute %@get (only one argument)"
  | {get_name = Some _} ->
    Location.raise_errorf ~loc "Attribute found that conflicts with %@get"

(** Note that the passed [type_annotation] is already processed by visitor pattern before*)
let handle_attributes (loc : Bs_loc.t) (type_annotation : Parsetree.core_type)
    (prim_attributes : Ast_attributes.t) (prim_name : string) :
    Parsetree.core_type * External_ffi_types.t * Parsetree.attributes * bool =
  let prim_name_with_source = {name = prim_name; source = External} in
  let result_type, arg_types_ty =
    (* Note this assumes external type is syntatic (no abstraction)*)
    Ast_core_type.list_of_arrow type_annotation
  in
  let no_arguments = arg_types_ty = [] in
  let unused_attrs, external_desc =
    parse_external_attributes no_arguments prim_name prim_name_with_source
      prim_attributes
  in
  if external_desc.mk_obj then
    (* warn unused attributes here ? *)
    let _arity, new_type, spec =
      process_obj loc external_desc prim_name arg_types_ty result_type
    in
    (new_type, spec, unused_attrs, false)
  else
    let splice = external_desc.splice in
    let arg_type_specs, args, arg_type_specs_length =
      Ext_list.fold_right arg_types_ty
        (([], [], 0) : External_arg_spec.params * Parsetree.arg list * int)
        (fun param_type (arg_type_specs, arg_types, i) ->
          let arg_label, ty, fixed =
            match param_type with
            | Parsetree.Parg_type {lbl; typ} -> (lbl, typ, None)
            | Parsetree.Parg_fixed {lbl; value} ->
              (lbl, Ast_helper.Typ.any ~loc:value.loc (), Some value)
          in
          (if i = 0 && splice then
             match (arg_label, fixed) with
             | _, Some value ->
               Location.raise_errorf ~loc:value.loc
                 "%@variadic expect the last type to be an array"
             | Optional _, None ->
               Location.raise_errorf ~loc
                 "%@variadic expect the last type to be a non optional"
             | (Labelled _ | Nolabel), None -> (
               if ty.ptyp_desc = Ptyp_any then
                 Location.raise_errorf ~loc
                   "%@variadic expect the last type to be an array";
               if spec_of_ptyp true ty <> Nothing then
                 Location.raise_errorf ~loc
                   "%@variadic expect the last type to be an array";
               match ty.ptyp_desc with
               | Ptyp_constr ({txt = Lident "array"}, [_]) -> ()
               | _ ->
                 Location.raise_errorf ~loc
                   "%@variadic expect the last type to be an array"));
          let ( (arg_label : External_arg_spec.label_noname),
                arg_type,
                new_arg_types ) =
            match arg_label with
            | Optional {txt = s} -> (
              let arg_type =
                match fixed with
                | Some value -> fixed_arg_type value
                | None -> get_opt_arg_type ~nolabel:false ty
              in
              match arg_type with
              | Poly_var _ ->
                (* ?x:([`x of int ] [@string]) does not make sense *)
                Location.raise_errorf ~loc
                  "%@string does not work with optional when it has arities in \
                   label %s"
                  s
              | _ -> (Arg_optional, arg_type, param_type :: arg_types))
            | Labelled _ -> (
              let arg_type =
                match fixed with
                | Some value -> fixed_arg_type value
                | None -> refine_arg_type ~nolabel:false ty
              in
              ( Arg_label,
                arg_type,
                match arg_type with
                | Arg_cst _ -> arg_types
                | _ -> param_type :: arg_types ))
            | Nolabel -> (
              let arg_type =
                match fixed with
                | Some value -> fixed_arg_type value
                | None -> refine_arg_type ~nolabel:true ty
              in
              ( Arg_empty,
                arg_type,
                match arg_type with
                | Arg_cst _ -> arg_types
                | _ -> param_type :: arg_types ))
          in
          ( {arg_label; arg_type} :: arg_type_specs,
            new_arg_types,
            if arg_type = Ignore then i else i + 1 ))
    in
    (* If every original argument was erased (e.g. all `@as(json ...) _`),
       keep the external binding callable by threading a final `unit`
       parameter through the type and arg specs. *)
    let args, arg_type_specs =
      match (args, arg_type_specs_length) with
      | [], n when n > 0 ->
        let unit_type =
          Ast_helper.Typ.constr ~loc
            (Location.mkloc (Longident.Lident "unit") loc)
            []
        in
        let unit_arg =
          Parsetree.Parg_type {attrs = []; lbl = Nolabel; typ = unit_type}
        in
        ( [unit_arg],
          arg_type_specs
          @ [{External_arg_spec.arg_label = Arg_empty; arg_type = Extern_unit}]
        )
      | _ -> (args, arg_type_specs)
    in
    let decl : External_ffi_types.external_decl =
      external_decl_of_non_obj loc external_desc prim_name_with_source
        arg_type_specs_length arg_type_specs
    in
    let relative = External_ffi_types.check_decl ~loc decl ~prim_name in
    (* result type can not be labeled *)
    (* currently we don't process attributes of
       return type, in the future we may *)
    let return_wrapper =
      check_return_wrapper loc external_desc.return_wrapper result_type
    in
    ( (match args with
      | [] -> result_type
      | _ -> Ast_helper.Typ.arrow ~loc args result_type),
      External_ffi_types.ffi_bs arg_type_specs return_wrapper decl,
      unused_attrs,
      relative )

let handle_attributes_as_prim (pval_loc : Location.t) (typ : Ast_core_type.t)
    (attrs : Ast_attributes.t) (prim_name : string) : response =
  let pval_type, ffi, pval_attributes, no_inline_cross_module =
    handle_attributes pval_loc typ attrs prim_name
  in
  {
    pval_type;
    pval_prim = Prim_ffi {name = prim_name; spec = ffi};
    pval_attributes;
    no_inline_cross_module;
  }

let pval_prim_of_option_labels (labels : (bool * string Asttypes.loc) list)
    (ends_with_unit : bool) =
  let arg_kinds =
    Ext_list.fold_right labels
      (if ends_with_unit then [External_arg_spec.empty_kind Extern_unit] else [])
      (fun (is_option, p) arg_kinds ->
        let label_name = p.txt in
        let obj_arg_label =
          if is_option then External_arg_spec.optional false label_name
          else External_arg_spec.obj_label label_name
        in
        {obj_arg_type = Nothing; obj_arg_label} :: arg_kinds)
  in
  Parsetree.Prim_ffi
    {name = ""; spec = External_ffi_types.ffi_obj_create arg_kinds}
