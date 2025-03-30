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
      | Int i ->
        (* This type is used in obj only to construct obj type*)
        Arg_cst (External_arg_spec.cst_int i)
      | Str (i, delim) -> Arg_cst (External_arg_spec.cst_string i delim))
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
    | Some (Int i) ->
      (* @as(24) *)
      (* This type is used in obj only to construct obj type *)
      Arg_cst (External_arg_spec.cst_int i)
    | Some (Str (s, delim)) ->
      (* @as("foo") *)
      Arg_cst (External_arg_spec.cst_string s delim))
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
  tagged_template: bool;
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
    tagged_template = false;
  }

let return_wrapper loc (txt : string) : External_ffi_types.return_wrapper =
  match txt with
  | "undefined_to_opt" -> Return_undefined_to_opt
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
      match Ast_payload.is_single_string payload with
      | Some (val_name, _) -> {name = val_name; source = Payload}
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
              fields
              |> List.iter
                   (fun
                     ((l, exp, _) :
                       Longident.t Location.loc * Parsetree.expression * bool)
                   ->
                     match (l, exp.pexp_desc) with
                     | ( {txt = Lident "from"},
                         Pexp_constant (Pconst_string (s, _)) ) ->
                       from_name := Some s
                     | {txt = Lident "with"}, Pexp_record (fields, _) ->
                       with_ := Some fields
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
                  with_fields
                  |> List.filter_map
                       (fun
                         ((l, exp, _) :
                           Longident.t Location.loc
                           * Parsetree.expression
                           * bool)
                       ->
                         match exp.pexp_desc with
                         | Pexp_constant (Pconst_string (s, _)) -> (
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
                let import_attributes =
                  Hashtbl.create (List.length import_attributes_from_record)
                in
                import_attributes_from_record
                |> List.iter (fun (key, value) ->
                       Hashtbl.replace import_attributes key value);
                {
                  st with
                  external_module_name =
                    Some
                      {
                        bundle = from_name;
                        module_bind_name = Phint_nothing;
                        import_attributes = Some import_attributes;
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
          | "taggedTemplate" -> {st with splice = true; tagged_template = true}
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
  | Return_identity -> wrapper
  | Return_unset ->
    if Ast_core_type.is_unit result_type then Return_replaced_with_unit
    else wrapper
  | Return_undefined_to_opt | Return_null_to_opt | Return_null_undefined_to_opt
    ->
    if Ast_core_type.is_user_option result_type then wrapper
    else Bs_syntaxerr.err loc Expect_opt_in_bs_return_to_opt
  | Return_replaced_with_unit -> assert false
(* Not going to happen from user input*)

type response = {
  pval_type: Parsetree.core_type;
  pval_prim: string list;
  pval_attributes: Parsetree.attributes;
  no_inline_cross_module: bool;
}

let process_obj (loc : Location.t) (st : external_desc) (prim_name : string)
    (arg_types_ty : Ast_core_type.param_type list)
    (result_type : Ast_core_type.t) :
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
   tagged_template = _;
   set_index = false;
   mk_obj = _;
   scopes =
     [] (* wrapper does not work with @obj
        TODO: better error message *);
  } ->
    if String.length prim_name <> 0 then
      Location.raise_errorf ~loc
        "%@obj expect external names to be empty string";
    let ( arg_kinds,
          new_arg_types_ty,
          (result_types : Parsetree.object_field list) ) =
      Ext_list.fold_right arg_types_ty ([], [], [])
        (fun
          param_type
          (arg_labels, (arg_types : Ast_core_type.param_type list), result_types)
        ->
          let arg_label = param_type.label in
          let loc = param_type.loc in
          let ty = param_type.ty in
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
                match
                  Ast_attributes.iter_process_bs_string_as param_type.attr
                with
                | Some alias -> alias
                | None -> label
              in
              let obj_arg_type = refine_obj_arg_type ~nolabel:false ty in
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
                match
                  Ast_attributes.iter_process_bs_string_as param_type.attr
                with
                | Some alias -> alias
                | None -> label
              in
              let obj_arg_type = get_opt_arg_type ~nolabel:false ty in
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

    ( List.length new_arg_types_ty,
      Ast_core_type.mk_fn_type new_arg_types_ty result,
      External_ffi_types.ffi_obj_create arg_kinds )
  | _ -> Location.raise_errorf ~loc "Attribute found that conflicts with %@obj"

let external_desc_of_non_obj (loc : Location.t) (st : external_desc)
    (prim_name_or_pval_prim : bundle_source) (arg_type_specs_length : int)
    arg_types_ty (arg_type_specs : External_arg_spec.params) :
    External_ffi_types.external_spec =
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
   tagged_template = _;
  } ->
    if arg_type_specs_length = 3 then
      Js_set_index {js_set_index_scopes = scopes}
    else
      Location.raise_errorf ~loc
        "Ill defined attribute %@set_index (arity of 3)"
  | {set_index = true} ->
    Bs_syntaxerr.err loc
      (Conflict_ffi_attribute "Attribute found that conflicts with %@set_index")
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
   tagged_template = _;
  } ->
    if arg_type_specs_length = 2 then
      Js_get_index {js_get_index_scopes = scopes}
    else
      Location.raise_errorf ~loc
        "Ill defined attribute %@get_index (arity expected 2 : while %d)"
        arg_type_specs_length
  | {get_index = true} ->
    Bs_syntaxerr.err loc
      (Conflict_ffi_attribute "Attribute found that conflicts with %@get_index")
  | {
   module_as_val = Some external_module_name;
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
   tagged_template = _;
  } -> (
    match (arg_types_ty, new_name, val_name) with
    | [], None, _ -> Js_module_as_var external_module_name
    | _, None, _ -> Js_module_as_fn {splice; external_module_name}
    | _, Some _, Some _ ->
      Bs_syntaxerr.err loc
        (Conflict_ffi_attribute "Attribute found that conflicts with @module.")
    | _, Some {source = External; name = _}, None ->
      Js_module_as_class external_module_name
    | _, Some {source = Payload; name = _}, None ->
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
   tagged_template;
  } ->
    let name = prim_name_or_pval_prim.name in
    if arg_type_specs_length = 0 then
      (*
         {[
           external ff : int -> int [@bs] = "" [@@module "xx"]
         ]}
         FIXME: splice is not supported here 
      *)
      Js_var {name; external_module_name = None; scopes}
    else
      Js_call
        {splice; name; external_module_name = None; scopes; tagged_template}
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
   tagged_template;
  } ->
    if arg_type_specs_length = 0 then
      (*
           {[
             external ff : int -> int = "" [@@module "xx"]
           ]}
        *)
      Js_var {name; external_module_name; scopes}
      (*FIXME: splice is not supported here *)
    else Js_call {splice; name; external_module_name; scopes; tagged_template}
  | {call_name = Some _} ->
    Bs_syntaxerr.err loc
      (Conflict_ffi_attribute "Attribute found that conflicts with %@val")
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
   tagged_template = _;
  } ->
    (*
         if no_arguments -->
               {[
                 external ff : int = "" [@@val]
               ]}
      *)
    Js_var {name; external_module_name; scopes}
  | {val_name = Some _} ->
    Bs_syntaxerr.err loc
      (Conflict_ffi_attribute "Attribute found that conflicts with %@val")
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
   tagged_template;
  } ->
    let name = prim_name_or_pval_prim.name in
    if arg_type_specs_length = 0 then
      (*
         {[
           external ff : int = "" [@@module "xx"]
         ]}
      *)
      Js_var {name; external_module_name; scopes}
    else Js_call {splice; name; external_module_name; scopes; tagged_template}
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
   tagged_template = _;
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
    | _ :: _ -> Js_send {splice; name; js_send_scopes = scopes})
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
   tagged_template = _;
  } ->
    Js_new {name; external_module_name; splice; scopes}
  | {new_name = Some _} ->
    Bs_syntaxerr.err loc
      (Conflict_ffi_attribute "Attribute found that conflicts with %@new")
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
   tagged_template = _;
  } ->
    if arg_type_specs_length = 2 then
      Js_set {js_set_scopes = scopes; js_set_name = name}
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
   tagged_template = _;
  } ->
    if arg_type_specs_length = 1 then
      Js_get {js_get_name = name; js_get_scopes = scopes}
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
  let type_annotation, build_uncurried_type =
    match type_annotation with
    | {ptyp_desc = Ptyp_arrow {arity = Some _}} ->
      ( type_annotation,
        fun ~arity (x : Parsetree.core_type) ->
          Ast_uncurried.uncurried_type ~arity x )
    | _ -> (type_annotation, fun ~arity:_ x -> x)
  in
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
    let arity, new_type, spec =
      process_obj loc external_desc prim_name arg_types_ty result_type
    in
    (build_uncurried_type ~arity new_type, spec, unused_attrs, false)
  else
    let splice = external_desc.splice in
    let arg_type_specs, new_arg_types_ty, arg_type_specs_length =
      Ext_list.fold_right arg_types_ty
        (([], [], 0)
          : External_arg_spec.params * Ast_core_type.param_type list * int)
        (fun param_type (arg_type_specs, arg_types, i) ->
          let arg_label = param_type.label in
          let ty = param_type.ty in
          (if i = 0 && splice then
             match arg_label with
             | Optional _ ->
               Location.raise_errorf ~loc
                 "%@variadic expect the last type to be a non optional"
             | Labelled _ | Nolabel -> (
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
              let arg_type = get_opt_arg_type ~nolabel:false ty in
              match arg_type with
              | Poly_var _ ->
                (* ?x:([`x of int ] [@string]) does not make sense *)
                Location.raise_errorf ~loc
                  "%@string does not work with optional when it has arities in \
                   label %s"
                  s
              | _ -> (Arg_optional, arg_type, param_type :: arg_types))
            | Labelled _ -> (
              let arg_type = refine_arg_type ~nolabel:false ty in
              ( Arg_label,
                arg_type,
                match arg_type with
                | Arg_cst _ -> arg_types
                | _ -> param_type :: arg_types ))
            | Nolabel -> (
              let arg_type = refine_arg_type ~nolabel:true ty in
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
    let ffi : External_ffi_types.external_spec =
      external_desc_of_non_obj loc external_desc prim_name_with_source
        arg_type_specs_length arg_types_ty arg_type_specs
    in
    let relative = External_ffi_types.check_ffi ~loc ffi in
    (* result type can not be labeled *)
    (* currently we don't process attributes of
       return type, in the future we may *)
    let return_wrapper =
      check_return_wrapper loc external_desc.return_wrapper result_type
    in
    let fn_type = Ast_core_type.mk_fn_type new_arg_types_ty result_type in
    ( build_uncurried_type ~arity:(List.length new_arg_types_ty) fn_type,
      External_ffi_types.ffi_bs arg_type_specs return_wrapper ffi,
      unused_attrs,
      relative )

let encode_attributes_as_string (pval_loc : Location.t) (typ : Ast_core_type.t)
    (attrs : Ast_attributes.t) (prim_name : string) : response =
  let pval_type, ffi, pval_attributes, no_inline_cross_module =
    handle_attributes pval_loc typ attrs prim_name
  in
  {
    pval_type;
    pval_prim = [prim_name; External_ffi_types.to_string ffi];
    pval_attributes;
    no_inline_cross_module;
  }

let pval_prim_of_labels (labels : string Asttypes.loc list) =
  let arg_kinds =
    Ext_list.fold_right labels
      ([] : External_arg_spec.obj_params)
      (fun p arg_kinds ->
        let obj_arg_label = External_arg_spec.obj_label p.txt in
        {obj_arg_type = Nothing; obj_arg_label} :: arg_kinds)
  in
  External_ffi_types.ffi_obj_as_prims arg_kinds

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
  External_ffi_types.ffi_obj_as_prims arg_kinds
