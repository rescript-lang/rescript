open Shared_types

let is_module_type (declared : Module.t Declared.t) =
  match declared.module_path with
  | ExportedModule {is_type} -> is_type
  | _ -> false

let add_declared ~(name : string Location.loc) ~extent ~stamp ~(env : Env.t)
    ~item attributes add_exported add_stamp =
  let is_exported = add_exported name.txt stamp in
  let declared =
    Process_attributes.new_declared ~item ~extent ~name ~stamp
      ~module_path:env.module_path is_exported attributes
  in
  add_stamp env.stamps stamp declared;
  declared

let attrs_to_docstring attrs =
  match Process_attributes.find_doc_attribute attrs with
  | None -> []
  | Some docstring -> [docstring]

let map_record_field {Types.ld_id; ld_type; ld_attributes; ld_optional} =
  let astamp = Ident.binding_time ld_id in
  let name = Ident.name ld_id in
  {
    stamp = astamp;
    fname = Location.mknoloc name;
    typ = ld_type;
    optional = ld_optional;
    docstring =
      (match Process_attributes.find_doc_attribute ld_attributes with
      | None -> []
      | Some docstring -> [docstring]);
    deprecated = Process_attributes.find_deprecated_attribute ld_attributes;
  }

let rec for_type_signature_item ~(env : Shared_types.Env.t)
    ~(exported : Exported.t) (item : Types.signature_item) =
  match item with
  | Sig_value (ident, {val_type; val_attributes; val_loc = loc}) ->
    let item = val_type in
    let stamp = Ident.binding_time ident in
    let old_declared = Stamps.find_value env.stamps stamp in
    let declared =
      add_declared
        ~name:(Location.mkloc (Ident.name ident) loc)
        ~extent:loc ~stamp ~env ~item val_attributes
        (Exported.add exported Exported.Value)
        Stamps.add_value
    in
    let declared =
      (* When an id is shadowed, a module constraint without the doc comment is created.
         Here the existing doc comment is restored. See https://github.com/rescript-lang/rescript-vscode/issues/621 *)
      match old_declared with
      | Some old_declared when declared.docstring = [] ->
        let new_declared = {declared with docstring = old_declared.docstring} in
        Stamps.add_value env.stamps stamp new_declared;
        new_declared
      | _ -> declared
    in
    [
      {
        Module.kind = Module.Value declared.item;
        name = declared.name.txt;
        docstring = declared.docstring;
        deprecated = declared.deprecated;
        loc = declared.extent_loc;
      };
    ]
  | Sig_type
      ( ident,
        ({type_loc; type_kind; type_manifest; type_attributes} as decl),
        rec_status ) ->
    let declared =
      let name = Location.mknoloc (Ident.name ident) in
      add_declared ~extent:type_loc
        ~item:
          {
            Type.decl;
            attributes = type_attributes;
            name = name.txt;
            kind =
              (match type_kind with
              | Type_abstract -> (
                match type_manifest with
                | Some {desc = Tconstr (path, args, _)} ->
                  Abstract (Some (path, args))
                | Some {desc = Ttuple items} -> Tuple items
                (* TODO dig *)
                | _ -> Abstract None)
              | Type_open -> Open
              | Type_variant constructors ->
                Variant
                  (constructors
                  |> List.map
                       (fun
                         {Types.cd_loc; cd_id; cd_args; cd_res; cd_attributes}
                       ->
                         let name = Ident.name cd_id in
                         let stamp = Ident.binding_time cd_id in
                         let item =
                           {
                             Constructor.stamp;
                             cname = Location.mknoloc name;
                             args =
                               (match cd_args with
                               | Cstr_tuple args ->
                                 Args
                                   (args
                                   |> List.map (fun t -> (t, Location.none)))
                               | Cstr_record fields ->
                                 InlineRecord
                                   (fields |> List.map map_record_field));
                             res = cd_res;
                             type_decl = (name, decl);
                             docstring = attrs_to_docstring cd_attributes;
                             deprecated =
                               Process_attributes.find_deprecated_attribute
                                 cd_attributes;
                           }
                         in
                         let declared =
                           Process_attributes.new_declared ~item ~extent:cd_loc
                             ~name:(Location.mknoloc name)
                             ~stamp (* TODO maybe this needs another child *)
                             ~module_path:env.module_path true cd_attributes
                         in
                         Stamps.add_constructor env.stamps stamp declared;
                         item))
              | Type_record (fields, _) ->
                Record (fields |> List.map map_record_field));
          }
        ~name ~stamp:(Ident.binding_time ident) ~env type_attributes
        (Exported.add exported Exported.Type)
        Stamps.add_type
    in
    [
      {
        Module.kind = Type (declared.item, rec_status);
        name = declared.name.txt;
        docstring = declared.docstring;
        deprecated = declared.deprecated;
        loc = declared.extent_loc;
      };
    ]
  | Sig_module (ident, {md_type; md_attributes; md_loc}, _) ->
    let name = Ident.name ident in
    let declared =
      add_declared ~extent:md_loc
        ~item:(for_type_module ~name ~env md_type)
        ~name:(Location.mkloc name md_loc)
        ~stamp:(Ident.binding_time ident) ~env md_attributes
        (Exported.add exported Exported.Module)
        Stamps.add_module
    in
    [
      {
        Module.kind =
          Module
            {type_ = declared.item; is_module_type = is_module_type declared};
        name = declared.name.txt;
        docstring = declared.docstring;
        deprecated = declared.deprecated;
        loc = declared.extent_loc;
      };
    ]
  | _ -> []

and for_type_signature ~name ~env signature =
  let exported = Exported.init () in
  let items =
    List.fold_right
      (fun item items -> for_type_signature_item ~env ~exported item @ items)
      signature []
  in
  {Module.name; docstring = []; exported; items; deprecated = None}

and for_type_module ~name ~env module_type =
  match module_type with
  | Types.Mty_ident path -> Ident path
  | Mty_alias (_ (* 402 *), path) -> Ident path
  | Mty_signature signature ->
    Structure (for_type_signature ~name ~env signature)
  | Mty_functor (_argIdent, _argType, result_type) ->
    for_type_module ~name ~env result_type

let get_module_type_path mod_desc =
  match mod_desc with
  | Typedtree.Tmty_ident (path, _) | Tmty_alias (path, _) -> Some path
  | Tmty_signature _ | Tmty_functor _ | Tmty_with _ | Tmty_typeof _ -> None

let for_type_declaration ~env ~(exported : Exported.t)
    {
      Typedtree.typ_id;
      typ_loc;
      typ_name = name;
      typ_attributes;
      typ_type;
      typ_kind;
      typ_manifest;
    } ~rec_status =
  let stamp = Ident.binding_time typ_id in
  let declared =
    add_declared ~extent:typ_loc
      ~item:
        {
          Type.decl = typ_type;
          attributes = typ_attributes;
          name = name.txt;
          kind =
            (match typ_kind with
            | Ttype_abstract -> (
              match typ_manifest with
              | Some {ctyp_desc = Ttyp_constr (path, _lident, args)} ->
                Abstract
                  (Some (path, args |> List.map (fun t -> t.Typedtree.ctyp_type)))
              | Some {ctyp_desc = Ttyp_tuple items} ->
                Tuple (items |> List.map (fun t -> t.Typedtree.ctyp_type))
              (* TODO dig *)
              | _ -> Abstract None)
            | Ttype_open -> Open
            | Ttype_variant constructors ->
              Variant
                (constructors
                |> List.map
                     (fun
                       {
                         Typedtree.cd_id;
                         cd_name = cname;
                         cd_args;
                         cd_res;
                         cd_attributes;
                         cd_loc;
                       }
                     ->
                       let stamp = Ident.binding_time cd_id in
                       let item =
                         {
                           Constructor.stamp;
                           cname;
                           deprecated =
                             Process_attributes.find_deprecated_attribute
                               cd_attributes;
                           args =
                             (match cd_args with
                             | Cstr_tuple args ->
                               Args
                                 (args
                                 |> List.map (fun t ->
                                        (t.Typedtree.ctyp_type, t.ctyp_loc)))
                             | Cstr_record fields ->
                               InlineRecord
                                 (fields
                                 |> List.map
                                      (fun (f : Typedtree.label_declaration) ->
                                        let astamp =
                                          Ident.binding_time f.ld_id
                                        in
                                        let name = Ident.name f.ld_id in
                                        {
                                          stamp = astamp;
                                          fname = Location.mknoloc name;
                                          typ = f.ld_type.ctyp_type;
                                          optional = f.ld_optional;
                                          docstring =
                                            (match
                                               Process_attributes
                                               .find_doc_attribute
                                                 f.ld_attributes
                                             with
                                            | None -> []
                                            | Some docstring -> [docstring]);
                                          deprecated =
                                            Process_attributes
                                            .find_deprecated_attribute
                                              f.ld_attributes;
                                        })));
                           res =
                             (match cd_res with
                             | None -> None
                             | Some t -> Some t.ctyp_type);
                           type_decl = (name.txt, typ_type);
                           docstring = attrs_to_docstring cd_attributes;
                         }
                       in
                       let declared =
                         Process_attributes.new_declared ~item ~extent:cd_loc
                           ~name:cname ~stamp ~module_path:env.module_path true
                           cd_attributes
                       in
                       Stamps.add_constructor env.stamps stamp declared;
                       item))
            | Ttype_record fields ->
              Record
                (fields
                |> List.map
                     (fun
                       {
                         Typedtree.ld_id;
                         ld_name = fname;
                         ld_type = {ctyp_type};
                         ld_attributes;
                         ld_optional;
                       }
                     ->
                       let fstamp = Ident.binding_time ld_id in
                       {
                         stamp = fstamp;
                         fname;
                         typ = ctyp_type;
                         optional = ld_optional;
                         docstring = attrs_to_docstring ld_attributes;
                         deprecated =
                           Process_attributes.find_deprecated_attribute
                             ld_attributes;
                       })));
        }
      ~name ~stamp ~env typ_attributes
      (Exported.add exported Exported.Type)
      Stamps.add_type
  in
  {
    Module.kind = Module.Type (declared.item, rec_status);
    name = declared.name.txt;
    docstring = declared.docstring;
    deprecated = declared.deprecated;
    loc = declared.extent_loc;
  }

let rec for_signature_item ~env ~(exported : Exported.t)
    (item : Typedtree.signature_item) =
  match item.sig_desc with
  | Tsig_value {val_id; val_loc; val_name = name; val_desc; val_attributes} ->
    let declared =
      add_declared ~name
        ~stamp:(Ident.binding_time val_id)
        ~extent:val_loc ~item:val_desc.ctyp_type ~env val_attributes
        (Exported.add exported Exported.Value)
        Stamps.add_value
    in
    [
      {
        Module.kind = Module.Value declared.item;
        name = declared.name.txt;
        docstring = declared.docstring;
        deprecated = declared.deprecated;
        loc = declared.extent_loc;
      };
    ]
  | Tsig_type (rec_flag, decls) ->
    decls
    |> List.mapi (fun i decl ->
           let rec_status =
             match rec_flag with
             | Recursive when i = 0 -> Types.Trec_first
             | Nonrecursive when i = 0 -> Types.Trec_not
             | _ -> Types.Trec_next
           in
           decl |> for_type_declaration ~env ~exported ~rec_status)
  | Tsig_module
      {md_id; md_attributes; md_loc; md_name = name; md_type = {mty_type}} ->
    let item =
      for_type_module ~name:name.txt
        ~env:(env |> Env.add_module ~name:name.txt)
        mty_type
    in
    let declared =
      add_declared ~item ~name ~extent:md_loc ~stamp:(Ident.binding_time md_id)
        ~env md_attributes
        (Exported.add exported Exported.Module)
        Stamps.add_module
    in
    [
      {
        Module.kind =
          Module
            {type_ = declared.item; is_module_type = is_module_type declared};
        name = declared.name.txt;
        docstring = declared.docstring;
        deprecated = declared.deprecated;
        loc = declared.extent_loc;
      };
    ]
  | Tsig_recmodule mod_decls ->
    mod_decls
    |> List.map (fun mod_decl ->
           for_signature_item ~env ~exported
             {item with sig_desc = Tsig_module mod_decl})
    |> List.flatten
  | Tsig_include {incl_mod; incl_type} ->
    let env =
      match get_module_type_path incl_mod.mty_desc with
      | None -> env
      | Some path ->
        {env with module_path = IncludedModule (path, env.module_path)}
    in
    let top_level =
      List.fold_right
        (fun item items -> for_type_signature_item ~env ~exported item @ items)
        incl_type []
    in
    top_level
  (* TODO: process other things here *)
  | _ -> []

let for_signature ~name ~env sig_items =
  let exported = Exported.init () in
  let items =
    sig_items |> List.map (for_signature_item ~env ~exported) |> List.flatten
  in
  let attributes =
    match sig_items with
    | {sig_desc = Tsig_attribute attribute} :: _ -> [attribute]
    | _ -> []
  in
  let docstring = attrs_to_docstring attributes in
  let deprecated = Process_attributes.find_deprecated_attribute attributes in
  {Module.name; docstring; exported; items; deprecated}

let for_tree_module_type ~name ~env {Typedtree.mty_desc} =
  match mty_desc with
  | Tmty_ident _ -> None
  | Tmty_signature {sig_items} ->
    let contents = for_signature ~name ~env sig_items in
    Some (Module.Structure contents)
  | _ -> None

let rec get_module_path mod_desc =
  match mod_desc with
  | Typedtree.Tmod_ident (path, _lident) -> Some path
  | Tmod_structure _ -> None
  | Tmod_functor (_ident, _argName, _maybeType, _resultExpr) -> None
  | Tmod_apply (functor_, _arg, _coercion) -> get_module_path functor_.mod_desc
  | Tmod_unpack (_expr, _moduleType) -> None
  | Tmod_constraint (expr, _typ, _constraint, _coercion) ->
    get_module_path expr.mod_desc

let rec for_structure_item ~(env : Shared_types.Env.t) ~(exported : Exported.t)
    item =
  match item.Typedtree.str_desc with
  | Tstr_value (_isRec, bindings) ->
    let items = ref [] in
    let rec handle_pattern attributes pat =
      match pat.Typedtree.pat_desc with
      | Tpat_var (ident, name)
      | Tpat_alias (_, ident, name) (* let x : t = ... *) ->
        (* Detect first-class module unpack patterns and register them as modules. *)
        let unpack_loc_opt =
          match
            pat.pat_extra
            |> Utils.filter_map (function
                 | Typedtree.Tpat_unpack, loc, _ -> Some loc
                 | _ -> None)
          with
          | loc :: _ -> Some loc
          | [] -> None
        in
        if unpack_loc_opt <> None then
          match (Shared.dig pat.pat_type).desc with
          | Tpackage (path, _, _) ->
            let declared =
              Process_attributes.new_declared ~item:(Module.Ident path)
                ~extent:(Option.get unpack_loc_opt)
                ~name ~stamp:(Ident.binding_time ident) ~module_path:NotVisible
                false attributes
            in
            Stamps.add_module env.stamps (Ident.binding_time ident) declared;
            items :=
              {
                Module.kind =
                  Module
                    {
                      type_ = declared.item;
                      is_module_type = is_module_type declared;
                    };
                name = declared.name.txt;
                docstring = declared.docstring;
                deprecated = declared.deprecated;
                loc = declared.extent_loc;
              }
              :: !items
          | _ ->
            let item = pat.pat_type in
            let declared =
              add_declared ~name ~stamp:(Ident.binding_time ident) ~env
                ~extent:pat.pat_loc ~item attributes
                (Exported.add exported Exported.Value)
                Stamps.add_value
            in
            items :=
              {
                Module.kind = Module.Value declared.item;
                name = declared.name.txt;
                docstring = declared.docstring;
                deprecated = declared.deprecated;
                loc = declared.extent_loc;
              }
              :: !items
        else
          let item = pat.pat_type in
          let declared =
            add_declared ~name ~stamp:(Ident.binding_time ident) ~env
              ~extent:pat.pat_loc ~item attributes
              (Exported.add exported Exported.Value)
              Stamps.add_value
          in
          items :=
            {
              Module.kind = Module.Value declared.item;
              name = declared.name.txt;
              docstring = declared.docstring;
              deprecated = declared.deprecated;
              loc = declared.extent_loc;
            }
            :: !items
      | Tpat_tuple pats | Tpat_array pats | Tpat_construct (_, _, pats) ->
        pats |> List.iter (fun p -> handle_pattern [] p)
      | Tpat_or (p, _, _) -> handle_pattern [] p
      | Tpat_record (record_items, _, rest) -> (
        record_items |> List.iter (fun (_, _, p, _) -> handle_pattern [] p);
        match rest with
        | None -> ()
        | Some rest ->
          let declared =
            add_declared ~name:rest.rest_name
              ~stamp:(Ident.binding_time rest.rest_ident)
              ~env ~extent:rest.rest_name.loc ~item:rest.rest_type []
              (Exported.add exported Exported.Value)
              Stamps.add_value
          in
          items :=
            {
              Module.kind = Module.Value declared.item;
              name = declared.name.txt;
              docstring = declared.docstring;
              deprecated = declared.deprecated;
              loc = declared.extent_loc;
            }
            :: !items)
      | Tpat_variant (_, Some p, _) -> handle_pattern [] p
      | Tpat_variant (_, None, _) | Tpat_any | Tpat_constant _ -> ()
    in
    List.iter
      (fun {Typedtree.vb_pat; vb_attributes} ->
        handle_pattern vb_attributes vb_pat)
      bindings;
    bindings
    |> List.iter (fun {Typedtree.vb_expr} -> scan_let_modules ~env vb_expr);
    !items
  | Tstr_module
      {mb_id; mb_attributes; mb_loc; mb_name = name; mb_expr = {mod_desc}}
    when not
           (String.length name.txt >= 6
           && (String.sub name.txt 0 6 = "local_") [@doesNotRaise])
         (* %%private generates a dummy module called local_... *) ->
    let item = for_module ~env mod_desc name.txt in
    let declared =
      add_declared ~item ~name ~extent:mb_loc ~stamp:(Ident.binding_time mb_id)
        ~env mb_attributes
        (Exported.add exported Exported.Module)
        Stamps.add_module
    in
    [
      {
        Module.kind =
          Module
            {type_ = declared.item; is_module_type = is_module_type declared};
        name = declared.name.txt;
        docstring = declared.docstring;
        deprecated = declared.deprecated;
        loc = declared.extent_loc;
      };
    ]
  | Tstr_recmodule mod_decls ->
    mod_decls
    |> List.map (fun mod_decl ->
           for_structure_item ~env ~exported
             {item with str_desc = Tstr_module mod_decl})
    |> List.flatten
  | Tstr_modtype
      {
        mtd_name = name;
        mtd_id;
        mtd_attributes;
        mtd_type = Some {mty_type = mod_type};
        mtd_loc;
      } ->
    let env = env |> Env.add_module_type ~name:name.txt in
    let mod_type_item = for_type_module ~name:name.txt ~env mod_type in
    let declared =
      add_declared ~item:mod_type_item ~name ~extent:mtd_loc
        ~stamp:(Ident.binding_time mtd_id)
        ~env mtd_attributes
        (Exported.add exported Exported.Module)
        Stamps.add_module
    in
    [
      {
        Module.kind =
          Module
            {type_ = declared.item; is_module_type = is_module_type declared};
        name = declared.name.txt;
        docstring = declared.docstring;
        deprecated = declared.deprecated;
        loc = declared.extent_loc;
      };
    ]
  | Tstr_include {incl_mod; incl_type} ->
    let env =
      match get_module_path incl_mod.mod_desc with
      | None -> env
      | Some path ->
        {env with module_path = IncludedModule (path, env.module_path)}
    in
    let top_level =
      List.fold_right
        (fun item items -> for_type_signature_item ~env ~exported item @ items)
        incl_type []
    in
    top_level
  | Tstr_primitive vd when Jsx_hacks.primitive_is_fragment vd = false ->
    let declared =
      add_declared ~extent:vd.val_loc ~item:vd.val_val.val_type
        ~name:vd.val_name
        ~stamp:(Ident.binding_time vd.val_id)
        ~env vd.val_attributes
        (Exported.add exported Exported.Value)
        Stamps.add_value
    in
    [
      {
        Module.kind = Value declared.item;
        name = declared.name.txt;
        docstring = declared.docstring;
        deprecated = declared.deprecated;
        loc = declared.extent_loc;
      };
    ]
  | Tstr_type (rec_flag, decls) ->
    decls
    |> List.mapi (fun i decl ->
           let rec_status =
             match rec_flag with
             | Recursive when i = 0 -> Types.Trec_first
             | Nonrecursive when i = 0 -> Types.Trec_not
             | _ -> Types.Trec_next
           in
           decl |> for_type_declaration ~env ~exported ~rec_status)
  | _ -> []

and for_module ~env mod_desc module_name =
  match mod_desc with
  | Tmod_ident (path, _lident) -> Ident path
  | Tmod_structure structure ->
    let env = env |> Env.add_module ~name:module_name in
    let contents = for_structure ~name:module_name ~env structure.str_items in
    Structure contents
  | Tmod_functor (ident, arg_name, maybe_type, result_expr) ->
    (match maybe_type with
    | None -> ()
    | Some t ->
      let kind = for_type_module ~name:arg_name.txt ~env t.mty_type in
      let stamp = Ident.binding_time ident in
      let declared =
        Process_attributes.new_declared ~item:kind ~name:arg_name
          ~extent:arg_name.loc ~stamp ~module_path:NotVisible false []
      in
      Stamps.add_module env.stamps stamp declared);
    for_module ~env result_expr.mod_desc module_name
  | Tmod_apply (functor_, _arg, _coercion) ->
    for_module ~env functor_.mod_desc module_name
  | Tmod_unpack (_expr, module_type) ->
    let env = env |> Env.add_module ~name:module_name in
    for_type_module ~name:module_name ~env module_type
  | Tmod_constraint (expr, typ, _constraint, _coercion) ->
    (* TODO do this better I think *)
    let mod_kind = for_module ~env expr.mod_desc module_name in
    let env = env |> Env.add_module ~name:module_name in
    let mod_type_kind = for_type_module ~name:module_name ~env typ in
    Constraint (mod_kind, mod_type_kind)

(*
  Walk a typed expression and register any `let module M = ...` bindings as local
  modules in stamps. This makes trailing-dot completion work for aliases like `M.`
  that are introduced inside expression scopes. The declared module is marked as
  NotVisible (non-exported) and the extent is the alias identifier location so
  scope lookups match precisely.
*)
and scan_let_modules ~env (e : Typedtree.expression) =
  match e.exp_desc with
  | Texp_letmodule (id, name, mexpr, body) ->
    let stamp = Ident.binding_time id in
    let item = for_module ~env mexpr.mod_desc name.txt in
    let declared =
      Process_attributes.new_declared ~item ~extent:name.loc ~name ~stamp
        ~module_path:NotVisible false []
    in
    Stamps.add_module env.stamps stamp declared;
    scan_let_modules ~env body
  | Texp_let (_rf, bindings, body) ->
    List.iter
      (fun {Typedtree.vb_expr} -> scan_let_modules ~env vb_expr)
      bindings;
    scan_let_modules ~env body
  | Texp_apply {funct; args; _} ->
    scan_let_modules ~env funct;
    args
    |> List.iter (function
         | _, Some e -> scan_let_modules ~env e
         | _, None -> ())
  | Texp_tuple exprs -> List.iter (scan_let_modules ~env) exprs
  | Texp_sequence (e1, e2) ->
    scan_let_modules ~env e1;
    scan_let_modules ~env e2
  | Texp_match (e, cases, exn_cases, _) ->
    scan_let_modules ~env e;
    let scan_case {Typedtree.c_lhs = _; c_guard; c_rhs} =
      (match c_guard with
      | Some g -> scan_let_modules ~env g
      | None -> ());
      scan_let_modules ~env c_rhs
    in
    List.iter scan_case cases;
    List.iter scan_case exn_cases
  | Texp_function {case; _} ->
    let {Typedtree.c_lhs = _; c_guard; c_rhs} = case in
    (match c_guard with
    | Some g -> scan_let_modules ~env g
    | None -> ());
    scan_let_modules ~env c_rhs
  | Texp_try (e, cases) ->
    scan_let_modules ~env e;
    cases
    |> List.iter (fun {Typedtree.c_lhs = _; c_guard; c_rhs} ->
           (match c_guard with
           | Some g -> scan_let_modules ~env g
           | None -> ());
           scan_let_modules ~env c_rhs)
  | Texp_ifthenelse (e1, e2, e3_opt) -> (
    scan_let_modules ~env e1;
    scan_let_modules ~env e2;
    match e3_opt with
    | Some e3 -> scan_let_modules ~env e3
    | None -> ())
  | _ -> ()

and for_structure ~name ~env str_items =
  let exported = Exported.init () in
  let items =
    List.fold_right
      (fun item results -> for_structure_item ~env ~exported item @ results)
      str_items []
  in
  let attributes =
    str_items
    |> List.filter_map (fun (struc : Typedtree.structure_item) ->
           match struc with
           | {str_desc = Tstr_attribute attr} -> Some attr
           | _ -> None)
  in
  let docstring = attrs_to_docstring attributes in
  let deprecated = Process_attributes.find_deprecated_attribute attributes in
  {Module.name; docstring; exported; items; deprecated}

let file_for_cmt_infos ~module_name ~uri
    ({cmt_modname; cmt_annots} : Cmt_format.cmt_infos) =
  let env =
    {Env.stamps = Stamps.init (); module_path = File (uri, module_name)}
  in
  match cmt_annots with
  | Partial_implementation parts ->
    let items =
      parts |> Array.to_list
      |> Utils.filter_map (fun p ->
             match (p : Cmt_format.binary_part) with
             | Partial_structure str -> Some str.str_items
             | Partial_structure_item str -> Some [str]
             | _ -> None)
      |> List.concat
    in
    let structure = for_structure ~name:module_name ~env items in
    {File.uri; module_name = cmt_modname; stamps = env.stamps; structure}
  | Partial_interface parts ->
    let items =
      parts |> Array.to_list
      |> Utils.filter_map (fun (p : Cmt_format.binary_part) ->
             match p with
             | Partial_signature str -> Some str.sig_items
             | Partial_signature_item str -> Some [str]
             | _ -> None)
      |> List.concat
    in
    let structure = for_signature ~name:module_name ~env items in
    {uri; module_name = cmt_modname; stamps = env.stamps; structure}
  | Implementation structure ->
    let structure = for_structure ~name:module_name ~env structure.str_items in
    {uri; module_name = cmt_modname; stamps = env.stamps; structure}
  | Interface signature ->
    let structure = for_signature ~name:module_name ~env signature.sig_items in
    {uri; module_name = cmt_modname; stamps = env.stamps; structure}
  | _ -> File.create module_name uri

let file_for_cmt ~state ~module_name ~cmt ~uri =
  match Hashtbl.find_opt state.cmt_cache cmt with
  | Some file -> Some file
  | None -> (
    match Shared.try_read_cmt cmt with
    | None -> None
    | Some infos ->
      let file = file_for_cmt_infos ~module_name ~uri infos in
      Hashtbl.replace state.cmt_cache cmt file;
      Some file)

let file_for_module ~state module_name ~package =
  match Hashtbl.find_opt package.paths_for_module module_name with
  | Some paths ->
    let uri = get_uri paths in
    let cmt = get_cmt_path ~uri paths in
    Log.log ("fileForModule " ^ show_paths paths);
    file_for_cmt ~state ~cmt ~module_name ~uri
  | None ->
    Log.log ("No path for module " ^ module_name);
    None
