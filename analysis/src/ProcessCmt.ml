open SharedTypes

let isModuleType (declared : Module.t Declared.t) =
  match declared.modulePath with
  | ExportedModule {isType} -> isType
  | _ -> false

let addDeclared ~(name : string Location.loc) ~extent ~stamp ~(env : Env.t)
    ~item attributes addExported addStamp =
  let isExported = addExported name.txt stamp in
  let declared =
    ProcessAttributes.newDeclared ~item ~extent ~name ~stamp
      ~modulePath:env.modulePath isExported attributes
  in
  addStamp env.stamps stamp declared;
  declared

let attrsToDocstring attrs =
  match ProcessAttributes.findDocAttribute attrs with
  | None -> []
  | Some docstring -> [docstring]

let mapRecordField {Types.ld_id; ld_type; ld_attributes; ld_optional} =
  let astamp = Ident.binding_time ld_id in
  let name = Ident.name ld_id in
  {
    stamp = astamp;
    fname = Location.mknoloc name;
    typ = ld_type;
    optional = ld_optional;
    docstring =
      (match ProcessAttributes.findDocAttribute ld_attributes with
      | None -> []
      | Some docstring -> [docstring]);
    deprecated = ProcessAttributes.findDeprecatedAttribute ld_attributes;
  }

let rec forTypeSignatureItem ~(env : SharedTypes.Env.t) ~(exported : Exported.t)
    (item : Types.signature_item) =
  match item with
  | Sig_value (ident, {val_type; val_attributes; val_loc = loc}) ->
    let item = val_type in
    let stamp = Ident.binding_time ident in
    let oldDeclared = Stamps.findValue env.stamps stamp in
    let declared =
      addDeclared
        ~name:(Location.mkloc (Ident.name ident) loc)
        ~extent:loc ~stamp ~env ~item val_attributes
        (Exported.add exported Exported.Value)
        Stamps.addValue
    in
    let declared =
      (* When an id is shadowed, a module constraint without the doc comment is created.
         Here the existing doc comment is restored. See https://github.com/rescript-lang/rescript-vscode/issues/621 *)
      match oldDeclared with
      | Some oldDeclared when declared.docstring = [] ->
        let newDeclared = {declared with docstring = oldDeclared.docstring} in
        Stamps.addValue env.stamps stamp newDeclared;
        newDeclared
      | _ -> declared
    in
    [
      {
        Module.kind = Module.Value declared.item;
        name = declared.name.txt;
        docstring = declared.docstring;
        deprecated = declared.deprecated;
        loc = declared.extentLoc;
      };
    ]
  | Sig_type
      ( ident,
        ({type_loc; type_kind; type_manifest; type_attributes} as decl),
        recStatus ) ->
    let declared =
      let name = Location.mknoloc (Ident.name ident) in
      addDeclared ~extent:type_loc
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
                                 InlineRecord (fields |> List.map mapRecordField));
                             res = cd_res;
                             typeDecl = (name, decl);
                             docstring = attrsToDocstring cd_attributes;
                             deprecated =
                               ProcessAttributes.findDeprecatedAttribute
                                 cd_attributes;
                           }
                         in
                         let declared =
                           ProcessAttributes.newDeclared ~item ~extent:cd_loc
                             ~name:(Location.mknoloc name)
                             ~stamp (* TODO maybe this needs another child *)
                             ~modulePath:env.modulePath true cd_attributes
                         in
                         Stamps.addConstructor env.stamps stamp declared;
                         item))
              | Type_record (fields, _) ->
                Record (fields |> List.map mapRecordField));
          }
        ~name ~stamp:(Ident.binding_time ident) ~env type_attributes
        (Exported.add exported Exported.Type)
        Stamps.addType
    in
    [
      {
        Module.kind = Type (declared.item, recStatus);
        name = declared.name.txt;
        docstring = declared.docstring;
        deprecated = declared.deprecated;
        loc = declared.extentLoc;
      };
    ]
  | Sig_module (ident, {md_type; md_attributes; md_loc}, _) ->
    let name = Ident.name ident in
    let declared =
      addDeclared ~extent:md_loc
        ~item:(forTypeModule ~name ~env md_type)
        ~name:(Location.mkloc name md_loc)
        ~stamp:(Ident.binding_time ident) ~env md_attributes
        (Exported.add exported Exported.Module)
        Stamps.addModule
    in
    [
      {
        Module.kind =
          Module {type_ = declared.item; isModuleType = isModuleType declared};
        name = declared.name.txt;
        docstring = declared.docstring;
        deprecated = declared.deprecated;
        loc = declared.extentLoc;
      };
    ]
  | _ -> []

and forTypeSignature ~name ~env signature =
  let exported = Exported.init () in
  let items =
    List.fold_right
      (fun item items -> forTypeSignatureItem ~env ~exported item @ items)
      signature []
  in
  {Module.name; docstring = []; exported; items; deprecated = None}

and forTypeModule ~name ~env moduleType =
  match moduleType with
  | Types.Mty_ident path -> Ident path
  | Mty_alias (_ (* 402 *), path) -> Ident path
  | Mty_signature signature -> Structure (forTypeSignature ~name ~env signature)
  | Mty_functor (_argIdent, _argType, resultType) ->
    forTypeModule ~name ~env resultType

let getModuleTypePath mod_desc =
  match mod_desc with
  | Typedtree.Tmty_ident (path, _) | Tmty_alias (path, _) -> Some path
  | Tmty_signature _ | Tmty_functor _ | Tmty_with _ | Tmty_typeof _ -> None

let forTypeDeclaration ~env ~(exported : Exported.t)
    {
      Typedtree.typ_id;
      typ_loc;
      typ_name = name;
      typ_attributes;
      typ_type;
      typ_kind;
      typ_manifest;
    } ~recStatus =
  let stamp = Ident.binding_time typ_id in
  let declared =
    addDeclared ~extent:typ_loc
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
                             ProcessAttributes.findDeprecatedAttribute
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
                                               ProcessAttributes
                                               .findDocAttribute f.ld_attributes
                                             with
                                            | None -> []
                                            | Some docstring -> [docstring]);
                                          deprecated =
                                            ProcessAttributes
                                            .findDeprecatedAttribute
                                              f.ld_attributes;
                                        })));
                           res =
                             (match cd_res with
                             | None -> None
                             | Some t -> Some t.ctyp_type);
                           typeDecl = (name.txt, typ_type);
                           docstring = attrsToDocstring cd_attributes;
                         }
                       in
                       let declared =
                         ProcessAttributes.newDeclared ~item ~extent:cd_loc
                           ~name:cname ~stamp ~modulePath:env.modulePath true
                           cd_attributes
                       in
                       Stamps.addConstructor env.stamps stamp declared;
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
                         docstring = attrsToDocstring ld_attributes;
                         deprecated =
                           ProcessAttributes.findDeprecatedAttribute
                             ld_attributes;
                       })));
        }
      ~name ~stamp ~env typ_attributes
      (Exported.add exported Exported.Type)
      Stamps.addType
  in
  {
    Module.kind = Module.Type (declared.item, recStatus);
    name = declared.name.txt;
    docstring = declared.docstring;
    deprecated = declared.deprecated;
    loc = declared.extentLoc;
  }

let rec forSignatureItem ~env ~(exported : Exported.t)
    (item : Typedtree.signature_item) =
  match item.sig_desc with
  | Tsig_value {val_id; val_loc; val_name = name; val_desc; val_attributes} ->
    let declared =
      addDeclared ~name
        ~stamp:(Ident.binding_time val_id)
        ~extent:val_loc ~item:val_desc.ctyp_type ~env val_attributes
        (Exported.add exported Exported.Value)
        Stamps.addValue
    in
    [
      {
        Module.kind = Module.Value declared.item;
        name = declared.name.txt;
        docstring = declared.docstring;
        deprecated = declared.deprecated;
        loc = declared.extentLoc;
      };
    ]
  | Tsig_type (recFlag, decls) ->
    decls
    |> List.mapi (fun i decl ->
           let recStatus =
             match recFlag with
             | Recursive when i = 0 -> Types.Trec_first
             | Nonrecursive when i = 0 -> Types.Trec_not
             | _ -> Types.Trec_next
           in
           decl |> forTypeDeclaration ~env ~exported ~recStatus)
  | Tsig_module
      {md_id; md_attributes; md_loc; md_name = name; md_type = {mty_type}} ->
    let item =
      forTypeModule ~name:name.txt
        ~env:(env |> Env.addModule ~name:name.txt)
        mty_type
    in
    let declared =
      addDeclared ~item ~name ~extent:md_loc ~stamp:(Ident.binding_time md_id)
        ~env md_attributes
        (Exported.add exported Exported.Module)
        Stamps.addModule
    in
    [
      {
        Module.kind =
          Module {type_ = declared.item; isModuleType = isModuleType declared};
        name = declared.name.txt;
        docstring = declared.docstring;
        deprecated = declared.deprecated;
        loc = declared.extentLoc;
      };
    ]
  | Tsig_recmodule modDecls ->
    modDecls
    |> List.map (fun modDecl ->
           forSignatureItem ~env ~exported
             {item with sig_desc = Tsig_module modDecl})
    |> List.flatten
  | Tsig_include {incl_mod; incl_type} ->
    let env =
      match getModuleTypePath incl_mod.mty_desc with
      | None -> env
      | Some path ->
        {env with modulePath = IncludedModule (path, env.modulePath)}
    in
    let topLevel =
      List.fold_right
        (fun item items -> forTypeSignatureItem ~env ~exported item @ items)
        incl_type []
    in
    topLevel
  (* TODO: process other things here *)
  | _ -> []

let forSignature ~name ~env sigItems =
  let exported = Exported.init () in
  let items =
    sigItems |> List.map (forSignatureItem ~env ~exported) |> List.flatten
  in
  let attributes =
    match sigItems with
    | {sig_desc = Tsig_attribute attribute} :: _ -> [attribute]
    | _ -> []
  in
  let docstring = attrsToDocstring attributes in
  let deprecated = ProcessAttributes.findDeprecatedAttribute attributes in
  {Module.name; docstring; exported; items; deprecated}

let forTreeModuleType ~name ~env {Typedtree.mty_desc} =
  match mty_desc with
  | Tmty_ident _ -> None
  | Tmty_signature {sig_items} ->
    let contents = forSignature ~name ~env sig_items in
    Some (Module.Structure contents)
  | _ -> None

let rec getModulePath mod_desc =
  match mod_desc with
  | Typedtree.Tmod_ident (path, _lident) -> Some path
  | Tmod_structure _ -> None
  | Tmod_functor (_ident, _argName, _maybeType, _resultExpr) -> None
  | Tmod_apply (functor_, _arg, _coercion) -> getModulePath functor_.mod_desc
  | Tmod_unpack (_expr, _moduleType) -> None
  | Tmod_constraint (expr, _typ, _constraint, _coercion) ->
    getModulePath expr.mod_desc

let rec forStructureItem ~(env : SharedTypes.Env.t) ~(exported : Exported.t)
    item =
  match item.Typedtree.str_desc with
  | Tstr_value (_isRec, bindings) ->
    let items = ref [] in
    let rec handlePattern attributes pat =
      match pat.Typedtree.pat_desc with
      | Tpat_var (ident, name)
      | Tpat_alias (_, ident, name) (* let x : t = ... *) ->
        (* Detect first-class module unpack patterns and register them as modules. *)
        let unpack_loc_opt =
          match
            pat.pat_extra
            |> Utils.filterMap (function
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
              ProcessAttributes.newDeclared ~item:(Module.Ident path)
                ~extent:(Option.get unpack_loc_opt)
                ~name ~stamp:(Ident.binding_time ident) ~modulePath:NotVisible
                false attributes
            in
            Stamps.addModule env.stamps (Ident.binding_time ident) declared;
            items :=
              {
                Module.kind =
                  Module
                    {
                      type_ = declared.item;
                      isModuleType = isModuleType declared;
                    };
                name = declared.name.txt;
                docstring = declared.docstring;
                deprecated = declared.deprecated;
                loc = declared.extentLoc;
              }
              :: !items
          | _ ->
            let item = pat.pat_type in
            let declared =
              addDeclared ~name ~stamp:(Ident.binding_time ident) ~env
                ~extent:pat.pat_loc ~item attributes
                (Exported.add exported Exported.Value)
                Stamps.addValue
            in
            items :=
              {
                Module.kind = Module.Value declared.item;
                name = declared.name.txt;
                docstring = declared.docstring;
                deprecated = declared.deprecated;
                loc = declared.extentLoc;
              }
              :: !items
        else
          let item = pat.pat_type in
          let declared =
            addDeclared ~name ~stamp:(Ident.binding_time ident) ~env
              ~extent:pat.pat_loc ~item attributes
              (Exported.add exported Exported.Value)
              Stamps.addValue
          in
          items :=
            {
              Module.kind = Module.Value declared.item;
              name = declared.name.txt;
              docstring = declared.docstring;
              deprecated = declared.deprecated;
              loc = declared.extentLoc;
            }
            :: !items
      | Tpat_tuple pats | Tpat_array pats | Tpat_construct (_, _, pats) ->
        pats |> List.iter (fun p -> handlePattern [] p)
      | Tpat_or (p, _, _) -> handlePattern [] p
      | Tpat_record (items, _) ->
        items |> List.iter (fun (_, _, p, _) -> handlePattern [] p)
      | Tpat_variant (_, Some p, _) -> handlePattern [] p
      | Tpat_variant (_, None, _) | Tpat_any | Tpat_constant _ -> ()
    in
    List.iter
      (fun {Typedtree.vb_pat; vb_attributes} ->
        handlePattern vb_attributes vb_pat)
      bindings;
    !items
  | Tstr_module
      {mb_id; mb_attributes; mb_loc; mb_name = name; mb_expr = {mod_desc}}
    when not
           (String.length name.txt >= 6
           && (String.sub name.txt 0 6 = "local_") [@doesNotRaise])
         (* %%private generates a dummy module called local_... *) ->
    let item = forModule ~env mod_desc name.txt in
    let declared =
      addDeclared ~item ~name ~extent:mb_loc ~stamp:(Ident.binding_time mb_id)
        ~env mb_attributes
        (Exported.add exported Exported.Module)
        Stamps.addModule
    in
    [
      {
        Module.kind =
          Module {type_ = declared.item; isModuleType = isModuleType declared};
        name = declared.name.txt;
        docstring = declared.docstring;
        deprecated = declared.deprecated;
        loc = declared.extentLoc;
      };
    ]
  | Tstr_recmodule modDecls ->
    modDecls
    |> List.map (fun modDecl ->
           forStructureItem ~env ~exported
             {item with str_desc = Tstr_module modDecl})
    |> List.flatten
  | Tstr_modtype
      {
        mtd_name = name;
        mtd_id;
        mtd_attributes;
        mtd_type = Some {mty_type = modType};
        mtd_loc;
      } ->
    let env = env |> Env.addModuleType ~name:name.txt in
    let modTypeItem = forTypeModule ~name:name.txt ~env modType in
    let declared =
      addDeclared ~item:modTypeItem ~name ~extent:mtd_loc
        ~stamp:(Ident.binding_time mtd_id)
        ~env mtd_attributes
        (Exported.add exported Exported.Module)
        Stamps.addModule
    in
    [
      {
        Module.kind =
          Module {type_ = declared.item; isModuleType = isModuleType declared};
        name = declared.name.txt;
        docstring = declared.docstring;
        deprecated = declared.deprecated;
        loc = declared.extentLoc;
      };
    ]
  | Tstr_include {incl_mod; incl_type} ->
    let env =
      match getModulePath incl_mod.mod_desc with
      | None -> env
      | Some path ->
        {env with modulePath = IncludedModule (path, env.modulePath)}
    in
    let topLevel =
      List.fold_right
        (fun item items -> forTypeSignatureItem ~env ~exported item @ items)
        incl_type []
    in
    topLevel
  | Tstr_primitive vd when JsxHacks.primitiveIsFragment vd = false ->
    let declared =
      addDeclared ~extent:vd.val_loc ~item:vd.val_val.val_type ~name:vd.val_name
        ~stamp:(Ident.binding_time vd.val_id)
        ~env vd.val_attributes
        (Exported.add exported Exported.Value)
        Stamps.addValue
    in
    [
      {
        Module.kind = Value declared.item;
        name = declared.name.txt;
        docstring = declared.docstring;
        deprecated = declared.deprecated;
        loc = declared.extentLoc;
      };
    ]
  | Tstr_type (recFlag, decls) ->
    decls
    |> List.mapi (fun i decl ->
           let recStatus =
             match recFlag with
             | Recursive when i = 0 -> Types.Trec_first
             | Nonrecursive when i = 0 -> Types.Trec_not
             | _ -> Types.Trec_next
           in
           decl |> forTypeDeclaration ~env ~exported ~recStatus)
  | _ -> []

and forModule ~env mod_desc moduleName =
  match mod_desc with
  | Tmod_ident (path, _lident) -> Ident path
  | Tmod_structure structure ->
    let env = env |> Env.addModule ~name:moduleName in
    let contents = forStructure ~name:moduleName ~env structure.str_items in
    Structure contents
  | Tmod_functor (ident, argName, maybeType, resultExpr) ->
    (match maybeType with
    | None -> ()
    | Some t -> (
      match forTreeModuleType ~name:argName.txt ~env t with
      | None -> ()
      | Some kind ->
        let stamp = Ident.binding_time ident in
        let declared =
          ProcessAttributes.newDeclared ~item:kind ~name:argName
            ~extent:t.Typedtree.mty_loc ~stamp ~modulePath:NotVisible false []
        in
        Stamps.addModule env.stamps stamp declared));
    forModule ~env resultExpr.mod_desc moduleName
  | Tmod_apply (functor_, _arg, _coercion) ->
    forModule ~env functor_.mod_desc moduleName
  | Tmod_unpack (_expr, moduleType) ->
    let env = env |> Env.addModule ~name:moduleName in
    forTypeModule ~name:moduleName ~env moduleType
  | Tmod_constraint (expr, typ, _constraint, _coercion) ->
    (* TODO do this better I think *)
    let modKind = forModule ~env expr.mod_desc moduleName in
    let env = env |> Env.addModule ~name:moduleName in
    let modTypeKind = forTypeModule ~name:moduleName ~env typ in
    Constraint (modKind, modTypeKind)

and forStructure ~name ~env strItems =
  let exported = Exported.init () in
  let items =
    List.fold_right
      (fun item results -> forStructureItem ~env ~exported item @ results)
      strItems []
  in
  let attributes =
    strItems
    |> List.filter_map (fun (struc : Typedtree.structure_item) ->
           match struc with
           | {str_desc = Tstr_attribute attr} -> Some attr
           | _ -> None)
  in
  let docstring = attrsToDocstring attributes in
  let deprecated = ProcessAttributes.findDeprecatedAttribute attributes in
  {Module.name; docstring; exported; items; deprecated}

let fileForCmtInfos ~moduleName ~uri
    ({cmt_modname; cmt_annots} : Cmt_format.cmt_infos) =
  let env =
    {Env.stamps = Stamps.init (); modulePath = File (uri, moduleName)}
  in
  match cmt_annots with
  | Partial_implementation parts ->
    let items =
      parts |> Array.to_list
      |> Utils.filterMap (fun p ->
             match (p : Cmt_format.binary_part) with
             | Partial_structure str -> Some str.str_items
             | Partial_structure_item str -> Some [str]
             | _ -> None)
      |> List.concat
    in
    let structure = forStructure ~name:moduleName ~env items in
    {File.uri; moduleName = cmt_modname; stamps = env.stamps; structure}
  | Partial_interface parts ->
    let items =
      parts |> Array.to_list
      |> Utils.filterMap (fun (p : Cmt_format.binary_part) ->
             match p with
             | Partial_signature str -> Some str.sig_items
             | Partial_signature_item str -> Some [str]
             | _ -> None)
      |> List.concat
    in
    let structure = forSignature ~name:moduleName ~env items in
    {uri; moduleName = cmt_modname; stamps = env.stamps; structure}
  | Implementation structure ->
    let structure = forStructure ~name:moduleName ~env structure.str_items in
    {uri; moduleName = cmt_modname; stamps = env.stamps; structure}
  | Interface signature ->
    let structure = forSignature ~name:moduleName ~env signature.sig_items in
    {uri; moduleName = cmt_modname; stamps = env.stamps; structure}
  | _ -> File.create moduleName uri

let fileForCmt ~moduleName ~cmt ~uri =
  match Hashtbl.find_opt state.cmtCache cmt with
  | Some file -> Some file
  | None -> (
    match Shared.tryReadCmt cmt with
    | None -> None
    | Some infos ->
      let file = fileForCmtInfos ~moduleName ~uri infos in
      Hashtbl.replace state.cmtCache cmt file;
      Some file)

let fileForModule moduleName ~package =
  match Hashtbl.find_opt package.pathsForModule moduleName with
  | Some paths ->
    let uri = getUri paths in
    let cmt = getCmtPath ~uri paths in
    Log.log ("fileForModule " ^ showPaths paths);
    fileForCmt ~cmt ~moduleName ~uri
  | None ->
    Log.log ("No path for module " ^ moduleName);
    None
