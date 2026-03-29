let kindNumber = DocumentSymbol.kindNumber

let getString key obj =
  match Json.get key obj with
  | Some (Json.String s) -> s
  | _ -> ""

let getBool key obj =
  match Json.get key obj with
  | Some Json.True -> true
  | _ -> false

(** Case-insensitive substring match. *)
let matches query name =
  if query = "" then true
  else
    let queryLower = String.lowercase_ascii query in
    let nameLower = String.lowercase_ascii name in
    let queryLen = String.length queryLower in
    let nameLen = String.length nameLower in
    if queryLen > nameLen then false
    else
      let found = ref false in
      for i = 0 to nameLen - queryLen do
        if (not !found) && String.sub nameLower i queryLen = queryLower then
          found := true
      done;
      !found

(** Shared symbol collector — used by both structure and signature walkers. *)
let makeCollector ~query ~sourcePath ~moduleName =
  let symbols = ref [] in
  let addSymbol name loc kind =
    if
      (not loc.Location.loc_ghost)
      && loc.loc_start.pos_cnum >= 0
      && loc.loc_end.pos_cnum >= 0 && matches query name
    then
      let range = Utils.cmtLocToRange loc in
      let symbol : Protocol.symbolInformation =
        {
          name;
          kind = kindNumber kind;
          location = {uri = sourcePath; range};
          containerName = Some moduleName;
        }
      in
      symbols := symbol :: !symbols
  in
  let processTypeKind (tk : Parsetree.type_kind) =
    match tk with
    | Ptype_variant constrDecls ->
      constrDecls
      |> List.iter (fun (cd : Parsetree.constructor_declaration) ->
             addSymbol cd.pcd_name.txt cd.pcd_loc EnumMember)
    | Ptype_record labelDecls ->
      labelDecls
      |> List.iter (fun (ld : Parsetree.label_declaration) ->
             addSymbol ld.pld_name.txt ld.pld_loc Property)
    | _ -> ()
  in
  let processTypeDeclaration (td : Parsetree.type_declaration) =
    addSymbol td.ptype_name.txt td.ptype_loc TypeParameter;
    processTypeKind td.ptype_kind
  in
  let processValueDescription (vd : Parsetree.value_description) =
    addSymbol vd.pval_name.txt vd.pval_loc Variable
  in
  let processModuleBinding (mb : Parsetree.module_binding) =
    addSymbol mb.pmb_name.txt mb.pmb_loc Module
  in
  let processModuleDeclaration (md : Parsetree.module_declaration) =
    addSymbol md.pmd_name.txt md.pmd_loc Module
  in
  let processModuleTypeDeclaration (mtd : Parsetree.module_type_declaration) =
    addSymbol mtd.pmtd_name.txt mtd.pmtd_loc Module
  in
  let processExtensionConstructor (et : Parsetree.extension_constructor) =
    addSymbol et.pext_name.txt et.pext_loc Constructor
  in
  let processTypeExtension (te : Parsetree.type_extension) =
    te.ptyext_constructors |> List.iter processExtensionConstructor
  in
  ( symbols,
    addSymbol,
    processTypeDeclaration,
    processValueDescription,
    processModuleBinding,
    processModuleDeclaration,
    processModuleTypeDeclaration,
    processExtensionConstructor,
    processTypeExtension )

(** Walk a structure (implementation) and collect matching symbols. *)
let collectFromStructure ~query ~sourcePath ~moduleName structure =
  let ( symbols,
        addSymbol,
        processTypeDeclaration,
        processValueDescription,
        processModuleBinding,
        _processModuleDeclaration,
        processModuleTypeDeclaration,
        processExtensionConstructor,
        processTypeExtension ) =
    makeCollector ~query ~sourcePath ~moduleName
  in
  let pat (iterator : Ast_iterator.iterator) (p : Parsetree.pattern) =
    (match p.ppat_desc with
    | Ppat_var {txt} -> addSymbol txt p.ppat_loc Variable
    | Ppat_alias (_, {txt; loc}) -> addSymbol txt loc Variable
    | _ -> ());
    Ast_iterator.default_iterator.pat iterator p
  in
  let expr (iterator : Ast_iterator.iterator) (e : Parsetree.expression) =
    (match e.pexp_desc with
    | Pexp_letmodule ({txt}, modExpr, _) ->
      addSymbol txt {e.pexp_loc with loc_end = modExpr.pmod_loc.loc_end} Module
    | Pexp_letexception (ec, _) -> processExtensionConstructor ec
    | _ -> ());
    Ast_iterator.default_iterator.expr iterator e
  in
  let structure_item (iterator : Ast_iterator.iterator)
      (item : Parsetree.structure_item) =
    (match item.pstr_desc with
    | Pstr_value _ -> ()
    | Pstr_primitive vd -> processValueDescription vd
    | Pstr_type (_, typDecls) -> typDecls |> List.iter processTypeDeclaration
    | Pstr_typext te -> processTypeExtension te
    | Pstr_module mb -> processModuleBinding mb
    | Pstr_recmodule mbs -> mbs |> List.iter processModuleBinding
    | Pstr_modtype mtd -> processModuleTypeDeclaration mtd
    | Pstr_exception ec -> processExtensionConstructor ec
    | _ -> ());
    Ast_iterator.default_iterator.structure_item iterator item
  in
  let module_expr (iterator : Ast_iterator.iterator)
      (me : Parsetree.module_expr) =
    match me.pmod_desc with
    | Pmod_constraint (modExpr, _modTyp) ->
      Ast_iterator.default_iterator.module_expr iterator modExpr
    | _ -> Ast_iterator.default_iterator.module_expr iterator me
  in
  let iterator =
    {Ast_iterator.default_iterator with expr; module_expr; pat; structure_item}
  in
  iterator.structure iterator structure;
  !symbols

(** Walk a signature (interface) and collect matching symbols. *)
let collectFromSignature ~query ~sourcePath ~moduleName signature =
  let ( symbols,
        _addSymbol,
        processTypeDeclaration,
        processValueDescription,
        _processModuleBinding,
        processModuleDeclaration,
        processModuleTypeDeclaration,
        processExtensionConstructor,
        processTypeExtension ) =
    makeCollector ~query ~sourcePath ~moduleName
  in
  let signature_item (iterator : Ast_iterator.iterator)
      (item : Parsetree.signature_item) =
    (match item.psig_desc with
    | Psig_value vd -> processValueDescription vd
    | Psig_type (_, typDecls) -> typDecls |> List.iter processTypeDeclaration
    | Psig_typext te -> processTypeExtension te
    | Psig_module md -> processModuleDeclaration md
    | Psig_recmodule mds -> mds |> List.iter processModuleDeclaration
    | Psig_modtype mtd -> processModuleTypeDeclaration mtd
    | Psig_exception ec -> processExtensionConstructor ec
    | _ -> ());
    Ast_iterator.default_iterator.signature_item iterator item
  in
  let iterator = {Ast_iterator.default_iterator with signature_item} in
  iterator.signature iterator signature;
  !symbols

let command () =
  let input = In_channel.input_all In_channel.stdin in
  match Json.parse input with
  | None ->
    prerr_endline "workspaceSymbol: failed to parse JSON from stdin";
    print_endline "[]"
  | Some json ->
    let query =
      match Json.get "query" json with
      | Some (Json.String s) -> s
      | _ -> ""
    in
    let files =
      match Json.get "files" json with
      | Some (Json.Array items) -> items
      | _ -> []
    in
    let symbols =
      files
      |> List.concat_map (fun item ->
             let moduleName = getString "moduleName" item in
             let astPath = getString "astPath" item in
             let sourcePath = getString "sourcePath" item in
             let isInterface = getBool "isInterface" item in
             if astPath = "" || sourcePath = "" then []
             else
               try
                 let uri = "file://" ^ sourcePath in
                 if isInterface then
                   let ast : Parsetree.signature =
                     Binary_ast.read_ast_exn ~fname:astPath Mli
                   in
                   collectFromSignature ~query ~sourcePath:uri ~moduleName ast
                 else
                   let ast : Parsetree.structure =
                     Binary_ast.read_ast_exn ~fname:astPath Ml
                   in
                   collectFromStructure ~query ~sourcePath:uri ~moduleName ast
               with _ ->
                 prerr_endline
                   ("workspaceSymbol: failed to read ast for " ^ astPath);
                 [])
    in
    let jsonItems = symbols |> List.map Protocol.stringifySymbolInformation in
    print_endline (Protocol.array jsonItems)
