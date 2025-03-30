(* https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocument_documentSymbol *)

type kind =
  | Module
  | Property
  | Constructor
  | Function
  | Variable
  | Constant
  | String
  | Number
  | EnumMember
  | TypeParameter

let kindNumber = function
  | Module -> 2
  | Property -> 7
  | Constructor -> 9
  | Function -> 12
  | Variable -> 13
  | Constant -> 14
  | String -> 15
  | Number -> 16
  | EnumMember -> 22
  | TypeParameter -> 26

let command ~path =
  let symbols = ref [] in
  let addSymbol name loc kind =
    if
      (not loc.Location.loc_ghost)
      && loc.loc_start.pos_cnum >= 0
      && loc.loc_end.pos_cnum >= 0
    then
      let range = Utils.cmtLocToRange loc in
      let symbol : Protocol.documentSymbolItem =
        {name; range; kind = kindNumber kind; children = []}
      in
      symbols := symbol :: !symbols
  in
  let rec exprKind (exp : Parsetree.expression) =
    match exp.pexp_desc with
    | Pexp_fun _ -> Function
    | Pexp_constraint (e, _) -> exprKind e
    | Pexp_constant (Pconst_string _) -> String
    | Pexp_constant (Pconst_float _ | Pconst_integer _) -> Number
    | Pexp_constant _ -> Constant
    | _ -> Variable
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
  let processExtensionConstructor (et : Parsetree.extension_constructor) =
    addSymbol et.pext_name.txt et.pext_loc Constructor
  in
  let value_binding (iterator : Ast_iterator.iterator)
      (vb : Parsetree.value_binding) =
    (match vb.pvb_pat.ppat_desc with
    | Ppat_var {txt} | Ppat_constraint ({ppat_desc = Ppat_var {txt}}, _) ->
      addSymbol txt vb.pvb_loc (exprKind vb.pvb_expr)
    | _ -> ());
    Ast_iterator.default_iterator.value_binding iterator vb
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
    | Pstr_module mb -> processModuleBinding mb
    | Pstr_recmodule mbs -> mbs |> List.iter processModuleBinding
    | Pstr_exception ec -> processExtensionConstructor ec
    | _ -> ());
    Ast_iterator.default_iterator.structure_item iterator item
  in
  let signature_item (iterator : Ast_iterator.iterator)
      (item : Parsetree.signature_item) =
    (match item.psig_desc with
    | Psig_value vd -> processValueDescription vd
    | Psig_type (_, typDecls) -> typDecls |> List.iter processTypeDeclaration
    | Psig_module md -> processModuleDeclaration md
    | Psig_recmodule mds -> mds |> List.iter processModuleDeclaration
    | Psig_exception ec -> processExtensionConstructor ec
    | _ -> ());
    Ast_iterator.default_iterator.signature_item iterator item
  in
  let module_expr (iterator : Ast_iterator.iterator)
      (me : Parsetree.module_expr) =
    match me.pmod_desc with
    | Pmod_constraint (modExpr, _modTyp) ->
      (* Don't double-list items in implementation and interface *)
      Ast_iterator.default_iterator.module_expr iterator modExpr
    | _ -> Ast_iterator.default_iterator.module_expr iterator me
  in
  let iterator =
    {
      Ast_iterator.default_iterator with
      expr;
      module_expr;
      signature_item;
      structure_item;
      value_binding;
    }
  in

  (if Filename.check_suffix path ".res" then
     let parser =
       Res_driver.parsing_engine.parse_implementation ~for_printer:false
     in
     let {Res_driver.parsetree = structure} = parser ~filename:path in
     iterator.structure iterator structure |> ignore
   else
     let parser =
       Res_driver.parsing_engine.parse_interface ~for_printer:false
     in
     let {Res_driver.parsetree = signature} = parser ~filename:path in
     iterator.signature iterator signature |> ignore);
  let isInside
      ({
         range =
           {
             start = {line = sl1; character = sc1};
             end_ = {line = el1; character = ec1};
           };
       } :
        Protocol.documentSymbolItem)
      ({
         range =
           {
             start = {line = sl2; character = sc2};
             end_ = {line = el2; character = ec2};
           };
       } :
        Protocol.documentSymbolItem) =
    (sl1 > sl2 || (sl1 = sl2 && sc1 >= sc2))
    && (el1 < el2 || (el1 = el2 && ec1 <= ec2))
  in
  let compareSymbol (s1 : Protocol.documentSymbolItem)
      (s2 : Protocol.documentSymbolItem) =
    let n = compare s1.range.start.line s2.range.start.line in
    if n <> 0 then n
    else
      let n = compare s1.range.start.character s2.range.start.character in
      if n <> 0 then n
      else
        let n = compare s1.range.end_.line s2.range.end_.line in
        if n <> 0 then n
        else compare s1.range.end_.character s2.range.end_.character
  in
  let rec addSymbolToChildren ~symbol children =
    match children with
    | [] -> [symbol]
    | last :: rest ->
      if isInside symbol last then
        let newLast =
          {last with children = last.children |> addSymbolToChildren ~symbol}
        in
        newLast :: rest
      else symbol :: children
  in
  let rec addSortedSymbolsToChildren ~sortedSymbols children =
    match sortedSymbols with
    | [] -> children
    | firstSymbol :: rest ->
      children
      |> addSymbolToChildren ~symbol:firstSymbol
      |> addSortedSymbolsToChildren ~sortedSymbols:rest
  in
  let sortedSymbols = !symbols |> List.sort compareSymbol in
  let symbolsWithChildren = [] |> addSortedSymbolsToChildren ~sortedSymbols in
  print_endline (Protocol.stringifyDocumentSymbolItems symbolsWithChildren)
