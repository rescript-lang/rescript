(* https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocument_documentSymbol *)

let command ~path =
  let symbols = ref [] in
  let add_symbol name loc kind =
    if
      (not loc.Location.loc_ghost)
      && loc.loc_start.pos_cnum >= 0
      && loc.loc_end.pos_cnum >= 0
    then
      let range = Utils.cmt_loc_to_range loc in
      let symbol =
        Lsp.Types.DocumentSymbol.create ~name ~range ~selectionRange:range
          ~children:[] ~kind ()
      in
      symbols := symbol :: !symbols
  in
  let rec expr_kind (exp : Parsetree.expression) =
    match exp.pexp_desc with
    | Pexp_fun _ -> Lsp.Types.SymbolKind.Function
    | Pexp_constraint (e, _) -> expr_kind e
    | Pexp_constant (Pconst_string _) -> Lsp.Types.SymbolKind.String
    | Pexp_constant (Pconst_float _ | Pconst_integer _) ->
      Lsp.Types.SymbolKind.Number
    | Pexp_constant _ -> Lsp.Types.SymbolKind.Constant
    | _ -> Lsp.Types.SymbolKind.Variable
  in
  let process_type_kind (tk : Parsetree.type_kind) =
    match tk with
    | Ptype_variant constr_decls ->
      constr_decls
      |> List.iter (fun (cd : Parsetree.constructor_declaration) ->
             add_symbol cd.pcd_name.txt cd.pcd_loc EnumMember)
    | Ptype_record label_decls ->
      label_decls
      |> List.iter (fun (ld : Parsetree.label_declaration) ->
             add_symbol ld.pld_name.txt ld.pld_loc Property)
    | _ -> ()
  in
  let process_type_declaration (td : Parsetree.type_declaration) =
    add_symbol td.ptype_name.txt td.ptype_loc TypeParameter;
    process_type_kind td.ptype_kind
  in
  let process_value_description (vd : Parsetree.value_description) =
    add_symbol vd.pval_name.txt vd.pval_loc Variable
  in
  let process_module_binding (mb : Parsetree.module_binding) =
    add_symbol mb.pmb_name.txt mb.pmb_loc Module
  in
  let process_module_declaration (md : Parsetree.module_declaration) =
    add_symbol md.pmd_name.txt md.pmd_loc Module
  in
  let process_extension_constructor (et : Parsetree.extension_constructor) =
    add_symbol et.pext_name.txt et.pext_loc Constructor
  in
  let value_binding (iterator : Ast_iterator.iterator)
      (vb : Parsetree.value_binding) =
    (match vb.pvb_pat.ppat_desc with
    | Ppat_var {txt} | Ppat_constraint ({ppat_desc = Ppat_var {txt}}, _) ->
      add_symbol txt vb.pvb_loc (expr_kind vb.pvb_expr)
    | _ -> ());
    Ast_iterator.default_iterator.value_binding iterator vb
  in
  let expr (iterator : Ast_iterator.iterator) (e : Parsetree.expression) =
    (match e.pexp_desc with
    | Pexp_letmodule ({txt}, mod_expr, _) ->
      add_symbol txt
        {e.pexp_loc with loc_end = mod_expr.pmod_loc.loc_end}
        Module
    | Pexp_letexception (ec, _) -> process_extension_constructor ec
    | _ -> ());
    Ast_iterator.default_iterator.expr iterator e
  in
  let structure_item (iterator : Ast_iterator.iterator)
      (item : Parsetree.structure_item) =
    (match item.pstr_desc with
    | Pstr_value _ -> ()
    | Pstr_primitive vd -> process_value_description vd
    | Pstr_type (_, typ_decls) ->
      typ_decls |> List.iter process_type_declaration
    | Pstr_module mb -> process_module_binding mb
    | Pstr_recmodule mbs -> mbs |> List.iter process_module_binding
    | Pstr_exception ec -> process_extension_constructor ec
    | _ -> ());
    Ast_iterator.default_iterator.structure_item iterator item
  in
  let signature_item (iterator : Ast_iterator.iterator)
      (item : Parsetree.signature_item) =
    (match item.psig_desc with
    | Psig_value vd -> process_value_description vd
    | Psig_type (_, typ_decls) ->
      typ_decls |> List.iter process_type_declaration
    | Psig_module md -> process_module_declaration md
    | Psig_recmodule mds -> mds |> List.iter process_module_declaration
    | Psig_exception ec -> process_extension_constructor ec
    | _ -> ());
    Ast_iterator.default_iterator.signature_item iterator item
  in
  let module_expr (iterator : Ast_iterator.iterator)
      (me : Parsetree.module_expr) =
    match me.pmod_desc with
    | Pmod_constraint (mod_expr, _modTyp) ->
      (* Don't double-list items in implementation and interface *)
      Ast_iterator.default_iterator.module_expr iterator mod_expr
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
  let is_inside
      ({
         range =
           {
             start = {line = sl1; character = sc1};
             end_ = {line = el1; character = ec1};
           };
       } :
        Lsp.Types.DocumentSymbol.t)
      ({
         range =
           {
             start = {line = sl2; character = sc2};
             end_ = {line = el2; character = ec2};
           };
       } :
        Lsp.Types.DocumentSymbol.t) =
    (sl1 > sl2 || (sl1 = sl2 && sc1 >= sc2))
    && (el1 < el2 || (el1 = el2 && ec1 <= ec2))
  in
  let compare_symbol (s1 : Lsp.Types.DocumentSymbol.t)
      (s2 : Lsp.Types.DocumentSymbol.t) =
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
  let rec add_symbol_to_children ~symbol children =
    match children with
    | [] -> [symbol]
    | last :: rest ->
      if is_inside symbol last then
        match last.children with
        | Some c ->
          let new_last =
            {last with children = Some (c |> add_symbol_to_children ~symbol)}
          in
          new_last :: rest
        | _ -> rest
      else symbol :: children
  in
  let rec add_sorted_symbols_to_children ~sorted_symbols children =
    match sorted_symbols with
    | [] -> children
    | first_symbol :: rest ->
      children
      |> add_symbol_to_children ~symbol:first_symbol
      |> add_sorted_symbols_to_children ~sorted_symbols:rest
  in
  let sorted_symbols = !symbols |> List.sort compare_symbol in
  let symbols_with_children =
    [] |> add_sorted_symbols_to_children ~sorted_symbols
  in
  `List (symbols_with_children |> List.map Lsp.Types.DocumentSymbol.yojson_of_t)
  |> Yojson.Safe.pretty_to_string ~std:true
  |> print_endline
