let printExpr ?(lineWidth = 60) typ =
  Printtyp.reset_names ();
  Printtyp.reset_and_mark_loops typ;
  Res_doc.to_string ~width:lineWidth
    (Res_outcome_printer.print_out_type_doc (Printtyp.tree_of_typexp false typ))

let printDecl ?printNameAsIs ~recStatus name decl =
  Printtyp.reset_names ();
  Res_doc.to_string ~width:60
    (Res_outcome_printer.print_out_sig_item_doc ?print_name_as_is:printNameAsIs
       (Printtyp.tree_of_type_declaration (Ident.create name) decl recStatus))
