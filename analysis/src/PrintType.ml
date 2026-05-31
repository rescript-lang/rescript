let print_expr ?(line_width = 60) typ =
  Printtyp.reset_names ();
  Printtyp.reset_and_mark_loops typ;
  Res_doc.to_string ~width:line_width
    (Res_outcome_printer.print_out_type_doc (Printtyp.tree_of_typexp false typ))

let print_decl ?print_name_as_is ~rec_status name decl =
  Printtyp.reset_names ();
  Res_doc.to_string ~width:60
    (Res_outcome_printer.print_out_sig_item_doc ?print_name_as_is:print_name_as_is
       (Printtyp.tree_of_type_declaration (Ident.create name) decl rec_status))
