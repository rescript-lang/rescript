module Comment_table = Res_comments_table

let print_engine =
  Res_driver.
    {
      print_implementation =
        (fun ~width:_ ~filename:_ ~comments:_ structure ->
          Printast.implementation
            (Compiler_request_output.stdout_formatter ())
            structure);
      print_interface =
        (fun ~width:_ ~filename:_ ~comments:_ signature ->
          Printast.interface
            (Compiler_request_output.stdout_formatter ())
            signature);
    }

let comments_print_engine =
  {
    Res_driver.print_implementation =
      (fun ~width:_ ~filename:_ ~comments s ->
        let cmt_tbl = Comment_table.make () in
        Comment_table.walk_structure s cmt_tbl comments;
        Comment_table.log cmt_tbl);
    Res_driver.print_interface =
      (fun ~width:_ ~filename:_ ~comments s ->
        let cmt_tbl = Comment_table.make () in
        Comment_table.walk_signature s cmt_tbl comments;
        Comment_table.log cmt_tbl);
  }

let doc_print_engine =
  {
    Res_driver.print_implementation =
      (fun ~width:_ ~filename:_ ~comments s ->
        Res_doc.debug (Res_printer.implementation_doc s ~comments));
    Res_driver.print_interface =
      (fun ~width:_ ~filename:_ ~comments s ->
        Res_doc.debug (Res_printer.interface_doc s ~comments));
  }
