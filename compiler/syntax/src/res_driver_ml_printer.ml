let print_engine =
  Res_driver.
    {
      print_implementation =
        (fun ~width:_ ~filename:_ ~comments:_ structure ->
          Pprintast.structure
            (Compiler_request_output.stdout_formatter ())
            structure);
      print_implementation_from_source =
        (fun ~width:_ ~source:_ ~comments:_ structure ->
          Pprintast.structure
            (Compiler_request_output.stdout_formatter ())
            structure);
      print_interface =
        (fun ~width:_ ~filename:_ ~comments:_ signature ->
          Pprintast.signature
            (Compiler_request_output.stdout_formatter ())
            signature);
      print_interface_from_source =
        (fun ~width:_ ~source:_ ~comments:_ signature ->
          Pprintast.signature
            (Compiler_request_output.stdout_formatter ())
            signature);
    }
