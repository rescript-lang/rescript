let print_engine =
  Res_driver.
    {
      print_implementation =
        (fun ~width:_ ~filename ~comments:_ structure ->
          output_string stdout Config.ast_impl_magic_number;
          output_value stdout filename;
          output_value stdout structure);
      print_implementation_from_source =
        (fun ~width:_ ~source:_ ~comments:_ structure ->
          output_string stdout Config.ast_impl_magic_number;
          output_value stdout "source";
          output_value stdout structure);
      print_interface =
        (fun ~width:_ ~filename ~comments:_ signature ->
          output_string stdout Config.ast_intf_magic_number;
          output_value stdout filename;
          output_value stdout signature);
      print_interface_from_source =
        (fun ~width:_ ~source:_ ~comments:_ signature ->
          output_string stdout Config.ast_intf_magic_number;
          output_value stdout "source";
          output_value stdout signature);
    }
