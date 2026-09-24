let print_engine =
  Res_driver.
    {
      print_implementation =
        (fun ~width:_ ~filename ~comments:_ structure ->
          let output = Compiler_request_output.stdout_channel () in
          output_string output Config.ast_impl_magic_number;
          output_value output filename;
          output_value output structure);
      print_implementation_from_source =
        (fun ~width:_ ~source:_ ~comments:_ structure ->
          let output = Compiler_request_output.stdout_channel () in
          output_string output Config.ast_impl_magic_number;
          output_value output "source";
          output_value output structure);
      print_interface =
        (fun ~width:_ ~filename ~comments:_ signature ->
          let output = Compiler_request_output.stdout_channel () in
          output_string output Config.ast_intf_magic_number;
          output_value output filename;
          output_value output signature);
      print_interface_from_source =
        (fun ~width:_ ~source:_ ~comments:_ signature ->
          let output = Compiler_request_output.stdout_channel () in
          output_string output Config.ast_intf_magic_number;
          output_value output "source";
          output_value output signature);
    }
