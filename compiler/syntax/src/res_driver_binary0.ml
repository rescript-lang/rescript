(* Binary0 output: parsetree0 format (frozen PPX-compatible) *)
let print_engine =
  Res_driver.
    {
      print_implementation =
        (fun ~width:_ ~filename ~comments:_ structure ->
          let structure0 = Ast_mapper_to0.default_mapper.structure
            Ast_mapper_to0.default_mapper structure in
          output_string stdout Config.ast_impl_magic_number;
          output_value stdout filename;
          output_value stdout structure0);
      print_interface =
        (fun ~width:_ ~filename ~comments:_ signature ->
          let signature0 = Ast_mapper_to0.default_mapper.signature
            Ast_mapper_to0.default_mapper signature in
          output_string stdout Config.ast_intf_magic_number;
          output_value stdout filename;
          output_value stdout signature0);
    }
