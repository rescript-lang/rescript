let print_engine =
  Res_driver.
    {
      print_implementation =
        (fun ~width:_ ~filename:_ ~comments:_ structure ->
          Pprintast.structure Format.std_formatter structure);
      print_interface =
        (fun ~width:_ ~filename:_ ~comments:_ signature ->
          Pprintast.signature Format.std_formatter signature);
    }
