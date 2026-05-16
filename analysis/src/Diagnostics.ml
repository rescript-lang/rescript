let document_syntax ~source ~kindFile =
  let get_diagnostics diagnostics =
    diagnostics
    |> List.map (fun diagnostic ->
           let _, startline, startcol =
             Location.get_pos_info (Res_diagnostics.get_start_pos diagnostic)
           in
           let _, endline, endcol =
             Location.get_pos_info (Res_diagnostics.get_end_pos diagnostic)
           in
           Protocol.
             {
               range =
                 {
                   start = {line = startline - 1; character = startcol};
                   end_ = {line = endline - 1; character = endcol};
                 };
               message = Res_diagnostics.explain diagnostic;
               severity = 1;
             })
  in
  if kindFile = Files.Res then
    let parseImplementation =
      Res_driver.parsing_engine.parse_implementation_from_source
        ~for_printer:false ~source
    in
    get_diagnostics parseImplementation.diagnostics
  else if kindFile = Files.Resi then
    let parseInterface =
      Res_driver.parsing_engine.parse_interface_from_source ~for_printer:false
        ~source
    in
    get_diagnostics parseInterface.diagnostics
  else []
