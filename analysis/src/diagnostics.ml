let document_syntax ~source ~kind_file =
  let get_diagnostics diagnostics =
    diagnostics
    |> List.map (fun diagnostic ->
           let _, startline, startcol =
             Location.get_pos_info (Res_diagnostics.get_start_pos diagnostic)
           in
           let _, endline, endcol =
             Location.get_pos_info (Res_diagnostics.get_end_pos diagnostic)
           in
           let range =
             Lsp.Types.Range.create
               ~start:
                 (Lsp.Types.Position.create ~line:(startline - 1)
                    ~character:startcol)
               ~end_:
                 (Lsp.Types.Position.create ~line:(endline - 1)
                    ~character:endcol)
           in
           Lsp.Types.Diagnostic.create ~range ~source:"ReScript"
             ~message:(`String (Res_diagnostics.explain diagnostic))
             ~severity:Lsp.Types.DiagnosticSeverity.Error ())
  in
  if kind_file = Files.Res then
    let parse_implementation =
      Res_driver.parsing_engine.parse_implementation_from_source
        ~for_printer:false ~source
    in
    get_diagnostics parse_implementation.diagnostics
  else if kind_file = Files.Resi then
    let parse_interface =
      Res_driver.parsing_engine.parse_interface_from_source ~for_printer:false
        ~source
    in
    get_diagnostics parse_interface.diagnostics
  else []
