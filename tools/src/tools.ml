let extractEmbedded ~extensionPoints ~filename =
  let {Res_driver.parsetree = structure} =
    Res_driver.parsing_engine.parse_implementation ~for_printer:false ~filename
  in
  let content = ref [] in
  let append item = content := item :: !content in
  let extension (iterator : Ast_iterator.iterator) (ext : Parsetree.extension) =
    (match ext with
    | ( {txt},
        PStr
          [
            {
              pstr_desc =
                Pstr_eval
                  ( {
                      pexp_loc;
                      pexp_desc = Pexp_constant (Pconst_string (contents, _));
                    },
                    _ );
            };
          ] )
      when extensionPoints |> List.exists (fun v -> v = txt) ->
      append (pexp_loc, txt, contents)
    | _ -> ());
    Ast_iterator.default_iterator.extension iterator ext
  in
  let iterator = {Ast_iterator.default_iterator with extension} in
  iterator.structure iterator structure;
  let open Analysis.Protocol in
  !content
  |> List.map (fun (loc, extensionName, contents) ->
         stringifyObject
           [
             ("extensionName", Some (wrapInQuotes extensionName));
             ("contents", Some (wrapInQuotes contents));
             ("loc", Some (Analysis.Utils.cmtLocToRange loc |> stringifyRange));
           ])
  |> List.rev |> array
