(* This is the return that's expected when resolving code actions *)
type result = Protocol.codeAction list

let stringifyCodeActions codeActions =
  Printf.sprintf {|%s|}
    (codeActions |> List.map Protocol.stringifyCodeAction |> Protocol.array)

let make ~title ~kind ~uri ~newText ~range =
  let uri = uri |> Uri.fromPath |> Uri.toString in
  {
    Protocol.title;
    codeActionKind = kind;
    edit =
      {
        documentChanges =
          [
            TextDocumentEdit
              {
                Protocol.textDocument = {version = None; uri};
                edits = [{newText; range}];
              };
          ];
      };
  }

let makeWithDocumentChanges ~title ~kind ~documentChanges =
  {Protocol.title; codeActionKind = kind; edit = {documentChanges}}
