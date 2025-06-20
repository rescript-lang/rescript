let getCompletions ~debug ~path ~pos ~currentFile ~forHover =
  let textOpt = Files.readFile currentFile in
  match textOpt with
  | None | Some "" -> None
  | Some text -> (
    match
      CompletionFrontEnd.completionWithParser ~debug ~path ~posCursor:pos
        ~currentFile ~text
    with
    | None -> None
    | Some (completable, scope) -> (
      (* uncomment when debugging *)
      if false then (
        Printf.printf "\nScope from frontend:\n";
        List.iter
          (fun item ->
            Printf.printf "%s\n" (SharedTypes.ScopeTypes.item_to_string item))
          scope;
        print_newline ());
      (* Only perform expensive ast operations if there are completables *)
      match Cmt.loadFullCmtFromPath ~path with
      | None -> None
      | Some full ->
        let env = SharedTypes.QueryEnv.fromFile full.file in
        let completables =
          completable
          |> CompletionBackEnd.processCompletable ~debug ~full ~pos ~scope ~env
               ~forHover
        in
        Some (completables, full, scope)))
