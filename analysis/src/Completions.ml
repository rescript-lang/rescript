let getCompletions ~debug ~path ~pos ~currentFile ~forHover =
  ignore forHover;
  let textOpt = Files.readFile currentFile in
  match textOpt with
  | None | Some "" -> None
  | Some text -> (
    match
      CompletionFrontEndRevamped.completionWithParser ~debug ~path
        ~posCursor:pos ~currentFile ~text
    with
    | None -> None
    | Some (completable, scope) -> (
      (* Only perform expensive ast operations if there are completables *)
      match Cmt.loadFullCmtFromPath ~path with
      | None -> None
      | Some full ->
        let env = SharedTypes.QueryEnv.fromFile full.file in
        let completables =
          completable
          |> CompletionBackEndRevamped.processCompletable ~debug ~full ~pos
               ~scope ~env
        in
        Some (completables, full, scope)))
