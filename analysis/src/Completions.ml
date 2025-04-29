let getCompletions (debug : bool) ~path ~pos ~currentFile ~forHover =
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

let getCompletionsRevamped ?(source = None) ~debug ~path ~pos ~currentFile =
  let textOpt = Files.readFile currentFile in
  match textOpt with
  | None | Some "" -> None
  | Some text -> (
    match
      CompletionFrontEndRevamped.completionWithParser ~debug ~path
        ~posCursor:pos ~currentFile ~text
    with
    | None ->
      source
      |> Option.iter (fun _ ->
             print_endline "Completion Frontend did not return completable");
      None
    | Some (completable, scope) -> (
      let _ =
        match source with
        | Some text -> (
          match SharedTypes.CompletableRevamped.try_loc completable with
          | Some loc ->
            let range =
              CodeFence.
                {
                  start = loc.Location.loc_start.pos_cnum;
                  finish = loc.Warnings.loc_end.pos_cnum;
                }
            in
            Printf.printf "Found Completable: %s\n\n"
              (SharedTypes.CompletableRevamped.toString completable);
            CodeFence.format_code_snippet_cropped text (Some range) 3
            |> print_endline
          | None -> ())
        | None -> ()
      in

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
