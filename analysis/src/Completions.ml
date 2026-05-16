let getCompletions ~debug ~source ~kindFile ~pos ~forHover
    ~(full : SharedTypes.full option) =
  match source with
  | "" -> None
  | source -> (
    match
      CompletionFrontEnd.completionWithParser ~debug ~source ~kindFile
        ~posCursor:pos
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
      match full with
      | None -> None
      | Some full ->
        let env = SharedTypes.QueryEnv.fromFile full.file in
        let completables =
          completable
          |> CompletionBackEnd.processCompletable ~debug ~full ~pos ~scope ~env
               ~forHover
        in
        Some (completables, full, scope)))
