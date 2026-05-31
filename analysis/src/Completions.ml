let get_completions ~debug ~source ~kind_file ~pos ~for_hover
    ~(full : SharedTypes.full option) =
  match source with
  | "" -> None
  | source -> (
    match
      CompletionFrontEnd.completion_with_parser ~debug ~source ~kind_file
        ~pos_cursor:pos
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
        let env = SharedTypes.QueryEnv.from_file full.file in
        let completables =
          completable
          |> CompletionBackEnd.process_completable ~debug ~full ~pos ~scope ~env
               ~for_hover
        in
        Some (completables, full, scope)))
