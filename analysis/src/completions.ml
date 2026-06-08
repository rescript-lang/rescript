let get_completions ~debug ~source ~kind_file ~pos ~for_hover
    ~(full : Shared_types.full option) ~state =
  match source with
  | "" -> None
  | source -> (
    match
      Completion_front_end.completion_with_parser ~debug ~source ~kind_file
        ~pos_cursor:pos
    with
    | None -> None
    | Some (completable, scope) -> (
      (* uncomment when debugging *)
      if false then (
        Printf.printf "\nScope from frontend:\n";
        List.iter
          (fun item ->
            Printf.printf "%s\n" (Shared_types.Scope_types.item_to_string item))
          scope;
        print_newline ());
      (* Only perform expensive ast operations if there are completables *)
      match full with
      | None -> None
      | Some full ->
        let env = Shared_types.Query_env.from_file full.file in
        let completables =
          completable
          |> Completion_back_end.process_completable ~debug ~full ~state ~pos
               ~scope ~env ~for_hover
        in
        Some (completables, full, scope)))
