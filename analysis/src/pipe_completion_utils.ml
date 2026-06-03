let add_jsx_completion_items ~main_type_id ~env ~prefix
    ~(full : Shared_types.full) ~raw_opens typ =
  match main_type_id with
  | ("array" | "float" | "string" | "int") as builtin_name_to_complete ->
    if Utils.check_name builtin_name_to_complete ~prefix ~exact:false then
      let name =
        match full.package.generic_jsx_module with
        | None -> "React." ^ builtin_name_to_complete
        | Some g ->
          g ^ "." ^ builtin_name_to_complete
          |> String.split_on_char '.'
          |> Type_utils.remove_opens_from_completion_path ~raw_opens
               ~package:full.package
          |> String.concat "."
      in
      [
        Shared_types.Completion.create name ~synthetic:true
          ~includes_snippets:true ~kind:(Value typ) ~env ~sort_text:"A"
          ~docstring:
            [
              "Turns `" ^ builtin_name_to_complete
              ^ "` into a JSX element so it can be used inside of JSX.";
            ];
      ]
    else []
  | _ -> []
