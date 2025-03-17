let addJsxCompletionItems ~mainTypeId ~env ~prefix ~(full : SharedTypes.full)
    ~rawOpens typ =
  match mainTypeId with
  | ("array" | "float" | "string" | "int") as builtinNameToComplete ->
    if Utils.checkName builtinNameToComplete ~prefix ~exact:false then
      let name =
        match full.package.genericJsxModule with
        | None -> "React." ^ builtinNameToComplete
        | Some g ->
          g ^ "." ^ builtinNameToComplete
          |> String.split_on_char '.'
          |> TypeUtils.removeOpensFromCompletionPath ~rawOpens
               ~package:full.package
          |> String.concat "."
      in
      [
        SharedTypes.Completion.create name ~synthetic:true
          ~includesSnippets:true ~kind:(Value typ) ~env ~sortText:"A"
          ~docstring:
            [
              "Turns `" ^ builtinNameToComplete
              ^ "` into a JSX element so it can be used inside of JSX.";
            ];
      ]
    else []
  | _ -> []
