open Lsp

module Open_compiled_file = struct
  let command_name = "rescript.openCompiled"
  let meth = "textDocument/openCompiled"
  let create ~(uri : Uri.t) ~(state : State.t) =
    let compiler_config = State.compiler_config state in
    let config_roots =
      compiler_config |> Compiler_config.Uri_map.to_seq
      |> Seq.map (fun (uri, config) -> (uri |> Uri.to_path, config))
      |> List.of_seq |> List.map fst
    in

    let config =
      match
        config_roots
        |> Helpers.best_root_match
             ~path:(State.workspace_root state |> Uri.to_path)
      with
      | Some root ->
        Compiler_config.Uri_map.find_opt (Uri.of_path root) compiler_config
      | None -> None
    in
    match config with
    | Some config ->
      let suffix, js_folder, in_source =
        Compiler_config.get_suffix_and_folder config
      in

      let ( /+ ) = Filename.concat in

      let js_file_path =
        let path = Uri.to_path uri in

        let file_path =
          if in_source then
            let filename = Filename.basename path in
            let js_file = Filename.remove_extension filename ^ suffix in
            Some (Filename.dirname path /+ js_file)
          else
            match Helpers.best_root_match ~path config_roots with
            | Some package_root_path ->
              (*
                Some example with sources
                package_root_path: /home/pedro/Desktop/projects/rescript-lang.org/apps/guide
                path:              /home/pedro/Desktop/projects/rescript-lang.org/apps/guide/app/GuideHome.res
                compiled js:       /home/pedro/Desktop/projects/rescript-lang.org/apps/guide/lib/es6/app/GuideHome.jsx
              *)
              let relative_to ~root path =
                let root =
                  if String.ends_with ~suffix:"/" root then root else root ^ "/"
                in

                if String.starts_with ~prefix:root path then
                  let root_len = String.length root in
                  String.sub path root_len (String.length path - root_len)
                else path
              in
              (* app/GuideHome.res *)
              let relative_path = relative_to ~root:package_root_path path in
              let sub_folders = Filename.dirname relative_path in
              let js_file =
                let res_file = Filename.basename relative_path in
                Filename.remove_extension res_file ^ suffix
              in
              Some
                (package_root_path /+ "lib" /+ js_folder /+ sub_folders
               /+ js_file)
            | None -> None
        in
        match file_path with
        | Some file_path when Analysis.Files.exists file_path ->
          Some (file_path |> Uri.of_path)
        | _ -> None
      in

      js_file_path
    | None -> None
end
