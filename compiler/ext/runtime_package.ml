let name = "@rescript/runtime"

(* This resolves the location of the standard library starting from the location of bsc.exe
   (@rescript/{platform}/bin/bsc.exe), handling different supported package layouts. *)
let path =
  let build_path rest path =
    String.concat Filename.dir_sep (List.rev_append rest path)
  in
  match
    Sys.executable_name |> Filename.dirname
    |> String.split_on_char Filename.dir_sep.[0]
    |> List.rev
  with
  (* 1. Packages installed via pnpm
     - bin:     node_modules/.pnpm/@rescript+darwin-arm64@12.0.0-alpha.13/node_modules/@rescript/darwin-arm64/bin
     - runtime: node_modules/.pnpm/node_modules/@rescript/runtime (symlink)
  *)
  | "bin" :: _platform :: "@rescript" :: "node_modules" :: _package :: ".pnpm"
    :: "node_modules" :: rest ->
    build_path rest
      ["node_modules"; ".pnpm"; "node_modules"; "@rescript"; "runtime"]
  (* 2. Packages installed via npm
     - bin:     node_modules/@rescript/{platform}/bin
     - runtime: node_modules/@rescript/runtime
  *)
  | "bin" :: _platform :: "@rescript" :: "node_modules" :: rest ->
    build_path rest ["node_modules"; "@rescript"; "runtime"]
  (* 3. Several other cases that can occur in local development, e.g.
     - bin:     <repo>/packages/@rescript/{platform}/bin, <repo>/_build/install/default/bin
     - runtime: <repo>/packages/@rescript/runtime
  *)
  | _ :: _ :: _ :: _ :: rest ->
    build_path rest ["packages"; "@rescript"; "runtime"]
  | _ -> ""
