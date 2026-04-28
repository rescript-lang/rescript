let name = "@rescript/runtime"

(* Simple default approach to find the runtime package path. This will not work with all package managers/layouts. *)
let default_path =
  let build_path rest path =
    String.concat Filename.dir_sep (List.rev_append rest path)
  in
  match
    Sys.executable_name |> Filename.dirname
    |> String.split_on_char Filename.dir_sep.[0]
    |> List.rev
  with
  (* 1. Packages installed via npm
     - bin:     node_modules/@rescript/{platform}/bin
     - runtime: node_modules/@rescript/runtime
  *)
  | "bin" :: _platform :: "@rescript" :: "node_modules" :: rest ->
    build_path rest ["node_modules"; "@rescript"; "runtime"]
  (* 2. Several other cases that can occur in local development, e.g.
     - bin:     <repo>/packages/@rescript/{platform}/bin, <repo>/_build/install/default/bin
     - runtime: <repo>/packages/@rescript/runtime
  *)
  | _ :: _ :: _ :: _ :: rest ->
    build_path rest ["packages"; "@rescript"; "runtime"]
  | _ -> ""

(* To support pnpm and other package managers/layouts, we determine the path on the JS side and pass it in
via -runtime-path to override the default. *)
let path = ref default_path
