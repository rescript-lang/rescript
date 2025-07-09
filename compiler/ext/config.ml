let version = "4.06.1+BS"

(* This resolves the location of the standard library starting from the location of bsc.exe,
  handling different supported package layouts. *)
let standard_library =
  let build_path rest path =
    String.concat Filename.dir_sep (List.rev_append rest path)
  in
  match
    Sys.executable_name |> Filename.dirname
    |> String.split_on_char Filename.dir_sep.[0]
    |> List.rev
  with
  (* 1. Packages installed via pnpm
     - bin:    node_modules/.pnpm/@rescript+darwin-arm64@12.0.0-alpha.13/node_modules/@rescript/darwin-arm64/bin
     - stdlib: node_modules/rescript/lib/ocaml (symlink)
  *)
  | "bin" :: _platform :: "@rescript" :: "node_modules" :: _package :: ".pnpm"
    :: "node_modules" :: rest ->
    build_path rest ["node_modules"; "rescript"; "lib"; "ocaml"]
  (* 2. Packages installed via npm
     - bin:    node_modules/@rescript/{platform}/bin
     - stdlib: node_modules/rescript/lib/ocaml
  *)
  | "bin" :: _platform :: "@rescript" :: "node_modules" :: rest ->
    build_path rest ["node_modules"; "rescript"; "lib"; "ocaml"]
  (* 3. Several other cases that can occur in local development, e.g.
     - bin:    <repo>/packages/@rescript/{platform}/bin, <repo>/_build/install/default/bin
     - stdlib: <repo>/lib/ocaml
  *)
  | _ :: _ :: _ :: _ :: rest -> build_path rest ["lib"; "ocaml"]
  | _ -> ""

let standard_library_default = standard_library

let unsafe_empty_array = ref false

let cmi_magic_number = "Caml1999I022"

and ast_impl_magic_number = "Caml1999M022"

and ast_intf_magic_number = "Caml1999N022"

and cmt_magic_number = "Caml1999T022"

let load_path = ref ([] : string list)

(* This is normally the same as in obj.ml, but we have to define it
   separately because it can differ when we're in the middle of a
   bootstrapping phase. *)

let print_config oc =
  let p name valu = Printf.fprintf oc "%s: %s\n" name valu in
  p "version" version;
  p "standard_library_default" standard_library_default;
  p "standard_library" standard_library;

  (* print the magic number *)
  p "cmi_magic_number" cmi_magic_number;
  p "ast_impl_magic_number" ast_impl_magic_number;
  p "ast_intf_magic_number" ast_intf_magic_number;
  p "cmt_magic_number" cmt_magic_number;
  flush oc
