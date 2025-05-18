(* This resolves the location of the standard library starting from the location of bsc.exe
   (@rescript/{platform}/bin/bsc.exe), assuming the standard npm node_modules layout.

   For pnpm and other special node_modules layouts, the correct path shall be passed
   via the `-runtime` flag. *)
let standard_library =
  let exe_path = Sys.executable_name in
  let bin_dir = Filename.dirname exe_path in
  let platform_dir = Filename.dirname bin_dir in
  let rescript_dir = Filename.dirname platform_dir in
  let ( // ) = Filename.concat in
  rescript_dir // "runtime" // "lib" // "ocaml"

let cmi_magic_number = "Caml1999I022"

and ast_impl_magic_number = "Caml1999M022"

and ast_intf_magic_number = "Caml1999N022"

and cmt_magic_number = "Caml1999T022"

let load_path = ref ([] : string list)
