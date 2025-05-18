let version = "4.06.1+BS"

let standard_library =
  let ( // ) = Filename.concat in
  (* @rescript/{platform}/bin/rescript.exe *)
  let exe_path = Sys.executable_name in
  let bin_dir = Filename.dirname exe_path in
  let platform_dir = Filename.dirname bin_dir in
  let rescript_dir = Filename.dirname platform_dir in
  rescript_dir // "runtime" // "lib" // "ocaml"

let unsafe_empty_array = ref false

let cmi_magic_number = "Caml1999I022"

and ast_impl_magic_number = "Caml1999M022"

and ast_intf_magic_number = "Caml1999N022"

and cmt_magic_number = "Caml1999T022"

let load_path = ref ([] : string list)
