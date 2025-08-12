(* To determine the standard library path in a reliable way that works with all package managers /
   package directory layouts, we need to do it on the JS side, see cli/common/stdlib.js.
   We pass the directory to the compiler exe via the environment variable RESCRIPT_STDLIB. *)
let standard_library =
  match Sys.getenv_opt "RESCRIPT_STDLIB" with
  | Some path -> path
  | None -> ""

let cmi_magic_number = "Caml1999I022"

and ast_impl_magic_number = "Caml1999M022"

and ast_intf_magic_number = "Caml1999N022"

and cmt_magic_number = "Caml1999T022"

let load_path = ref ([] : string list)
