(* Pull requests never change the magic numbers below: [yarn changelog:release]
   bumps their trailing counters when the format sources changed since the
   previous release (see scripts/magicNumbers.js). The .cmi and .cmt numbers
   must have the same length, since a .cmt reader compares its first bytes with
   both. *)
let cmi_magic_number = "ResCmi040"

(* Magic numbers for marshaled values of the *current* parsetree, whose layout
   changes across compiler versions. *)
and ast_impl_magic_number = "ResImpl01317"

and ast_intf_magic_number = "ResIntf01317"

(* Magic numbers of the frozen Parsetree0 (OCaml 4.06) layout used on the
   external-PPX wire. They must never be written in front of a
   current-parsetree value. *)
and ast0_impl_magic_number = "Caml1999M022"

and ast0_intf_magic_number = "Caml1999N022"

and cmt_magic_number = "ResCmt048"

let get_load_path () = (Compiler_request_state.current ()).load_path
let set_load_path path = (Compiler_request_state.current ()).load_path <- path
