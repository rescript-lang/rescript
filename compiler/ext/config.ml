let cmi_magic_number = "Caml1999I022"

and ast_impl_magic_number = "Caml1999M022"

and ast_intf_magic_number = "Caml1999N022"

(* Magic numbers for marshaled values of the *current* parsetree, whose layout
   changes across compiler versions. The [ast_impl_magic_number] /
   [ast_intf_magic_number] pair above identifies the frozen Parsetree0 (OCaml
   4.06) layout used on the external-PPX wire and must never be written in
   front of a current-parsetree value. *)
and res_ast_impl_magic_number = "ResImpl01300"

and res_ast_intf_magic_number = "ResIntf01300"

and cmt_magic_number = "Caml1999T022"

let load_path = ref ([] : string list)
