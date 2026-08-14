let cmi_magic_number = "Caml1999I023"

(* Magic numbers for marshaled values of the *current* parsetree, whose layout
   changes across compiler versions. *)
and ast_impl_magic_number = "ResImpl01300"

and ast_intf_magic_number = "ResIntf01300"

(* Magic numbers of the frozen Parsetree0 (OCaml 4.06) layout used on the
   external-PPX wire. They must never be written in front of a
   current-parsetree value. *)
and ast0_impl_magic_number = "Caml1999M022"

and ast0_intf_magic_number = "Caml1999N022"

and cmt_magic_number = "Caml1999T023"

let load_path = ref ([] : string list)
