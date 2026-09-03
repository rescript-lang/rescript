let cmi_magic_number = "Caml1999I032"

(* Magic numbers for marshaled values of the *current* parsetree, whose layout
   changes across compiler versions. *)
and ast_impl_magic_number = "ResImpl01307"

and ast_intf_magic_number = "ResIntf01307"

(* Magic numbers of the frozen Parsetree0 (OCaml 4.06) layout used on the
   external-PPX wire. They must never be written in front of a
   current-parsetree value. *)
and ast0_impl_magic_number = "Caml1999M022"

and ast0_intf_magic_number = "Caml1999N022"

and cmt_magic_number = "Caml1999T034"

let load_path = ref ([] : string list)
