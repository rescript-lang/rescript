(** Magic numbers of the frozen Parsetree0 (OCaml 4.06) layout used on the
    external-PPX wire. They never change. *)

val impl : string
(** The magic number of a v0 implementation syntax tree. *)

val intf : string
(** The magic number of a v0 interface syntax tree. *)
