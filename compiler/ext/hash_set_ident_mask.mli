type ident = Ident.t
(** Based on [hash_set] specialized for mask operations  *)

type t

val create : int -> t

(* add one ident
   ident is unmaksed by default
*)
val add_unmask : t -> ident -> unit

val mask_and_check_all_hit : t -> ident -> bool
(** [check_mask h key] if [key] exists mask it otherwise nothing
    return true if all keys are masked otherwise false
*)

val iter_and_unmask : t -> (ident -> bool -> unit) -> unit
(** [iter_and_unmask f h] iterating the collection and mask all idents,
    dont consul the collection in function [f]
    TODO: what happens if an exception raised in the callback,
    would the hashtbl still be in consistent state?
*)
