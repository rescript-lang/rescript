type t

val create : output_prefix:string -> t
val dump_lam : t -> pass:string -> Lam.t -> unit
val dump_groups : t -> Lam_group.t list -> unit
val dump_js : t -> pass:string -> J.program -> unit
