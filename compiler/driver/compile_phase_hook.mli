(** Lets an embedding build system see where compiling an implementation moves
    from type checking to code generation, for example to time the two parts
    (the build trace of [RESCRIPT_TRACE]). The hook belongs to the domain that
    runs the compile and does nothing by default. *)

val set_typing_finished : (unit -> unit) -> unit
(** Installs the hook for the current domain. *)

val typing_finished : unit -> unit
(** Called once an implementation is type-checked, before it is translated to
    Lambda and compiled to JavaScript. *)
