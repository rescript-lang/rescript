(** Interfaces published in lib/ocaml, compared by what their dependents read
    of them: an interface's own CRC and flags, and the CRCs that a module
    recorded for the interfaces it was compiled against. *)

val publish : source:string -> destination:string -> Build_state.cmi_change
(** Publishes the interface [source] at [destination]. It is [Cmi_changed]
    only when its own CRC or its flags changed. An interface that only records
    other CRCs for its imports leaves its dependents valid: it is written, but
    keeps its modification time, so that later builds do not take it for a new
    one. *)

val imports_changed :
  published:(string -> string option) ->
  cmi_path:string ->
  has_interface:bool ->
  bool
(** Whether an interface that the module at [cmi_path] was compiled against
    changed since: its CRC differs from the one the module recorded, in its
    published .cmi, or in its .cmt for the implementation of a module with an
    interface file. [published unit] is the path of the published interface of
    a unit of this build; imports from outside the build are not checked. A
    module whose files cannot be read counts as changed. *)
