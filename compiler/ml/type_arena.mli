(** Conversions between the boxed type graphs of {!Types_boxed}, the format
    of [.cmi] and [.cmt] files, and the stores of {!Types}.

    Conversions run in sessions. A session memoizes every converted record
    and block by physical identity, so it maps a graph with sharing and cycles
    to a graph with the same sharing and cycles; use one session for
    everything that must keep its sharing (for example a whole signature, or
    everything a [.cmt] holds). Identifiers and levels are copied, so ids, and
    the marshalled bytes of an exported graph, do not change: exporting an
    imported signature reproduces the bytes it was read from.

    While a session runs it temporarily marks the abbreviation cells
    ([Tconstr]'s [abbrev_memo ref]) of the source graph, as a copy marks
    nodes with [Tsubst]: nothing else may read the source graph until the
    session ends. The source graph must not change during the session.

    The design is described in rescript-perf-investigation
    [results/arena/DESIGN.md]. *)

module Import : sig
  type t

  val run : ?store:Types.Store.t -> (t -> 'a) -> 'a
  (** [run f] runs a session importing into [store], by default the current
      store at the time of the call. *)

  val type_expr : t -> Types_boxed.type_expr -> Types.type_expr
  val type_desc : t -> Types_boxed.type_desc -> Types.type_desc
  val row_desc : t -> Types_boxed.row_desc -> Types.row_desc
  val signature : t -> Types_boxed.signature -> Types.signature
  val module_type : t -> Types_boxed.module_type -> Types.module_type

  val value_description :
    t -> Types_boxed.value_description -> Types.value_description

  val type_declaration :
    t -> Types_boxed.type_declaration -> Types.type_declaration

  val extension_constructor :
    t -> Types_boxed.extension_constructor -> Types.extension_constructor

  val constructor_description :
    t -> Types_boxed.constructor_description -> Types.constructor_description

  val label_description :
    t -> Types_boxed.label_description -> Types.label_description

  val module_declaration :
    t -> Types_boxed.module_declaration -> Types.module_declaration

  val modtype_declaration :
    t -> Types_boxed.modtype_declaration -> Types.modtype_declaration

  val nodes : t -> int
  (** Number of nodes created so far. *)
end

module Export : sig
  type t

  val run : (t -> 'a) -> 'a
  val type_expr : t -> Types.type_expr -> Types_boxed.type_expr
  val type_desc : t -> Types.type_desc -> Types_boxed.type_desc
  val row_desc : t -> Types.row_desc -> Types_boxed.row_desc
  val signature : t -> Types.signature -> Types_boxed.signature
  val module_type : t -> Types.module_type -> Types_boxed.module_type

  val value_description :
    t -> Types.value_description -> Types_boxed.value_description

  val type_declaration :
    t -> Types.type_declaration -> Types_boxed.type_declaration

  val extension_constructor :
    t -> Types.extension_constructor -> Types_boxed.extension_constructor

  val constructor_description :
    t -> Types.constructor_description -> Types_boxed.constructor_description

  val label_description :
    t -> Types.label_description -> Types_boxed.label_description

  val module_declaration :
    t -> Types.module_declaration -> Types_boxed.module_declaration

  val modtype_declaration :
    t -> Types.modtype_declaration -> Types_boxed.modtype_declaration
end

(** Memo tables by physical identity, for converters of structures holding
    types (the typed tree's environments). Blocks are hashed by their
    contents to a small depth and compared with [==]; immediate values are not
    memoized. *)
module Memo : sig
  type ('a, 'b) t

  val create : unit -> ('a, 'b) t
  val memo : ('a, 'b) t -> ('a -> 'b) -> 'a -> 'b
end
