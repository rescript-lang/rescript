(** An immutable, indexed image of the type expressions in a compiled
    interface. The image contains no mutable compiler nodes and can be shared
    between domains. [thaw] creates request-local [Types.type_expr] nodes.

    This is the type-graph part of the immutable-interface experiment. A full
    interface image must also encode declarations, identifiers, and paths so
    that all roots use the same identity table. *)

type t

val freeze :
  ?identifiers:Ident.t list -> Types.type_expr list -> (t, string) result
(** Capture roots and their reachable graph. Active copy marks and abbreviation
    memo entries are rejected; saved interfaces must not contain either.
    [identifiers] registers signature binders in the same identifier table as
    paths inside the type graph. *)

type view

val create_view :
  ?map_type_path:(Path.t -> Path.t) ->
  ?map_modtype_path:(Path.t -> Path.t) ->
  t ->
  view
(** A request-local memo for lazy materialization. Do not share a view between
    compiler requests. The callbacks apply the request's path substitution
    while a type node is materialized. *)

val type_at : view -> int -> Types.type_expr
(** Materialize a zero-based root, preserving sharing with any other roots
    already materialized through this view. *)

val identifier_at : view -> int -> Ident.t
(** Materialize a zero-based identifier passed to [freeze]. A matching path
    inside a type root receives this exact identifier object. *)

val thaw : t -> Types.type_expr list
(** Materialize an independent graph, preserving sharing among its roots. *)

val thaw_root : t -> int -> Types.type_expr
(** Materialize only nodes reachable from one zero-based root. This is a
    request-local fallback for consumers which cannot read the frozen graph
    directly. *)

val root_count : t -> int

val node_count : t -> int
