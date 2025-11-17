module DeclId : sig
  type t

  val compare : t -> t -> int
  val to_string : t -> string
end

module DeclIdSet : Set.S with type elt = DeclId.t

type node = {
  id: DeclId.t;
  file: string;
  decl: Common.decl;
  summary: Summary.decl;
}

type t

val create : unit -> t
val add_summary : t -> Summary.t -> unit
val get_dirty_files : t -> string list
val find_node : t -> DeclId.t -> node option
val find_node_by_position : t -> Lexing.position -> node option
val annotation_snapshot_of_lexing_position :
  t -> Lexing.position -> Summary.annotation_snapshot option
val file_decls : t -> string -> DeclId.t list
val successors : t -> kind:[`Value | `Type] -> DeclId.t -> DeclIdSet.t
val reverse_successors : t -> kind:[`Value | `Type] -> DeclId.t -> DeclIdSet.t
val frontier : t -> changed_files:string list -> DeclIdSet.t
val has_unknown_value_ref : t -> DeclId.t -> bool
val has_unknown_type_ref : t -> DeclId.t -> bool
val unknown_value_ids : t -> DeclId.t list
val ordered_files : t -> (string, int) Hashtbl.t
val compare_decl_ids :
  t ->
  ordered_files:(string, int) Hashtbl.t ->
  DeclId.t ->
  DeclId.t ->
  int
val debug_log_nodes : t -> string list -> unit

