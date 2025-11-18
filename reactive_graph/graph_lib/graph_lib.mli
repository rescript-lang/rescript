type node_id = int

type node = {
  id : node_id;
  marked : bool;
}

type edge = node_id * node_id

type file_graph = {
  file : string;
  nodes : node list;
  edges : edge list;
}

exception Duplicate_node_in_file of {
  file : string;
  node : node_id;
}

exception Duplicate_node_across_files of {
  node : node_id;
  first_file : string;
  second_file : string;
}

exception Edge_source_not_local of {
  file : string;
  src : node_id;
  dst : node_id;
}

exception Edge_target_undefined of {
  file : string;
  src : node_id;
  dst : node_id;
}

val parse_file : file:string -> string -> file_graph
(** Parse a file definition. Lines use the form [node <int> [marked]] or [edge <src> <dst>]. *)

val load_files : string list -> file_graph list
(** Read and parse the provided files. *)

val unreachable_nodes : file_graph list -> node_id list
(** Compute unreachable nodes in ascending order. *)

