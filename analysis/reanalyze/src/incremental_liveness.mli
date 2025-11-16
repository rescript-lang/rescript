type result = {
  visited: Graph_store.DeclIdSet.t;
  dead_declarations: Common.decl list;
}

val recompute : graph:Graph_store.t -> changed_files:string list -> result

