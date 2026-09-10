(** The reactive pipeline for dead code analysis.

    One collection of per-file data feeding merge, liveness and the solver.
    Downstream collections update on their own as files are added, changed or
    removed, so a warm pipeline can be reused across runs and across server
    requests. *)

type t = {
  collection: Reactive_analysis.t;
  merged: Reactive_merge.t;
  liveness: Reactive_liveness.t;
  solver: Reactive_solver.t;
}

let create ~(config : Dce_config.t) : t =
  let collection = Reactive_analysis.create ~config in
  let merged =
    Reactive_merge.create (Reactive_analysis.to_file_data_collection collection)
  in
  let liveness = Reactive_liveness.create ~merged in
  let solver =
    (* value_refs_from feeds hasRefBelow, needed when transitive = false *)
    let value_refs_from =
      if config.Dce_config.run.transitive then None
      else Some merged.Reactive_merge.value_refs_from
    in
    Reactive_solver.create ~decls:merged.Reactive_merge.decls
      ~live:liveness.Reactive_liveness.live
      ~annotations:merged.Reactive_merge.annotations ~value_refs_from ~config
  in
  {collection; merged; liveness; solver}
