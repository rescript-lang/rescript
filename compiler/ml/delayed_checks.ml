(* A delayed check closes over the typing environment and warning state of the
   request that registered it. Another domain must not reset or force it. *)
let delayed_checks = Domain.DLS.new_key (fun () -> ref [])
let checks () = Domain.DLS.get delayed_checks
let reset_delayed_checks () = checks () := []
let add_delayed_check f = checks () := (f, Warnings.backup ()) :: !(checks ())

let force_delayed_checks () =
  (* checks may change type levels *)
  let snap = Btype.snapshot () in
  let w_old = Warnings.backup () in
  List.iter
    (fun (f, w) ->
      Warnings.restore w;
      f ())
    (List.rev !(checks ()));
  Warnings.restore w_old;
  reset_delayed_checks ();
  Btype.backtrack snap
