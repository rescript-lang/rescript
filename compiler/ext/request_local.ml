type 'a key = 'a Domain.DLS.key

(* Installs a fresh value for one key and returns how to restore the previous
   one. Keys are registered while modules initialize, so worker domains only
   ever read this list. *)
type entry = unit -> unit -> unit

let entries : entry list Atomic.t = Atomic.make []

let rec register entry =
  let current = Atomic.get entries in
  if not (Atomic.compare_and_set entries current (entry :: current)) then
    register entry

let new_key init =
  let key = Domain.DLS.new_key init in
  register (fun () ->
      let previous = Domain.DLS.get key in
      Domain.DLS.set key (init ());
      fun () -> Domain.DLS.set key previous);
  key

let get = Domain.DLS.get
let set = Domain.DLS.set

let with_fresh action =
  (* [entries] lists the newest key first. Initialize in registration order,
     which is module initialization order, so every request allocates its
     fresh state the same way, and restore in reverse. *)
  let restores =
    List.fold_left
      (fun restores fresh -> fresh () :: restores)
      []
      (List.rev (Atomic.get entries))
  in
  Fun.protect action ~finally:(fun () ->
      List.iter (fun restore -> restore ()) restores)
