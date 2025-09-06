(* Helpers for domain-local caches *)

let make_hashtbl (size : int) : ('k, 'v) Hashtbl.t Domain.DLS.key =
  Domain.DLS.new_key (fun () -> Hashtbl.create size)

let get_hashtbl (key : ('k, 'v) Hashtbl.t Domain.DLS.key) : ('k, 'v) Hashtbl.t =
  Domain.DLS.get key
