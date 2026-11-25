let key : (unit -> unit) Domain.DLS.key = Domain.DLS.new_key (fun () () -> ())

let set_typing_finished hook = Domain.DLS.set key hook

let typing_finished () = Domain.DLS.get key ()
