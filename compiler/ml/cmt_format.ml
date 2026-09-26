include Cmt_format_common

let set_args = Cmt_format_persistence.set_args

let last_saved = Domain.DLS.new_key (fun () -> ref None)
let capture_key = Domain.DLS.new_key (fun () -> None)

let with_capture capture action =
  let previous = Domain.DLS.get capture_key in
  Domain.DLS.set capture_key (Some capture);
  Fun.protect action ~finally:(fun () -> Domain.DLS.set capture_key previous)

let clear () =
  Cmt_format_common.clear ();
  Domain.DLS.get last_saved := None

let last_saved_cmt () = !(Domain.DLS.get last_saved)

let save_cmt filename modname binary_annots sourcefile initial_env cmi =
  Domain.DLS.get last_saved :=
    Cmt_format_persistence.save_cmt filename modname binary_annots sourcefile
      initial_env cmi;
  Option.iter
    (fun capture -> Option.iter (capture filename) (last_saved_cmt ()))
    (Domain.DLS.get capture_key);
  Cmt_format_common.clear ()
