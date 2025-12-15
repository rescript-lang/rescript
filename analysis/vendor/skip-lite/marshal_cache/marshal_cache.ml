(* Marshal Cache - OCaml implementation *)

exception Cache_error of string * string

type stats = {
  entry_count : int;
  mapped_bytes : int;
}

(* Register the exception with the C runtime for proper propagation *)
let () = Callback.register_exception
  "Marshal_cache.Cache_error"
  (Cache_error ("", ""))

(* External C stubs *)
external with_unmarshalled_file_stub : string -> ('a -> 'r) -> 'r
  = "mfc_with_unmarshalled_file"

external with_unmarshalled_if_changed_stub : string -> ('a -> 'r) -> 'r option
  = "mfc_with_unmarshalled_if_changed"

external clear_stub : unit -> unit = "mfc_clear"
external invalidate_stub : string -> unit = "mfc_invalidate"
external set_max_entries_stub : int -> unit = "mfc_set_max_entries"
external set_max_bytes_stub : int -> unit = "mfc_set_max_bytes"
external stats_stub : unit -> int * int = "mfc_stats"

(* Public API *)

let convert_failure path msg =
  (* C code raises Failure with "path: message" format *)
  (* Only convert if message starts with the path (i.e., from our C code) *)
  let prefix = path ^ ": " in
  let prefix_len = String.length prefix in
  if String.length msg >= prefix_len && String.sub msg 0 prefix_len = prefix then
    let error_msg = String.sub msg prefix_len (String.length msg - prefix_len) in
    raise (Cache_error (path, error_msg))
  else
    (* Re-raise user callback exceptions as-is *)
    raise (Failure msg)

let with_unmarshalled_file path f =
  try
    with_unmarshalled_file_stub path f
  with
  | Failure msg -> convert_failure path msg
  [@@alert "-unsafe"]

let with_unmarshalled_if_changed path f =
  try
    with_unmarshalled_if_changed_stub path f
  with
  | Failure msg -> convert_failure path msg
  [@@alert "-unsafe"]

let clear () = clear_stub ()

let invalidate path = invalidate_stub path

let set_max_entries n =
  if n < 0 then invalid_arg "Marshal_cache.set_max_entries: negative value";
  set_max_entries_stub n

let set_max_bytes n =
  if n < 0 then invalid_arg "Marshal_cache.set_max_bytes: negative value";
  set_max_bytes_stub n

let stats () =
  let (entry_count, mapped_bytes) = stats_stub () in
  { entry_count; mapped_bytes }

