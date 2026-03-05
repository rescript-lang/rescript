type trace_level = Off | Alloc_only | Alloc_and_ops

let level =
  match Sys.getenv_opt "RESCRIPT_REACTIVE_ALLOC_TRACE" with
  | Some "2" -> Alloc_and_ops
  | Some ("1" | "true" | "TRUE" | "yes" | "YES") -> Alloc_only
  | _ -> Off

let enabled = level <> Off
let ops_enabled = level = Alloc_and_ops

let file_path =
  match Sys.getenv_opt "RESCRIPT_REACTIVE_ALLOC_TRACE_FILE" with
  | Some p when String.length p > 0 -> p
  | _ -> "/tmp/rescript-reactive-alloc-events.log"

let fd : Unix.file_descr option ref = ref None

type alloc_event_kind =
  | Map_create
  | Map_vals_init
  | Table_resize
  | Set_create
  | Set_resize
  | Pool_set_resize
  | Pool_set_miss_create
  | Pool_map_resize
  | Pool_map_miss_create
  | Unknown_alloc

type op_event_kind =
  | Pool_set_drain_key
  | Pool_set_remove_recycle_if_empty
  | Pool_map_drain_outer
  | Pool_map_remove_recycle_if_empty
  | Unknown_op

let get_fd () =
  match !fd with
  | Some f -> f
  | None ->
    let f =
      Unix.openfile file_path
        [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_APPEND; Unix.O_CLOEXEC]
        0o644
    in
    fd := Some f;
    f

let emit_line line =
  if enabled then
    try
      let f = get_fd () in
      ignore (Unix.single_write_substring f line 0 (String.length line))
    with _ -> ()

let emit_alloc_kind kind =
  if enabled then
    match kind with
    | Map_create -> emit_line "[ALLOC_EVT] map_create\n"
    | Map_vals_init -> emit_line "[ALLOC_EVT] map_vals_init\n"
    | Table_resize -> emit_line "[ALLOC_EVT] table_resize\n"
    | Set_create -> emit_line "[ALLOC_EVT] set_create\n"
    | Set_resize -> emit_line "[ALLOC_EVT] set_resize\n"
    | Pool_set_resize -> emit_line "[ALLOC_EVT] pool_set_resize\n"
    | Pool_set_miss_create -> emit_line "[ALLOC_EVT] pool_set_miss_create\n"
    | Pool_map_resize -> emit_line "[ALLOC_EVT] pool_map_resize\n"
    | Pool_map_miss_create -> emit_line "[ALLOC_EVT] pool_map_miss_create\n"
    | Unknown_alloc -> emit_line "[ALLOC_EVT] unknown_alloc\n"

let emit_op_kind kind =
  if ops_enabled then
    match kind with
    | Pool_set_drain_key -> emit_line "[ALLOC_EVT] pool_set_drain_key\n"
    | Pool_set_remove_recycle_if_empty ->
      emit_line "[ALLOC_EVT] pool_set_remove_recycle_if_empty\n"
    | Pool_map_drain_outer -> emit_line "[ALLOC_EVT] pool_map_drain_outer\n"
    | Pool_map_remove_recycle_if_empty ->
      emit_line "[ALLOC_EVT] pool_map_remove_recycle_if_empty\n"
    | Unknown_op -> emit_line "[ALLOC_EVT] unknown_op\n"
