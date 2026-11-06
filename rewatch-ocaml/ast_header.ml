type t = {dependencies: string list; source: string option}

(* A damaged AST, such as one cut short by a killed build, has no usable
   header: it ends early or gives an impossible length. Like a missing AST,
   it makes the module parse again. *)
let damaged = {dependencies = []; source = None}

let of_contents ?length contents =
  Binary_ast.read_header ?length contents
  |> Option.map (fun header ->
      {
        dependencies = Binary_ast.header_dependencies contents header;
        source = Some (Binary_ast.header_source contents header);
      })

(* A header is a few hundred bytes at the start of an AST of tens of
   kilobytes. One read into a small buffer usually covers it; a channel would
   allocate 64 KiB per file and read the whole AST. *)
let header_buffer = Domain.DLS.new_key (fun () -> Bytes.create 4096)

let read_file path =
  let descriptor = File_util.open_for_reading path in
  Fun.protect
    ~finally:(fun () ->
      try Unix.close descriptor with Unix.Unix_error _ -> ())
    (fun () ->
      let rec read_prefix buffer filled =
        if filled = Bytes.length buffer then (buffer, filled)
        else
          match
            Unix.read descriptor buffer filled (Bytes.length buffer - filled)
          with
          | 0 -> (buffer, filled)
          | count -> read_prefix buffer (filled + count)
      in
      let buffer, filled = read_prefix (Domain.DLS.get header_buffer) 0 in
      (* The buffer is reused: only its first [filled] bytes are this file's,
         and the header is copied out of them before the next read. *)
      match of_contents ~length:filled (Bytes.unsafe_to_string buffer) with
      | Some header -> header
      | None when filled < Bytes.length buffer -> damaged
      | None ->
        (* The header may be longer than the buffer. *)
        of_contents (File_util.read_file path) |> Option.value ~default:damaged)

let read path =
  match Build_file_store.cached path with
  | Some contents -> of_contents contents |> Option.value ~default:damaged
  | None -> (
    try read_file path
    with Unix.Unix_error (error, operation, "") ->
      (* Unix.read does not name the file it failed to read. *)
      raise (Unix.Unix_error (error, operation, path)))
