type t = {dependencies: string list; source: string option}

(* The header of an AST's contents, or of a prefix of them. A damaged AST,
   such as one cut short by a killed build, has no usable header: it ends
   early or gives an impossible length. Like a missing AST, it makes the
   module parse again. *)
let read_contents contents =
  let length = String.length contents in
  if length < 4 then {dependencies = []; source = None}
  else
    let dependency_length = Int32.to_int (String.get_int32_be contents 0) in
    if dependency_length < 0 || dependency_length > length - 4 then
      {dependencies = []; source = None}
    else
      let dependencies =
        String.sub contents 4 dependency_length
        |> String.split_on_char '\n'
        |> List.filter (fun name -> name <> "")
      in
      let source_start = 4 + dependency_length in
      let source =
        if source_start >= length then None
        else
          let source_end =
            String.index_from_opt contents source_start '\n'
            |> Option.value ~default:length
          in
          Some (String.sub contents source_start (source_end - source_start))
      in
      {dependencies; source}

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
      let header_end prefix length =
        (* The source path ends at the first newline after the dependency
           block. *)
        if length < 4 then None
        else
          let dependency_length = Int32.to_int (Bytes.get_int32_be prefix 0) in
          if dependency_length < 0 then Some length
          else
            let source_start = 4 + dependency_length in
            (* The buffer is reused: only its first [length] bytes are this
               file's. *)
            let rec newline index =
              if index >= length then None
              else if Bytes.get prefix index = '\n' then Some (index + 1)
              else newline (index + 1)
            in
            newline source_start
      in
      (* The prefix ends inside the header only when the file is longer than
         the buffer: read the rest, which [read_contents] then bounds. *)
      match header_end buffer filled with
      | Some length -> read_contents (Bytes.sub_string buffer 0 length)
      | None when filled < Bytes.length buffer ->
        read_contents (Bytes.sub_string buffer 0 filled)
      | None -> read_contents (File_util.read_file path))

let read path =
  match Build_file_store.cached path with
  | Some contents -> read_contents contents
  | None -> read_file path
