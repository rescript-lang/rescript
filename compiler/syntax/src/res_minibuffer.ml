type t = {mutable buffer: bytes; mutable position: int; mutable length: int}

let create n =
  let n = if n < 1 then 1 else n in
  let s = (Bytes.create [@doesNotRaise]) n in
  {buffer = s; position = 0; length = n}

let contents b = (Bytes.sub_string [@doesNotRaise]) b.buffer 0 b.position

(* Can't be called directly, don't add to the interface *)
let resize_internal b more =
  let len = b.length in
  let new_len = ref len in
  while b.position + more > !new_len do
    new_len := 2 * !new_len
  done;
  if !new_len > Sys.max_string_length then
    if b.position + more <= Sys.max_string_length then
      new_len := Sys.max_string_length;
  let new_buffer = (Bytes.create [@doesNotRaise]) !new_len in
  (* PR#6148: let's keep using [blit] rather than [unsafe_blit] in
     this tricky function that is slow anyway. *)
  Bytes.blit b.buffer 0 new_buffer 0 b.position [@doesNotRaise];
  b.buffer <- new_buffer;
  b.length <- !new_len

let add_char b c =
  let pos = b.position in
  if pos >= b.length then resize_internal b 1;
  Bytes.unsafe_set b.buffer pos c;
  b.position <- pos + 1

let add_string b s =
  let len = String.length s in
  let new_position = b.position + len in
  if new_position > b.length then resize_internal b len;
  Bytes.blit_string s 0 b.buffer b.position len [@doesNotRaise];
  b.position <- new_position

(* adds newline and trims all preceding whitespace *)
let flush_newline b =
  let position = ref b.position in
  while Bytes.unsafe_get b.buffer (!position - 1) = ' ' && !position >= 0 do
    position := !position - 1
  done;
  b.position <- !position;
  add_char b '\n'
