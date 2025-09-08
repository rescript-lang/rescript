type t = {
  mutable buffer: bytes,
  mutable position: int,
  mutable length: int,
}

let create = n => {
  let n = if n < 1 {
    1
  } else {
    n
  }
  let s = (@doesNotRaise Bytes.create)(n)
  {buffer: s, position: 0, length: n}
}

let contents = b => (@doesNotRaise Bytes.sub_string)(b.buffer, 0, b.position)

/* Can't be called directly, don't add to the interface */
let resize_internal = (b, more) => {
  let len = b.length
  let new_len = ref(len)
  while b.position + more > new_len.contents {
    new_len := 2 * new_len.contents
  }
  if new_len.contents > Sys.max_string_length {
    if b.position + more <= Sys.max_string_length {
      new_len := Sys.max_string_length
    }
  }
  let new_buffer = (@doesNotRaise Bytes.create)(new_len.contents)
  /* PR#6148: let's keep using [blit] rather than [unsafe_blit] in
   this tricky function that is slow anyway. */

  @doesNotRaise
  Bytes.blit(b.buffer, 0, new_buffer, 0, b.position)
  b.buffer = new_buffer
  b.length = new_len.contents
}

let add_char = (b, c) => {
  let pos = b.position
  if pos >= b.length {
    resize_internal(b, 1)
  }
  Bytes.unsafe_set(b.buffer, pos, c)
  b.position = pos + 1
}

let add_string = (b, s) => {
  let len = String.length(s)
  let new_position = b.position + len
  if new_position > b.length {
    resize_internal(b, len)
  }

  @doesNotRaise
  Bytes.blit_string(s, 0, b.buffer, b.position, len)
  b.position = new_position
}

/* adds newline and trims all preceding whitespace */
let flush_newline = b => {
  let position = ref(b.position)
  while Bytes.unsafe_get(b.buffer, position.contents - 1) == ' ' && position.contents >= 0 {
    position := position.contents - 1
  }
  b.position = position.contents
  add_char(b, '\n')
}
