@@warning("-20-21")
type tag
type queue_elem<'a>
type queue<'a> = {
  mutable insert: queue_elem<'a>,
  mutable body: queue_elem<'a>,
}
type size
type pp_token
let peek_queue = _ => assert(false)
let int_of_size = _ => assert(false)
let take_queue = _ => assert(false)
let format_pp_token = (_, _, _) => assert(false)
let pp_infinity = 1000000010
type pp_queue_elem = {
  mutable elem_size: size,
  token: pp_token,
  length: int,
}

type formatter = {
  mutable pp_space_left: int,
  mutable pp_left_total: int,
  mutable pp_right_total: int,
  mutable pp_queue: queue<pp_queue_elem>,
}
let rec advance_loop = state =>
  switch peek_queue(state.pp_queue) {
  | {elem_size: size, token: tok, length: len} =>
    let size = int_of_size(size)
    if !(size < 0 && state.pp_right_total - state.pp_left_total < state.pp_space_left) {
      ignore(take_queue(state.pp_queue))
      format_pp_token(
        state,
        if size < 0 {
          pp_infinity
        } else {
          size
        },
        tok,
      )
      state.pp_left_total = len + state.pp_left_total
      advance_loop(state)
    }
  }
