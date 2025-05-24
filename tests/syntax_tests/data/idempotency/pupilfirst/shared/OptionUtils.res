let default = (e, v) =>
  switch v {
  | Some(v) => v
  | None => e
  }

// TODO: Remove all use of toString. Use `default("")` instead.
let toString = option => option->default("")

let map = (f, v) =>
  switch v {
  | Some(v) => Some(f(v))
  | None => None
  }

let flatMap = (f, v) => v->map(f)->default(None)

let mapWithDefault = (f, d, v) => v->map(f)->default(d)
