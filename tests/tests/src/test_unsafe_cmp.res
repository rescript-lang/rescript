let f = (x, y) => {
  open Js
  (unsafe_lt(x, y), unsafe_le(x, y), unsafe_gt(x, y), unsafe_ge(x, y))
}

let ff = (x, y) =>
  if Js.unsafe_lt(x, y) {
    1
  } else {
    2
  }
