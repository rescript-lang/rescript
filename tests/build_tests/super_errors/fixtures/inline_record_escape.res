type x = One({test: bool})

let g = (v: x) =>
  switch v {
  | One(r) => r
  }
