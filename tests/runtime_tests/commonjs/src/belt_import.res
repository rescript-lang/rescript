let f = (xs: Belt.Map.Int.t<string>, idx: int) => {
  Belt.Map.Int.get(xs, idx)
}

let g = () => {
  Belt.Map.Int.fromArray([(1, "hello"), (2, "world")])->f(1)
}
