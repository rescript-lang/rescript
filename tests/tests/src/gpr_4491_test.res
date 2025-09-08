let f = xs => {
  let unused = switch xs {
  | Some(l) =>
    Js.log("side effect")
    list{l, l}
  | None => list{1, 2}
  }

  Js.log2("nothing to see here", xs)
}
