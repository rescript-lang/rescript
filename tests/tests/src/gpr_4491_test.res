let f = xs => {
  let unused = switch xs {
  | Some(l) =>
    Console.log("side effect")
    list{l, l}
  | None => list{1, 2}
  }

  Console.log2("nothing to see here", xs)
}
