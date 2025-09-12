@live
let keep = xs => {
  for x of (xs->Array.asIterable) {
    ignore(x)
  }
}
