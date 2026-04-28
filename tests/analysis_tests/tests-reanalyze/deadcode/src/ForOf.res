@live
let keep = xs => {
  for x of (xs->Array.asIterable) {
    ignore(x)
  }
}

@val external iterable: Iterable.t<int> = "iterable"

let sideEffects = {
  // Keep the body pure so the loop's effect comes only from iteration itself.
  for _ of iterable {
    ()
  }
}
