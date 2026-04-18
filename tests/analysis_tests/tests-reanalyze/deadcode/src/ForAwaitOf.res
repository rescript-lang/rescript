@live
let keep = async (xs: AsyncIterable.t<int>) => {
  for await x of xs {
    ignore(x)
  }
}

@val external asyncIterable: AsyncIterable.t<int> = "asyncIterable"

let sideEffects = {
  // Keep the body pure so the loop's effect comes only from iteration itself.
  for await value of asyncIterable {
    ()
  }
}
