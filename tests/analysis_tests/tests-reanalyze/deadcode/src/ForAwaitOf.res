@live
let keep = async (xs: AsyncIterable.t<int>) => {
  for await x of xs {
    ignore(x)
  }
}

let sideEffects = {
  let asyncIterable: AsyncIterable.t<int> = %raw(`(async function* () {
    yield 1
  })()`)

  for await value of asyncIterable {
    Js.log(value)
  }
}
