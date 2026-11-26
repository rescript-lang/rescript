type cleanup = unit => unit

let fnExpectingCleanup = (cb: unit => cleanup) => {
  let cleanup = cb()
  cleanup()
}

let x = fnExpectingCleanup(() => {
  Console.log("Hello, world!")
  let _f = 2
  123
})
