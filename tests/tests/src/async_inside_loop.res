let topLevelAsyncFunction = async () => {
  for innerScopeVal in 0 to 3 {
    let asyncClosureAccessingScopedVal = async () => {
      Js.log2("Accessing scoped var inside loop", innerScopeVal)
      await Js.Promise.resolve()
    }

    await asyncClosureAccessingScopedVal()
  }
}

let _ = topLevelAsyncFunction()
