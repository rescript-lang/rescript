let doStuff = (~ctx) => {
  let multiply: (int, int) => int = ctx["multiply"]
  multiply(1, 2)
}

let main = () => {
  let ctx = {
    "log": (a, b) => a ++ b,
  }

  let _ = doStuff(~ctx)
}
