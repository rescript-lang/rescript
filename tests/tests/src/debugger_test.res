let f = (x, y) => {
  Console.log((x, y))
  x + y
}

let g = () => {
  ignore(f(1, 2))
  debugger()
  ignore(f(1, 2))
  debugger()
  3
}

// The argument runs before the debugger statement.
let with_effect = () => {
  debugger(Console.log("before pause"))
  3
}

let with_effect_value = () => {
  let v = debugger(ignore(f(1, 2)))
  v
}

let exterme_g = () => {
  ignore(f(1, 2))
  let v = debugger()
  Console.log(v)
  ignore(f(1, 2))
  debugger()
  3
}
