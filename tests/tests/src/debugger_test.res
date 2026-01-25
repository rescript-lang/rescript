let f = (x, y) => {
  Console.log((x, y))
  x + y
}

let g = () => {
  ignore(f(1, 2))
  %debugger
  ignore(f(1, 2))
  %debugger
  3
}

let exterme_g = () => {
  ignore(f(1, 2))
  let v = %debugger
  Console.log(v)
  ignore(f(1, 2))
  %debugger
  3
}
