let add = (a, b) => a + b

exception PureMarker

let crash = () => Js.Exn.raiseError("source map test")

let debugStatement = () => {
  %debugger
  add(1, 2)
}

let debugValue = () => {
  let v = %debugger
  ignore(v)
  add(3, 4)
}

let value = add(20, 22)
