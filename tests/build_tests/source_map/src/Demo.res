let add = (a, b) => a + b

exception PureMarker

type item =
  | Empty
  | Single(int)
  | Pair(int, int)

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

let applyPipe = (input, fn) => input->fn(10)

let pipedValue = applyPipe(5, add)

let describeItem = item =>
  switch item {
  | Empty => "empty"
  | Single(value) => Int.toString(value)
  | Pair(left, right) => Int.toString(left + right)
  }

let unicodeMessage = "한글 🌏"

let unicodeCrash = () => Js.Exn.raiseError(unicodeMessage)

let value = add(pipedValue, 22)
