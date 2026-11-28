let add = (a, b) => a + b

exception PureMarker

type item =
  | Empty
  | Single(int)
  | Pair(int, int)

let crash = () => JsError.throwWithMessage("source map test")

let debugStatement = () => {
  debugger()
  add(1, 2)
}

let debugValue = () => {
  let v = debugger()
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

type settings = {retries: int, delay: int, label?: string}

let updateSettings = base => {
  ...base,
  retries: add(base.retries, 1),
  delay: try applyPipe(base.delay, add) catch {
  | _ => 0
  },
}

let unicodeMessage = "한글 🌏"

let unicodeCrash = () => JsError.throwWithMessage(unicodeMessage)

let value = add(pipedValue, 22)
