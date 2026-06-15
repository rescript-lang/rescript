type payload = {
  label: string,
  count: int,
}

let multiply = (left, right) => left * right

let pipeThrough = (value, fn) => value->fn(2)

let describe = payload =>
  switch payload.count {
  | 0 => payload.label ++ ":empty"
  | count => payload.label ++ ":" ++ Int.toString(count)
  }

let unicodeLabel = "helper 한글 🌏"

let fail = () => Js.Exn.raiseError("helper source map")

let value = pipeThrough(21, multiply)
