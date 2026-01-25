@send
external slice: (string, ~from: int, ~to_: int) => string = "slice"

@send
external shift: array<'a> => option<'a> = "shift"

module Constants = {
  let otherThing = [2, 3]
}

let deprecatedThing = [1, 2]
