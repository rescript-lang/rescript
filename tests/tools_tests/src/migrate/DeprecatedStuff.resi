@deprecated({
  reason: "Use `String.slice` instead",
  migrate: String.slice(
    ~start=%insert.labelledArgument("from"),
    ~end=%insert.labelledArgument("to_"),
  ),
})
@send
external slice: (string, ~from: int, ~to_: int) => string = "slice"

@send
@deprecated({
  reason: "Use `Array.shift` instead.",
  migrate: Array.shift(),
})
external shift: array<'a> => option<'a> = "shift"
