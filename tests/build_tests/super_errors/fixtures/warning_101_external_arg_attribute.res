type t
@send external first: (t, @as("x") int) => unit = "first"
@send external second: (t, @as("y") int) => unit = "second"
