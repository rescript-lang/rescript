// A @@warning in the middle of a file applies to the externals after it.
@send external first: (string, @as("x") int) => unit = "first"

@@warning("-101")

@send external second: (string, @as("y") int) => unit = "second"
