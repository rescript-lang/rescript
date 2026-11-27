type t
@send external first: (t, @as("x") int) => unit = "first"
@send external second: (t, @as("y") int) => unit = "second"

/** A doc comment doesn't hide an unused attribute, nor does a nested type. */
@send external third: (t, option<@as("z") int>) => unit = "third"

// An external in a local module is resolved once, so it warns once.
let local = () => {
  module M = {
    @send external fourth: (t, @as("w") int) => unit = "fourth"
  }
  M.fourth
}
