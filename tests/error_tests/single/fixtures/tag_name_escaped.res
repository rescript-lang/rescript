// A @tag name with a quote, a backslash or non-ASCII letters prints as it is
// written in source
module M: {
  @tag("k\"ind") type t = A({x: int}) | B({y: int})
} = {
  @tag("typ\\é") type t = A({x: int}) | B({y: int})
}
