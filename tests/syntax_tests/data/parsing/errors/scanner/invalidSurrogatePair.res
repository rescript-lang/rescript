/* A valid JavaScript surrogate pair must not be rejected along with the
   malformed escape below. */
let validPair = "\uD83D\uDE00"

let malformed = "\uD83D\uZZZZ"
let after = 1
