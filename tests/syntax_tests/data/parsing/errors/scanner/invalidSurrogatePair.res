/* Known bug: this valid JavaScript surrogate pair is rejected as two invalid
   standalone surrogate escapes. */
let validPairCurrentlyRejected = "\uD83D\uDE00"

let malformed = "\uD83D\uZZZZ"
let after = 1
