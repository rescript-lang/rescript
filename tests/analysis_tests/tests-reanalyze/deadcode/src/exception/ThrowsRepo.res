// Functions that throw exceptions defined in another module, ThrowsErrors.

@throws(ThrowsErrors.NotFound)
let find = x => x > 0 ? x : throw(ThrowsErrors.NotFound)

@throws(ThrowsErrors.NotFound)
external findExternal: int => int = "findExternal"

module L = ThrowsErrors

@throws(L.NotFound)
let viaAlias = x => x > 0 ? x : throw(L.NotFound)

open ThrowsErrors

@throws(Bad)
let viaOpen = x => x > 0 ? x : throw(Bad("negative"))

let catchViaOpen = x =>
  try viaOpen(x) catch {
  | Bad(_) => 0
  }
