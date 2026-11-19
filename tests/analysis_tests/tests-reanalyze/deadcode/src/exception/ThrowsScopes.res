// An interface's @throws is resolved after the interface's own open.
let catchIfaceOpen = x =>
  try ThrowsIfaceOpen.find(x) catch {
  | ThrowsErrors.NotFound => 0
  }

// An alias of an alias in another file
module M = ThrowsAlias.L

let catchAliasChain = s =>
  try M.parse(s) catch {
  | M.E => 0
  }

// An include of an include
module Twice = {
  include ThrowsReexport
}

let catchIncludedTwice = s =>
  try Twice.parse(s) catch {
  | ThrowsLibExn.E => 0
  }

// A functor body's own exception
module MakeThrower = (X: {}) => {
  exception Inner

  @throws(Inner)
  let throwInner = x => x > 0 ? x : throw(Inner)

  let catchInner = x =>
    try throwInner(x) catch {
    | Inner => 0
    }
}
