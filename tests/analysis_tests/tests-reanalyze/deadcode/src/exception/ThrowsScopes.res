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

// A list payload
@throws(list{ThrowsErrors.NotFound, Not_found})
let listPayload = x => x > 0 ? throw(Not_found) : throw(ThrowsErrors.NotFound)

// An exception of the runtime, whose .cmi keeps no locations
@throws(Lazy.Undefined)
let lazyUndefined = () => throw(Lazy.Undefined)

let catchLazyUndefined = () =>
  try lazyUndefined() catch {
  | Lazy.Undefined => 0
  }
