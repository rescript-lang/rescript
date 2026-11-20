// Calls into ThrowsLibExn, which declares its own exceptions. A catch names
// an exception by where it is defined, however the source spells it.

let catchExternal = s =>
  try ThrowsLibExn.parse(s) catch {
  | ThrowsLibExn.E => 0
  }

let catchViaLet = x =>
  try ThrowsLibExn.viaLet(x) catch {
  | ThrowsLibExn.E => 0
  }

let catchNested = x =>
  try ThrowsLibExn.Inner.find(x) catch {
  | ThrowsLibExn.Inner.F => 0
  }

// The declaration in ThrowsLibExn.Outer names the top-level E.
let catchOuter = x =>
  try ThrowsLibExn.Outer.findOuter(x) catch {
  | ThrowsLibExn.E => 0
  }

let catchOpen = s => {
  open ThrowsLibExn
  try parse(s) catch {
  | E => 0
  }
}

module L = ThrowsLibExn

let catchAlias = s =>
  try L.parse(s) catch {
  | L.E => 0
  }

exception E

// This file's own E is a different exception: reported.
let catchUnrelatedLocal = s =>
  try ThrowsLibExn.parse(s) catch {
  | E => 0
  }

// Not caught: reported with the qualified name.
let uncaught = s => ThrowsLibExn.parse(s)

@throws(ThrowsLibExn.E)
let annotated = s => ThrowsLibExn.parse(s)

// Same file: a nested module's exception, reached through open.
module Local = {
  exception G

  @throws(G)
  let find = x => x > 0 ? x : throw(G)
}

let catchLocalOpen = x => {
  open Local
  try find(x) catch {
  | G => 0
  }
}

let catchLocalQualified = x =>
  try Local.find(x) catch {
  | Local.G => 0
  }

// Outer.findOuter's @throws(E) names ThrowsLibExn.E.
@throws(ThrowsLibExn.E)
let annotatedOuter = x => ThrowsLibExn.Outer.findOuter(x)

// Known limit: a rebound exception is a new definition, so catching Rebound
// does not catch ThrowsLibExn.E.
exception Rebound = ThrowsLibExn.E

let catchRebound = s =>
  try ThrowsLibExn.parse(s) catch {
  | Rebound => 0
  }

let catchReexported = s =>
  try ThrowsReexport.parse(s) catch {
  | ThrowsLibExn.E => 0
  }

module LocalReexport = {
  include ThrowsLibExn
}

let catchLocalReexport = s =>
  try LocalReexport.parse(s) catch {
  | ThrowsLibExn.E => 0
  }

// X.get's @throws(E) is resolved where the module type S declares it, so it
// is ThrowsLibExn.E.
module UseS = (X: ThrowsLibExn.S) => {
  let caught = x =>
    try X.get(x) catch {
    | ThrowsLibExn.E => 0
    }
}

// Reported with the exceptions' full names, which an annotation can use.
let uncaughtNested = x => ThrowsLibExn.Inner.find(x)

let uncaughtLocal = x => Local.find(x)

@throws(Local.G)
let annotatedLocal = x => Local.find(x)
