/* A record field's runtime name is its decoded value, not the way it was
   spelled. The signature spells the same name with a unicode escape, so the
   two describe one field and the constraint is satisfied. */
module Renamed: {
  type t = {@as("\u0041") a: int, b: int}

  let v: t

  let getA: t => int
} = {
  type t = {@as("A") a: int, b: int}

  let v = {a: 1, b: 2}

  let getA = (x: t) => x.a
}
