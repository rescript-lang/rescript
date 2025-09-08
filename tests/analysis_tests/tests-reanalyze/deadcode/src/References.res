// Test pervasive references

@genType
let create = (x: int) => ref(x)

@genType
let access = r => r.contents + 1

@genType
let update = r => r.contents = r.contents + 1

// Abstract version of references: works when conversion is required.

module R: {
  @genType
  type t<'a>
  let get: t<'a> => 'a
  let make: 'a => t<'a>
  let set: (t<'a>, 'a) => unit
} = {
  type t<'a> = ref<'a>
  let get = r => r.contents
  let make = ref
  let set = (r, v) => r.contents = v
}

@genType
type t<'a> = R.t<'a>

@genType
let get = R.get

@gentype
let make = R.make

@genType
let set = R.set

type requiresConversion = {x: int}

// Careful: conversion makes a copy and destroys the reference identity.
@genType
let destroysRefIdentity = (x: ref<requiresConversion>) => x

// Using abstract references preserves the identity.
@genType
let preserveRefIdentity = (x: R.t<requiresConversion>) => x

