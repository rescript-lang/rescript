// Test pervasive references

@genType let create = (x: int) => ref(x)

@genType let access = r => r.contents + 1

@genType let update = r => r.contents = r.contents + 1

// Abstract version of references, exported as an opaque type.

module R: {
  @genType type t<'a>
  let get: t<'a> => 'a
  let make: 'a => t<'a>
  let set: (t<'a>, 'a) => unit
} = {
  type t<'a> = ref<'a>
  let get = r => r.contents
  let make = ref
  let set = (r, v) => r.contents = v
}

@genType type t<'a> = R.t<'a>

@genType let get = R.get

@gentype
let make = R.make

@genType let set = R.set

type requiresConversion = {x: int}

@genType let destroysRefIdentity = (x: ref<requiresConversion>) => x

@genType let preserveRefIdentity = (x: R.t<requiresConversion>) => x
