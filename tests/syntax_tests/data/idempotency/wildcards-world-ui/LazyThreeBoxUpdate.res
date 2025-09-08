module type T = module type of ThreeBoxUpdate.Main

/*
   Needed for ReScript to not import the original component :
   See https://github.com/rescript-lang/rescript-compiler/issues/3543
 */
let unsafePlaceholder: module(T) = %raw(`{}`)

module UnsafePlaceholder = unpack(unsafePlaceholder)

type props = UnsafePlaceholder.props

let make = ReLoadable.lazy_(() =>
  ReLoadable.\"import"(UnsafePlaceholder.make, "./ThreeBoxUpdate.bs.js")
)
