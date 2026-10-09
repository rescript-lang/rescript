module Variant: {
  type t = X
  let value: t
}

module Record: {
  type t = {value: int}
  let value: t
}

module Alias: {
  type t = int
  let value: t
}

module Abstract: {
  type t
  let value: t
  let isUndefined: t => bool
}

module Unboxed: {
  @unboxed
  type t = Value(option<int>)
  let value: t
  let isUndefined: t => bool
}
