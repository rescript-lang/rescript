module Variant = {
  type t = X
  let value = X
}

module Record = {
  type t = {value: int}
  let value = {value: 42}
}

module Alias = {
  type t = int
  let value: t = 42
}

module Abstract = {
  type t = option<int>
  let value: t = None
  let isUndefined = value => Option.isNone(value)
}

module Unboxed = {
  @unboxed
  type t = Value(option<int>)
  let value = Value(None)
  let isUndefined = value => {
    let Value(inner) = value
    Option.isNone(inner)
  }
}
