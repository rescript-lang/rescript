module PrivateRecord: {
  type t = private {mutable value: int}
  let make: int => t
} = {
  type t = {mutable value: int}
  let make = value => {value: value}
}

let item = PrivateRecord.make(1)
item.value = 2
