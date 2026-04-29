module PrivateRecord: {
  @allowMutation
  type t = private {mutable value: int}
  let make: int => t
} = {
  type t = {mutable value: int}
  let make = value => {value: value}
}

let _item: PrivateRecord.t = {value: 1}
