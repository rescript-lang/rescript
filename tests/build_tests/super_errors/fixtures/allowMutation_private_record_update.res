module PrivateRecord: {
  @allowMutation
  type t = private {mutable value: int}
  let make: int => t
} = {
  type t = {mutable value: int}
  let make = value => {value: value}
}

let item = PrivateRecord.make(1)
let _updated = {...item, value: 2}
