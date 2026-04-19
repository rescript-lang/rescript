module PrivateRecord: {
  @allowMutation
  type t = private {mutable value: int, name: string}
  let make: int => t
} = {
  type t = {mutable value: int, name: string}
  let make = value => {value, name: "stable"}
}

let item = PrivateRecord.make(1)
item.name = "changed"
