type t = {
  mutable value: int,
  name: string,
}

let make = value => {value, name: "stable"}
let value = t => t.value
let name = t => t.name
