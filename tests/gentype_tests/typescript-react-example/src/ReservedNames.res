/* https://github.com/rescript-lang/rescript/issues/6871 */
@genType
let delete = 1

@genType
let x' = 2

@genType
let window = 3

module Products = {
  @genType
  let createNew = "createNew"

  @genType
  let delete = "delete"
}

module Array = {
  @genType
  let new = "new"
}

module Outer = {
  module Array = {
    @genType
    let delete = "delete"
  }
}
