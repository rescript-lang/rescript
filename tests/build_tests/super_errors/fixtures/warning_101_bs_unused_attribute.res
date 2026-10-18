@as("foo")
let x = 1

module Hoisted = {
  @res.hoistedFunction
  let make = () => ()
}
