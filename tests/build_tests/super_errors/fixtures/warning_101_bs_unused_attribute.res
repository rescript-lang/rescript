@as("foo")
let x = 1

@res.hoistedFunction
type t = int

module ExpressionAttribute = {
  let make = @res.hoistedFunction () => ()
}
