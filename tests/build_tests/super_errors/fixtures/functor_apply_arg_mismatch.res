module type HasFoo = {
  let foo: int
}

module Make = (M: HasFoo) => {
  let bar = M.foo + 1
}

module Concrete = {
  let unrelated = "string"
}

module Result = Make(Concrete)
let _ = Result.bar
