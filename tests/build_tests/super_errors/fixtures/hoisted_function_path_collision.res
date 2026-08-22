module A = {
  module B = {
    @res.hoistedFunction
    let make = () => ()
  }
  @res.hoistedFunction
  let \"B$make" = () => ()
}
let after = ()
