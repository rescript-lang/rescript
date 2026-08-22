let run = () => {
  @res.hoistedFunction
  let local = () => ()
  local()
}

let localModule = () => {
  module Local = {
    @res.hoistedFunction
    let make = () => ()
  }
  Local.make()
}
module Make = () => {
  @res.hoistedFunction
  let make = () => ()
}
%%private(
  @res.hoistedFunction
  let privateMake = () => ()
)

let usePrivate = privateMake()
