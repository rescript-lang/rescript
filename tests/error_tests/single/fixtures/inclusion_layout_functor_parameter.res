// A difference in a functor's parameter is shown at its position in the
// functor's type
module M: {
  module F: (
    X: {
      let a: int
    },
  ) => {}
} = {
  module F = (
    X: {
      let a: string
    },
  ) => {}
}
