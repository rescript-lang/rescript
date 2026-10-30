// A module where a functor is expected has no declarations to compare: the
// module types are shown
module M: {
  module F: (
    X: {
      let a: int
    },
  ) => {
    let b: int
  }
} = {
  module F = {
    let b = 1
  }
}
