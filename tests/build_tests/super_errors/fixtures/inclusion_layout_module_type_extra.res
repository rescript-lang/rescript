// Module types must be equal: an item only the actual module type has is
// found by comparing the other way round, and reported on the actual side
module M: {
  module type T = {
    let a: int
  }
} = {
  module type T = {
    let a: int
    let b: int
  }
}
