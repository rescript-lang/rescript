module M: {
  module type T = {
    let a: int
    let b: int
  }
} = {
  module type T = {
    let b: int
    let a: int
  }
}
