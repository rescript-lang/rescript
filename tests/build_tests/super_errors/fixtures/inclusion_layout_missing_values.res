// Several missing values of one module are listed under its position once
module M: {
  module N: {
    let a: int
    let b: string
  }
} = {
  module N = {}
}
