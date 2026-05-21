module rec A: B.S = {
  let x = 1
}
and B: {
  module type S = {
    let x: int
  }
} = {
  module type S = {
    let x: int
  }
}
