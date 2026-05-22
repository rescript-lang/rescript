module type Inner = {
  let a: int
}

module type Outer = {
  module M: Inner
}

module M: Outer = {
  module M = {
    let a = "wrong type"
  }
}
