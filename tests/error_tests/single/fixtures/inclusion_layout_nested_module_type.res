// A difference in a module type inside a module is named by its path
module M: {
  module N: {
    module type T = {
      let v: int
    }
  }
} = {
  module N = {
    module type T = {
      let v: string
    }
  }
}
