module Inner = {
  @genType
  let x = 34
  @genType
  let y = "hello"
}

module Outer = {
  module Medium = {
    module Inner = {
      @genType
      let y = 44
    }
  }
}

