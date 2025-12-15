@genType
let notNested = 1

module Universe = {
  @genType
  let theAnswer = 42

  let notExported = 33

  @genType
  type nestedType = array<string>

  module Nested2 = {
    let x = 0

    @genType
    let nested2Value = 1

    let y = 2

    @genType
    type nested2Type = array<array<string>>

    module Nested3 = {
      let x = 0
      let y = 1
      let z = 2
      let w = 3

      @genType
      type nested3Type = array<array<array<string>>>

      @genType
      let nested3Value = "nested3Value"

      @genType
      let nested3Function = (x: nested2Type) => x
    }

    @genType
    let nested2Function = (x: Nested3.nested3Type) => x
  }

  @genType
  type variant =
    | A
    | B(string)

  @genType
  let someString = "some exported string"
}

