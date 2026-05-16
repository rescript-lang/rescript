module One = {
  @res.hoistedFunction
  let make = () => "one"

  let keep = () => "one-keep"
}

module Two = {
  let keep = () => "two-keep"

  module Inner = {
    @res.hoistedFunction
    let make = () => "two"

    let keep = () => "two-inner-keep"
  }
}

module Three = {
  module Inner = {
    let keep = () => "three-inner-keep"

    module Deep = {
      @res.hoistedFunction
      let make = () => "three"

      let keep = () => "three-deep-keep"
    }
  }
}
