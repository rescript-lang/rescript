let make = (): module(Sig.S) =>
  module(
    {
      let v = 1
    }
  )

let module(M) = make()
