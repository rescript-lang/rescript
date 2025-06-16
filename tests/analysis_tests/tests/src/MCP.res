module TestType = {
  type first = {
    one: bool,
    two?: string,
  }

  type variant =
    | One
    | Two(string)
    | Three(bool)

  type second = {
    first: first,
    variant: variant,
  }
}

let xx = {
  TestType.first: {
    one: true,
  },
  variant: One,
}

let ft = () => xx

let ff = ft()
//  ^mli
