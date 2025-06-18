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

type xx = Hello

let ft = () => xx

let ff = ft()
//  ^mli

// ^mif MCP.xx
// ^mif Rxjs.Subscriber.t
