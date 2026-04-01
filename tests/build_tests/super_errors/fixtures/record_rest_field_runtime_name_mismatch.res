type source = {
  a: int,
  @as("runtime-b")
  b: string,
}

type wrong = {
  @as("other-b")
  b: string,
}

let {a, ...wrong as rest} = ({a: 1, b: "x"}: source)
