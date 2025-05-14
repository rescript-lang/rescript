type foo = {bar: int}

type a = {
  k: {"x": int},
  mutable f: int,
  g: {
    @as("ad") g1: int,
    mutable g2: int
  }
}

type b = {
  ...foo,
  mutable f: int,
  g: string
}
