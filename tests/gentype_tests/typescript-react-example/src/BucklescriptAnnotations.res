@genType
type someMutableFields = {
  @set
  "mutable0": string,
  "immutable": int,
  @set
  "mutable1": string,
  @set
  "mutable2": string,
}

@genType
type someMethods = {
  "send": string => unit,
  "on": (string, int => unit) => unit,
  "threeargs": (int, string, int) => string,
  "twoArgs": (int, string) => int,
}

// let foo = (x: someMethods) => x["threeargs"](3, "a", 4)

let bar = (x: someMethods) => {
  let f = x["twoArgs"]
  f(3, "a")
}
