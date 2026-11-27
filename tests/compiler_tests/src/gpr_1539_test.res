module rec CRS: {
  type t
} = CRS
and Layer: {
  type t
} = Layer

and Point: {
  type t
  let add: (t, t) => t
} = {
  type t
  @send external add: (t, t) => t = "add"
}
