module Kind: {
  type t = private Yes | No
} = {
  type t = Yes | No
}
@unboxed type t = Enum({kind: Kind.t, value: int}) | Number({kind: int, reason: string})
