type special = | @as("special") SpecialKind
type text = {kind: string, value: int}
type number = {kind: float, value: int}
module Source = {
  @unboxed type t = Text(text) | Number(number)
}
module Target = {
  @unboxed type t = Text(text) | Number(number) | Special({kind: special, value: int})
}
let convert = (value: Source.t) => (value :> Target.t)
