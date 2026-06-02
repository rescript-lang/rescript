module type S = {
  @tag("kind")
  type t = | @as("left") Other({value: int})
}

module M: S = {
  @tag("kind")
  type t = | @as("right") Other({value: int})
}
