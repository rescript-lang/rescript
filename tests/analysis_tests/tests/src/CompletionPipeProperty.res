module ObservablePoint = {
  type op = {
    mutable x: int,
    mutable y: int,
  }

  @send
  external setBoth: (op, float) => unit = "set"

  @send
  external set: (op, float, float) => unit = "set"
}

module Sprite = {
  type s = {
    anchor: ObservablePoint.op,
  }
}

let sprite : Sprite.s = %todo

// sprite.anchor.
//               ^com