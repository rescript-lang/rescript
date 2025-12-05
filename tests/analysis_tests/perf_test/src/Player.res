@unboxed
type key =
  // | @as("f2") F2
  // | @as("f3") F3
  // | @as("f4") F4
  // | @as("f5") F5
  // | @as("f6") F6
  // | @as("f7") F7
  // | @as("f8") F8
  // | @as("f9") F9
  | @as("f10") F10
  | @as("l") L
  | @as("m") M
  | @as("n") N
  | @as("o") O
  | @as("p") P
  | @as("q") Q
  | @as("r") R
  | @as("s") S
  | @as("t") T
  | @as("u") U
  | @as("v") V
  | @as("w") W
  | @as("x") X
  | @as("space") Space
  | @as("left") Left
  | @as("right") Right
  | @as("up") Up
  | @as("down") Down
  | @as("escape") Escape
  | @as("backspace") Backspace
  | @as("enter") Enter
  | @as("tab") Tab
  | @as("control") Control
  | @as("alt") Alt
  | @as("meta") Meta
  | @as("shift") Shift
  | @as("string") String(string)

@send
external onKeyRelease: (key => unit) => unit = "onKeyRelease"

let make = () => {
  let gameObj: Thundershock.t = Obj.magic(3)

  onKeyRelease(key => {
    let isYAxis = !(gameObj.direction.y == 0.)
    switch key {
    | Space =>
      if isYAxis {
        Thundershock.cast(gameObj)->ignore
      }
    | _ => ()
    }
  })

  gameObj
}
