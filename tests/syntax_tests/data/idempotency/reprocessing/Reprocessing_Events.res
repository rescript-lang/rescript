type buttonStateT = Reasongl.Gl.Events.buttonStateT = LeftButton | MiddleButton | RightButton

type stateT = Reasongl.Gl.Events.stateT = MouseDown | MouseUp

type keycodeT = Reasongl.Gl.Events.keycodeT =
  | Backspace
  | Delete
  | Tab
  | Enter
  | Escape
  | Space
  | Quote
  | Comma
  | Minus
  | Period
  | Slash
  | Num_0
  | Num_1
  | Num_2
  | Num_3
  | Num_4
  | Num_5
  | Num_6
  | Num_7
  | Num_8
  | Num_9
  | Semicolon
  | Equals
  | OpenBracket
  | Backslash
  | CloseBracket
  | A
  | B
  | C
  | D
  | E
  | F
  | G
  | H
  | I
  | J
  | K
  | L
  | M
  | N
  | O
  | P
  | Q
  | R
  | S
  | T
  | U
  | V
  | W
  | X
  | Y
  | Z
  | Right
  | Left
  | Down
  | Up
  | LeftCtrl
  | LeftShift
  | LeftAlt
  | LeftOsKey
  | RightCtrl
  | RightShift
  | RightAlt
  | RightOsKey
  | CapsLock
  | Backtick
  | Nothing

let keycodeMap: int => keycodeT = x =>
  switch x {
  | 8 => Backspace
  | 9 => Tab
  | 13 => Enter
  | 16 => LeftShift /* There is no RightShift :( */
  | 17 => LeftCtrl /* There is no RightCtrl :( */
  | 18 => LeftAlt /* There is no RightAlt :( */
  | 20 => CapsLock
  | 27 => Escape
  | 32 => Space
  | 37 => Left
  | 38 => Up
  | 39 => Right
  | 40 => Down
  | 48 => Num_0
  | 49 => Num_1
  | 50 => Num_2
  | 51 => Num_3
  | 52 => Num_4
  | 53 => Num_5
  | 54 => Num_6
  | 55 => Num_7
  | 56 => Num_8
  | 57 => Num_9
  | 65 => A
  | 66 => B
  | 67 => C
  | 68 => D
  | 69 => E
  | 70 => F
  | 71 => G
  | 72 => H
  | 73 => I
  | 74 => J
  | 75 => K
  | 76 => L
  | 77 => M
  | 78 => N
  | 79 => O
  | 80 => P
  | 81 => Q
  | 82 => R
  | 83 => S
  | 84 => T
  | 85 => U
  | 86 => V
  | 87 => W
  | 88 => X
  | 89 => Y
  | 90 => Z
  | 91 => LeftOsKey
  | 93 => RightOsKey
  | 186 => Semicolon
  | 187 => Equals
  | 188 => Comma
  | 189 => Minus
  | 190 => Period
  | 191 => Slash
  | 192 => Backtick
  | 219 => OpenBracket
  | 220 => Backslash
  | 221 => CloseBracket
  | 222 => Quote
  | _ => Nothing
  }
