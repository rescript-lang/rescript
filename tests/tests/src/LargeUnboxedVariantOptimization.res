// Test for large unboxed variant compilation performance
// This used to cause exponential blowup in simplify_and_ (issue #8039)

@unboxed
type key =
  | @as("a") A
  | @as("b") B
  | @as("c") C
  | @as("d") D
  | @as("e") E
  | @as("f") F
  | @as("g") G
  | @as("h") H
  | @as("i") I
  | @as("j") J
  | @as("k") K
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
  | @as("y") Y
  | @as("z") Z
  | @as("space") Space
  | @as("string") String(string)

type state = {mutable active: bool}

@val external doAction: state => unit = "doAction"

let handleKey = (state: state, key: key) => {
  switch key {
  | Space =>
    if state.active {
      doAction(state)
    }
  | _ => ()
  }
}
