type t =
  | A
  | B
  | C

let compare = (x: t, y: t) =>
  switch x {
  | A => y == A
  | B => y == B
  | C => y == C
  }

/* There is a downside in this way of writing
    if I comment [C,C] there will be no warning
*/
let compare2 = (x: t, y: t) =>
  switch (x, y) {
  | (A, A)
  | (B, B)
  | (C, C) => true
  | (A | B | C, _) => false
  }

let compare3 = (x: t, y: t) => x == y
