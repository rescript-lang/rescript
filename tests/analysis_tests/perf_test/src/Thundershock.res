type team = Player | Opponent

type vec2 = {
  mutable x: float,
  mutable y: float,
}


type t = {mutable direction: vec2}


let cast = (_: t) => {
  ()
}

