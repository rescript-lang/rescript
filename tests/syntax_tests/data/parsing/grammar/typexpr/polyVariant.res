type animation = [
  | #"ease-in"
  | #"ease-out"
  | #"never ease ✍️"
]

module type Conjunctive = {
  type u1 = [ | #A | #B]
  type u2 = [ | #A | #B | #C]

  let f: [< | #T([< u2]) & ([< u2]) & ([< u1])] => unit
  let g: [< | #S&([< u2]) & ([< u2]) & ([< u1])] => unit
  let g: [< | #"Exotic-S+"&([< #"Exotic-u2+"]) & ([< #"Exotic-u2-"]) & ([< #"Exotic-u1+++"])] => unit
};

type t = [s]
type t = [ListStyleType.t];

type number = [
  | #1
  | #42
  | #4244
]

type complexNumbericPolyVar = [
  | #1(string)
  | #2(int, string)
]
