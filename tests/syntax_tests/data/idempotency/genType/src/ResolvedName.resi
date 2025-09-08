type t

type eq = (t, t)

let applyEquations: (~eqs: list<eq>, t) => list<eq>

let dot: (string, t) => t

let fromString: string => t

let toList: t => list<string>

let toString: t => string
