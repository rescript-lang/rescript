type t = {f: 'a. 'a => 'a}

let g = ({f: (f: int => int)}: t) => f(42)
