let f = (x: int) => (y: int) => x + y
let g = (f :> (int, int) => int)
