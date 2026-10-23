// A generator resumed with `[#X]` can't be resumed with `[#X | #Y]`.
let widen = (g: Generator.t<int, unit, [#X]>) => (g :> Generator.t<int, unit, [#X | #Y]>)
