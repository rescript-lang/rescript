let f = (@res.doc("The runner") run: string) => run
let f = (/** The runner */ run: string) => run
let f = (/** Positional */ x, /** Labelled */ ~y, /** Typed */ ~z: int) => x + y + z
let f = (/** Optional */ ~a=?, /** Default */ ~b=1, /** Aliased */ ~c as d) => d
let f = (/** Locally abstract */ type a, x: a) => x
