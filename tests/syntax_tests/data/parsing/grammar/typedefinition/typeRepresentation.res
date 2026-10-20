// extensible variants
type t = ..
type t = private ..

// constructor declaration
type t = Blue
type t = | Blue
type t = | Blue | Red
type t = Blue | Red
type t = | Blue | Red | Green
type t = Blue | Red | Green

// private constructor declaration
type t = private Blue
type t = private | Blue
type t = private | Blue | Red
type t = private Blue | Red
type t = private | Blue | Red | Green
type t = private Blue | Red | Green

// empty variant, not implemented
// type t = |
// type t = private |

// record declaration
type t = {x: int, y: int}
// private record declaration
type t = private {x: int, y: int}

// attributes can precede record or open variant markers
type t = @attr {x: int}
type t = @attr /**doc before record*/ {x: int}
type t = @attr ..
type t = @attr /**doc before open*/ ..
type t = @attr {x: {y: int}}
type t = M.t = @attr {x: int}
type t<'a> = @attr {x: 'a}
type t = int and u = @attr {x: int}
type t = /**doc before record*/ {x: int}
type t = /**doc before open*/ ..
type t = /**doc*/ @attr {x: int}
type t = @attr /**doc*/ @attr2 ..
type t = /**doc*/ {x: {y: int}}
type t = private /**doc*/ {x: int}
type t = /**doc*/ A | B
type t = /**doc*/ | A | /**doc2*/ B
type t = /**doc*/ @attr A | B
