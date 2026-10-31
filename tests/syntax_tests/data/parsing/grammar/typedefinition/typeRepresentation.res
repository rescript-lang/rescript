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

// attributes and doc comments after `private`, and doc comments before a manifest
type t = private @attr {x: int}
type t = private @attr ..
type t = private @attr A | B
type t = private @attr int
type t = private @attr {"a": int}
type t = private /**doc*/ int
type t = private /**doc*/ M.t
type t = private /**doc*/ A | B
type t = private /**doc*/ @attr {x: int}
type t = /**doc*/ int
type t = /**doc*/ {"a": int}
type t = /**doc*/ (int, int)
type t = /**doc*/ [#A]
type t = /**doc*/ M.t
type t = /**doc*/ M.t = A | B
type t = /**doc*/ @attr int
type t = /**doc*/ {...base, y: int}
type t = /**doc*/ {...base}
type t = /**doc*/ @attr {...base, y: int}
type t = /**doc*/ {..}
type t = /**doc*/ {.}
type t = /**doc*/ {.. "a": int}
type t = @attr {...base, "y": int}
type t = @attr {...base}
type t = private /**doc*/ {...base, y: int}
type t = private /**doc*/ {...base}
type t = private /**doc*/ @attr {...base, y: int}
type t = private /**doc*/ {..}
type t = private /**doc*/ {.. "a": int}
type t = private @attr {...base, "y": int}
type t = private @attr {...base}
type t = /**doc*/ ...base | C
type t = /**doc*/ /**doc2*/ ...base | C
type t = /**doc*/ @attr ...base | C
type t = private /**doc*/ ...base | C
type t = private /**doc*/ @attr ...base | C
type t = /**doc*/ 'a
type t = /**doc*/ int => int
type t = /**doc*/ M.t = {x: int}
type t = private /**doc*/ Dict.t<'a>
