@attr type t = float
@attr
type t = float

@attr @attr @attr @attr @attr @attr @attr @attr @attr @attr @attr @attr type t = int
@attr @attr @attr @attr @attr @attr @attr @attr @attr @attr @attr @attr
type t = int

@attrWithSuperLongIdentifeirName @attrWithSuperLongIdentifeirName @attrWithSuperLongIdentifeirName@attrWithSuperLongIdentifeirName type t = string
@attrWithSuperLongIdentifeirName @attrWithSuperLongIdentifeirName @attrWithSuperLongIdentifeirName@attrWithSuperLongIdentifeirName
type t = string

// attributes in front of a record or `..` move to the declaration
@schema
type t = @s.strict {
  email: string,
  name: string,
}
type t = @attr /** doc */ {x: int}
type t = @attr ..
type t = /** doc */ {x: int}
type t = /** doc */ ..
type t = /** doc */ @attr {x: int}
type t = @attr /** doc */ @attr2 ..
type t = /** d1 */ @attr /** d2 */ {x: int}
@outer
type t = /* c1 */ /** doc */ /* c2 */ {x: int} // trail
type t = /** doc */ A | B
type t = | /** doc */ A | /** doc2 */ B
type t = private @attr {x: int}
type t = private /** doc */ @attr ..
type t = private @attr int
type t = private /** doc */ int
type t = private /** doc */ A | B
type t = /** doc */ int
type t = /** doc */ {"a": int}
type t = /** doc */ (int, int)
type t = /** doc */ [#A]
type t = /** doc */ M.t = A | B
type t = /** doc */ {...base, y: int}
type t = /** doc */ {.. "a": int}
type t = private /** doc */ {...base, y: int}
type t = private /** doc */ {.}
type t = /** doc */ ...base | C
type t = private /** doc */ ...base | C
type t = /** doc */ 'a
