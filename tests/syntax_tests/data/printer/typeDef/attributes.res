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
@outer
type t = /* c1 */ /** doc */ /* c2 */ {x: int} // trail
type t = /** doc */ A | B
type t = | /** doc */ A | /** doc2 */ B
