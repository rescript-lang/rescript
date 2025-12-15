@ocaml.doc("
  * Wrap JS values to be used from Reason
  ")
@genType.import("./MyMath")
external /* This is the module to import from. */
/* Name and type of the JS value to bind to. */
round: float => float = "round"

@genType
type point = {
  x: int,
  y: option<int>,
}

@genType.import("./MyMath")
external /* This is the module to import from. */
/* Name and type of the JS value to bind to. */
area: point => int = "area"

@genType.import("./MyMath")
type numberOrString

@genType.import("./MyMath")
external returnMixedArray: unit => array<numberOrString> = "returnMixedArray"

@genType
let roundedNumber = round(1.8)

@genType
let areaValue = area({x: 3, y: None})

module AbsoluteValue = {
  @genType.import(("./MyMath", "AbsoluteValue"))
  type t = {"getAbs": (. unit) => int}

  /* This is untyped */
  @send external getProp: t => int = "getProp"

  /* This is also untyped, as we "trust" the type declaration in absoluteVaue */
  let getAbs = (x: t) => {
    let getAbs = x["getAbs"]
    getAbs(.)
  }
}

@genType
let useGetProp = (x: AbsoluteValue.t) => x->AbsoluteValue.getProp + 1

@genType
let useGetAbs = (x: AbsoluteValue.t) => x->AbsoluteValue.getAbs + 1

@genType.import("./MyMath")
type stringFunction

@genType
type color = [#tomato | #gray]

@genType.import("./MyMath") external useColor: color => int = "useColor"

@genType.import("./MyMath")
external higherOrder: ((int, int) => int) => int = "higherOrder"

@genType
let returnedFromHigherOrder = higherOrder(\"+")

type variant =
  | I(int)
  | S(string)

@genType.import("./MyMath")
external convertVariant: variant => variant = "convertVariant"

@genType.import("./MyMath") external polymorphic: 'a => 'a = "polymorphic"

@genType.import("./MyMath") external default: int = "default"

@genType.import(("./MyMath", "num"))
type num

@genType.import(("./MyMath", "num"))
type myNum

@genType.import("./MyMath")
type polyType<'a>

