let one = Hoisted_function_attr.One.make()
let oneKeep = Hoisted_function_attr.One.keep()

let two = Hoisted_function_attr.Two.Inner.make()
let twoKeep = Hoisted_function_attr.Two.Inner.keep()

let three = Hoisted_function_attr.Three.Inner.Deep.make()
let threeKeep = Hoisted_function_attr.Three.Inner.Deep.keep()

let keyword = Hoisted_function_attr.Escaped.\"switch"()
let dollar = Hoisted_function_attr.Escaped.\"$plus"()
let operator = Hoisted_function_attr.Operator.\"+"()

let nested = Hoisted_function_attr.Ambiguous.B.make()
let exoticPath = Hoisted_function_attr.Ambiguous.\"B$make"()
let recursive = Hoisted_function_attr.RecursiveA.make()
let typed = Hoisted_function_attr.Typed.make()
let coerced = Hoisted_function_attr.Coerced.make()
let included = Hoisted_function_attr.Included.make()
let aliased = Hoisted_function_attr.Aliased.make()
