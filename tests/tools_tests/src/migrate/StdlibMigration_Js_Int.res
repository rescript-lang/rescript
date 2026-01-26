let toExponential1 = 77->Js.Int.toExponential
let toExponential2 = Js.Int.toExponential(77)

let toExponentialWithPrecision1 = 77->Js.Int.toExponentialWithPrecision(~digits=2)
let toExponentialWithPrecision2 = Js.Int.toExponentialWithPrecision(77, ~digits=2)

let toPrecision1 = 123456789->Js.Int.toPrecision
let toPrecision2 = Js.Int.toPrecision(123456789)

let toPrecisionWithPrecision1 = 123456789->Js.Int.toPrecisionWithPrecision(~digits=2)
let toPrecisionWithPrecision2 = Js.Int.toPrecisionWithPrecision(123456789, ~digits=2)

let toString1 = 123456789->Js.Int.toString
let toString2 = Js.Int.toString(123456789)

let toStringWithRadix1 = 373592855->Js.Int.toStringWithRadix(~radix=16)
let toStringWithRadix2 = Js.Int.toStringWithRadix(373592855, ~radix=16)

let toFloat1 = 42->Js.Int.toFloat
let toFloat2 = Js.Int.toFloat(42)

let equal1 = Js.Int.equal(1, 1)
let equal2 = 1->Js.Int.equal(2)
