let nan1 = Js.Float._NaN

let isNaN1 = Js.Float._NaN->Js.Float.isNaN
let isNaN2 = Js.Float.isNaN(Js.Float._NaN)

let isFinite1 = 1234.0->Js.Float.isFinite
let isFinite2 = Js.Float.isFinite(1234.0)

let toExponential1 = 77.1234->Js.Float.toExponential
let toExponential2 = Js.Float.toExponential(77.1234)

let toExponentialWithPrecision1 = 77.1234->Js.Float.toExponentialWithPrecision(~digits=2)
let toExponentialWithPrecision2 = Js.Float.toExponentialWithPrecision(77.1234, ~digits=2)

let toFixed1 = 12345.6789->Js.Float.toFixed
let toFixed2 = Js.Float.toFixed(12345.6789)

let toFixedWithPrecision1 = 12345.6789->Js.Float.toFixedWithPrecision(~digits=1)
let toFixedWithPrecision2 = Js.Float.toFixedWithPrecision(12345.6789, ~digits=1)

let toPrecision1 = 12345.6789->Js.Float.toPrecision
let toPrecision2 = Js.Float.toPrecision(12345.6789)

let toPrecisionWithPrecision1 = 12345.6789->Js.Float.toPrecisionWithPrecision(~digits=2)
let toPrecisionWithPrecision2 = Js.Float.toPrecisionWithPrecision(12345.6789, ~digits=2)

let toString1 = 12345.6789->Js.Float.toString
let toString2 = Js.Float.toString(12345.6789)

let toStringWithRadix1 = 6.0->Js.Float.toStringWithRadix(~radix=2)
let toStringWithRadix2 = Js.Float.toStringWithRadix(6.0, ~radix=2)

let parse1 = "123"->Js.Float.fromString
let parse2 = Js.Float.fromString("123")

