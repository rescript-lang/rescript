let i1 = Int.toExponentialWithPrecision(77, ~digits=2)
let i2 = Int.toFixedWithPrecision(300, ~digits=1)
let i3 = Int.toPrecisionWithPrecision(100, ~digits=2)
let i4 = Int.toStringWithRadix(6, ~radix=2)
let i5 = Int.rangeWithOptions(1, 5, {step: 2})

let f1 = Float.parseIntWithRadix("10.0", ~radix=2)
let f2 = Float.toExponentialWithPrecision(77.0, ~digits=2)
let f3 = Float.toFixedWithPrecision(300.0, ~digits=1)
let f4 = Float.toPrecisionWithPrecision(100.0, ~digits=2)
let f5 = Float.toStringWithRadix(6.0, ~radix=2)

let b1 = Bool.fromStringExn("true")

let buf = ArrayBuffer.make(8)
let ab1 = buf->ArrayBuffer.sliceToEnd(~start=2)

let re1 = RegExp.fromStringWithFlags("\\w+", ~flags="g")
