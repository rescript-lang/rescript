// Exercise migrations from Js.Math to Math

let e = Js.Math._E
let pi = Js.Math._PI
let ln2 = Js.Math._LN2
let ln10 = Js.Math._LN10
let log2e = Js.Math._LOG2E
let log10e = Js.Math._LOG10E
let sqrt_half = Js.Math._SQRT1_2
let sqrt2c = Js.Math._SQRT2

let absInt1 = Js.Math.abs_int(-5)
let absFloat1 = Js.Math.abs_float(-3.5)

let acos1 = Js.Math.acos(1.0)
let acosh1 = Js.Math.acosh(1.5)
let asinh1 = Js.Math.asinh(1.0)
let asin1 = Js.Math.asin(0.5)
let atan1 = Js.Math.atan(1.0)
let atanh1 = Js.Math.atanh(0.5)

let atan21 = Js.Math.atan2(~y=0.0, ~x=10.0, ())

let cbrt1 = Js.Math.cbrt(27.0)

let ceilInt1 = Js.Math.unsafe_ceil_int(3.2)
let ceilInt2 = Js.Math.unsafe_ceil_int(3.2)
let ceilFloat1 = Js.Math.ceil_float(3.2)

let clz1 = Js.Math.clz32(255)

let cos1 = Js.Math.cos(0.0)
let cosh1 = Js.Math.cosh(0.0)
let exp1 = Js.Math.exp(1.0)
let expm11 = Js.Math.expm1(1.0)
let log1p1 = Js.Math.log1p(1.0)

let floorInt1 = Js.Math.unsafe_floor_int(3.7)
let floorInt2 = Js.Math.unsafe_floor_int(3.7)
let floorFloat1 = Js.Math.floor_float(3.7)

let fround1 = Js.Math.fround(5.05)

let hypot1 = Js.Math.hypot(3.0, 4.0)
let hypotMany1 = Js.Math.hypotMany([3.0, 4.0, 12.0])

let imul1 = Js.Math.imul(3, 4)

let log1 = Js.Math.log(Js.Math._E)
let log10_1 = Js.Math.log10(1000.0)
let log2_1 = Js.Math.log2(512.0)

let maxInt1 = Js.Math.max_int(1, 2)
let maxIntMany1 = Js.Math.maxMany_int([1, 10, 3])
let maxFloat1 = Js.Math.max_float(1.5, 2.5)
let maxFloatMany1 = Js.Math.maxMany_float([1.5, 2.5, 0.5])

let minInt1 = Js.Math.min_int(1, 2)
let minIntMany1 = Js.Math.minMany_int([1, 10, 3])
let minFloat1 = Js.Math.min_float(1.5, 2.5)
let minFloatMany1 = Js.Math.minMany_float([1.5, 2.5, 0.5])

let powInt1 = Js.Math.pow_int(~base=3, ~exp=4)
let powFloat1 = Js.Math.pow_float(~base=3.0, ~exp=4.0)

let rand1 = Js.Math.random()

let roundUnsafe1 = Js.Math.unsafe_round(3.7)
let round1 = Js.Math.round(3.7)

let signInt1 = Js.Math.sign_int(-5)
let signFloat1 = Js.Math.sign_float(-5.0)

let sin1 = Js.Math.sin(0.0)
let sinh1 = Js.Math.sinh(0.0)
let sqrt1 = Js.Math.sqrt(9.0)
let tan1 = Js.Math.tan(0.5)
let tanh1 = Js.Math.tanh(0.0)

let truncUnsafe1 = Js.Math.unsafe_trunc(3.7)
let trunc1 = Js.Math.trunc(3.7)
