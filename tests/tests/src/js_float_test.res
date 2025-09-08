open Js_float

let suites = {
  open Mt
  list{
    ("_NaN <> _NaN", _ => Eq(false, _NaN == _NaN)),
    ("isNaN - _NaN", _ => Eq(true, isNaN(_NaN))),
    ("isNaN - 0.", _ => Eq(false, isNaN(0.))),
    ("isFinite - infinity", _ => Eq(false, isFinite(infinity))),
    ("isFinite - neg_infinity", _ => Eq(false, isFinite(neg_infinity))),
    ("isFinite - _NaN", _ => Eq(false, isFinite(_NaN))),
    ("isFinite - 0.", _ => Eq(true, isFinite(0.))),
    ("toExponential", _ => Eq("1.23456e+2", toExponential(123.456))),
    ("toExponential - large number", _ => Eq("1.2e+21", toExponential(1.2e21))),
    (
      "toExponentialWithPrecision - digits:2",
      _ => Eq("1.23e+2", toExponentialWithPrecision(123.456, ~digits=2)),
    ),
    (
      "toExponentialWithPrecision - digits:4",
      _ => Eq("1.2346e+2", toExponentialWithPrecision(123.456, ~digits=4)),
    ),
    (
      "toExponentialWithPrecision - digits:20",
      _ => Eq("0.00000000000000000000e+0", toExponentialWithPrecision(0., ~digits=20)),
    ),
    (__LOC__, _ => ThrowAny(() => ignore(toExponentialWithPrecision(0., ~digits=101)))),
    (
      "toExponentialWithPrecision - digits:-1",
      _ => ThrowAny(() => ignore(toExponentialWithPrecision(0., ~digits=-1))),
    ),
    ("toFixed", _ => Eq("123", toFixed(123.456))),
    ("toFixed - large number", _ => Eq("1.2e+21", toFixed(1.2e21))),
    (
      "toFixedWithPrecision - digits:2",
      _ => Eq("123.46", toFixedWithPrecision(123.456, ~digits=2)),
    ),
    (
      "toFixedWithPrecision - digits:4",
      _ => Eq("123.4560", toFixedWithPrecision(123.456, ~digits=4)),
    ),
    (
      "toFixedWithPrecision - digits:20",
      _ => Eq("0.00000000000000000000", toFixedWithPrecision(0., ~digits=20)),
    ),
    (
      "toFixedWithPrecision - digits:101",
      _ => ThrowAny(() => ignore(toFixedWithPrecision(0., ~digits=101))),
    ),
    (
      "toFixedWithPrecision - digits:-1",
      _ => ThrowAny(() => ignore(toFixedWithPrecision(0., ~digits=-1))),
    ),
    ("toPrecision", _ => Eq("123.456", toPrecision(123.456))),
    ("toPrecision - large number", _ => Eq("1.2e+21", toPrecision(1.2e21))),
    (
      "toPrecisionWithPrecision - digits:2",
      _ => Eq("1.2e+2", toPrecisionWithPrecision(123.456, ~digits=2)),
    ),
    (
      "toPrecisionWithPrecision - digits:4",
      _ => Eq("123.5", toPrecisionWithPrecision(123.456, ~digits=4)),
    ),
    (
      "toPrecisionWithPrecision - digits:20",
      _ => Eq("0.0000000000000000000", toPrecisionWithPrecision(0., ~digits=20)),
    ),
    (__LOC__, _ => ThrowAny(() => ignore(toPrecisionWithPrecision(0., ~digits=101)))),
    (
      "toPrecisionWithPrecision - digits:-1",
      _ => ThrowAny(() => ignore(toPrecisionWithPrecision(0., ~digits=-1))),
    ),
    ("toString", _ => Eq("1.23", toString(1.23))),
    ("toString - large number", _ => Eq("1.2e+21", toString(1.2e21))),
    (
      "toStringWithRadix - radix:2",
      _ => Eq(
        "1111011.0111010010111100011010100111111011111001110111",
        toStringWithRadix(123.456, ~radix=2),
      ),
    ),
    (
      "toStringWithRadix - radix:16",
      _ => Eq("7b.74bc6a7ef9dc", toStringWithRadix(123.456, ~radix=16)),
    ),
    ("toStringWithRadix - radix:36", _ => Eq("3f", toStringWithRadix(123., ~radix=36))),
    ("toStringWithRadix - radix:37", _ => ThrowAny(() => ignore(toStringWithRadix(0., ~radix=37)))),
    ("toStringWithRadix - radix:1", _ => ThrowAny(() => ignore(toStringWithRadix(0., ~radix=1)))),
    ("toStringWithRadix - radix:-1", _ => ThrowAny(() => ignore(toStringWithRadix(0., ~radix=-1)))),
    ("fromString - 123", _ => Eq(123., fromString("123"))),
    ("fromString - 12.3", _ => Eq(12.3, fromString("12.3"))),
    ("fromString - empty string", _ => Eq(0., fromString(""))),
    ("fromString - 0x11", _ => Eq(17., fromString("0x11"))),
    ("fromString - 0b11", _ => Eq(3., fromString("0b11"))),
    ("fromString - 0o11", _ => Eq(9., fromString("0o11"))),
    ("fromString - invalid string", _ => Eq(true, isNaN(fromString("foo")))),
  }
}

Mt.from_pair_suites(__MODULE__, suites)
