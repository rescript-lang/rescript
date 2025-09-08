open Js.Math

let suites = {
  open Mt
  list{
    ("_E", _ => ApproxThreshold(0.001, 2.718, _E)),
    ("_LN2", _ => ApproxThreshold(0.001, 0.693, _LN2)),
    ("_LN10", _ => ApproxThreshold(0.001, 2.303, _LN10)),
    ("_LOG2E", _ => ApproxThreshold(0.001, 1.443, _LOG2E)),
    ("_LOG10E", _ => ApproxThreshold(0.001, 0.434, _LOG10E)),
    ("_PI", _ => ApproxThreshold(0.00001, 3.14159, _PI)),
    ("_SQRT1_2", _ => ApproxThreshold(0.001, 0.707, _SQRT1_2)),
    ("_SQRT2", _ => ApproxThreshold(0.001, 1.414, _SQRT2)),
    ("abs_int", _ => Eq(4, abs_int(-4))),
    ("abs_float", _ => Eq(1.2, abs_float(-1.2))),
    ("acos", _ => ApproxThreshold(0.001, 1.159, acos(0.4))),
    ("acosh", _ => ApproxThreshold(0.001, 0.622, acosh(1.2))),
    ("asin", _ => ApproxThreshold(0.001, 0.411, asin(0.4))),
    ("asinh", _ => ApproxThreshold(0.001, 0.390, asinh(0.4))),
    ("atan", _ => ApproxThreshold(0.001, 0.380, atan(0.4))),
    ("atanh", _ => ApproxThreshold(0.001, 0.423, atanh(0.4))),
    ("atan2", _ => ApproxThreshold(0.001, 0.588, atan2(~x=0.6, ~y=0.4, ()))),
    ("cbrt", _ => Eq(2., cbrt(8.))),
    ("unsafe_ceil_int", _ => Eq(4, unsafe_ceil_int(3.2))),
    ("ceil_int", _ => Eq(4, ceil_int(3.2))),
    ("ceil_float", _ => Eq(4., ceil_float(3.2))),
    ("cos", _ => ApproxThreshold(0.001, 0.921, cos(0.4))),
    ("cosh", _ => ApproxThreshold(0.001, 1.081, cosh(0.4))),
    ("exp", _ => ApproxThreshold(0.001, 1.491, exp(0.4))),
    ("expm1", _ => ApproxThreshold(0.001, 0.491, expm1(0.4))),
    ("unsafe_floor_int", _ => Eq(3, unsafe_floor_int(3.2))),
    ("floor_int", _ => Eq(3, floor_int(3.2))),
    ("floor_float", _ => Eq(3., floor_float(3.2))),
    ("fround", _ => Approx(3.2, fround(3.2))),
    ("hypot", _ => ApproxThreshold(0.001, 0.721, hypot(0.4, 0.6))),
    ("hypotMany", _ => ApproxThreshold(0.001, 1.077, hypotMany([0.4, 0.6, 0.8]))),
    ("imul", _ => Eq(8, imul(4, 2))),
    ("log", _ => ApproxThreshold(0.001, -0.916, log(0.4))),
    ("log1p", _ => ApproxThreshold(0.001, 0.336, log1p(0.4))),
    ("log10", _ => ApproxThreshold(0.001, -0.397, log10(0.4))),
    ("log2", _ => ApproxThreshold(0.001, -1.321, log2(0.4))),
    ("max_int", _ => Eq(4, max_int(2, 4))),
    ("maxMany_int", _ => Eq(4, maxMany_int([2, 4, 3]))),
    ("max_float", _ => Eq(4.2, max_float(2.7, 4.2))),
    ("maxMany_float", _ => Eq(4.2, maxMany_float([2.7, 4.2, 3.9]))),
    ("min_int", _ => Eq(2, min_int(2, 4))),
    ("minMany_int", _ => Eq(2, minMany_int([2, 4, 3]))),
    ("min_float", _ => Eq(2.7, min_float(2.7, 4.2))),
    ("minMany_float", _ => Eq(2.7, minMany_float([2.7, 4.2, 3.9]))),
    (
      "random",
      _ => Ok({
        let a = random()
        a >= 0. && a < 1.
      }),
    ),
    (
      "random_int",
      _ => Ok({
        let a = random_int(1, 3)
        a >= 1 && a < 3
      }),
    ),
    ("unsafe_round", _ => Eq(3, unsafe_round(3.2))),
    ("round", _ => Eq(3., round(3.2))),
    ("sign_int", _ => Eq(-1, sign_int(-4))),
    ("sign_float", _ => Eq(-1., sign_float(-4.2))),
    ("sign_float -0", _ => Eq(-0., sign_float(-0.))),
    ("sin", _ => ApproxThreshold(0.001, 0.389, sin(0.4))),
    ("sinh", _ => ApproxThreshold(0.001, 0.410, sinh(0.4))),
    ("sqrt", _ => ApproxThreshold(0.001, 0.632, sqrt(0.4))),
    ("tan", _ => ApproxThreshold(0.001, 0.422, tan(0.4))),
    ("tanh", _ => ApproxThreshold(0.001, 0.379, tanh(0.4))),
    ("unsafe_trunc", _ => Eq(4, unsafe_trunc(4.2156))),
    ("trunc", _ => Eq(4., trunc(4.2156))),
  }
}

Mt.from_pair_suites(__MODULE__, suites)
