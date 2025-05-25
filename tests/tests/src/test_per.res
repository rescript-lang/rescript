/* Exceptions */

external raise: exn => 'a = "%raise"

let failwith = s => throw(Failure(s))
let invalid_arg = s => throw(Invalid_argument(s))

exception Exit

/* Composition operators */

external \"|>": ('a, 'a => 'b) => 'b = "%revapply"
external \"@@": ('a => 'b, 'a) => 'b = "%apply"

/* Debugging */

external __LOC__: string = "%loc_LOC"
external __MODULE__: string = "%loc_FILE"
external __LINE__: int = "%loc_LINE"
external __MODULE__: string = "%loc_MODULE"
external __POS__: (string, int, int, int) = "%loc_POS"

external __LOC_OF__: 'a => (string, 'a) = "%loc_LOC"
external __LINE_OF__: 'a => (int, 'a) = "%loc_LINE"
external __POS_OF__: 'a => ((string, int, int, int), 'a) = "%loc_POS"

/* Comparisons */

external \"=": ('a, 'a) => bool = "%equal"
external \"<>": ('a, 'a) => bool = "%notequal"
external \"<": ('a, 'a) => bool = "%lessthan"
external \">": ('a, 'a) => bool = "%greaterthan"
external \"<=": ('a, 'a) => bool = "%lessequal"
external \">=": ('a, 'a) => bool = "%greaterequal"
external compare: ('a, 'a) => int = "%compare"

let min = (x, y) =>
  if x <= y {
    x
  } else {
    y
  }
let max = (x, y) =>
  if x >= y {
    x
  } else {
    y
  }

external \"==": ('a, 'a) => bool = "%eq"
external \"!=": ('a, 'a) => bool = "%noteq"

/* Boolean operations */

external not: bool => bool = "%boolnot"
external \"&": (bool, bool) => bool = "%sequand"
external \"&&": (bool, bool) => bool = "%sequand"
external or: (bool, bool) => bool = "%sequor"
external \"||": (bool, bool) => bool = "%sequor"

/* Integer operations */

external \"~-": int => int = "%negint"
external \"~+": int => int = "%identity"
external succ: int => int = "%succint"
external pred: int => int = "%predint"
external \"+": (int, int) => int = "%addint"
external \"-": (int, int) => int = "%subint"
external \"*": (int, int) => int = "%mulint"
external \"/": (int, int) => int = "%divint"
external mod: (int, int) => int = "%modint"

let abs = x =>
  if x >= 0 {
    x
  } else {
    -x
  }

external land: (int, int) => int = "%andint"
external lor: (int, int) => int = "%orint"
external lxor: (int, int) => int = "%xorint"

let lnot = x => lxor(x, -1)

external lsl: (int, int) => int = "%lslint"
external lsr: (int, int) => int = "%lsrint"
external asr: (int, int) => int = "%asrint"

let max_int = lsr(-1, 1)
let min_int = max_int + 1

/* Floating-point operations */

external \"~-.": float => float = "%negfloat"
external \"~+.": float => float = "%identity"
external \"+.": (float, float) => float = "%addfloat"
external \"-.": (float, float) => float = "%subfloat"
external \"*.": (float, float) => float = "%mulfloat"
external \"/.": (float, float) => float = "%divfloat"
external \"**": (float, float) => float = "?power_float"
external exp: float => float = "?exp_float"
external expm1: float => float = "?expm1_float"
external acos: float => float = "?acos_float"
external asin: float => float = "?asin_float"
external atan: float => float = "?atan_float"
external atan2: (float, float) => float = "?atan2_float"
external hypot: (float, float) => float = "?hypot_float"
external cos: float => float = "?cos_float"
external cosh: float => float = "?cosh_float"
external log: float => float = "?log_float"
external log10: float => float = "?log10_float"
external log1p: float => float = "?log1p_float"
external sin: float => float = "?sin_float"
external sinh: float => float = "?sinh_float"
external sqrt: float => float = "?sqrt_float"
external tan: float => float = "?tan_float"
external tanh: float => float = "?tanh_float"
external ceil: float => float = "?ceil_float"
external floor: float => float = "?floor_float"
external abs_float: float => float = "%absfloat"
external copysign: (float, float) => float = "?copysign_float"
external mod_float: (float, float) => float = "%modfloat"
external frexp: float => (float, int) = "?frexp_float"
external ldexp: (float, int) => float = "?ldexp_float"
external modf: float => (float, float) = "?modf_float"
external float: int => float = "%floatofint"
external float_of_int: int => float = "%floatofint"
external truncate: float => int = "%intoffloat"
external int_of_float: float => int = "%intoffloat"

type fpclass =
  | FP_normal
  | FP_subnormal
  | FP_zero
  | FP_infinite
  | FP_nan
external classify_float: float => fpclass = "?classify_float"

external string_length: string => int = "%string_length"

/* Character operations -- more in module Char */

external int_of_char: char => int = "%identity"
external unsafe_char_of_int: int => char = "%identity"
let char_of_int = n =>
  if n < 0 || n > 255 {
    invalid_arg("char_of_int")
  } else {
    unsafe_char_of_int(n)
  }

/* Unit operations */

external ignore: 'a => unit = "%ignore"

/* Pair operations */

external fst: (('a, 'b)) => 'a = "%field0"
external snd: (('a, 'b)) => 'b = "%field1"

/* References */

type ref<'a> = {mutable contents: 'a}
external ref: 'a => ref<'a> = "%makeref"
external \"!": ref<'a> => 'a = "%refget"
external \":=": (ref<'a>, 'a) => unit = "%refset"
external incr: ref<int> => unit = "%incr"
external decr: ref<int> => unit = "%decr"
