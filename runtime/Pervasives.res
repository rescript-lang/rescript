/**
   Since [others] depend on this file, its public mli files **should not
   export types** introduced here, otherwise it would cause 
   conflicts here.

   If the type exported here is also exported in modules from others,
   you will get a type not equivalent.
*/
@deprecated("Do not use. This will be removed in v13")
external /* Internal */

__unsafe_cast: 'a => 'b = "%identity"

/* Exceptions */

external raise: exn => 'a = "%raise"

@deprecated("Use custom exception instead")
let failwith = s => raise(Failure(s))

@deprecated("Use custom exception instead")
let invalid_arg = s => raise(Invalid_argument(s))

@deprecated("Use custom exception instead") exception Exit

/* Composition operators */

external \"|>": ('a, 'a => 'b) => 'b = "%revapply"
external \"@@": ('a => 'b, 'a) => 'b = "%apply"

/* Debugging */

external __LOC__: string = "%loc_LOC"
external __FILE__: string = "%loc_FILE"
external __LINE__: int = "%loc_LINE"
external __MODULE__: string = "%loc_MODULE"
external __POS__: (string, int, int, int) = "%loc_POS"

external __LOC_OF__: 'a => (string, 'a) = "%loc_LOC"
external __LINE_OF__: 'a => (int, 'a) = "%loc_LINE"
external __POS_OF__: 'a => ((string, int, int, int), 'a) = "%loc_POS"

/* Unified operations */

external \"~+": 'a => 'a = "%plus"
external \"~-": 'a => 'a = "%neg"

external \"+": ('a, 'a) => 'a = "%add"
external \"-": ('a, 'a) => 'a = "%sub"
external \"*": ('a, 'a) => 'a = "%mul"
external \"/": ('a, 'a) => 'a = "%div"
external \"%": ('a, 'a) => 'a = "%mod"
external mod: ('a, 'a) => 'a = "%mod"

/* Comparisons */
/* Note: Later comparisons will be converted to unified operations too */

external \"==": ('a, 'a) => bool = "%equal"
external \"!=": ('a, 'a) => bool = "%notequal"
external \"<": ('a, 'a) => bool = "%lessthan"
external \">": ('a, 'a) => bool = "%greaterthan"
external \"<=": ('a, 'a) => bool = "%lessequal"
external \">=": ('a, 'a) => bool = "%greaterequal"
external compare: ('a, 'a) => int = "%compare"
external min: ('a, 'a) => 'a = "%min"
external max: ('a, 'a) => 'a = "%max"
external \"===": ('a, 'a) => bool = "%eq"
external \"!==": ('a, 'a) => bool = "%noteq"

/* Boolean operations */

external not: bool => bool = "%boolnot"

external \"&&": (bool, bool) => bool = "%sequand"

external \"||": (bool, bool) => bool = "%sequor"

/* Integer operations */

external succ: int => int = "%succint"
external pred: int => int = "%predint"

@deprecated("Use Core instead. This will be removed in v13")
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

@deprecated("Use Core instead. This will be removed in v13")
let max_int = lsr(-1, 1)

@deprecated("Use Core instead. This will be removed in v13")
let min_int =
  max_int + 1

/* Floating-point operations */

external \"~-.": float => float = "%negfloat"
external \"~+.": float => float = "%identity"
external \"+.": (float, float) => float = "%addfloat"
external \"-.": (float, float) => float = "%subfloat"
external \"*.": (float, float) => float = "%mulfloat"
external \"/.": (float, float) => float = "%divfloat"

@deprecated("Use Core instead. This will be removed in v13") @val @scope("Math")
external \"**": (float, float) => float = "pow"

@deprecated("Use Core instead. This will be removed in v13") @val @scope("Math")
external exp: float => float = "exp"

@deprecated("Use Core instead. This will be removed in v13") @val @scope("Math")
external acos: float => float = "acos"

@deprecated("Use Core instead. This will be removed in v13") @val @scope("Math")
external asin: float => float = "asin"

@deprecated("Use Core instead. This will be removed in v13") @val @scope("Math")
external atan: float => float = "atan"

@deprecated("Use Core instead. This will be removed in v13") @val @scope("Math")
external atan2: (float, float) => float = "atan2"

@deprecated("Use Core instead. This will be removed in v13") @val @scope("Math")
external cos: float => float = "cos"

@deprecated("Use Core instead. This will be removed in v13") @val @scope("Math")
external cosh: float => float = "cosh"

@deprecated("Use Core instead. This will be removed in v13") @val @scope("Math")
external log: float => float = "log"

@deprecated("Use Core instead. This will be removed in v13") @val @scope("Math")
external log10: float => float = "log10"

@deprecated("Use Core instead. This will be removed in v13") @val @scope("Math")
external log1p: float => float = "log1p"

@deprecated("Use Core instead. This will be removed in v13") @val @scope("Math")
external sin: float => float = "sin"

@deprecated("Use Core instead. This will be removed in v13") @val @scope("Math")
external sinh: float => float = "sinh"

@deprecated("Use Core instead. This will be removed in v13") @val @scope("Math")
external sqrt: float => float = "sqrt"

@deprecated("Use Core instead. This will be removed in v13") @val @scope("Math")
external tan: float => float = "tan"

@deprecated("Use Core instead. This will be removed in v13") @val @scope("Math")
external tanh: float => float = "tanh"

@deprecated("Use Core instead. This will be removed in v13") @val @scope("Math")
external ceil: float => float = "ceil"

@deprecated("Use Core instead. This will be removed in v13") @val @scope("Math")
external floor: float => float = "floor"

@deprecated("Use Core instead. This will be removed in v13") @val @scope("Math")
external abs_float: float => float = "abs"

@deprecated("Use Core instead. This will be removed in v13")
external mod_float: (float, float) => float = "%modfloat"

@deprecated("Use Core instead. This will be removed in v13")
external float: int => float = "%floatofint"

@deprecated("Use Core instead. This will be removed in v13")
external float_of_int: int => float = "%floatofint"

@deprecated("Use Core instead. This will be removed in v13")
external truncate: float => int = "%intoffloat"

@deprecated("Use Core instead. This will be removed in v13")
external int_of_float: float => int = "%intoffloat"

@deprecated("Use Core instead. This will be removed in v13")
let infinity = 0x1p2047

@deprecated("Use Core instead. This will be removed in v13")
let neg_infinity = -0x1p2047

@deprecated("Use Core instead. This will be removed in v13") @val @scope("Number")
external nan: float = "NaN"

@deprecated("Use Core instead. This will be removed in v13")
let max_float = 1.79769313486231571e+308 /* 0x1.ffff_ffff_ffff_fp+1023 */

@deprecated("Use Core instead. This will be removed in v13")
let min_float = 2.22507385850720138e-308 /* 0x1p-1022 */

@deprecated("Use Core instead. This will be removed in v13")
let epsilon_float = 2.22044604925031308e-16 /* 0x1p-52 */

@deprecated("Do not use. This will be removed in v13")
type fpclass =
  | FP_normal
  | FP_subnormal
  | FP_zero
  | FP_infinite
  | FP_nan

@deprecated("Do not use. This will be removed in v13")
let classify_float = (x: float): fpclass =>
  if (%raw(`isFinite`): _ => _)(x) {
    if abs_float(x) >= /* 0x1p-1022 */ /* 2.22507385850720138e-308 */ min_float {
      FP_normal
    } else if x != 0. {
      FP_subnormal
    } else {
      FP_zero
    }
  } else if (%raw(`isNaN`): _ => _)(x) {
    FP_nan
  } else {
    FP_infinite
  }

/* String and byte sequence operations -- more in modules String and Bytes */

external \"++": (string, string) => string = "%string_concat"

/* Character operations -- more in module Char */

@deprecated("Use Core instead. This will be removed in v13")
external int_of_char: char => int = "%identity"

@deprecated("Use Core instead. This will be removed in v13")
external unsafe_char_of_int: int => char = "%identity"

@deprecated("Use Core instead. This will be removed in v13")
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

/* String conversion functions */

@deprecated("Use Core instead. This will be removed in v13")
let string_of_bool = b =>
  if b {
    "true"
  } else {
    "false"
  }

@deprecated("Use Core instead. This will be removed in v13")
let bool_of_string = param =>
  switch param {
  | "true" => true
  | "false" => false
  | _ => invalid_arg("bool_of_string")
  }

@deprecated("Use Core instead. This will be removed in v13")
let bool_of_string_opt = param =>
  switch param {
  | "true" => Some(true)
  | "false" => Some(false)
  | _ => None
  }

@deprecated("Use Core instead. This will be removed in v13")
external string_of_int: int => string = "String"

@deprecated("Use Core instead. This will be removed in v13") @scope("Number")
external int_of_string: string => int = "parseInt"

let int_of_string_opt = s =>
  switch int_of_string(s) {
  | n if n == %raw("NaN") => None
  | n => Some(n)
  }

@deprecated("Use Core instead. This will be removed in v13")
external string_get: (string, int) => char = "%string_safe_get"

/* List operations -- more in module List */

@deprecated("Use Core instead. This will be removed in v13")
let rec \"@" = (l1, l2) =>
  switch l1 {
  | list{} => l2
  | list{hd, ...tl} => list{hd, ...\"@"(tl, l2)}
  }

/* Miscellaneous */

type int32 = int
