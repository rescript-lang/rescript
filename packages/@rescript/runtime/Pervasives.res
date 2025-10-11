@deprecated("Do not use. This will be removed in v13")
external __unsafe_cast: 'a => 'b = "%identity"

/* Exceptions */

/**
Raises the given exception, terminating execution unless caught by a surrounding try/catch block.

## Examples

```rescript
exception MyException(string)

let result = try {
  throw(MyException("Out of milk"))
} catch {
| MyException(message) => "Caught exception: " ++ message
}

result == "Caught exception: Out of milk"
```
*/
external throw: exn => 'a = "%raise"

@deprecated({
  reason: "`raise` has been renamed to `throw` to align with JavaScript vocabulary. Please use `throw` instead",
  migrate: throw(),
})
external raise: exn => 'a = "%raise"

@deprecated("Use custom exception instead")
let failwith = s => throw(Failure(s))

@deprecated("Use custom exception instead")
let invalid_arg = s => throw(Invalid_argument(s))

@deprecated("Use custom exception instead") exception Exit

/* Composition operators */

@deprecated("This will be removed in v13")
external \"|>": ('a, 'a => 'b) => 'b = "%revapply"

@deprecated("This will be removed in v13")
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
external \"<<": ('a, 'a) => 'a = "%lsl"
external mod: ('a, 'a) => 'a = "%mod"
external \"**": ('a, 'a) => 'a = "%pow"

external \"~~~": 'a => 'a = "%bitnot"
external \"|||": ('a, 'a) => 'a = "%bitor"
external \"^^^": ('a, 'a) => 'a = "%bitxor"
external \"&&&": ('a, 'a) => 'a = "%bitand"

external \">>": ('a, 'a) => 'a = "%asr"
external \">>>": ('a, 'a) => 'a = "%lsr"

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

@deprecated("Use `x => x + 1` instead. This will be removed in v13")
external succ: int => int = "%succint"

@deprecated("Use `x => x - 1` instead. This will be removed in v13")
external pred: int => int = "%predint"

@deprecated("Use `Math.abs` instead. This will be removed in v13")
let abs = x =>
  if x >= 0 {
    x
  } else {
    -x
  }

@deprecated("Use `Int.bitwiseAnd` instead. This will be removed in v13")
external land: (int, int) => int = "%andint"

@deprecated("Use `Int.bitwiseOr` instead. This will be removed in v13")
external lor: (int, int) => int = "%orint"

@deprecated("Use `Int.bitwiseXor` instead. This will be removed in v13")
external lxor: (int, int) => int = "%xorint"

@deprecated("Use `Int.bitwiseNot` instead. This will be removed in v13")
external lnot: int => int = "%bitnot_int"

@deprecated("Use `Int.shiftLeft` instead. This will be removed in v13")
external lsl: (int, int) => int = "%lslint"

@deprecated("Use `Int.shiftRightUnsigned` instead. This will be removed in v13")
external lsr: (int, int) => int = "%lsrint"

@deprecated("Use `Int.shiftRight` instead. This will be removed in v13")
external asr: (int, int) => int = "%asrint"

@deprecated("Use `Int.Constants.maxValue` instead. This will be removed in v13")
let max_int = lsr(-1, 1)

@deprecated("Use `Int.Constants.minValue` instead. This will be removed in v13")
let min_int =
  max_int + 1

/* Floating-point operations */

external \"~-.": float => float = "%negfloat"
external \"~+.": float => float = "%identity"
external \"+.": (float, float) => float = "%addfloat"
external \"-.": (float, float) => float = "%subfloat"
external \"*.": (float, float) => float = "%mulfloat"
external \"/.": (float, float) => float = "%divfloat"

@deprecated("Use `Math.exp` instead. This will be removed in v13") @val @scope("Math")
external exp: float => float = "exp"

@deprecated("Use `Math.acos` instead. This will be removed in v13") @val @scope("Math")
external acos: float => float = "acos"

@deprecated("Use `Math.asin` instead. This will be removed in v13") @val @scope("Math")
external asin: float => float = "asin"

@deprecated("Use `Math.atan` instead. This will be removed in v13") @val @scope("Math")
external atan: float => float = "atan"

@deprecated("Use `Math.atan2` instead. This will be removed in v13") @val @scope("Math")
external atan2: (float, float) => float = "atan2"

@deprecated("Use `Math.cos` instead. This will be removed in v13") @val @scope("Math")
external cos: float => float = "cos"

@deprecated("Use `Math.cosh` instead. This will be removed in v13") @val @scope("Math")
external cosh: float => float = "cosh"

@deprecated("Use `Math.log` instead. This will be removed in v13") @val @scope("Math")
external log: float => float = "log"

@deprecated("Use `Math.log10` instead. This will be removed in v13") @val @scope("Math")
external log10: float => float = "log10"

@deprecated("Use `Math.log1p` instead. This will be removed in v13") @val @scope("Math")
external log1p: float => float = "log1p"

@deprecated("Use  `Math.sin` instead. This will be removed in v13") @val @scope("Math")
external sin: float => float = "sin"

@deprecated("Use `Math.sinh` instead. This will be removed in v13") @val @scope("Math")
external sinh: float => float = "sinh"

@deprecated("Use `Math.sqrt` instead. This will be removed in v13") @val @scope("Math")
external sqrt: float => float = "sqrt"

@deprecated("Use `Math.tan` instead. This will be removed in v13") @val @scope("Math")
external tan: float => float = "tan"

@deprecated("Use `Math.tanh` instead. This will be removed in v13") @val @scope("Math")
external tanh: float => float = "tanh"

@deprecated("Use `Math.ceil` instead. This will be removed in v13") @val @scope("Math")
external ceil: float => float = "ceil"

@deprecated("Use `Math.floor` instead. This will be removed in v13") @val @scope("Math")
external floor: float => float = "floor"

@deprecated("Use `Math.abs` instead. This will be removed in v13") @val @scope("Math")
external abs_float: float => float = "abs"

@deprecated("Use `%` instead. This will be removed in v13")
external mod_float: (float, float) => float = "%modfloat"

@deprecated("Use `Int.toFloat` instead. This will be removed in v13")
external float: int => float = "%floatofint"

@deprecated("Use `Int.toFloat` instead. This will be removed in v13")
external float_of_int: int => float = "%floatofint"

@deprecated("Use `Float.toInt` instead. This will be removed in v13")
external truncate: float => int = "%intoffloat"

@deprecated("Use `Float.toInt` instead. This will be removed in v13")
external int_of_float: float => int = "%intoffloat"

@deprecated("Use `Float.positiveInfinity` instead. This will be removed in v13")
let infinity = 0x1p2047

@deprecated("Use `Float.negativeInfinity` instead. This will be removed in v13")
let neg_infinity = -0x1p2047

@deprecated("Use `Float.nan` instead. This will be removed in v13") @val @scope("Number")
external nan: float = "NaN"

@deprecated("Use `Float.Constants.maxValue` instead. This will be removed in v13")
let max_float = 1.79769313486231571e+308 /* 0x1.ffff_ffff_ffff_fp+1023 */

@deprecated("Use `Float.Constants.minValue` instead. This will be removed in v13")
let min_float = 2.22507385850720138e-308 /* 0x1p-1022 */

@deprecated("Use `Float.Constants.epsilon` instead. This will be removed in v13")
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

@deprecated("Use type `string` and `String.charCodeAt` instead. This will be removed in v13")
external int_of_char: char => int = "%identity"

@deprecated("Use type `string` and `String.fromCharCode` instead. This will be removed in v13")
external unsafe_char_of_int: int => char = "%identity"

@deprecated("Use type `string` and `String.fromCharCode` instead. This will be removed in v13")
let char_of_int = n =>
  if n < 0 || n > 255 {
    invalid_arg("char_of_int")
  } else {
    unsafe_char_of_int(n)
  }

/* Unit operations */

external ignore: 'a => unit = "%ignore"

/* Pair operations */

@deprecated("Use `Pair.first` instead. This will be removed in v13")
external fst: (('a, 'b)) => 'a = "%field0"

@deprecated("Use `Pair.second` instead. This will be removed in v13")
external snd: (('a, 'b)) => 'b = "%field1"

/* References */

type ref<'a> = {mutable contents: 'a}
external ref: 'a => ref<'a> = "%makeref"
external \":=": (ref<'a>, 'a) => unit = "%refset"

@deprecated("Do not use. This will be removed in v13")
external \"!": ref<'a> => 'a = "%refget"

@deprecated("Use `Int.Ref.increment` instead. This will be removed in v13")
external incr: ref<int> => unit = "%incr"

@deprecated("Use `Int.Ref.decrement` instead. This will be removed in v13")
external decr: ref<int> => unit = "%decr"

/* String conversion functions */

@deprecated("Use `Bool.toString` instead. This will be removed in v13")
let string_of_bool = b =>
  if b {
    "true"
  } else {
    "false"
  }

@deprecated("Use `Bool.fromString` instead. This will be removed in v13")
let bool_of_string = param =>
  switch param {
  | "true" => true
  | "false" => false
  | _ => invalid_arg("bool_of_string")
  }

@deprecated("Use `Bool.fromString` instead. This will be removed in v13")
let bool_of_string_opt = param =>
  switch param {
  | "true" => Some(true)
  | "false" => Some(false)
  | _ => None
  }

@deprecated("Use `Int.toString` instead. This will be removed in v13")
external string_of_int: int => string = "String"

@deprecated("Use `Int.fromString` instead. This will be removed in v13") @scope("Number")
external int_of_string: string => int = "parseInt"

@deprecated("Use `Int.fromString` instead. This will be removed in v13")
let int_of_string_opt = s =>
  switch int_of_string(s) {
  | n if n == %raw("NaN") => None
  | n => Some(n)
  }

@deprecated("Use `String.get` instead. This will be removed in v13")
external string_get: (string, int) => char = "%string_safe_get"

/* List operations -- more in module List */

@deprecated("Use `List.concat` instead. This will be removed in v13")
let rec \"@" = (l1, l2) =>
  switch l1 {
  | list{} => l2
  | list{hd, ...tl} => list{hd, ...\"@"(tl, l2)}
  }

/* Miscellaneous */

@deprecated("This will be removed in v13")
type int32 = int
