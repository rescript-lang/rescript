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

/* Floating-point operations */

external \"~-.": float => float = "%negfloat"
external \"~+.": float => float = "%identity"
external \"+.": (float, float) => float = "%addfloat"
external \"-.": (float, float) => float = "%subfloat"
external \"*.": (float, float) => float = "%mulfloat"
external \"/.": (float, float) => float = "%divfloat"

/* String and byte sequence operations -- more in modules String and Bytes */

external \"++": (string, string) => string = "%string_concat"

/* Unit operations */

external ignore: 'a => unit = "%ignore"

/* References */

type ref<'a> = {mutable contents: 'a}
external ref: 'a => ref<'a> = "%makeref"
external \":=": (ref<'a>, 'a) => unit = "%refset"
