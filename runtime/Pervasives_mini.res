/* Exceptions */
external raise: exn => 'a = "%raise"

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
/*
  Note:

  Unified operations only work on `Pervasives`.
  That means we can't rely on it when building stdlib until we remove the `Pervasives_mini`.
*/

external \"~+": int => int = "%identity"
external \"~-": int => int = "%negint"

external \"+": (int, int) => int = "%addint"
external \"-": (int, int) => int = "%subint"
external \"*": (int, int) => int = "%mulint"
external \"/": (int, int) => int = "%divint"
external \"%": (int, int) => int = "%modint"
external mod: (int, int) => int = "%modint"
external \"**": (int, int) => int = "%powint"

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

external land: (int, int) => int = "%andint"
external lor: (int, int) => int = "%orint"
external lxor: (int, int) => int = "%xorint"

external lsl: (int, int) => int = "%lslint"
external lsr: (int, int) => int = "%lsrint"
external asr: (int, int) => int = "%asrint"

/* Floating-point operations */

external \"~-.": float => float = "%negfloat"
external \"~+.": float => float = "%identity"
external \"+.": (float, float) => float = "%addfloat"
external \"-.": (float, float) => float = "%subfloat"
external \"*.": (float, float) => float = "%mulfloat"
external \"/.": (float, float) => float = "%divfloat"

/* String operations */

external \"++": (string, string) => string = "%string_concat"

/* Unit operations */

external ignore: 'a => unit = "%ignore"

/* References */

type ref<'a> = {mutable contents: 'a}
external ref: 'a => ref<'a> = "%makeref"
external \":=": (ref<'a>, 'a) => unit = "%refset"
