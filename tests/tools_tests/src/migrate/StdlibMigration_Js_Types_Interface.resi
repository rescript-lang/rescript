/* Migration tests for Js.res type deprecations */

// Type alias migrations exercised via externals
external nullT: Js.null<int> = "nullT"
external nullableT: Js.nullable<string> = "nullableT"
external nullUndefT: Js.null_undefined<float> = "nullUndefT"

// Js.Types migrations
external symbolT: Js.Types.symbol = "symbolT"
external objValT: Js.Types.obj_val = "objValT"
external functionValT: Js.Types.function_val = "functionValT"
