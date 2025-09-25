/* Migration tests for Js.res type deprecations */

// Type alias migrations exercised via externals
external nullT: Js.null<int> = "nullT"
external nullableT: Js.nullable<string> = "nullableT"
external nullUndefT: Js.null_undefined<float> = "nullUndefT"
