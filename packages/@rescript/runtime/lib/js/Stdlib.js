'use strict';

let Stdlib_Global = require("./Stdlib_Global.js");
let Stdlib_JsError = require("./Stdlib_JsError.js");
let Primitive_object = require("./Primitive_object.js");

function assertEqual(a, b) {
  if (!Primitive_object.notequal(a, b)) {
    return;
  }
  throw {
    RE_EXN_ID: "Assert_failure",
    _1: [
      "Stdlib.res",
      153,
      4
    ],
    Error: new Error()
  };
}

let TimeoutId = Stdlib_Global.TimeoutId;

let IntervalId = Stdlib_Global.IntervalId;

let $$Array;

let $$BigInt;

let Bool;

let Console;

let $$DataView;

let $$Date;

let Dict;

let Exn;

let $$Error;

let Float;

let Int;

let $$Intl;

let JsError;

let JsExn;

let $$JSON;

let Lazy;

let List;

let $$Math;

let Null;

let Nullable;

let $$Object;

let Option;

let Ordering;

let Pair;

let $$Promise;

let $$RegExp;

let Result;

let $$String;

let $$Symbol;

let Type;

let $$Iterator;

let $$AsyncIterator;

let $$Map;

let $$WeakMap;

let $$Set;

let $$WeakSet;

let $$ArrayBuffer;

let $$TypedArray;

let $$Float32Array;

let $$Float64Array;

let $$Int8Array;

let $$Int16Array;

let $$Int32Array;

let $$Uint8Array;

let $$Uint16Array;

let $$Uint32Array;

let $$Uint8ClampedArray;

let $$BigInt64Array;

let $$BigUint64Array;

let panic = Stdlib_JsError.panic;

exports.TimeoutId = TimeoutId;
exports.IntervalId = IntervalId;
exports.$$Array = $$Array;
exports.$$BigInt = $$BigInt;
exports.Bool = Bool;
exports.Console = Console;
exports.$$DataView = $$DataView;
exports.$$Date = $$Date;
exports.Dict = Dict;
exports.Exn = Exn;
exports.$$Error = $$Error;
exports.Float = Float;
exports.Int = Int;
exports.$$Intl = $$Intl;
exports.JsError = JsError;
exports.JsExn = JsExn;
exports.$$JSON = $$JSON;
exports.Lazy = Lazy;
exports.List = List;
exports.$$Math = $$Math;
exports.Null = Null;
exports.Nullable = Nullable;
exports.$$Object = $$Object;
exports.Option = Option;
exports.Ordering = Ordering;
exports.Pair = Pair;
exports.$$Promise = $$Promise;
exports.$$RegExp = $$RegExp;
exports.Result = Result;
exports.$$String = $$String;
exports.$$Symbol = $$Symbol;
exports.Type = Type;
exports.$$Iterator = $$Iterator;
exports.$$AsyncIterator = $$AsyncIterator;
exports.$$Map = $$Map;
exports.$$WeakMap = $$WeakMap;
exports.$$Set = $$Set;
exports.$$WeakSet = $$WeakSet;
exports.$$ArrayBuffer = $$ArrayBuffer;
exports.$$TypedArray = $$TypedArray;
exports.$$Float32Array = $$Float32Array;
exports.$$Float64Array = $$Float64Array;
exports.$$Int8Array = $$Int8Array;
exports.$$Int16Array = $$Int16Array;
exports.$$Int32Array = $$Int32Array;
exports.$$Uint8Array = $$Uint8Array;
exports.$$Uint16Array = $$Uint16Array;
exports.$$Uint32Array = $$Uint32Array;
exports.$$Uint8ClampedArray = $$Uint8ClampedArray;
exports.$$BigInt64Array = $$BigInt64Array;
exports.$$BigUint64Array = $$BigUint64Array;
exports.panic = panic;
exports.assertEqual = assertEqual;
/* No side effect */
