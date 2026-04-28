'use strict';


function classify(x) {
  let ty = typeof x;
  if (ty === "undefined") {
    return "JSUndefined";
  } else if (x === null) {
    return "JSNull";
  } else if (ty === "number") {
    return {
      TAG: "JSNumber",
      _0: x
    };
  } else if (ty === "bigint") {
    return {
      TAG: "JSBigInt",
      _0: x
    };
  } else if (ty === "string") {
    return {
      TAG: "JSString",
      _0: x
    };
  } else if (ty === "boolean") {
    if (x === true) {
      return "JSTrue";
    } else {
      return "JSFalse";
    }
  } else if (ty === "symbol") {
    return {
      TAG: "JSSymbol",
      _0: x
    };
  } else if (ty === "function") {
    return {
      TAG: "JSFunction",
      _0: x
    };
  } else {
    return {
      TAG: "JSObject",
      _0: x
    };
  }
}

function test(x, v) {
  switch (v) {
    case "Undefined" :
      return typeof x === "undefined";
    case "Null" :
      return x === null;
    case "Boolean" :
      return typeof x === "boolean";
    case "Number" :
      return typeof x === "number";
    case "String" :
      return typeof x === "string";
    case "Function" :
      return typeof x === "function";
    case "Object" :
      return typeof x === "object";
    case "Symbol" :
      return typeof x === "symbol";
    case "BigInt" :
      return typeof x === "bigint";
  }
}

exports.test = test;
exports.classify = classify;
/* No side effect */
