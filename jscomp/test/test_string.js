'use strict';

var Caml_bytes = require("../../lib/js/caml_bytes.js");
var Caml_string = require("../../lib/js/caml_string.js");

function f(x) {
  switch (x) {
    case "aaaabb" :
        return 0;
    case "bbbb" :
        return 1;
    default:
      throw {
            RE_EXN_ID: "Assert_failure",
            _1: [
              "test_string.res",
              5,
              17
            ],
            Error: new Error()
          };
  }
}

function a(x) {
  return "helloworldhello" + x;
}

function b(y, x) {
  return y + ("helloworldhello" + x);
}

function c(x, y) {
  return x + "hellohiuhi" + y;
}

var v = "xx".length;

function h(s, b) {
  if (Caml_string.get(s, 0) === /* 'a' */97 && Caml_bytes.get(b, 0) === /* 'b' */98) {
    return Caml_string.get(s, 1) === Caml_bytes.get(b, 2);
  } else {
    return false;
  }
}

exports.f = f;
exports.a = a;
exports.b = b;
exports.c = c;
exports.v = v;
exports.h = h;
/* No side effect */
