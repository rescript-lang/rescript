// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

let Test_common = require("./test_common.js");
let Caml_exceptions = require("../../lib/js/caml_exceptions.js");

let Local = /* @__PURE__ */Caml_exceptions.create("Test_exception.Local");

function f() {
  throw new Error(Local, {
    cause: {
      RE_EXN_ID: Local,
      _1: 3
    }
  });
}

function g() {
  throw new Error("Not_found", {
    cause: {
      RE_EXN_ID: "Not_found"
    }
  });
}

function h() {
  throw new Error(Test_common.U, {
    cause: {
      RE_EXN_ID: Test_common.U,
      _1: 3
    }
  });
}

function x() {
  throw new Error(Test_common.H, {
    cause: {
      RE_EXN_ID: Test_common.H
    }
  });
}

function xx() {
  throw new Error("Invalid_argument", {
    cause: {
      RE_EXN_ID: "Invalid_argument",
      _1: "x"
    }
  });
}

let Nullary = /* @__PURE__ */Caml_exceptions.create("Test_exception.Nullary");

let a = {
  RE_EXN_ID: Nullary
};

exports.Local = Local;
exports.f = f;
exports.g = g;
exports.h = h;
exports.x = x;
exports.xx = xx;
exports.Nullary = Nullary;
exports.a = a;
/* No side effect */
