// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

let Exn = require("rescript/lib/js/Exn.js");
let Primitive_exceptions = require("rescript/lib/js/Primitive_exceptions.js");

function test_js_error() {
  let e;
  try {
    e = JSON.parse(" {\"x\" : }");
  } catch (raw_err) {
    let err = Primitive_exceptions.internalToException(raw_err);
    if (err.RE_EXN_ID === Exn.$$Error) {
      console.log(err._1.stack);
      return;
    }
    throw err;
  }
  return e;
}

function test_js_error2() {
  try {
    return JSON.parse(" {\"x\" : }");
  } catch (raw_e) {
    let e = Primitive_exceptions.internalToException(raw_e);
    if (e.RE_EXN_ID === Exn.$$Error) {
      console.log(e._1.stack);
      throw e;
    }
    throw e;
  }
}

function example1() {
  let v;
  try {
    v = JSON.parse(" {\"x\"  }");
  } catch (raw_err) {
    let err = Primitive_exceptions.internalToException(raw_err);
    if (err.RE_EXN_ID === Exn.$$Error) {
      console.log(err._1.stack);
      return;
    }
    throw err;
  }
  return v;
}

function example2() {
  try {
    return JSON.parse(" {\"x\"}");
  } catch (raw_exn) {
    let exn = Primitive_exceptions.internalToException(raw_exn);
    if (exn.RE_EXN_ID === Exn.$$Error) {
      return;
    }
    throw exn;
  }
}

exports.test_js_error = test_js_error;
exports.test_js_error2 = test_js_error2;
exports.example1 = example1;
exports.example2 = example2;
/* No side effect */
