'use strict';

var Mt = require("./mt.js");
var Sys = require("../../lib/js/sys.js");
var Caml_sys = require("../../lib/js/caml_sys.js");
var Node_process = require("../../lib/js/node_process.js");
var Caml_js_exceptions = require("../../lib/js/caml_js_exceptions.js");

var suites = {
  contents: /* [] */0
};

var test_id = {
  contents: 0
};

function eq(loc, x, y) {
  test_id.contents = test_id.contents + 1 | 0;
  suites.contents = {
    hd: [
      loc + (" id " + String(test_id.contents)),
      (function (param) {
          return {
                  TAG: "Eq",
                  _0: x,
                  _1: y
                };
        })
    ],
    tl: suites.contents
  };
}

Node_process.putEnvVar("Caml_sys_poly_fill_test", "X");

var v = Caml_sys.sys_getenv("Caml_sys_poly_fill_test");

eq("File \"caml_sys_poly_fill_test.res\", line 11, characters 4-11", "X", (Node_process.deleteEnvVar("Caml_sys_poly_fill_test"), v));

Node_process.putEnvVar("Caml_sys_poly_fill_test", "Y");

var v$1 = Caml_sys.sys_getenv("Caml_sys_poly_fill_test");

eq("File \"caml_sys_poly_fill_test.res\", line 21, characters 4-11", "Y", (Node_process.deleteEnvVar("Caml_sys_poly_fill_test"), v$1));

Node_process.deleteEnvVar("Caml_sys_poly_fill_test");

var tmp;

try {
  tmp = Caml_sys.sys_getenv("Caml_sys_poly_fill_test");
}
catch (raw_exn){
  var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
  if (exn.RE_EXN_ID === "Not_found") {
    tmp = "Z";
  } else {
    throw exn;
  }
}

eq("File \"caml_sys_poly_fill_test.res\", line 31, characters 4-11", "Z", tmp);

console.log([
      Caml_sys.sys_getcwd(undefined),
      Caml_sys.sys_time(undefined),
      Sys.argv,
      Sys.executable_name
    ]);

Mt.from_pair_suites("Caml_sys_poly_fill_test", suites.contents);

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
/*  Not a pure module */
