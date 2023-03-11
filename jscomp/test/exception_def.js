'use strict';

var Mt = require("./mt.js");
var Caml_exceptions = require("../../lib/js/caml_exceptions.js");

var suites = {
  contents: /* [] */0
};

var test_id = {
  contents: 0
};

function eq(loc, x, y) {
  Mt.eq_suites(test_id, suites, loc, x, y);
}

var A = /* @__PURE__ */Caml_exceptions.create("Exception_def.A");

var A$1 = /* @__PURE__ */Caml_exceptions.create("Exception_def.U.A");

var U = {
  A: A$1
};

var H = {};

var Bx = /* @__PURE__ */Caml_exceptions.create("Exception_def.Bx");

var u = {
  RE_EXN_ID: Bx
};

var Ax = /* @__PURE__ */Caml_exceptions.create("Exception_def.Ax");

var XXX = /* @__PURE__ */Caml_exceptions.create("Exception_def.XXX");

var Aa = "Match_failure";

var v_1 = [
  "",
  0,
  0
];

var v = {
  RE_EXN_ID: Aa,
  _1: v_1
};

var H0 = "Not_found";

var H1 = /* @__PURE__ */Caml_exceptions.create("Exception_def.H1");

var H2 = /* @__PURE__ */Caml_exceptions.create("Exception_def.H2");

var h2 = {
  RE_EXN_ID: H2
};

var h3 = {
  RE_EXN_ID: H2
};

var h4 = {
  RE_EXN_ID: H0
};

var H4 = "Invalid_argument";

var h5 = {
  RE_EXN_ID: H4,
  _1: "xx"
};

function p(e) {
  if (e.RE_EXN_ID === H4) {
    return 0;
  } else if (e.RE_EXN_ID === H2) {
    return 1;
  } else if (e.RE_EXN_ID === H2) {
    return 2;
  } else if (e.RE_EXN_ID === H0) {
    return 4;
  } else if (e.RE_EXN_ID === "Not_found") {
    return 3;
  } else {
    return -1;
  }
}

eq("File \"exception_def.ml\", line 50, characters 6-13", p(h5), 0);

eq("File \"exception_def.ml\", line 51, characters 6-13", p({
          RE_EXN_ID: "Not_found"
        }), 4);

eq("File \"exception_def.ml\", line 52, characters 6-13", p({
          RE_EXN_ID: H0
        }), 4);

eq("File \"exception_def.ml\", line 53, characters 6-13", p({
          RE_EXN_ID: H2
        }), 1);

eq("File \"exception_def.ml\", line 54, characters 6-13", p({
          RE_EXN_ID: H2
        }), 1);

eq("File \"exception_def.ml\", line 55, characters 6-13", p({
          RE_EXN_ID: "Invalid_argument",
          _1: ""
        }), 0);

Mt.from_pair_suites("exception_def.ml", suites.contents);

var a = 3;

var H3 = H2;

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.A = A;
exports.U = U;
exports.H = H;
exports.Bx = Bx;
exports.a = a;
exports.u = u;
exports.Ax = Ax;
exports.XXX = XXX;
exports.Aa = Aa;
exports.v = v;
exports.H0 = H0;
exports.H1 = H1;
exports.H2 = H2;
exports.H3 = H3;
exports.h2 = h2;
exports.h3 = h3;
exports.h4 = h4;
exports.H4 = H4;
exports.h5 = h5;
exports.p = p;
/*  Not a pure module */
