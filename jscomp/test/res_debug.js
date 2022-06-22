'use strict';

var Caml_obj = require("../../lib/js/caml_obj.js");

function f($$window, a, b) {
  return $$window.location(a, b);
}

var v0 = {
  x: 3,
  z: 2
};

var newrecord = Caml_obj.obj_dup(v0);

newrecord.x = 3;

function testMatch(v) {
  return v.y;
}

var v2 = newrecord;

var v1 = {
  x: 3,
  z: 3
};

var h = /* '\522' */128522;

var hey = "hello, 世界";

exports.f = f;
exports.v0 = v0;
exports.v2 = v2;
exports.v1 = v1;
exports.testMatch = testMatch;
exports.h = h;
exports.hey = hey;
/*  Not a pure module */
