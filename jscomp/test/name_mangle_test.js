'use strict';

var Mt = require("./mt.js");

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

function f0(x) {
  var old = x.open;
  x.open = old + 1 | 0;
  return x.open;
}

function f1(x) {
  var old = x.in;
  x.in = old + 1 | 0;
  return x.in;
}

function f2(x) {
  var old = x.MAX_LENGTH;
  x.MAX_LENGTH = old + 1 | 0;
  return x.MAX_LENGTH;
}

function f3(x) {
  var old = x.Capital;
  x.Capital = old + 1 | 0;
  return x.Capital;
}

function f4(x) {
  var old = x._open;
  x._open = old + 1 | 0;
  return x._open;
}

function f5(x) {
  var old = x.open;
  x.open = old + 1 | 0;
  return x.open;
}

function f6(x) {
  var old = x["'x"];
  x["'x"] = old + 1 | 0;
  return x["'x"];
}

function f7(x) {
  var old = x._Capital;
  x._Capital = old + 1 | 0;
  return x._Capital;
}

function f8(x) {
  var old = x._MAX;
  x._MAX = old + 1 | 0;
  return x._MAX;
}

function f9(x) {
  var old = x.__;
  x.__ = old + 1 | 0;
  return x.__;
}

function f10(x) {
  var old = x.__x;
  x.__x = old + 1 | 0;
  return x.__x;
}

function f11(x) {
  var old = x._;
  x._ = old + 1 | 0;
  return x._;
}

function f12(x) {
  var old = x.__;
  x.__ = old + 1 | 0;
  return x.__;
}

eq("File \"name_mangle_test.res\", line 94, characters 5-12", f0({open:0}), 1);

eq("File \"name_mangle_test.res\", line 95, characters 5-12", f1({in:0}), 1);

eq("File \"name_mangle_test.res\", line 96, characters 5-12", f2({MAX_LENGTH:0}), 1);

eq("File \"name_mangle_test.res\", line 97, characters 5-12", f3({Capital:0}), 1);

eq("File \"name_mangle_test.res\", line 98, characters 5-12", f4({_open:0}), 1);

eq("File \"name_mangle_test.res\", line 99, characters 5-12", f5({open:0}), 1);

eq("File \"name_mangle_test.res\", line 100, characters 5-12", f6({ "'x" :0}), 1);

eq("File \"name_mangle_test.res\", line 101, characters 5-12", f7({_Capital:0}), 1);

eq("File \"name_mangle_test.res\", line 102, characters 5-12", f8({_MAX:0}), 1);

eq("File \"name_mangle_test.res\", line 103, characters 5-12", f9({__:0}), 1);

eq("File \"name_mangle_test.res\", line 104, characters 5-12", f10({__x:0}), 1);

eq("File \"name_mangle_test.res\", line 105, characters 5-12", f11({_:0}), 1);

eq("File \"name_mangle_test.res\", line 106, characters 5-12", f12({__:0}), 1);

Mt.from_pair_suites("File \"name_mangle_test.res\", line 109, characters 20-27", suites.contents);

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.f0 = f0;
exports.f1 = f1;
exports.f2 = f2;
exports.f3 = f3;
exports.f4 = f4;
exports.f5 = f5;
exports.f6 = f6;
exports.f7 = f7;
exports.f8 = f8;
exports.f9 = f9;
exports.f10 = f10;
exports.f11 = f11;
exports.f12 = f12;
/*  Not a pure module */
