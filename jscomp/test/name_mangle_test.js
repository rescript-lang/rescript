// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

let Mt = require("./mt.js");

let suites = {
  contents: /* [] */0
};

let test_id = {
  contents: 0
};

function eq(loc, x, y) {
  test_id.contents = test_id.contents + 1 | 0;
  suites.contents = {
    hd: [
      loc + (" id " + String(test_id.contents)),
      () => ({
        TAG: "Eq",
        _0: x,
        _1: y
      })
    ],
    tl: suites.contents
  };
}

function f0(x) {
  let old = x._open;
  x._open = old + 1 | 0;
  return x._open;
}

function f1(x) {
  let old = x._in;
  x._in = old + 1 | 0;
  return x._in;
}

function f2(x) {
  let old = x._MAX_LENGTH;
  x._MAX_LENGTH = old + 1 | 0;
  return x._MAX_LENGTH;
}

function f3(x) {
  let old = x._Capital;
  x._Capital = old + 1 | 0;
  return x._Capital;
}

function f4(x) {
  let old = x._open__;
  x._open__ = old + 1 | 0;
  return x._open__;
}

function f5(x) {
  let old = x.open__;
  x.open__ = old + 1 | 0;
  return x.open__;
}

function f6(x) {
  let old = x["_'x"];
  x["_'x"] = old + 1 | 0;
  return x["_'x"];
}

function f7(x) {
  let old = x._Capital__;
  x._Capital__ = old + 1 | 0;
  return x._Capital__;
}

function f8(x) {
  let old = x._MAX__;
  x._MAX__ = old + 1 | 0;
  return x._MAX__;
}

function f9(x) {
  let old = x.__;
  x.__ = old + 1 | 0;
  return x.__;
}

function f10(x) {
  let old = x.__x;
  x.__x = old + 1 | 0;
  return x.__x;
}

function f11(x) {
  let old = x.___;
  x.___ = old + 1 | 0;
  return x.___;
}

function f12(x) {
  let old = x.____;
  x.____ = old + 1 | 0;
  return x.____;
}

eq("File \"name_mangle_test.res\", line 94, characters 5-12", f0({_open:0}), 1);

eq("File \"name_mangle_test.res\", line 95, characters 5-12", f1({_in:0}), 1);

eq("File \"name_mangle_test.res\", line 96, characters 5-12", f2({_MAX_LENGTH:0}), 1);

eq("File \"name_mangle_test.res\", line 97, characters 5-12", f3({_Capital:0}), 1);

eq("File \"name_mangle_test.res\", line 98, characters 5-12", f4({_open__:0}), 1);

eq("File \"name_mangle_test.res\", line 99, characters 5-12", f5({open__:0}), 1);

eq("File \"name_mangle_test.res\", line 100, characters 5-12", f6({ "_'x" :0}), 1);

eq("File \"name_mangle_test.res\", line 101, characters 5-12", f7({_Capital__:0}), 1);

eq("File \"name_mangle_test.res\", line 102, characters 5-12", f8({_MAX__:0}), 1);

eq("File \"name_mangle_test.res\", line 103, characters 5-12", f9({__:0}), 1);

eq("File \"name_mangle_test.res\", line 104, characters 5-12", f10({__x:0}), 1);

eq("File \"name_mangle_test.res\", line 105, characters 5-12", f11({___:0}), 1);

eq("File \"name_mangle_test.res\", line 106, characters 5-12", f12({____:0}), 1);

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
