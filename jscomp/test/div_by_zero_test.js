// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

let Mt = require("./mt.js");
let Caml_int32 = require("../../lib/js/caml_int32.js");
let Caml_int64 = require("../../lib/js/caml_int64.js");

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
      (function () {
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

function add(suite) {
  suites.contents = {
    hd: suite,
    tl: suites.contents
  };
}

add([
  "File \"div_by_zero_test.res\", line 11, characters 7-14",
  (function () {
    return {
      TAG: "ThrowAny",
      _0: (function () {
        Caml_int32.div(3, 0);
      })
    };
  })
]);

add([
  "File \"div_by_zero_test.res\", line 12, characters 7-14",
  (function () {
    return {
      TAG: "ThrowAny",
      _0: (function () {
        Caml_int32.mod_(3, 0);
      })
    };
  })
]);

add([
  "File \"div_by_zero_test.res\", line 13, characters 7-14",
  (function () {
    return {
      TAG: "ThrowAny",
      _0: (function () {
        Caml_int32.div(3, 0);
      })
    };
  })
]);

add([
  "File \"div_by_zero_test.res\", line 14, characters 7-14",
  (function () {
    return {
      TAG: "ThrowAny",
      _0: (function () {
        Caml_int32.mod_(3, 0);
      })
    };
  })
]);

add([
  "File \"div_by_zero_test.res\", line 15, characters 7-14",
  (function () {
    return {
      TAG: "ThrowAny",
      _0: (function () {
        Caml_int64.div([
          0,
          3
        ], Caml_int64.zero);
      })
    };
  })
]);

add([
  "File \"div_by_zero_test.res\", line 16, characters 7-14",
  (function () {
    return {
      TAG: "ThrowAny",
      _0: (function () {
        Caml_int64.mod_([
          0,
          3
        ], Caml_int64.zero);
      })
    };
  })
]);

function div(x, y) {
  return Caml_int32.div(x, y) + 3 | 0;
}

Mt.from_pair_suites("Div_by_zero_test", suites.contents);

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.add = add;
exports.div = div;
/*  Not a pure module */
