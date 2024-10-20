// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

let Mt = require("./mt.js");
let Primitive_option = require("rescript/lib/js/Primitive_option.js");

let suites = {
  contents: /* [] */0
};

let test_id = {
  contents: 0
};

function eq(loc, param) {
  let y = param[1];
  let x = param[0];
  test_id.contents = test_id.contents + 1 | 0;
  suites.contents = {
    hd: [
      loc + (" id " + test_id.contents.toString()),
      () => ({
        TAG: "Eq",
        _0: x,
        _1: y
      })
    ],
    tl: suites.contents
  };
}

function hey(x, y) {
    if (x === void 0) { x = 3; }
    return x + y;
  }
;

let u = hey(undefined, 3);

let z = hey(5, 3);

eq("File \"optional_ffi_test.res\", line 24, characters 12-19", [
  [
    u,
    z
  ],
  [
    6,
    8
  ]
]);

let counter = {
  contents: 0
};

function side_effect(x) {
  x.contents = x.contents + 1 | 0;
  return x.contents;
}

function bug_to_fix(f, x) {
  return hey(f(x), 3);
}

function bug_to_fix2(f, x) {
  return hey(Primitive_option.toUndefined(f(x)), 3);
}

let counter2 = {
  contents: 0
};

function side_effect2(x) {
  x.contents = x.contents + 1 | 0;
  return x.contents;
}

let v = bug_to_fix(side_effect, counter);

let pair_0 = [
  v,
  counter.contents
];

let pair_1 = [
  4,
  1
];

let pair = [
  pair_0,
  pair_1
];

let v2 = bug_to_fix2(side_effect2, counter2);

let pair2_0 = [
  v2,
  counter.contents
];

let pair2_1 = [
  4,
  1
];

let pair2 = [
  pair2_0,
  pair2_1
];

eq("File \"optional_ffi_test.res\", line 48, characters 5-12", pair);

eq("File \"optional_ffi_test.res\", line 49, characters 5-12", pair2);

function heystr(x, y) {
    if (x === void 0) { x = "3"; }
    return x + y;
  }
;

let pair_1$1 = heystr("name", "4");

let pair$1 = [
  "name4",
  pair_1$1
];

eq("File \"optional_ffi_test.res\", line 64, characters 5-12", pair$1);

Mt.from_pair_suites("Optional_ffi_test", suites.contents);

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.u = u;
exports.z = z;
exports.counter = counter;
exports.side_effect = side_effect;
exports.bug_to_fix = bug_to_fix;
exports.bug_to_fix2 = bug_to_fix2;
exports.counter2 = counter2;
exports.side_effect2 = side_effect2;
/*  Not a pure module */
