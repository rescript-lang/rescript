// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Mt from "./mt.mjs";

function f(v) {
  if (v % 2 === 0) {
    return v => v * v | 0;
  } else {
    return v => v + v | 0;
  }
}

let v = [
  1,
  2,
  3
].map((a, b) => f(a)(b));

let vv = [
  1,
  2,
  3
].map((a, b) => a + b | 0);

let hh = [
  "1",
  "2",
  "3"
].map(x => parseInt(x));

function u() {
  return 3;
}

let vvv = {
  contents: 0
};

function fff() {
  console.log("x");
  console.log("x");
  vvv.contents = vvv.contents + 1 | 0;
}

function g() {
  fff();
}

function abc(x, y, z) {
  console.log("xx");
  console.log("yy");
  return (x + y | 0) + z | 0;
}

let abc_u = abc;

fff();

Mt.from_pair_suites("Ffi_arity_test", {
  hd: [
    "File \"ffi_arity_test.res\", line 51, characters 7-14",
    () => ({
      TAG: "Eq",
      _0: v,
      _1: [
        0,
        1,
        4
      ]
    })
  ],
  tl: {
    hd: [
      "File \"ffi_arity_test.res\", line 52, characters 7-14",
      () => ({
        TAG: "Eq",
        _0: vv,
        _1: [
          1,
          3,
          5
        ]
      })
    ],
    tl: {
      hd: [
        "File \"ffi_arity_test.res\", line 53, characters 7-14",
        () => ({
          TAG: "Eq",
          _0: hh,
          _1: [
            1,
            2,
            3
          ]
        })
      ],
      tl: /* [] */0
    }
  }
});

function bar(fn) {
  return fn();
}

((function(){console.log("forgiving arity")})());

export {
  f,
  v,
  vv,
  hh,
  u,
  vvv,
  fff,
  g,
  abc,
  abc_u,
  bar,
}
/* v Not a pure module */
