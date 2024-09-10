// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

let Mt = require("./mt.js");
let Primitive_object = require("../../lib/js/primitive_object.js");

let suites = {
  contents: /* [] */0
};

let test_id = {
  contents: 0
};

function eqs(loc, x, y) {
  Mt.eq_suites(test_id, suites, loc, x, y);
}

function eq(param, param$1) {
  let x = param.contents;
  let y = param$1.contents;
  return x === y;
}

function eq2(x, param) {
  let y = param.contents;
  return Primitive_object.equal(x.contents, y);
}

eqs("File \"mutable_uncurry_test.res\", line 15, characters 4-11", false, eq({
  contents: 1
}, {
  contents: 2
}));

eqs("File \"mutable_uncurry_test.res\", line 16, characters 4-11", true, eq({
  contents: 2
}, {
  contents: 2
}));

function ut3(param, param$1, param$2) {
  let x0 = param.contents;
  let x1 = param$1.contents;
  let x2 = param$2.contents;
  return [
    x0,
    x1,
    x2
  ];
}

function t3(param, param$1, param$2) {
  let x0 = param.contents;
  let x1 = param$1.contents;
  let x2 = param$2.contents;
  return [
    x0,
    x1,
    x2
  ];
}

function ut4(param, param$1, param$2, param$3) {
  let x0 = param.contents;
  let x1 = param$1.contents;
  return (param => {
    let x2 = param.contents;
    return param => {
      let x3 = param.contents;
      return [
        x0,
        x1,
        x2,
        x3
      ];
    };
  })(param$2)(param$3);
}

function t4(param, param$1, param$2, param$3) {
  let x0 = param.contents;
  let x1 = param$1.contents;
  return (param => {
    let x2 = param.contents;
    return param => {
      let x3 = param.contents;
      return [
        x0,
        x1,
        x2,
        x3
      ];
    };
  })(param$2)(param$3);
}

function ut5(param, param$1, param$2, param$3, param$4) {
  let x0 = param.contents;
  let x1 = param$1.contents;
  return (param => {
    let x2 = param.contents;
    return param => {
      let x3 = param.contents;
      return param => {
        let x4 = param.contents;
        return [
          x0,
          x1,
          x2,
          x3,
          x4
        ];
      };
    };
  })(param$2)(param$3)(param$4);
}

function t5(param, param$1, param$2, param$3, param$4) {
  let x0 = param.contents;
  let x1 = param$1.contents;
  return (param => {
    let x2 = param.contents;
    return param => {
      let x3 = param.contents;
      return param => {
        let x4 = param.contents;
        return [
          x0,
          x1,
          x2,
          x3,
          x4
        ];
      };
    };
  })(param$2)(param$3)(param$4);
}

function nested0(param, param$1, param$2) {
  let x0 = param.contents;
  let x1 = param$1.contents;
  let x2 = param$2.contents;
  let a = (x0 + x1 | 0) + x2 | 0;
  return (param, param$1, param$2) => {
    let x0 = param.contents;
    let x1 = param$1.contents;
    let x2 = param$2.contents;
    return ((a + x0 | 0) + x1 | 0) + x2 | 0;
  };
}

function nested1(param, param$1, param$2) {
  let x0 = param.contents;
  let x1 = param$1.contents;
  let x2 = param$2.contents;
  let a = (x0 + x1 | 0) + x2 | 0;
  return (param, param$1, param$2) => {
    let x0 = param.contents;
    let x1 = param$1.contents;
    let x2 = param$2.contents;
    return ((a + x0 | 0) + x1 | 0) + x2 | 0;
  };
}

eqs("File \"mutable_uncurry_test.res\", line 51, characters 4-11", ut3({
  contents: 1
}, {
  contents: 2
}, {
  contents: 3
}), [
  1,
  2,
  3
]);

eqs("File \"mutable_uncurry_test.res\", line 52, characters 4-11", t3({
  contents: 1
}, {
  contents: 2
}, {
  contents: 3
}), [
  1,
  2,
  3
]);

eqs("File \"mutable_uncurry_test.res\", line 54, characters 4-11", ut5({
  contents: 1
}, {
  contents: 2
}, {
  contents: 3
}, {
  contents: 1
}, {
  contents: 1
}), [
  1,
  2,
  3,
  1,
  1
]);

Mt.from_pair_suites("mutable_uncurry_test.res", suites.contents);

exports.suites = suites;
exports.test_id = test_id;
exports.eqs = eqs;
exports.eq = eq;
exports.eq2 = eq2;
exports.ut3 = ut3;
exports.t3 = t3;
exports.ut4 = ut4;
exports.t4 = t4;
exports.ut5 = ut5;
exports.t5 = t5;
exports.nested0 = nested0;
exports.nested1 = nested1;
/*  Not a pure module */
