// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Mt from "./mt.mjs";
import * as Pervasives from "rescript/lib/es6/Pervasives.js";
import * as Stdlib_Exn from "rescript/lib/es6/Stdlib_Exn.js";
import * as Primitive_exceptions from "rescript/lib/es6/Primitive_exceptions.js";

let suites = {
  contents: /* [] */0
};

let counter = {
  contents: 0
};

function add_test(loc, test) {
  counter.contents = counter.contents + 1 | 0;
  let id = loc + (" id " + counter.contents.toString());
  suites.contents = {
    hd: [
      id,
      test
    ],
    tl: suites.contents
  };
}

function eq(loc, x, y) {
  add_test(loc, () => ({
    TAG: "Eq",
    _0: x,
    _1: y
  }));
}

function false_(loc) {
  add_test(loc, () => ({
    TAG: "Ok",
    _0: false
  }));
}

function true_(loc) {
  add_test(loc, () => ({
    TAG: "Ok",
    _0: true
  }));
}

let exit = 0;

let e;

try {
  e = JSON.parse(" {\"x\"}");
  exit = 1;
} catch (raw_x) {
  let x = Primitive_exceptions.internalToException(raw_x);
  if (x.RE_EXN_ID === Stdlib_Exn.$$Error) {
    add_test("File \"js_exception_catch_test.res\", line 18, characters 37-44", () => ({
      TAG: "Ok",
      _0: true
    }));
  } else {
    throw x;
  }
}

if (exit === 1) {
  add_test("File \"js_exception_catch_test.res\", line 19, characters 14-21", () => ({
    TAG: "Ok",
    _0: false
  }));
}

let A = /* @__PURE__ */Primitive_exceptions.create("Js_exception_catch_test.A");

let B = /* @__PURE__ */Primitive_exceptions.create("Js_exception_catch_test.B");

let C = /* @__PURE__ */Primitive_exceptions.create("Js_exception_catch_test.C");

function test(f) {
  try {
    f();
    return "No_error";
  } catch (raw_e) {
    let e = Primitive_exceptions.internalToException(raw_e);
    if (e.RE_EXN_ID === "Not_found") {
      return "Not_found";
    } else if (e.RE_EXN_ID === "Invalid_argument") {
      if (e._1 === "x") {
        return "Invalid_argument";
      } else {
        return "Invalid_any";
      }
    } else if (e.RE_EXN_ID === A) {
      if (e._1 !== 2) {
        return "A_any";
      } else {
        return "A2";
      }
    } else if (e.RE_EXN_ID === B) {
      return "B";
    } else if (e.RE_EXN_ID === C) {
      if (e._1 !== 1 || e._2 !== 2) {
        return "C_any";
      } else {
        return "C";
      }
    } else if (e.RE_EXN_ID === Stdlib_Exn.$$Error) {
      return "Js_error";
    } else {
      return "Any";
    }
  }
}

eq("File \"js_exception_catch_test.res\", line 44, characters 5-12", test(() => {}), "No_error");

eq("File \"js_exception_catch_test.res\", line 45, characters 5-12", test(() => {
  throw {
    RE_EXN_ID: "Not_found",
    Error: new Error()
  };
}), "Not_found");

eq("File \"js_exception_catch_test.res\", line 46, characters 5-12", test(() => Pervasives.invalid_arg("x")), "Invalid_argument");

eq("File \"js_exception_catch_test.res\", line 47, characters 5-12", test(() => Pervasives.invalid_arg("")), "Invalid_any");

eq("File \"js_exception_catch_test.res\", line 48, characters 5-12", test(() => {
  throw {
    RE_EXN_ID: A,
    _1: 2,
    Error: new Error()
  };
}), "A2");

eq("File \"js_exception_catch_test.res\", line 49, characters 5-12", test(() => {
  throw {
    RE_EXN_ID: A,
    _1: 3,
    Error: new Error()
  };
}), "A_any");

eq("File \"js_exception_catch_test.res\", line 50, characters 5-12", test(() => {
  throw {
    RE_EXN_ID: B,
    Error: new Error()
  };
}), "B");

eq("File \"js_exception_catch_test.res\", line 51, characters 5-12", test(() => {
  throw {
    RE_EXN_ID: C,
    _1: 1,
    _2: 2,
    Error: new Error()
  };
}), "C");

eq("File \"js_exception_catch_test.res\", line 52, characters 5-12", test(() => {
  throw {
    RE_EXN_ID: C,
    _1: 0,
    _2: 2,
    Error: new Error()
  };
}), "C_any");

eq("File \"js_exception_catch_test.res\", line 53, characters 5-12", test(() => Stdlib_Exn.raiseError("x")), "Js_error");

eq("File \"js_exception_catch_test.res\", line 54, characters 5-12", test(() => Pervasives.failwith("x")), "Any");

Mt.from_pair_suites("Js_exception_catch_test", suites.contents);

export {
  suites,
  add_test,
  eq,
  false_,
  true_,
  A,
  B,
  C,
  test,
}
/*  Not a pure module */
