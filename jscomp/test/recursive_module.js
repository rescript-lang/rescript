// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

let Mt = require("./mt.js");
let Lazy = require("../../lib/js/lazy.js");
let Curry = require("../../lib/js/curry.js");
let Caml_module = require("../../lib/js/caml_module.js");
let CamlinternalLazy = require("../../lib/js/camlinternalLazy.js");
let Caml_js_exceptions = require("../../lib/js/caml_js_exceptions.js");
let Caml_external_polyfill = require("../../lib/js/caml_external_polyfill.js");

let suites = {
  contents: /* [] */0
};

let test_id = {
  contents: 0
};

function eq(loc, x, y) {
  Mt.eq_suites(test_id, suites, loc, x, y);
}

let Xx = {
  f: (function (prim0, prim1) {
    return Caml_external_polyfill.resolve("hfiehi")(prim0, prim1);
  })
};

let Int3 = Caml_module.init_mod([
  "recursive_module.res",
  25,
  4
], {
  TAG: "Module",
  _0: [[
      "Function",
      "u"
    ]]
});

Caml_module.update_mod({
  TAG: "Module",
  _0: [[
      "Function",
      "u"
    ]]
}, Int3, Int3);

let Inta = Caml_module.init_mod([
  "recursive_module.res",
  29,
  4
], {
  TAG: "Module",
  _0: [[
      "Lazy",
      "a"
    ]]
});

let Intb = Caml_module.init_mod([
  "recursive_module.res",
  34,
  4
], {
  TAG: "Module",
  _0: [[
      "Lazy",
      "a"
    ]]
});

let a = CamlinternalLazy.from_fun(function () {
  return CamlinternalLazy.force(Intb.a);
});

Caml_module.update_mod({
  TAG: "Module",
  _0: [[
      "Lazy",
      "a"
    ]]
}, Inta, {
  a: a
});

let a$1 = CamlinternalLazy.from_fun(function () {
  return CamlinternalLazy.force(Inta.a) + 1 | 0;
});

Caml_module.update_mod({
  TAG: "Module",
  _0: [[
      "Lazy",
      "a"
    ]]
}, Intb, {
  a: a$1
});

let tmp;

try {
  tmp = CamlinternalLazy.force(Intb.a);
}
catch (raw_exn){
  let exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
  if (exn.RE_EXN_ID === Lazy.Undefined) {
    tmp = -1;
  } else {
    throw new Error(exn.RE_EXN_ID, {
              cause: exn
            });
  }
}

eq("File \"recursive_module.res\", line 39, characters 2-9", -1, tmp);

let Inta$1 = Caml_module.init_mod([
  "recursive_module.res",
  49,
  6
], {
  TAG: "Module",
  _0: [[
      "Lazy",
      "a"
    ]]
});

let Intb$1 = Caml_module.init_mod([
  "recursive_module.res",
  54,
  6
], {
  TAG: "Module",
  _0: [[
      "Lazy",
      "a"
    ]]
});

let a$2 = CamlinternalLazy.from_fun(function () {
  return CamlinternalLazy.force(Intb$1.a) + 1 | 0;
});

Caml_module.update_mod({
  TAG: "Module",
  _0: [[
      "Lazy",
      "a"
    ]]
}, Inta$1, {
  a: a$2
});

let a$3 = CamlinternalLazy.from_fun(function () {
  return 2;
});

Caml_module.update_mod({
  TAG: "Module",
  _0: [[
      "Lazy",
      "a"
    ]]
}, Intb$1, {
  a: a$3
});

let A = {
  Inta: Inta$1,
  Intb: Intb$1
};

eq("File \"recursive_module.res\", line 59, characters 3-10", CamlinternalLazy.force(Inta$1.a), 3);

let tmp$1;

try {
  Curry._1(Int3.u, 3);
  tmp$1 = 3;
}
catch (raw_exn$1){
  let exn$1 = Caml_js_exceptions.internalToOCamlException(raw_exn$1);
  if (exn$1.RE_EXN_ID === "Undefined_recursive_module") {
    tmp$1 = 4;
  } else {
    throw new Error(exn$1.RE_EXN_ID, {
              cause: exn$1
            });
  }
}

eq("File \"recursive_module.res\", line 62, characters 2-9", 4, tmp$1);

Mt.from_pair_suites("Recursive_module", suites.contents);

let Int32;

let uuu = Xx.f;

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.Int32 = Int32;
exports.Xx = Xx;
exports.uuu = uuu;
exports.Int3 = Int3;
exports.Inta = Inta;
exports.Intb = Intb;
exports.A = A;
/* Int3 Not a pure module */
