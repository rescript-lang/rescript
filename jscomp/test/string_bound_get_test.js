// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

let Caml_bytes = require("../../lib/js/caml_bytes.js");
let Caml_string = require("../../lib/js/caml_string.js");

let v = "ghos";

let u_a = Caml_string.get(v, 0);

function u_b(param) {
  return Caml_string.get(v, -1);
}

let u_c = Caml_string.get("ghos", 0);

function u_d(param) {
  return Caml_string.get("ghos", -1);
}

let u_e = Caml_bytes.create(32);

let u_f = Caml_bytes.get(u_e, 0);

function u_g(param) {
  return Caml_bytes.get(u_e, -1);
}

exports.v = v;
exports.u_a = u_a;
exports.u_b = u_b;
exports.u_c = u_c;
exports.u_d = u_d;
exports.u_e = u_e;
exports.u_f = u_f;
exports.u_g = u_g;
/* No side effect */
