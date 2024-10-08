// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

let Nodeassert = require("node:assert");

function ok(loc, a) {
  Nodeassert.ok(a, loc);
}

function eq(loc, a, b) {
  Nodeassert.deepStrictEqual(a, b, loc);
}

function $$throw(loc, f) {
  Nodeassert.throws(f, undefined, loc);
}

exports.ok = ok;
exports.eq = eq;
exports.$$throw = $$throw;
/* node:assert Not a pure module */