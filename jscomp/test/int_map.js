// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

let Belt_Map = require("../../lib/js/belt_Map.js");
let Primitive_object = require("../../lib/js/primitive_object.js");

let cmp = Primitive_object.compare;

let IntCmp = {
  cmp: cmp
};

let m_cmp = IntCmp.cmp;

let m = {
  cmp: m_cmp,
  data: undefined
};

let m$1 = Belt_Map.set(m, 0, "test");

exports.IntCmp = IntCmp;
exports.m = m$1;
/* IntCmp Not a pure module */
