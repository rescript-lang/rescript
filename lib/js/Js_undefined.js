'use strict';

let Primitive_option = require("./Primitive_option.js");

function test(x) {
  return x === undefined;
}

function testAny(x) {
  return x === undefined;
}

function getExn(f) {
  if (f !== undefined) {
    return Primitive_option.valFromOption(f);
  }
  throw new Error("Js.Undefined.getExn");
}

function bind(x, f) {
  if (x !== undefined) {
    return f(Primitive_option.valFromOption(x));
  }
  
}

function iter(x, f) {
  if (x !== undefined) {
    return f(Primitive_option.valFromOption(x));
  }
  
}

function fromOption(x) {
  if (x !== undefined) {
    return Primitive_option.valFromOption(x);
  }
  
}

let from_opt = fromOption;

exports.test = test;
exports.testAny = testAny;
exports.getExn = getExn;
exports.bind = bind;
exports.iter = iter;
exports.fromOption = fromOption;
exports.from_opt = from_opt;
/* No side effect */
