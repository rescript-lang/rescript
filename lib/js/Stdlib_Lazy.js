'use strict';

let Primitive_lazy = require("./Primitive_lazy.js");

let make = Primitive_lazy.from_fun;

let get = Primitive_lazy.force;

let isEvaluated = Primitive_lazy.is_val;

let Undefined = Primitive_lazy.Undefined;

let force = Primitive_lazy.force;

let force_val = Primitive_lazy.force_val;

let from_fun = Primitive_lazy.from_fun;

let from_val = Primitive_lazy.from_val;

let is_val = Primitive_lazy.is_val;

exports.make = make;
exports.get = get;
exports.isEvaluated = isEvaluated;
exports.Undefined = Undefined;
exports.force = force;
exports.force_val = force_val;
exports.from_fun = from_fun;
exports.from_val = from_val;
exports.is_val = is_val;
/* No side effect */
