

import * as Primitive_lazy from "./Primitive_lazy.js";

let make = Primitive_lazy.from_fun;

let get = Primitive_lazy.force;

let isEvaluated = Primitive_lazy.is_val;

let Undefined = Primitive_lazy.Undefined;

let force = Primitive_lazy.force;

let force_val = Primitive_lazy.force_val;

let from_fun = Primitive_lazy.from_fun;

let from_val = Primitive_lazy.from_val;

let is_val = Primitive_lazy.is_val;

export {
  make,
  get,
  isEvaluated,
  Undefined,
  force,
  force_val,
  from_fun,
  from_val,
  is_val,
}
/* No side effect */
