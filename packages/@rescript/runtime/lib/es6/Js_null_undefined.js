

import * as Primitive_option from "./Primitive_option.js";

function bind(x, f) {
  if (x == null) {
    return x;
  } else {
    return f(x);
  }
}

function iter(x, f) {
  if (!(x == null)) {
    return f(x);
  }
}

function fromOption(x) {
  if (x !== undefined) {
    return Primitive_option.valFromOption(x);
  }
}

let from_opt = fromOption;

export {
  bind,
  iter,
  fromOption,
  from_opt,
}
/* No side effect */
