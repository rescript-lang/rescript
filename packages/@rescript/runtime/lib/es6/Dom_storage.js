

import * as Primitive_option from "./Primitive_option.js";

function getItem(s, obj) {
  return Primitive_option.fromNull(obj.getItem(s));
}

function setItem(k, v, obj) {
  obj.setItem(k, v);
}

function removeItem(s, obj) {
  obj.removeItem(s);
}

function key(i, obj) {
  return Primitive_option.fromNull(obj.key(i));
}

export {
  getItem,
  setItem,
  removeItem,
  key,
}
/* No side effect */
