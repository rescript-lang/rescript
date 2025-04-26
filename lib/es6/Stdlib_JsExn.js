

import * as Primitive_option from "./Primitive_option.js";

function fromException(exn) {
  if (exn.RE_EXN_ID === "JsExn") {
    return Primitive_option.some(exn._1);
  }
  
}

export {
  fromException,
}
/* No side effect */
