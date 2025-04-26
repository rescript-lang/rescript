'use strict';

let Primitive_option = require("./Primitive_option.js");

function fromException(exn) {
  if (exn.RE_EXN_ID === "JsExn") {
    return Primitive_option.some(exn._1);
  }
  
}

exports.fromException = fromException;
/* No side effect */
