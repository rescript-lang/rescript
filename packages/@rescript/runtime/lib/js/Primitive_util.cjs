'use strict';


function raiseWhenNotFound(x) {
  if (x == null) {
    throw {
      RE_EXN_ID: "Not_found",
      Error: new Error()
    };
  }
  return x;
}

let Js;

exports.Js = Js;
exports.raiseWhenNotFound = raiseWhenNotFound;
/* No side effect */
