'use strict';


function f(param) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "noassert.res",
          1,
          14
        ],
        Error: new Error()
      };
}

function h(param) {
  
}

exports.f = f;
exports.h = h;
/* No side effect */
