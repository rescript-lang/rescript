'use strict';


function equal(param, param$1, eq1, eq2) {
  if (eq1(param[0], param$1[0])) {
    return eq2(param[1], param$1[1]);
  } else {
    return false;
  }
}

function compare(param, param$1, cmp1, cmp2) {
  let result = cmp1(param[0], param$1[0]);
  if (result !== 0) {
    return result;
  } else {
    return cmp2(param[1], param$1[1]);
  }
}

exports.equal = equal;
exports.compare = compare;
/* No side effect */
