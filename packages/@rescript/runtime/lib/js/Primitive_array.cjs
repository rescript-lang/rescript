'use strict';


function length(prim) {
  return prim.length;
}

function get(xs, index) {
  if (index < 0 || index >= xs.length) {
    throw {
      RE_EXN_ID: "Invalid_argument",
      _1: "index out of bounds",
      Error: new Error()
    };
  }
  return xs[index];
}

function set(xs, index, newval) {
  if (index < 0 || index >= xs.length) {
    throw {
      RE_EXN_ID: "Invalid_argument",
      _1: "index out of bounds",
      Error: new Error()
    };
  }
  xs[index] = newval;
}

function spread(arrays) {
  let arraysLength = arrays.length;
  let resultLength = 0;
  for (let i = 0; i < arraysLength; ++i) {
    resultLength = resultLength + arrays[i].length | 0;
  }
  let result = new Array(resultLength);
  resultLength = 0;
  for (let i$1 = 0; i$1 < arraysLength; ++i$1) {
    let array = arrays[i$1];
    for (let j = 0, j_finish = array.length; j < j_finish; ++j) {
      result[resultLength] = array[j];
      resultLength = resultLength + 1 | 0;
    }
  }
  return result;
}

exports.length = length;
exports.get = get;
exports.set = set;
exports.spread = spread;
/* No side effect */
