'use strict';


function forEach(iterator, f) {
  let iteratorDone = false;
  while (!iteratorDone) {
    let match = iterator.next();
    f(match.value);
    iteratorDone = match.done;
  };
}

exports.forEach = forEach;
/* No side effect */
