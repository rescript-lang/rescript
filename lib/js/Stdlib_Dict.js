'use strict';


function $$delete$1(dict, string) {
  delete(dict[string]);
}

let forEach = ((dict, f) => {
    for (var key in dict) {
      f(dict[key]);
    }
  });

let forEachWithKey = ((dict, f) => {
    for (var key in dict) {
      f(dict[key], key);
    }
  });

let mapValues = ((dict, f) => {
    var target = {};
    for (var key in dict) {
      target[key] = f(dict[key]);
    }
    return target;
  });

exports.$$delete = $$delete$1;
exports.forEach = forEach;
exports.forEachWithKey = forEachWithKey;
exports.mapValues = mapValues;
/* No side effect */
