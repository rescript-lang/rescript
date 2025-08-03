'use strict';


function $$delete$1(dict, string) {
  delete(dict[string]);
}

function size(dict) {
  return Object.keys(dict).length;
}

function forEach(dict, f) {
  Object.values(dict).forEach(value => f(value));
}

function forEachWithKey(dict, f) {
  Object.keys(dict).forEach(key => f(dict[key], key));
}

function mapValues(dict, f) {
  let target = {};
  Object.keys(dict).forEach(key => {
    let value = dict[key];
    target[key] = f(value);
  });
  return target;
}

exports.$$delete = $$delete$1;
exports.size = size;
exports.forEach = forEach;
exports.forEachWithKey = forEachWithKey;
exports.mapValues = mapValues;
/* No side effect */
