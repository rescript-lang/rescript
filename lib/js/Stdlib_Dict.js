'use strict';


function $$delete$1(dict, string) {
  delete(dict[string]);
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

let has = ((dict, key) => key in dict);

exports.$$delete = $$delete$1;
exports.forEach = forEach;
exports.forEachWithKey = forEachWithKey;
exports.mapValues = mapValues;
exports.has = has;
/* No side effect */
