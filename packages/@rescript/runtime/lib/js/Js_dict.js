'use strict';

let Primitive_option = require("./Primitive_option.js");

function get(dict, k) {
  if ((k in dict)) {
    return Primitive_option.some(dict[k]);
  }
}

let unsafeDeleteKey = (function (dict,key){
      delete dict[key];
     });

function entries(dict) {
  let keys = Object.keys(dict);
  let l = keys.length;
  let values = new Array(l);
  for (let i = 0; i < l; ++i) {
    let key = keys[i];
    values[i] = [
      key,
      dict[key]
    ];
  }
  return values;
}

function values(dict) {
  let keys = Object.keys(dict);
  let l = keys.length;
  let values$1 = new Array(l);
  for (let i = 0; i < l; ++i) {
    values$1[i] = dict[keys[i]];
  }
  return values$1;
}

function fromList(entries) {
  let dict = {};
  let _x = entries;
  while (true) {
    let x = _x;
    if (x === 0) {
      return dict;
    }
    let match = x.hd;
    dict[match[0]] = match[1];
    _x = x.tl;
    continue;
  };
}

function fromArray(entries) {
  let dict = {};
  let l = entries.length;
  for (let i = 0; i < l; ++i) {
    let match = entries[i];
    dict[match[0]] = match[1];
  }
  return dict;
}

function map(f, source) {
  let target = {};
  let keys = Object.keys(source);
  let l = keys.length;
  for (let i = 0; i < l; ++i) {
    let key = keys[i];
    target[key] = f(source[key]);
  }
  return target;
}

exports.get = get;
exports.unsafeDeleteKey = unsafeDeleteKey;
exports.entries = entries;
exports.values = values;
exports.fromList = fromList;
exports.fromArray = fromArray;
exports.map = map;
/* No side effect */
