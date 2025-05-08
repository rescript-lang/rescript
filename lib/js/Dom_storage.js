'use strict';


function getItem(s, obj) {
  return obj.getItem(s);
}

function setItem(k, v, obj) {
  obj.setItem(k, v);
}

function removeItem(s, obj) {
  obj.removeItem(s);
}

function key(i, obj) {
  return obj.key(i);
}

exports.getItem = getItem;
exports.setItem = setItem;
exports.removeItem = removeItem;
exports.key = key;
/* No side effect */
