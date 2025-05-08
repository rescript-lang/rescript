


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

export {
  getItem,
  setItem,
  removeItem,
  key,
}
/* No side effect */
