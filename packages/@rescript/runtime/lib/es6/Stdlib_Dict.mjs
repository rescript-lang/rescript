


function $$delete$1(dict, string) {
  delete(dict[string]);
}

let forEach = ((dict, f) => {
  for (var i in dict) {
    f(dict[i]);
  }
});

let forEachWithKey = ((dict, f) => {
  for (var i in dict) {
    f(dict[i], i);
  }
});

let mapValues = ((dict, f) => {
  var target = {}, i;
  for (i in dict) {
    target[i] = f(dict[i]);
  }
  return target;
});

let size = ((dict) => {
  var size = 0, i;
  for (i in dict) {
    size++;
  }
  return size;
});

let isEmpty = ((dict) => {
  for (var _ in dict) {
    return false
  }
  return true
});

export {
  $$delete$1 as $$delete,
  size,
  isEmpty,
  forEach,
  forEachWithKey,
  mapValues,
}
/* No side effect */
