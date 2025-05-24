


function $$delete$1(dict, string) {
  delete(dict[string]);
}

function size(dict) {
  return Object.keys(dict).length;
}

function isEmpty(dict) {
  return Object.keys(dict).length === 0;
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

export {
  $$delete$1 as $$delete,
  size,
  isEmpty,
  forEach,
  forEachWithKey,
  mapValues,
}
/* No side effect */
