


function $$delete$1(dict, string) {
  delete(dict[string]);
}

function forEach(dict, f) {
  Object.values(dict).forEach(value => f(value));
}

function forEachWithKey(dict, f) {
  Object.entries(dict).forEach(param => f(param[1], param[0]));
}

function mapValues(dict, f) {
  let target = {};
  forEachWithKey(dict, (value, key) => {
    target[key] = f(value);
  });
  return target;
}

export {
  $$delete$1 as $$delete,
  forEach,
  forEachWithKey,
  mapValues,
}
/* No side effect */
