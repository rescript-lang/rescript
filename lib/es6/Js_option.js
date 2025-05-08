


function some(x) {
  return x;
}

function isSome(x) {
  return x !== undefined;
}

function isSomeValue(eq, v, x) {
  if (x !== undefined) {
    return eq(v, x);
  } else {
    return false;
  }
}

function isNone(x) {
  return x === undefined;
}

function getExn(x) {
  if (x !== undefined) {
    return x;
  }
  throw new Error("getExn");
}

function equal(eq, a, b) {
  if (a !== undefined) {
    if (b !== undefined) {
      return eq(a, b);
    } else {
      return false;
    }
  } else {
    return b === undefined;
  }
}

function andThen(f, x) {
  if (x !== undefined) {
    return f(x);
  }
  
}

function map(f, x) {
  if (x !== undefined) {
    return f(x);
  }
  
}

function getWithDefault(a, x) {
  if (x !== undefined) {
    return x;
  } else {
    return a;
  }
}

function filter(f, x) {
  if (x === undefined) {
    return;
  }
  let x$1 = x;
  if (f(x$1)) {
    return x$1;
  }
  
}

function firstSome(a, b) {
  if (a !== undefined) {
    return a;
  } else if (b !== undefined) {
    return b;
  } else {
    return;
  }
}

let $$default = getWithDefault;

export {
  some,
  isSome,
  isSomeValue,
  isNone,
  getExn,
  equal,
  andThen,
  map,
  getWithDefault,
  $$default as default,
  filter,
  firstSome,
}
/* No side effect */
