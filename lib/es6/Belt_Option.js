


function keep(opt, p) {
  if (opt !== undefined && p(opt)) {
    return opt;
  }
  
}

function forEach(opt, f) {
  if (opt !== undefined) {
    return f(opt);
  }
  
}

function getExn(x) {
  if (x !== undefined) {
    return x;
  }
  throw {
    RE_EXN_ID: "Not_found",
    Error: new Error()
  };
}

function mapWithDefault(opt, $$default, f) {
  if (opt !== undefined) {
    return f(opt);
  } else {
    return $$default;
  }
}

function map(opt, f) {
  if (opt !== undefined) {
    return f(opt);
  }
  
}

function flatMap(opt, f) {
  if (opt !== undefined) {
    return f(opt);
  }
  
}

function getWithDefault(opt, $$default) {
  if (opt !== undefined) {
    return opt;
  } else {
    return $$default;
  }
}

function orElse(opt, other) {
  if (opt !== undefined) {
    return opt;
  } else {
    return other;
  }
}

function isSome(x) {
  return x !== undefined;
}

function isNone(x) {
  return x === undefined;
}

function eq(a, b, f) {
  if (a !== undefined) {
    if (b !== undefined) {
      return f(a, b);
    } else {
      return false;
    }
  } else {
    return b === undefined;
  }
}

function cmp(a, b, f) {
  if (a !== undefined) {
    if (b !== undefined) {
      return f(a, b);
    } else {
      return 1;
    }
  } else if (b !== undefined) {
    return -1;
  } else {
    return 0;
  }
}

let keepU = keep;

let forEachU = forEach;

let mapWithDefaultU = mapWithDefault;

let mapU = map;

let flatMapU = flatMap;

let eqU = eq;

let cmpU = cmp;

export {
  keepU,
  keep,
  forEachU,
  forEach,
  getExn,
  mapWithDefaultU,
  mapWithDefault,
  mapU,
  map,
  flatMapU,
  flatMap,
  getWithDefault,
  orElse,
  isSome,
  isNone,
  eqU,
  eq,
  cmpU,
  cmp,
}
/* No side effect */
