

import * as Stdlib_Error from "./Stdlib_Error.js";

function filter(opt, p) {
  if (opt !== undefined && p(opt)) {
    return opt;
  }
  
}

function forEach(opt, f) {
  if (opt !== undefined) {
    return f(opt);
  }
  
}

function getExn(x, message) {
  if (x !== undefined) {
    return x;
  } else {
    return Stdlib_Error.panic(message !== undefined ? message : "Option.getExn called for None value");
  }
}

function mapOr(opt, $$default, f) {
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

function getOr(opt, $$default) {
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

function equal(a, b, eq) {
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

function compare(a, b, cmp) {
  if (a !== undefined) {
    if (b !== undefined) {
      return cmp(a, b);
    } else {
      return 1;
    }
  } else if (b !== undefined) {
    return -1;
  } else {
    return 0;
  }
}

function all(options) {
  let acc = [];
  let hasNone = false;
  let index = 0;
  while (hasNone === false && index < options.length) {
    let value = options[index];
    if (value !== undefined) {
      acc.push(value);
      index = index + 1 | 0;
    } else {
      hasNone = true;
    }
  };
  if (hasNone) {
    return;
  } else {
    return acc;
  }
}

function all2(param) {
  let b = param[1];
  let a = param[0];
  if (a !== undefined && b !== undefined) {
    return [
      a,
      b
    ];
  }
  
}

function all3(param) {
  let c = param[2];
  let b = param[1];
  let a = param[0];
  if (a !== undefined && b !== undefined && c !== undefined) {
    return [
      a,
      b,
      c
    ];
  }
  
}

function all4(param) {
  let d = param[3];
  let c = param[2];
  let b = param[1];
  let a = param[0];
  if (a !== undefined && b !== undefined && c !== undefined && d !== undefined) {
    return [
      a,
      b,
      c,
      d
    ];
  }
  
}

function all5(param) {
  let e = param[4];
  let d = param[3];
  let c = param[2];
  let b = param[1];
  let a = param[0];
  if (a !== undefined && b !== undefined && c !== undefined && d !== undefined && e !== undefined) {
    return [
      a,
      b,
      c,
      d,
      e
    ];
  }
  
}

function all6(param) {
  let f = param[5];
  let e = param[4];
  let d = param[3];
  let c = param[2];
  let b = param[1];
  let a = param[0];
  if (a !== undefined && b !== undefined && c !== undefined && d !== undefined && e !== undefined && f !== undefined) {
    return [
      a,
      b,
      c,
      d,
      e,
      f
    ];
  }
  
}

let mapWithDefault = mapOr;

let getWithDefault = getOr;

export {
  filter,
  forEach,
  getExn,
  mapOr,
  mapWithDefault,
  map,
  flatMap,
  getOr,
  getWithDefault,
  orElse,
  isSome,
  isNone,
  equal,
  compare,
  all,
  all2,
  all3,
  all4,
  all5,
  all6,
}
/* No side effect */
