

import * as Stdlib_Error from "./Stdlib_Error.js";
import * as Primitive_option from "./Primitive_option.js";

function filter(opt, p) {
  if (opt !== undefined && p(Primitive_option.valFromOption(opt))) {
    return opt;
  }
  
}

function forEach(opt, f) {
  if (opt !== undefined) {
    return f(Primitive_option.valFromOption(opt));
  }
  
}

function getExn(x, message) {
  if (x !== undefined) {
    return Primitive_option.valFromOption(x);
  } else {
    return Stdlib_Error.panic(message !== undefined ? message : "Option.getExn called for None value");
  }
}

function mapOr(opt, $$default, f) {
  if (opt !== undefined) {
    return f(Primitive_option.valFromOption(opt));
  } else {
    return $$default;
  }
}

function map(opt, f) {
  if (opt !== undefined) {
    return Primitive_option.some(f(Primitive_option.valFromOption(opt)));
  }
  
}

function flatMap(opt, f) {
  if (opt !== undefined) {
    return f(Primitive_option.valFromOption(opt));
  }
  
}

function getOr(opt, $$default) {
  if (opt !== undefined) {
    return Primitive_option.valFromOption(opt);
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
      return eq(Primitive_option.valFromOption(a), Primitive_option.valFromOption(b));
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
      return cmp(Primitive_option.valFromOption(a), Primitive_option.valFromOption(b));
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
      acc.push(Primitive_option.valFromOption(value));
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
      Primitive_option.valFromOption(a),
      Primitive_option.valFromOption(b)
    ];
  }
  
}

function all3(param) {
  let c = param[2];
  let b = param[1];
  let a = param[0];
  if (a !== undefined && b !== undefined && c !== undefined) {
    return [
      Primitive_option.valFromOption(a),
      Primitive_option.valFromOption(b),
      Primitive_option.valFromOption(c)
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
      Primitive_option.valFromOption(a),
      Primitive_option.valFromOption(b),
      Primitive_option.valFromOption(c),
      Primitive_option.valFromOption(d)
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
      Primitive_option.valFromOption(a),
      Primitive_option.valFromOption(b),
      Primitive_option.valFromOption(c),
      Primitive_option.valFromOption(d),
      Primitive_option.valFromOption(e)
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
      Primitive_option.valFromOption(a),
      Primitive_option.valFromOption(b),
      Primitive_option.valFromOption(c),
      Primitive_option.valFromOption(d),
      Primitive_option.valFromOption(e),
      Primitive_option.valFromOption(f)
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
