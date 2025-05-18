'use strict';


function init(loc, shape) {
  let undef_module = param => {
    throw {
      RE_EXN_ID: "Undefined_recursive_module",
      _1: loc,
      Error: new Error()
    };
  };
  let loop = (shape, struct_, idx) => {
    if (typeof shape !== "object") {
      switch (shape) {
        case "Function" :
        case "Lazy" :
          struct_[idx] = undef_module;
          return;
        case "Class" :
          struct_[idx] = [
            undef_module,
            undef_module,
            undef_module,
            0
          ];
          return;
      }
    } else {
      if (shape.TAG !== "Module") {
        struct_[idx] = shape._0;
        return;
      }
      let comps = shape._0;
      let v = {};
      struct_[idx] = v;
      let len = comps.length;
      for (let i = 0; i < len; ++i) {
        let match = comps[i];
        loop(match[0], v, match[1]);
      }
      return;
    }
  };
  let res = {};
  let dummy_name = "dummy";
  loop(shape, res, dummy_name);
  return res[dummy_name];
}

function update(shape, o, n) {
  let aux = (shape, o, n, parent, i) => {
    if (typeof shape !== "object") {
      switch (shape) {
        case "Function" :
          parent[i] = n;
          return;
        case "Lazy" :
        case "Class" :
          Object.assign(o, n);
          return;
      }
    } else {
      if (shape.TAG !== "Module") {
        return;
      }
      let comps = shape._0;
      for (let i$1 = 0, i_finish = comps.length; i$1 < i_finish; ++i$1) {
        let match = comps[i$1];
        let name = match[1];
        aux(match[0], o[name], n[name], o, name);
      }
      return;
    }
  };
  if (typeof shape !== "object") {
    throw {
      RE_EXN_ID: "Assert_failure",
      _1: [
        "Primitive_module.res",
        68,
        9
      ],
      Error: new Error()
    };
  }
  if (shape.TAG === "Module") {
    let comps = shape._0;
    for (let i = 0, i_finish = comps.length; i < i_finish; ++i) {
      let match = comps[i];
      let name = match[1];
      aux(match[0], o[name], n[name], o, name);
    }
    return;
  }
  throw {
    RE_EXN_ID: "Assert_failure",
    _1: [
      "Primitive_module.res",
      68,
      9
    ],
    Error: new Error()
  };
}

exports.init = init;
exports.update = update;
/* No side effect */
