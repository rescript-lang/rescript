// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Primitive_object from "rescript/lib/es6/Primitive_object.js";
import * as Primitive_exceptions from "rescript/lib/es6/Primitive_exceptions.js";

function foo(x) {
  if (typeof x !== "object") {
    if (x === "A1") {
      return 1;
    } else {
      return 2;
    }
  }
  switch (x.TAG) {
    case "B" :
      return x._0;
    case "C" :
      return x._0 + x._1 | 0;
    case "D" :
      let match = x._0;
      return match[0] + match[1] | 0;
  }
}

function fooA1(x) {
  if (typeof x !== "object" && x === "A1") {
    return 1;
  } else {
    return 42;
  }
}

function fooC(x) {
  if (x.TAG === "C") {
    return x._0 + x._1 | 0;
  } else {
    return 42;
  }
}

function switchNum(x) {
  switch (x) {
    case 0 :
      return "0";
    case 1 :
      return "1";
    case 2 :
      return "2";
    default:
      return "_";
  }
}

let same = Primitive_object.equal;

let compare = Primitive_object.compare;

let Path = {
  same: same,
  compare: compare
};

function Make(M) {
  let find = x => {};
  return {
    find: find
  };
}

function find(x) {
  
}

let M = {
  find: find
};

function rollback_path(subst, p) {
  try {
    return "try";
  } catch (raw_exn) {
    let exn = Primitive_exceptions.internalToException(raw_exn);
    if (exn.RE_EXN_ID === "Not_found") {
      switch (p.TAG) {
        case "Pdot" :
          return "Pdot";
        case "Pident" :
        case "Papply" :
          return "Pident | Papply";
      }
    } else {
      throw exn;
    }
  }
}

let EA1 = /* @__PURE__ */Primitive_exceptions.create("Variant.EA1");

let EA2 = /* @__PURE__ */Primitive_exceptions.create("Variant.EA2");

let EB = /* @__PURE__ */Primitive_exceptions.create("Variant.EB");

let EC = /* @__PURE__ */Primitive_exceptions.create("Variant.EC");

let ED = /* @__PURE__ */Primitive_exceptions.create("Variant.ED");

function fooExn(f) {
  try {
    return f();
  } catch (raw_n) {
    let n = Primitive_exceptions.internalToException(raw_n);
    if (n.RE_EXN_ID === EA1) {
      return 1;
    }
    if (n.RE_EXN_ID === EA2) {
      return 2;
    }
    if (n.RE_EXN_ID === EB) {
      return n._1;
    }
    if (n.RE_EXN_ID === EC) {
      return n._1 + n._2 | 0;
    }
    if (n.RE_EXN_ID === ED) {
      let match = n._1;
      return match[0] + match[1] | 0;
    }
    throw n;
  }
}

let a1 = "A1";

let a2 = "A2";

let b = {
  TAG: "B",
  _0: 34
};

let c = {
  TAG: "C",
  _0: 4,
  _1: 2
};

let d = {
  TAG: "D",
  _0: [
    4,
    2
  ]
};

export {
  a1,
  a2,
  b,
  c,
  d,
  foo,
  fooA1,
  fooC,
  switchNum,
  Path,
  Make,
  M,
  rollback_path,
  EA1,
  EA2,
  EB,
  EC,
  ED,
  fooExn,
}
/* No side effect */
