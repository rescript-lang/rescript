// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';


function f(x) {
  switch (x) {
    case "A" :
      return 1;
    case "B" :
      return 2;
    case "C" :
      return 3;
  }
}

function f_0(x) {
  switch (x) {
    case "A" :
      return -1;
    case "B" :
      return 0;
    case "C" :
      return 1;
  }
}

function f2(x) {
  if (x >= 3) {
    return "T003";
  }
  switch (x) {
    case 0 :
      return "T000";
    case 1 :
      return "T001";
    case 2 :
      return "T002";
  }
}

function f3(x) {
  switch (x) {
    case "X0" :
      return "Y0";
    case "X1" :
      return "Y1";
    case "X2" :
      return "Y2";
    case "X3" :
      return "Y3";
    case "X4" :
      return "Y4";
  }
}

function f4(x) {
  return 3;
}

function f5(x) {
  if (typeof x !== "object") {
    switch (x) {
      case "A" :
        return 1;
      case "B" :
        return 3;
      case "F" :
        return 4;
    }
  } else {
    switch (x.TAG) {
      case "C" :
      case "D" :
        return 1;
      case "E" :
        return 2;
    }
  }
}

function f6(x) {
  if (typeof x === "object") {
    return 1;
  }
  switch (x) {
    case "A" :
    case "B" :
      return 0;
    case "F" :
      return 2;
  }
}

function f7(x) {
  if (typeof x !== "object") {
    switch (x) {
      case "A" :
        return 1;
      case "B" :
        return 2;
      case "F" :
        return -1;
    }
  } else {
    switch (x.TAG) {
      case "C" :
        return 3;
      case "D" :
        return 4;
      case "E" :
        return -1;
    }
  }
}

function f8(x) {
  if (typeof x !== "object") {
    switch (x) {
      case "T60" :
      case "T61" :
        return 1;
      default:
        return 3;
    }
  } else {
    switch (x.TAG) {
      case "T64" :
      case "T65" :
        return 2;
      default:
        return 3;
    }
  }
}

function f9(x) {
  if (typeof x !== "object") {
    if (x === "T63") {
      return 3;
    } else {
      return 1;
    }
  }
  switch (x.TAG) {
    case "T64" :
    case "T65" :
      return 2;
    case "T66" :
    case "T68" :
      return 3;
  }
}

function f10(x) {
  if (typeof x !== "object") {
    switch (x) {
      case "T60" :
        return 0;
      case "T61" :
        return 2;
      case "T62" :
        return 4;
      case "T63" :
        return 1;
    }
  } else {
    switch (x.TAG) {
      case "T64" :
      case "T65" :
        return 2;
      case "T66" :
      case "T68" :
        return 3;
    }
  }
}

function f11(x) {
  if (typeof x !== "object") {
    return 2;
  }
  if (x.TAG === "D") {
    return 1;
  }
  throw new Error("Assert_failure", {
    cause: {
      RE_EXN_ID: "Assert_failure",
      _1: [
        "adt_optimize_test.res",
        202,
        9
      ]
    }
  });
}

exports.f = f;
exports.f_0 = f_0;
exports.f2 = f2;
exports.f3 = f3;
exports.f4 = f4;
exports.f5 = f5;
exports.f6 = f6;
exports.f7 = f7;
exports.f8 = f8;
exports.f9 = f9;
exports.f10 = f10;
exports.f11 = f11;
/* No side effect */
