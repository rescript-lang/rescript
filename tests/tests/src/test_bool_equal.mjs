// Generated by ReScript, PLEASE EDIT WITH CARE


function bool_equal(x, y) {
  if (x) {
    return y;
  } else {
    return !y;
  }
}

function assertions() {
  if (!bool_equal(true, true)) {
    throw {
      RE_EXN_ID: "Assert_failure",
      _1: [
        "test_bool_equal.res",
        16,
        2
      ],
      Error: new Error()
    };
  }
  if (!bool_equal(false, false)) {
    throw {
      RE_EXN_ID: "Assert_failure",
      _1: [
        "test_bool_equal.res",
        17,
        2
      ],
      Error: new Error()
    };
  }
  if (bool_equal(true, false)) {
    throw {
      RE_EXN_ID: "Assert_failure",
      _1: [
        "test_bool_equal.res",
        18,
        2
      ],
      Error: new Error()
    };
  }
  if (bool_equal(false, true)) {
    throw {
      RE_EXN_ID: "Assert_failure",
      _1: [
        "test_bool_equal.res",
        19,
        2
      ],
      Error: new Error()
    };
  }
  
}

function f0(x) {
  if (x === true) {
    return 1;
  } else {
    return 2;
  }
}

function f1(x) {
  if (x !== true) {
    return 1;
  } else {
    return 2;
  }
}

function f2(x) {
  if (x === true) {
    return 1;
  } else {
    return 2;
  }
}

function f3(x) {
  if (x === false) {
    return 1;
  } else {
    return 2;
  }
}

function f4(x) {
  if (x !== true) {
    return 1;
  } else {
    return 2;
  }
}

function f5(x) {
  if (x !== 0) {
    return 2;
  } else {
    return 1;
  }
}

function f6(x) {
  if (x === /* [] */0) {
    return 1;
  } else {
    return 2;
  }
}

function f7(x) {
  if (x.length !== 0) {
    return 1;
  } else {
    return 2;
  }
}

function f8(x) {
  return 1;
}

export {
  bool_equal,
  assertions,
  f0,
  f1,
  f2,
  f3,
  f4,
  f5,
  f6,
  f7,
  f8,
}
/* No side effect */
