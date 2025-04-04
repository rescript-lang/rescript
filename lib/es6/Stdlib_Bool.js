


function toString(b) {
  if (b) {
    return "true";
  } else {
    return "false";
  }
}

function fromString(s) {
  switch (s) {
    case "false" :
      return false;
    case "true" :
      return true;
    default:
      return;
  }
}

function fromStringExn(param) {
  switch (param) {
    case "false" :
      return false;
    case "true" :
      return true;
    default:
      throw {
        RE_EXN_ID: "Invalid_argument",
        _1: "Bool.fromStringExn: value is neither \"true\" nor \"false\"",
        Error: new Error()
      };
  }
}

export {
  toString,
  fromString,
  fromStringExn,
}
/* No side effect */
