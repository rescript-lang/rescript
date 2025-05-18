'use strict';


function isExtension(e) {
  if (e == null) {
    return false;
  } else {
    return typeof e.RE_EXN_ID === "string";
  }
}

function internalToException(e) {
  if (isExtension(e)) {
    return e;
  } else {
    return {
      RE_EXN_ID: "JsExn",
      _1: e
    };
  }
}

let idMap = {};

function create(str) {
  let v = idMap[str];
  if (v !== undefined) {
    let id = v + 1 | 0;
    idMap[str] = id;
    return str + ("/" + id);
  }
  idMap[str] = 1;
  return str;
}

let $$Error = "JsExn";

exports.$$Error = $$Error;
exports.create = create;
exports.internalToException = internalToException;
/* No side effect */
