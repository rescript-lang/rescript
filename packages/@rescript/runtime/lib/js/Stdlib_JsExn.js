'use strict';

let Primitive_option = require("./Primitive_option.js");

function fromException(exn) {
  if (exn.RE_EXN_ID === "JsExn") {
    return Primitive_option.some(exn._1);
  }
}

let getOrUndefined = (fieldName => t => (t && typeof t[fieldName] === "string" ? t[fieldName] : undefined));

let stack = getOrUndefined("stack");

let message = getOrUndefined("message");

let name = getOrUndefined("name");

let fileName = getOrUndefined("fileName");

exports.fromException = fromException;
exports.stack = stack;
exports.message = message;
exports.name = name;
exports.fileName = fileName;
/* stack Not a pure module */
