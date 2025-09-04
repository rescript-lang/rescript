'use strict';


function throwWithMessage(str) {
  throw new Error(str);
}

function throwWithMessage$1(s) {
  throw new EvalError(s);
}

let $$EvalError$1 = {
  throwWithMessage: throwWithMessage$1
};

function throwWithMessage$2(s) {
  throw new RangeError(s);
}

let $$RangeError$1 = {
  throwWithMessage: throwWithMessage$2
};

function throwWithMessage$3(s) {
  throw new ReferenceError(s);
}

let $$ReferenceError$1 = {
  throwWithMessage: throwWithMessage$3
};

function throwWithMessage$4(s) {
  throw new SyntaxError(s);
}

let $$SyntaxError$1 = {
  throwWithMessage: throwWithMessage$4
};

function throwWithMessage$5(s) {
  throw new TypeError(s);
}

let $$TypeError$1 = {
  throwWithMessage: throwWithMessage$5
};

function throwWithMessage$6(s) {
  throw new URIError(s);
}

let $$URIError$1 = {
  throwWithMessage: throwWithMessage$6
};

function panic(msg) {
  throw new Error(`Panic! ` + msg);
}

exports.$$EvalError = $$EvalError$1;
exports.$$RangeError = $$RangeError$1;
exports.$$ReferenceError = $$ReferenceError$1;
exports.$$SyntaxError = $$SyntaxError$1;
exports.$$TypeError = $$TypeError$1;
exports.$$URIError = $$URIError$1;
exports.throwWithMessage = throwWithMessage;
exports.panic = panic;
/* No side effect */
