'use strict';

let Primitive_exceptions = require("./Primitive_exceptions.cjs");

function $$catch(promise, callback) {
  return promise.catch(err => callback(Primitive_exceptions.internalToException(err)));
}

exports.$$catch = $$catch;
/* No side effect */
