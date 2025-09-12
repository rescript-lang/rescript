'use strict';

let Primitive_exceptions = require("./Primitive_exceptions.js");

function $$catch(promise, callback) {
  return promise.catch(err => callback(Primitive_exceptions.internalToException(err)));
}

function sleep(ms) {
  return new Promise((resolve, param) => {
    setTimeout(resolve, ms);
  });
}

exports.$$catch = $$catch;
exports.sleep = sleep;
/* No side effect */
