'use strict';


let then = (function(p, cont) {
    return Promise.resolve(p).then(cont)
  });

let $$catch = (function(p, cont) {
      return Promise.resolve(p).catch(cont)
    });

exports.then = then;
exports.$$catch = $$catch;
/* No side effect */
