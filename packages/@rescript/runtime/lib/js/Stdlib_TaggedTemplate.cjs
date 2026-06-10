'use strict';


let make = (transform => (strings, ...values) => transform(strings, values));

exports.make = make;
/* No side effect */
