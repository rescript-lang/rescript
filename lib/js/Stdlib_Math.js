'use strict';


let Constants = {};

function floor(f) {
  return Math.floor(f) | 0;
}

function ceil(f) {
  return Math.ceil(f) | 0;
}

function random(min, max) {
  let f = Math.random() * (max - min | 0);
  return (Math.floor(f) | 0) + min | 0;
}

let Int = {
  floor: floor,
  ceil: ceil,
  random: random
};

exports.Constants = Constants;
exports.Int = Int;
/* No side effect */
