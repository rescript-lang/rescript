'use strict';


function f0() {
  return 0;
}

function f1(a0) {
  return a0;
}

function f2(a0, a1) {
  return [
          a0,
          a1
        ];
}

console.log(f0(undefined));

console.log(0);

console.log([
      0,
      1
    ]);

function xx() {
  while(true) {
    continue ;
  };
}

function log2(logger, message, obj) {
  logger.log2(message, obj);
}

exports.f0 = f0;
exports.f1 = f1;
exports.f2 = f2;
exports.xx = xx;
exports.log2 = log2;
/*  Not a pure module */
