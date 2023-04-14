'use strict';

var Belt_SetInt = require("../../lib/js/belt_SetInt.js");

function bench(param) {
  var data;
  console.time("bs_set_bench.res 6");
  for(var i = 0; i <= 1000000; ++i){
    data = Belt_SetInt.add(data, i);
  }
  console.timeEnd("bs_set_bench.res 6");
  console.time("bs_set_bench.res 11");
  for(var i$1 = 0; i$1 <= 1000000; ++i$1){
    if (!Belt_SetInt.has(data, i$1)) {
      throw {
            RE_EXN_ID: "Assert_failure",
            _1: [
              "bs_set_bench.res",
              13,
              6
            ],
            Error: new Error()
          };
    }
    
  }
  console.timeEnd("bs_set_bench.res 11");
  console.time("bs_set_bench.res 16");
  for(var i$2 = 0; i$2 <= 1000000; ++i$2){
    data = Belt_SetInt.remove(data, i$2);
  }
  console.timeEnd("bs_set_bench.res 16");
  if (Belt_SetInt.size(data) === 0) {
    return ;
  }
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "bs_set_bench.res",
          21,
          2
        ],
        Error: new Error()
      };
}

console.time("bs_set_bench.res 24");

bench(undefined);

console.timeEnd("bs_set_bench.res 24");

var count = 1000000;

var N;

exports.count = count;
exports.N = N;
exports.bench = bench;
/*  Not a pure module */
