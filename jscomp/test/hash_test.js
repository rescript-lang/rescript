// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

let Mt = require("./mt.js");
let Char = require("../../lib/js/char.js");
let $$Array = require("../../lib/js/array.js");
let Hashtbl = require("../../lib/js/hashtbl.js");
let Mt_global = require("./mt_global.js");
let Caml_string = require("../../lib/js/caml_string.js");

let suites = {
  contents: /* [] */0
};

let test_id = {
  contents: 0
};

function eq(f, x, y) {
  Mt_global.collect_eq(test_id, suites, f, x, y);
}

let test_strings = $$Array.init(32, (function (i) {
  return Caml_string.make(i, Char.chr(i));
}));

let test_strings_hash_results = [
  0,
  904391063,
  889600889,
  929588010,
  596566298,
  365199070,
  448044845,
  311625091,
  681445541,
  634941451,
  82108334,
  17482990,
  491949228,
  696194769,
  711728152,
  594966620,
  820561748,
  958901713,
  102794744,
  378848504,
  349314368,
  114167579,
  71240932,
  110067399,
  280623927,
  323523937,
  310683234,
  178511779,
  585018975,
  544388424,
  1043872806,
  831138595
];

function normalize(x) {
  return x & 1073741823;
}

function caml_hash(x) {
  return Hashtbl.hash(x) & 1073741823;
}

eq("File \"hash_test.res\", line 44, characters 12-19", $$Array.map(caml_hash, test_strings), test_strings_hash_results);

eq("File \"hash_test.res\", line 46, characters 12-19", Hashtbl.hash(0) & 1073741823, 129913994);

eq("File \"hash_test.res\", line 48, characters 12-19", Hashtbl.hash("x") & 1073741823, 780510073);

eq("File \"hash_test.res\", line 50, characters 12-19", Hashtbl.hash("xy") & 1073741823, 194127723);

Mt.from_pair_suites("Hash_test", suites.contents);

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.test_strings = test_strings;
exports.test_strings_hash_results = test_strings_hash_results;
exports.normalize = normalize;
exports.caml_hash = caml_hash;
/* test_strings Not a pure module */
