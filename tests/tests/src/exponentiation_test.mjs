// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Mt from "./mt.mjs";

let suites = {
  contents: /* [] */0
};

let test_id = {
  contents: 0
};

function eq(loc, x, y) {
  Mt.eq_suites(test_id, suites, loc, x, y);
}

let intPow = ((a, b) => Math.pow(a, b) | 0);

let four = 4;

eq("File \"exponentiation_test.res\", line 11, characters 5-12", 2 ** 3 ** 2, Math.pow(2, Math.pow(3, 2)));

eq("File \"exponentiation_test.res\", line 12, characters 5-12", 2 ** (-3) ** 2, Math.pow(2, Math.pow(-3, 2)));

eq("File \"exponentiation_test.res\", line 13, characters 5-12", (2 ** 3) ** 2, Math.pow(Math.pow(2, 3), 2));

eq("File \"exponentiation_test.res\", line 14, characters 5-12", (-2) ** 2, Math.pow(-2, 2));

eq("File \"exponentiation_test.res\", line 16, characters 5-12", 512, intPow(2, intPow(3, 2)));

eq("File \"exponentiation_test.res\", line 17, characters 5-12", 512, intPow(2, intPow(-3, 2)));

eq("File \"exponentiation_test.res\", line 18, characters 5-12", 64, intPow(intPow(2, 3), 2));

eq("File \"exponentiation_test.res\", line 19, characters 5-12", -2147483648, intPow(-2, 31));

eq("File \"exponentiation_test.res\", line 20, characters 5-12", 0, intPow(2, 32));

eq("File \"exponentiation_test.res\", line 21, characters 5-12", 1, intPow(2147483647, 2));

eq("File \"exponentiation_test.res\", line 23, characters 5-12", 256, four ** four | 0);

Mt.from_pair_suites("Exponentiation_test", suites.contents);

export {
  suites,
  test_id,
  eq,
  intPow,
  four,
}
/*  Not a pure module */
