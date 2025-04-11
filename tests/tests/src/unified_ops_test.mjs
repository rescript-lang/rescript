// Generated by ReScript, PLEASE EDIT WITH CARE


let float = 1 + 2;

let string = "12";

let bigint = 1n + 2n;

function unknown(a, b) {
  return a + b | 0;
}

function lhsint(a, b) {
  return a + b | 0;
}

function lhsfloat(a, b) {
  return a + b;
}

function lhsbigint(a, b) {
  return a + b;
}

function lhsstring(a, b) {
  return a + b;
}

function rhsint(a, b) {
  return a + b | 0;
}

function rhsfloat(a, b) {
  return a + b;
}

function rhsbigint(a, b) {
  return a + b;
}

function rhsstring(a, b) {
  return a + b;
}

function case1(a) {
  return 1 + a | 0;
}

function case2(a, b) {
  return a + "test" + b;
}

function even(n) {
  return n % 2 === 0;
}

function odd(n) {
  return n % 2 === 1;
}

let pow1 = 4;

let pow2 = 2 ** 2;

let pow3 = 2n ** 2n;

let pow_overflow = 0;

function bxor_int(a, b) {
  return a ^ b;
}

function bxor_bigint(a, b) {
  return a ^ b;
}

let shl_bigint = (1n << 2n);

let shr_bigint = (8n >> 2n);

let int = 3;

let shl_int = 4;

let shr_int = 2;

let lsr_int = 2147483647;

export {
  int,
  float,
  string,
  bigint,
  unknown,
  lhsint,
  lhsfloat,
  lhsbigint,
  lhsstring,
  rhsint,
  rhsfloat,
  rhsbigint,
  rhsstring,
  case1,
  case2,
  even,
  odd,
  pow1,
  pow2,
  pow3,
  pow_overflow,
  bxor_int,
  bxor_bigint,
  shl_int,
  shr_int,
  lsr_int,
  shl_bigint,
  shr_bigint,
}
/* No side effect */
