'use strict';


function fromIntExn(n) {
  if (n < 0 || n > 255) {
    throw {
      RE_EXN_ID: "Invalid_argument",
      _1: "`Char.fromIntExn` expects an integer between 0 and 255",
      Error: new Error()
    };
  }
  return n;
}

function fromInt(n) {
  if (n < 0 || n > 255) {
    return;
  } else {
    return n;
  }
}

function escaped(param) {
  let exit = 0;
  if (param >= 40) {
    if (param === 92) {
      return "\\\\";
    }
    exit = param >= 127 ? 1 : 2;
  } else if (param >= 32) {
    if (param >= 39) {
      return "\\'";
    }
    exit = 2;
  } else if (param >= 14) {
    exit = 1;
  } else {
    switch (param) {
      case 8 :
        return "\\b";
      case 9 :
        return "\\t";
      case 10 :
        return "\\n";
      case 0 :
      case 1 :
      case 2 :
      case 3 :
      case 4 :
      case 5 :
      case 6 :
      case 7 :
      case 11 :
      case 12 :
        exit = 1;
        break;
      case 13 :
        return "\\r";
    }
  }
  switch (exit) {
    case 1 :
      let s = Array(4);
      s[0] = /* '\\' */92;
      s[1] = 48 + (param / 100 | 0) | 0;
      s[2] = 48 + (param / 10 | 0) % 10 | 0;
      s[3] = 48 + param % 10 | 0;
      return String.fromCodePoint(...s);
    case 2 :
      let s$1 = Array(1);
      s$1[0] = param;
      return String.fromCodePoint(...s$1);
  }
}

function toLowerCaseAscii(c) {
  if (c >= /* 'A' */65 && c <= /* 'Z' */90) {
    return c + 32 | 0;
  } else {
    return c;
  }
}

function toUpperCaseAscii(c) {
  if (c >= /* 'a' */97 && c <= /* 'z' */122) {
    return c - 32 | 0;
  } else {
    return c;
  }
}

let lowercase_ascii = toLowerCaseAscii;

let uppercase_ascii = toUpperCaseAscii;

exports.escaped = escaped;
exports.lowercase_ascii = lowercase_ascii;
exports.uppercase_ascii = uppercase_ascii;
exports.toLowerCaseAscii = toLowerCaseAscii;
exports.toUpperCaseAscii = toUpperCaseAscii;
exports.fromIntExn = fromIntExn;
exports.fromInt = fromInt;
/* No side effect */
