'use strict';


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

function lowercase_ascii(c) {
  if (c >= /* 'A' */65 && c <= /* 'Z' */90) {
    return c + 32 | 0;
  } else {
    return c;
  }
}

function uppercase_ascii(c) {
  if (c >= /* 'a' */97 && c <= /* 'z' */122) {
    return c - 32 | 0;
  } else {
    return c;
  }
}

function compare(c1, c2) {
  return c1 - c2 | 0;
}

function equal(c1, c2) {
  return (c1 - c2 | 0) === 0;
}

exports.escaped = escaped;
exports.lowercase_ascii = lowercase_ascii;
exports.uppercase_ascii = uppercase_ascii;
exports.compare = compare;
exports.equal = equal;
/* No side effect */
