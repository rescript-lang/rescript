

import * as Stdlib_Int from "./Stdlib_Int.mjs";

function decode(value) {
  let match = value.split(";");
  if (match.length !== 5) {
    return;
  }
  let file = match[0];
  let startLine = match[1];
  let startCol = match[2];
  let endLine = match[3];
  let endCol = match[4];
  let match$1 = Stdlib_Int.fromString(startLine);
  let match$2 = Stdlib_Int.fromString(startCol);
  let match$3 = Stdlib_Int.fromString(endLine);
  let match$4 = Stdlib_Int.fromString(endCol);
  if (match$1 !== undefined && match$2 !== undefined && match$3 !== undefined && match$4 !== undefined) {
    return {
      file: file,
      startLine: match$1,
      startCol: match$2,
      endLine: match$3,
      endCol: match$4
    };
  }
}

let Pos = {
  decode: decode
};

function segments(value) {
  return value.split(".");
}

function name(value) {
  let segments = value.split(".");
  let length = segments.length;
  if (length === 0) {
    return "";
  } else {
    return segments[length - 1 | 0];
  }
}

let ValuePath = {
  segments: segments,
  name: name
};

export {
  Pos,
  ValuePath,
}
/* No side effect */
