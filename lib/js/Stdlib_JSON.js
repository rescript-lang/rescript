'use strict';


function classify(value) {
  let match = Object.prototype.toString.call(value);
  switch (match) {
    case "[object Array]" :
      return {
        TAG: "Array",
        _0: value
      };
    case "[object Boolean]" :
      return {
        TAG: "Bool",
        _0: value
      };
    case "[object Null]" :
      return "Null";
    case "[object Number]" :
      return {
        TAG: "Number",
        _0: value
      };
    case "[object String]" :
      return {
        TAG: "String",
        _0: value
      };
    default:
      return {
        TAG: "Object",
        _0: value
      };
  }
}

let Classify = {
  classify: classify
};

let Encode = {};

function bool(json) {
  if (typeof json === "boolean") {
    return json;
  }
  
}

function $$null(json) {
  if (json === null) {
    return null;
  }
  
}

function string(json) {
  if (typeof json === "string") {
    return json;
  }
  
}

function float(json) {
  if (typeof json === "number") {
    return json;
  }
  
}

function object(json) {
  if (typeof json === "object" && !Array.isArray(json) && json !== null) {
    return json;
  }
  
}

function array(json) {
  if (Array.isArray(json)) {
    return json;
  }
  
}

let Decode = {
  bool: bool,
  $$null: $$null,
  string: string,
  float: float,
  object: object,
  array: array
};

exports.Classify = Classify;
exports.Encode = Encode;
exports.Decode = Decode;
/* No side effect */
