


function classify(value) {
  let match = Object.prototype.toString.call(value);
  switch (match) {
    case "[object BigInt]" :
      return {
        TAG: "BigInt",
        _0: value
      };
    case "[object Boolean]" :
      return {
        TAG: "Bool",
        _0: value
      };
    case "[object AsyncFunction]" :
    case "[object Function]" :
    case "[object GeneratorFunction]" :
      return {
        TAG: "Function",
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
    case "[object Symbol]" :
      return {
        TAG: "Symbol",
        _0: value
      };
    case "[object Undefined]" :
      return "Undefined";
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

export {
  Classify,
}
/* No side effect */
