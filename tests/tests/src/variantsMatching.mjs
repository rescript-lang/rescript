// Generated by ReScript, PLEASE EDIT WITH CARE


function toEnum(x) {
  switch (x) {
    case "thisIsA" :
      return 0;
    case 42 :
      return 1;
    case null :
      return 2;
    case "D" :
      return 3;
    case 3.14 :
      return 5;
  }
}

function toString(x) {
  switch (x) {
    case "thisIsA" :
      return "A";
    case 42 :
      return "B";
    case null :
      return "C";
    case "D" :
      return "D";
    case 3.14 :
      return "Pi";
  }
}

function bar(x) {
  switch (x) {
    case "thisIsA" :
    case 3.14 :
      return 10;
    default:
      return 0;
  }
}

function and_(x, y) {
  if (x === "True" && y === "True") {
    return "True";
  } else {
    return "False";
  }
}

function id(x) {
  if (x === "True") {
    return "True";
  } else {
    return "False";
  }
}

function not_(x) {
  if (x === "True") {
    return "False";
  } else {
    return "True";
  }
}

function st(state) {
  if (typeof state !== "object") {
    return 0;
  } else {
    return 23;
  }
}

function showToJs(x) {
  if (typeof x !== "object" && x === "No") {
    return false;
  } else {
    return true;
  }
}

function third(l) {
  if (!l) {
    return false;
  }
  if (l.hd !== 1) {
    return false;
  }
  let match = l.tl;
  if (!match) {
    return false;
  }
  if (match.hd !== 2) {
    return false;
  }
  let match$1 = match.tl;
  return match$1 && !(match$1.hd !== 3 || match$1.tl);
}

function third2(l) {
  if (typeof l !== "object") {
    return false;
  }
  if (l._0 !== 1) {
    return false;
  }
  let match = l._1;
  if (typeof match !== "object") {
    return false;
  }
  if (match._0 !== 2) {
    return false;
  }
  let match$1 = match._1;
  if (typeof match$1 !== "object") {
    return false;
  }
  if (match$1._0 !== 3) {
    return false;
  }
  let tmp = match$1._1;
  return typeof tmp !== "object";
}

function foo(x) {
  if (typeof x !== "object") {
    switch (x) {
      case "dd" :
        return 1;
      case 12 :
        return 2;
      case false :
        return 3;
    }
  } else {
    switch (x.TAG) {
      case "qq" :
        return 4;
      case 42 :
        return 5;
      case "F" :
        return 6;
    }
  }
}

let CustomizeTags_d = {
  TAG: "qq",
  _0: 42
};

let CustomizeTags_e = {
  TAG: 42,
  _0: 0
};

let CustomizeTags = {
  foo: foo,
  a: "dd",
  b: 12,
  c: false,
  d: CustomizeTags_d,
  e: CustomizeTags_e
};

function isUndefined(x) {
  return x === undefined;
}

function plus(x, y) {
  if (x === undefined) {
    return y;
  } else if (y === undefined) {
    return x;
  } else {
    return x + y | 0;
  }
}

let MyUndefined = {
  $$undefined: undefined,
  isUndefined: isUndefined,
  plus: plus
};

function isNull(x) {
  return x === null;
}

function plus$1(x, y) {
  if (x === null) {
    return y;
  } else if (y === null) {
    return x;
  } else {
    return x + y | 0;
  }
}

let MyNull_null = null;

let MyNull = {
  $$null: MyNull_null,
  isNull: isNull,
  plus: plus$1
};

function isNull$1(x) {
  return x === null;
}

function isUndefined$1(x) {
  return x === undefined;
}

function plus$2(x, y) {
  if (x === null || x === undefined) {
    return y;
  } else if (y === null || y === undefined) {
    return x;
  } else {
    return x + y | 0;
  }
}

function kind(x) {
  if (x === null || x === undefined) {
    if (x === null) {
      return "null";
    } else {
      return "undefined";
    }
  } else {
    return "present";
  }
}

let expectSeven = plus$2(3, 4);

console.log("expect 7:", expectSeven);

let MyNullable_null = null;

let MyNullable = {
  $$null: MyNullable_null,
  $$undefined: undefined,
  isNull: isNull$1,
  isUndefined: isUndefined$1,
  plus: plus$2,
  kind: kind,
  expectSeven: expectSeven
};

function isNull$2(x) {
  return x === null;
}

function isUndefined$2(x) {
  return x === undefined;
}

function isWhyNot(x) {
  return x === "WhyNotAnotherOne";
}

function plus$3(x, y) {
  if (x === null || x === "WhyNotAnotherOne" || x === undefined) {
    switch (x) {
      case null :
      case undefined :
        return y;
      case "WhyNotAnotherOne" :
        break;
    }
  } else if (!(y === null || y === "WhyNotAnotherOne" || y === undefined)) {
    return {
      x: x.x + y.x,
      y: x.y + y.y
    };
  }
  if (!(y === null || y === "WhyNotAnotherOne" || y === undefined)) {
    return "WhyNotAnotherOne";
  }
  switch (y) {
    case null :
    case undefined :
      return x;
    case "WhyNotAnotherOne" :
      return "WhyNotAnotherOne";
  }
}

function kind$1(x) {
  if (!(x === null || x === "WhyNotAnotherOne" || x === undefined)) {
    return "present";
  }
  switch (x) {
    case null :
      return "null";
    case undefined :
      return "undefined";
    case "WhyNotAnotherOne" :
      return "whynot";
  }
}

let expectSeven$1 = plus$3({
  x: 4,
  y: 3
}, {
  x: 3,
  y: 4
});

console.log("expect {x:7, y:7}:", expectSeven$1);

let MyNullableExtended_null = null;

let MyNullableExtended = {
  $$null: MyNullableExtended_null,
  $$undefined: undefined,
  whynot: "WhyNotAnotherOne",
  isNull: isNull$2,
  isUndefined: isUndefined$2,
  isWhyNot: isWhyNot,
  plus: plus$3,
  kind: kind$1,
  expectSeven: expectSeven$1
};

function area(shape) {
  switch (shape.kind) {
    case 1 :
      return Math.PI * Math.pow(shape.radius, 2);
    case "square" :
      return Math.pow(shape.sideLength, 2);
    case "rectangle" :
      return shape.width * shape.height;
  }
}

let TaggedUnions_circle = {
  kind: 1,
  radius: 10
};

let TaggedUnions_square = {
  kind: "square",
  sideLength: 10
};

let TaggedUnions = {
  area: area,
  circle: TaggedUnions_circle,
  square: TaggedUnions_square
};

let CustomTagNotInline_a = {
  "custom-tag": "A",
  _0: 10
};

let CustomTagNotInline_b = {
  "custom-tag": "B",
  _0: 20
};

let CustomTagNotInline = {
  a: CustomTagNotInline_a,
  b: CustomTagNotInline_b
};

function classify(x) {
  switch (typeof x) {
    case "string" :
      return "string";
    case "number" :
      return "int";
    case "boolean" :
      if (x) {
        return "true";
      } else {
        return "boolean";
      }
    case "object" :
      return "Object" + x.name;
  }
}

let UntaggedWithBool = {
  classify: classify
};

function classify$1(x) {
  if (typeof x === "string") {
    return "string";
  } else {
    return "tuple";
  }
}

let UntaggedWithTuple = {
  classify: classify$1
};

export {
  toEnum,
  toString,
  bar,
  and_,
  id,
  not_,
  st,
  showToJs,
  third,
  third2,
  CustomizeTags,
  MyUndefined,
  MyNull,
  MyNullable,
  MyNullableExtended,
  TaggedUnions,
  CustomTagNotInline,
  UntaggedWithBool,
  UntaggedWithTuple,
}
/* expectSeven Not a pure module */
