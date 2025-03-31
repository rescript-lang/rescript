// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Test from "./Test.mjs";
import * as Primitive_object from "rescript/lib/es6/Primitive_object.js";

let eq = Primitive_object.equal;

let someString = "hello";

let createdDict = {
  name: "hello",
  age: "what",
  more: "stuff",
  otherStr: someString
};

let intDict = {
  one: 1,
  two: 2,
  three: 3
};

function inferDictByPattern(dict) {
  if (dict.one === 1 && dict.three === 3 && dict.four === 4) {
    dict["five"] = 5;
    return;
  }
  if (dict.two !== 1) {
    console.log("not one");
  } else {
    console.log("two");
  }
}

function constrainedAsDict(dict) {
  if (dict.one !== 1) {
    console.log("not one");
  } else {
    console.log("one");
  }
}

let PatternMatching = {
  inferDictByPattern: inferDictByPattern,
  constrainedAsDict: constrainedAsDict
};

Test.run([
  [
    "Core_DictTests.res",
    39,
    20,
    26
  ],
  "make"
], {}, eq, {});

Test.run([
  [
    "Core_DictTests.res",
    41,
    20,
    31
  ],
  "fromArray"
], Object.fromEntries([[
    "foo",
    "bar"
  ]]), eq, {foo: "bar"});

Test.run([
  [
    "Core_DictTests.res",
    44,
    13,
    35
  ],
  "getUnsafe - existing"
], Object.fromEntries([[
      "foo",
      "bar"
    ]])["foo"], eq, "bar");

Test.run([
  [
    "Core_DictTests.res",
    50,
    13,
    34
  ],
  "getUnsafe - missing"
], ({})["foo"], eq, undefined);

let dict = {
  key1: false,
  key2: undefined
};

Test.run([
  [
    "Core_DictTests.res",
    62,
    22,
    38
  ],
  "has - existing"
], "key1" in dict, eq, true);

Test.run([
  [
    "Core_DictTests.res",
    63,
    22,
    43
  ],
  "has - existing None"
], "key2" in dict, eq, true);

Test.run([
  [
    "Core_DictTests.res",
    64,
    22,
    37
  ],
  "has - missing"
], "key3" in dict, eq, false);

Test.run([
  [
    "Core_DictTests.res",
    65,
    22,
    39
  ],
  "has - prototype"
], "toString" in dict, eq, true);

Test.run([
  [
    "Core_DictTests.res",
    67,
    15,
    51
  ],
  "has - parantesis in generated code"
], typeof ("key1" in dict), eq, "boolean");

let Has = {
  dict: dict
};

let three = 3;

export {
  eq,
  someString,
  createdDict,
  three,
  intDict,
  PatternMatching,
  Has,
}
/*  Not a pure module */
