// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Test from "./Test.mjs";
import * as Option from "rescript/lib/es6/Option.js";
import * as Primitive_object from "rescript/lib/es6/Primitive_object.js";

let eq = Primitive_object.equal;

Test.run([
  [
    "Core_ObjectTests.res",
    10,
    20,
    30
  ],
  "is: ints"
], Object.is(25, 25), eq, true);

Test.run([
  [
    "Core_ObjectTests.res",
    12,
    20,
    33
  ],
  "is: strings"
], Object.is("abc", "abc"), eq, true);

Test.run([
  [
    "Core_ObjectTests.res",
    13,
    20,
    33
  ],
  "is: strings"
], Object.is("abc", "ABC"), eq, false);

Test.run([
  [
    "Core_ObjectTests.res",
    15,
    20,
    44
  ],
  "is: null and undefined"
], Object.is(null, undefined), eq, false);

Test.run([
  [
    "Core_ObjectTests.res",
    16,
    20,
    44
  ],
  "is: null and undefined"
], Object.is(undefined, undefined), eq, true);

Test.run([
  [
    "Core_ObjectTests.res",
    17,
    20,
    44
  ],
  "is: null and undefined"
], Object.is(null, null), eq, true);

let nums = [
  1,
  2,
  3
];

Test.run([
  [
    "Core_ObjectTests.res",
    20,
    20,
    32
  ],
  "is: arrays"
], Object.is([
  1,
  2,
  3
], [
  1,
  2,
  3
]), eq, false);

Test.run([
  [
    "Core_ObjectTests.res",
    21,
    20,
    32
  ],
  "is: arrays"
], Object.is(nums, nums), eq, true);

Test.run([
  [
    "Core_ObjectTests.res",
    22,
    20,
    32
  ],
  "is: arrays"
], Primitive_object.equal([
  1,
  2,
  3
], [
  1,
  2,
  3
]), eq, true);

Test.run([
  [
    "Core_ObjectTests.res",
    23,
    20,
    32
  ],
  "is: arrays"
], [
  1,
  2,
  3
] === [
  1,
  2,
  3
], eq, false);

Test.run([
  [
    "Core_ObjectTests.res",
    25,
    20,
    30
  ],
  "is: list"
], Object.is({
  hd: 1,
  tl: {
    hd: 2,
    tl: {
      hd: 3,
      tl: /* [] */0
    }
  }
}, {
  hd: 1,
  tl: {
    hd: 2,
    tl: {
      hd: 3,
      tl: /* [] */0
    }
  }
}), eq, false);

Test.run([
  [
    "Core_ObjectTests.res",
    26,
    20,
    30
  ],
  "is: list"
], Primitive_object.equal({
  hd: 1,
  tl: {
    hd: 2,
    tl: {
      hd: 3,
      tl: /* [] */0
    }
  }
}, {
  hd: 1,
  tl: {
    hd: 2,
    tl: {
      hd: 3,
      tl: /* [] */0
    }
  }
}), eq, true);

Test.run([
  [
    "Core_ObjectTests.res",
    27,
    20,
    30
  ],
  "is: list"
], ({
  hd: 1,
  tl: {
    hd: 2,
    tl: {
      hd: 3,
      tl: /* [] */0
    }
  }
}) === ({
  hd: 1,
  tl: {
    hd: 2,
    tl: {
      hd: 3,
      tl: /* [] */0
    }
  }
}), eq, false);

let d = new Date(2000, 1);

Test.run([
  [
    "Core_ObjectTests.res",
    31,
    13,
    23
  ],
  "is: date"
], Object.is(new Date(2000, 1), new Date(2000, 1)), eq, false);

Test.run([
  [
    "Core_ObjectTests.res",
    36,
    20,
    30
  ],
  "is: date"
], Object.is(d, d), eq, true);

let x = {
  a: 1
};

Test.run([
  [
    "Core_ObjectTests.res",
    39,
    20,
    33
  ],
  "is: objects"
], Object.is(x, x), eq, true);

Test.run([
  [
    "Core_ObjectTests.res",
    40,
    20,
    33
  ],
  "is: objects"
], Object.is({
  a: 1
}, {
  a: 1
}), eq, false);

Test.run([
  [
    "Core_ObjectTests.res",
    41,
    20,
    33
  ],
  "is: objects"
], Object.is({}, {}), eq, false);

Test.run([
  [
    "Core_ObjectTests.res",
    42,
    20,
    45
  ],
  "is: === and == operator"
], x === x, eq, true);

Test.run([
  [
    "Core_ObjectTests.res",
    43,
    20,
    45
  ],
  "is: === and == operator"
], Primitive_object.equal(x, x), eq, true);

Test.run([
  [
    "Core_ObjectTests.res",
    44,
    20,
    45
  ],
  "is: === and == operator"
], Primitive_object.equal({
  a: 1
}, {
  a: 1
}), eq, true);

Test.run([
  [
    "Core_ObjectTests.res",
    46,
    20,
    31
  ],
  "is: zeros"
], Object.is(0, 0), eq, true);

Test.run([
  [
    "Core_ObjectTests.res",
    47,
    20,
    31
  ],
  "is: zeros"
], Object.is(-0.0, -0.0), eq, true);

Test.run([
  [
    "Core_ObjectTests.res",
    48,
    20,
    31
  ],
  "is: zeros"
], Object.is(0.0, -0.0), eq, false);

function mkBig(s) {
  return BigInt(s);
}

Test.run([
  [
    "Core_ObjectTests.res",
    51,
    20,
    32
  ],
  "is: bigint"
], Object.is(BigInt("123456789"), BigInt("123456789")), eq, true);

Test.run([
  [
    "Core_ObjectTests.res",
    52,
    20,
    32
  ],
  "is: bigint"
], Object.is(BigInt("123489"), BigInt("123456789")), eq, false);

Test.run([
  [
    "Core_ObjectTests.res",
    53,
    20,
    32
  ],
  "is: bigint"
], Object.is(BigInt("000000000"), BigInt("0")), eq, true);

Test.run([
  [
    "Core_ObjectTests.res",
    54,
    20,
    32
  ],
  "is: bigint"
], BigInt("123") === BigInt("123"), eq, true);

Test.run([
  [
    "Core_ObjectTests.res",
    55,
    20,
    32
  ],
  "is: bigint"
], BigInt("123") === BigInt("123"), eq, true);

Test.run([
  [
    "Core_ObjectTests.res",
    60,
    13,
    50
  ],
  "assign copies from source to target"
], Object.assign({
  a: 1,
  b: 2
}, {
  b: 3,
  c: 0
}), eq, {
  a: 1,
  b: 3,
  c: 0
});

function assignOverwritesTarget(title, source) {
  let sourceObj = {
    a: source
  };
  Test.run([
    [
      "Core_ObjectTests.res",
      68,
      22,
      39
    ],
    "assign " + title
  ], Object.assign({
    a: 1
  }, sourceObj), eq, sourceObj);
  Test.run([
    [
      "Core_ObjectTests.res",
      69,
      22,
      39
    ],
    "assign " + title
  ], Object.assign({
    a: undefined
  }, sourceObj), eq, sourceObj);
  Test.run([
    [
      "Core_ObjectTests.res",
      70,
      22,
      39
    ],
    "assign " + title
  ], Object.assign({
    a: null
  }, sourceObj), eq, sourceObj);
}

assignOverwritesTarget("when source is undefined", undefined);

assignOverwritesTarget("when source is null", null);

assignOverwritesTarget("when source is a number", 1);

assignOverwritesTarget("when source is a string", "abc");

function runGetTest(i) {
  Test.run([
    [
      "Core_ObjectTests.res",
      88,
      22,
      46
    ],
    "Object.get: " + i.title
  ], i.get(i.source()), eq, i.expected);
}

runGetTest({
  title: "prop exists, return Some",
  source: () => ({
    a: 1
  }),
  get: __x => __x["a"],
  expected: 1
});

runGetTest({
  title: "prop NOT exist, return None",
  source: () => ({
    a: 1
  }),
  get: i => i["banana"],
  expected: undefined
});

runGetTest({
  title: "prop like toString, return Some",
  source: () => ({
    a: 1
  }),
  get: i => Option.isSome(i["toString"]),
  expected: true
});

runGetTest({
  title: "prop exist but explicitly undefined, return None",
  source: () => ({
    a: undefined
  }),
  get: i => i["a"],
  expected: undefined
});

runGetTest({
  title: "prop exist but explicitly null, return None",
  source: () => ({
    a: null
  }),
  get: i => i["a"],
  expected: null
});

runGetTest({
  title: "prop exists and is an array, can get it",
  source: () => ({
    a: [
      1,
      2,
      3
    ]
  }),
  get: i => Option.getOr(Option.map(i["a"], i => i.concat([
    4,
    5
  ])), []),
  expected: [
    1,
    2,
    3,
    4,
    5
  ]
});

function getSymbolTestWhenExists() {
  let obj = {};
  let fruit = Symbol("fruit");
  obj[fruit] = "banana";
  let retrieved = obj[fruit];
  Test.run([
    [
      "Core_ObjectTests.res",
      148,
      15,
      63
    ],
    "Object.getSymbol when exists return it as Some"
  ], retrieved, eq, "banana");
}

getSymbolTestWhenExists();

Test.run([
  [
    "Core_ObjectTests.res",
    157,
    13,
    65
  ],
  "Object.getSymbol when not exists return it as None"
], ({})[Symbol("fruit")], eq, undefined);

Test.run([
  [
    "Core_ObjectTests.res",
    166,
    13,
    46
  ],
  "Object.create clones properties"
], Object.create({
    a: 1
  })["a"], eq, 1);

Test.run([
  [
    "Core_ObjectTests.res",
    173,
    13,
    46
  ],
  "Object.create clones properties"
], Object.create({
    a: 1
  })["b"], eq, undefined);

export {
  eq,
  nums,
  d,
  x,
  mkBig,
  assignOverwritesTarget,
  runGetTest,
  getSymbolTestWhenExists,
}
/*  Not a pure module */
