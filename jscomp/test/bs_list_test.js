// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

let Mt = require("./mt.js");
let Caml = require("../../lib/js/caml.js");
let Caml_obj = require("../../lib/js/caml_obj.js");
let Belt_List = require("../../lib/js/belt_List.js");
let Belt_Array = require("../../lib/js/belt_Array.js");

let suites = {
  contents: /* [] */0
};

let test_id = {
  contents: 0
};

function eq(loc, x, y) {
  Mt.eq_suites(test_id, suites, loc, x, y);
}

function b(loc, x) {
  Mt.bool_suites(test_id, suites, loc, x);
}

function $$throw(loc, x) {
  Mt.throw_suites(test_id, suites, loc, x);
}

function sum(xs) {
  let v = {
    contents: 0
  };
  Belt_List.forEach(xs, (function (x) {
    v.contents = v.contents + x | 0;
  }));
  return v.contents;
}

function sum2(xs, ys) {
  let v = {
    contents: 0
  };
  Belt_List.forEach2(xs, ys, (function (x, y) {
    v.contents = (v.contents + x | 0) + y | 0;
  }));
  return v.contents;
}

let u = Belt_List.makeBy(5, (function (i) {
  return Math.imul(i, i);
}));

function f(i) {
  eq("File \"bs_list_test.res\", line 27, characters 18-25", Belt_List.getExn(u, i), Math.imul(i, i));
}

for(let i = 0; i <= 4; ++i){
  f(i);
}

eq("File \"bs_list_test.res\", line 31, characters 5-12", Belt_List.map(u, (function (i) {
  return i + 1 | 0;
})), {
  hd: 1,
  tl: {
    hd: 2,
    tl: {
      hd: 5,
      tl: {
        hd: 10,
        tl: {
          hd: 17,
          tl: /* [] */0
        }
      }
    }
  }
});

eq("File \"bs_list_test.res\", line 32, characters 5-12", Belt_List.getBy({
  hd: 1,
  tl: {
    hd: 4,
    tl: {
      hd: 3,
      tl: {
        hd: 2,
        tl: /* [] */0
      }
    }
  }
}, (function (x) {
  return x % 2 === 0;
})), 4);

eq("File \"bs_list_test.res\", line 33, characters 5-12", Belt_List.getBy({
  hd: 1,
  tl: {
    hd: 4,
    tl: {
      hd: 3,
      tl: {
        hd: 2,
        tl: /* [] */0
      }
    }
  }
}, (function (x) {
  return x % 5 === 0;
})), undefined);

function $eq$tilde(extra, extra$1) {
  return eq("FLATTEN", extra, extra$1);
}

$eq$tilde(Belt_List.flatten({
  hd: {
    hd: 1,
    tl: /* [] */0
  },
  tl: {
    hd: {
      hd: 2,
      tl: /* [] */0
    },
    tl: {
      hd: {
        hd: 3,
        tl: /* [] */0
      },
      tl: {
        hd: /* [] */0,
        tl: {
          hd: Belt_List.makeBy(4, (function (i) {
            return i;
          })),
          tl: /* [] */0
        }
      }
    }
  }
}), {
  hd: 1,
  tl: {
    hd: 2,
    tl: {
      hd: 3,
      tl: {
        hd: 0,
        tl: {
          hd: 1,
          tl: {
            hd: 2,
            tl: {
              hd: 3,
              tl: /* [] */0
            }
          }
        }
      }
    }
  }
});

$eq$tilde(Belt_List.flatten(/* [] */0), /* [] */0);

$eq$tilde(Belt_List.flatten({
  hd: /* [] */0,
  tl: {
    hd: /* [] */0,
    tl: {
      hd: {
        hd: 2,
        tl: /* [] */0
      },
      tl: {
        hd: {
          hd: 1,
          tl: /* [] */0
        },
        tl: {
          hd: {
            hd: 2,
            tl: /* [] */0
          },
          tl: {
            hd: /* [] */0,
            tl: /* [] */0
          }
        }
      }
    }
  }
}), {
  hd: 2,
  tl: {
    hd: 1,
    tl: {
      hd: 2,
      tl: /* [] */0
    }
  }
});

function $eq$tilde$1(extra, extra$1) {
  return eq("CONCATMANY", extra, extra$1);
}

$eq$tilde$1(Belt_List.concatMany([
  {
    hd: 1,
    tl: /* [] */0
  },
  {
    hd: 2,
    tl: /* [] */0
  },
  {
    hd: 3,
    tl: /* [] */0
  },
  /* [] */0,
  Belt_List.makeBy(4, (function (i) {
    return i;
  }))
]), {
  hd: 1,
  tl: {
    hd: 2,
    tl: {
      hd: 3,
      tl: {
        hd: 0,
        tl: {
          hd: 1,
          tl: {
            hd: 2,
            tl: {
              hd: 3,
              tl: /* [] */0
            }
          }
        }
      }
    }
  }
});

$eq$tilde$1(Belt_List.concatMany([]), /* [] */0);

$eq$tilde$1(Belt_List.concatMany([
  /* [] */0,
  /* [] */0,
  {
    hd: 2,
    tl: /* [] */0
  },
  {
    hd: 1,
    tl: /* [] */0
  },
  {
    hd: 2,
    tl: /* [] */0
  },
  /* [] */0
]), {
  hd: 2,
  tl: {
    hd: 1,
    tl: {
      hd: 2,
      tl: /* [] */0
    }
  }
});

$eq$tilde$1(Belt_List.concatMany([
  /* [] */0,
  /* [] */0,
  {
    hd: 2,
    tl: {
      hd: 3,
      tl: /* [] */0
    }
  },
  {
    hd: 1,
    tl: /* [] */0
  },
  {
    hd: 2,
    tl: /* [] */0
  },
  /* [] */0
]), {
  hd: 2,
  tl: {
    hd: 3,
    tl: {
      hd: 1,
      tl: {
        hd: 2,
        tl: /* [] */0
      }
    }
  }
});

$eq$tilde$1(Belt_List.concatMany([{
    hd: 1,
    tl: {
      hd: 2,
      tl: {
        hd: 3,
        tl: /* [] */0
      }
    }
  }]), {
  hd: 1,
  tl: {
    hd: 2,
    tl: {
      hd: 3,
      tl: /* [] */0
    }
  }
});

eq("File \"bs_list_test.res\", line 66, characters 2-9", Belt_List.toArray(Belt_List.concat(Belt_List.makeBy(100, (function (i) {
  return i;
})), Belt_List.makeBy(100, (function (i) {
  return i;
})))), Belt_Array.concat(Belt_Array.makeBy(100, (function (i) {
  return i;
})), Belt_Array.makeBy(100, (function (i) {
  return i;
}))));

function $eq$tilde$2(extra, extra$1) {
  return eq("APPEND", extra, extra$1);
}

$eq$tilde$2(Belt_List.concat({
  hd: 1,
  tl: /* [] */0
}, /* [] */0), {
  hd: 1,
  tl: /* [] */0
});

$eq$tilde$2(Belt_List.concat(/* [] */0, {
  hd: 1,
  tl: /* [] */0
}), {
  hd: 1,
  tl: /* [] */0
});

function $eq$tilde$3(extra, extra$1) {
  return eq("ZIP", extra, extra$1);
}

$eq$tilde$3(Belt_List.zip({
  hd: 1,
  tl: {
    hd: 2,
    tl: {
      hd: 3,
      tl: /* [] */0
    }
  }
}, {
  hd: 3,
  tl: {
    hd: 4,
    tl: /* [] */0
  }
}), {
  hd: [
    1,
    3
  ],
  tl: {
    hd: [
      2,
      4
    ],
    tl: /* [] */0
  }
});

$eq$tilde$3(Belt_List.zip(/* [] */0, {
  hd: 1,
  tl: /* [] */0
}), /* [] */0);

$eq$tilde$3(Belt_List.zip(/* [] */0, /* [] */0), /* [] */0);

$eq$tilde$3(Belt_List.zip({
  hd: 1,
  tl: {
    hd: 2,
    tl: {
      hd: 3,
      tl: /* [] */0
    }
  }
}, /* [] */0), /* [] */0);

$eq$tilde$3(Belt_List.zip({
  hd: 1,
  tl: {
    hd: 2,
    tl: {
      hd: 3,
      tl: /* [] */0
    }
  }
}, {
  hd: 2,
  tl: {
    hd: 3,
    tl: {
      hd: 4,
      tl: /* [] */0
    }
  }
}), {
  hd: [
    1,
    2
  ],
  tl: {
    hd: [
      2,
      3
    ],
    tl: {
      hd: [
        3,
        4
      ],
      tl: /* [] */0
    }
  }
});

function mod2(x) {
  return x % 2 === 0;
}

function evenIndex(_x, i) {
  return i % 2 === 0;
}

function $eq$tilde$4(extra, extra$1) {
  return eq("PARTITION", extra, extra$1);
}

$eq$tilde$4(Belt_List.partition({
  hd: 1,
  tl: {
    hd: 2,
    tl: {
      hd: 3,
      tl: {
        hd: 2,
        tl: {
          hd: 3,
          tl: {
            hd: 4,
            tl: /* [] */0
          }
        }
      }
    }
  }
}, mod2), [
  {
    hd: 2,
    tl: {
      hd: 2,
      tl: {
        hd: 4,
        tl: /* [] */0
      }
    }
  },
  {
    hd: 1,
    tl: {
      hd: 3,
      tl: {
        hd: 3,
        tl: /* [] */0
      }
    }
  }
]);

$eq$tilde$4(Belt_List.partition({
  hd: 2,
  tl: {
    hd: 2,
    tl: {
      hd: 2,
      tl: {
        hd: 4,
        tl: /* [] */0
      }
    }
  }
}, mod2), [
  {
    hd: 2,
    tl: {
      hd: 2,
      tl: {
        hd: 2,
        tl: {
          hd: 4,
          tl: /* [] */0
        }
      }
    }
  },
  /* [] */0
]);

$eq$tilde$4(Belt_List.partition({
  hd: 2,
  tl: {
    hd: 2,
    tl: {
      hd: 2,
      tl: {
        hd: 4,
        tl: /* [] */0
      }
    }
  }
}, (function (x) {
  return !mod2(x);
})), [
  /* [] */0,
  {
    hd: 2,
    tl: {
      hd: 2,
      tl: {
        hd: 2,
        tl: {
          hd: 4,
          tl: /* [] */0
        }
      }
    }
  }
]);

$eq$tilde$4(Belt_List.partition(/* [] */0, mod2), [
  /* [] */0,
  /* [] */0
]);

function $eq$tilde$5(extra, extra$1) {
  return eq("UNZIP", extra, extra$1);
}

$eq$tilde$5(Belt_List.unzip(/* [] */0), [
  /* [] */0,
  /* [] */0
]);

$eq$tilde$5(Belt_List.unzip({
  hd: [
    1,
    2
  ],
  tl: /* [] */0
}), [
  {
    hd: 1,
    tl: /* [] */0
  },
  {
    hd: 2,
    tl: /* [] */0
  }
]);

$eq$tilde$5(Belt_List.unzip({
  hd: [
    1,
    2
  ],
  tl: {
    hd: [
      3,
      4
    ],
    tl: /* [] */0
  }
}), [
  {
    hd: 1,
    tl: {
      hd: 3,
      tl: /* [] */0
    }
  },
  {
    hd: 2,
    tl: {
      hd: 4,
      tl: /* [] */0
    }
  }
]);

function $eq$tilde$6(extra, extra$1) {
  return eq("FILTER", extra, extra$1);
}

$eq$tilde$6(Belt_List.keep({
  hd: 1,
  tl: {
    hd: 2,
    tl: {
      hd: 3,
      tl: {
        hd: 4,
        tl: /* [] */0
      }
    }
  }
}, mod2), {
  hd: 2,
  tl: {
    hd: 4,
    tl: /* [] */0
  }
});

$eq$tilde$6(Belt_List.keep({
  hd: 1,
  tl: {
    hd: 3,
    tl: {
      hd: 41,
      tl: /* [] */0
    }
  }
}, mod2), /* [] */0);

$eq$tilde$6(Belt_List.keep(/* [] */0, mod2), /* [] */0);

$eq$tilde$6(Belt_List.keep({
  hd: 2,
  tl: {
    hd: 2,
    tl: {
      hd: 2,
      tl: {
        hd: 4,
        tl: {
          hd: 6,
          tl: /* [] */0
        }
      }
    }
  }
}, mod2), {
  hd: 2,
  tl: {
    hd: 2,
    tl: {
      hd: 2,
      tl: {
        hd: 4,
        tl: {
          hd: 6,
          tl: /* [] */0
        }
      }
    }
  }
});

function $eq$tilde$7(extra, extra$1) {
  return eq("FILTER2", extra, extra$1);
}

$eq$tilde$7(Belt_List.keepWithIndex(/* [] */0, evenIndex), /* [] */0);

$eq$tilde$7(Belt_List.keepWithIndex({
  hd: 1,
  tl: {
    hd: 2,
    tl: {
      hd: 3,
      tl: {
        hd: 4,
        tl: /* [] */0
      }
    }
  }
}, evenIndex), {
  hd: 1,
  tl: {
    hd: 3,
    tl: /* [] */0
  }
});

$eq$tilde$7(Belt_List.keepWithIndex({
  hd: 0,
  tl: {
    hd: 1,
    tl: {
      hd: 2,
      tl: {
        hd: 3,
        tl: {
          hd: 4,
          tl: {
            hd: 5,
            tl: {
              hd: 6,
              tl: {
                hd: 7,
                tl: /* [] */0
              }
            }
          }
        }
      }
    }
  }
}, evenIndex), {
  hd: 0,
  tl: {
    hd: 2,
    tl: {
      hd: 4,
      tl: {
        hd: 6,
        tl: /* [] */0
      }
    }
  }
});

function id(x) {
  return x;
}

function $eq$tilde$8(extra, extra$1) {
  return eq("MAP", extra, extra$1);
}

$eq$tilde$8(Belt_List.map(Belt_List.makeBy(5, id), (function (x) {
  return (x << 1);
})), {
  hd: 0,
  tl: {
    hd: 2,
    tl: {
      hd: 4,
      tl: {
        hd: 6,
        tl: {
          hd: 8,
          tl: /* [] */0
        }
      }
    }
  }
});

$eq$tilde$8(Belt_List.map(/* [] */0, id), /* [] */0);

$eq$tilde$8(Belt_List.map({
  hd: 1,
  tl: /* [] */0
}, (function (x) {
  return -x | 0;
})), {
  hd: -1,
  tl: /* [] */0
});

function add(a, b) {
  return a + b | 0;
}

let length_10_id = Belt_List.makeBy(10, id);

let length_8_id = Belt_List.makeBy(8, id);

function $eq$tilde$9(extra, extra$1) {
  return eq("MAP2", extra, extra$1);
}

let d = Belt_List.makeBy(10, (function (x) {
  return (x << 1);
}));

function map2_add(x, y) {
  return Belt_List.zipBy(x, y, add);
}

$eq$tilde$9(map2_add(length_10_id, length_10_id), d);

$eq$tilde$9(map2_add(/* [] */0, {
  hd: 1,
  tl: /* [] */0
}), /* [] */0);

$eq$tilde$9(map2_add({
  hd: 1,
  tl: /* [] */0
}, /* [] */0), /* [] */0);

$eq$tilde$9(map2_add(/* [] */0, /* [] */0), /* [] */0);

$eq$tilde$9(map2_add(length_10_id, length_10_id), Belt_List.concat(Belt_List.map(length_8_id, (function (x) {
  return (x << 1);
})), {
  hd: 16,
  tl: {
    hd: 18,
    tl: /* [] */0
  }
}));

$eq$tilde$9(map2_add(length_10_id, length_8_id), Belt_List.mapWithIndex(length_8_id, (function (i, x) {
  return i + x | 0;
})));

$eq$tilde$9(Belt_List.reverse(Belt_List.mapReverse2(length_10_id, length_10_id, add)), Belt_List.map(length_10_id, (function (x) {
  return (x << 1);
})));

let xs = Belt_List.reverse(Belt_List.mapReverse2(length_8_id, length_10_id, add));

eq("File \"bs_list_test.res\", line 163, characters 5-12", Belt_List.length(xs), 8);

$eq$tilde$9(xs, Belt_List.zipBy(length_10_id, length_8_id, add));

$eq$tilde$9(Belt_List.mapReverse2({
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
    tl: /* [] */0
  }
}, (function (x, y) {
  return x + y | 0;
})), {
  hd: 4,
  tl: {
    hd: 2,
    tl: /* [] */0
  }
});

function $eq$tilde$10(extra, extra$1) {
  return eq("TAKE", extra, extra$1);
}

$eq$tilde$10(Belt_List.take({
  hd: 1,
  tl: {
    hd: 2,
    tl: {
      hd: 3,
      tl: /* [] */0
    }
  }
}, 2), {
  hd: 1,
  tl: {
    hd: 2,
    tl: /* [] */0
  }
});

$eq$tilde$10(Belt_List.take(/* [] */0, 1), undefined);

$eq$tilde$10(Belt_List.take({
  hd: 1,
  tl: {
    hd: 2,
    tl: /* [] */0
  }
}, 3), undefined);

$eq$tilde$10(Belt_List.take({
  hd: 1,
  tl: {
    hd: 2,
    tl: /* [] */0
  }
}, 2), {
  hd: 1,
  tl: {
    hd: 2,
    tl: /* [] */0
  }
});

$eq$tilde$10(Belt_List.take(length_10_id, 8), length_8_id);

$eq$tilde$10(Belt_List.take(length_10_id, 0), /* [] */0);

$eq$tilde$10(Belt_List.take(length_8_id, -2), undefined);

function $eq$tilde$11(extra, extra$1) {
  return eq("DROP", extra, extra$1);
}

$eq$tilde$11(Belt_List.drop(length_10_id, 10), /* [] */0);

$eq$tilde$11(Belt_List.drop(length_10_id, 8), {
  hd: 8,
  tl: {
    hd: 9,
    tl: /* [] */0
  }
});

$eq$tilde$11(Belt_List.drop(length_10_id, 0), length_10_id);

$eq$tilde$11(Belt_List.drop(length_8_id, -1), undefined);

function $eq$tilde$12(extra, extra$1) {
  return eq("SPLIT", extra, extra$1);
}

let a = Belt_List.makeBy(5, id);

$eq$tilde$12(Belt_List.splitAt(/* [] */0, 1), undefined);

$eq$tilde$12(Belt_List.splitAt(a, 6), undefined);

$eq$tilde$12(Belt_List.splitAt(a, 5), [
  a,
  /* [] */0
]);

$eq$tilde$12(Belt_List.splitAt(a, 4), [
  {
    hd: 0,
    tl: {
      hd: 1,
      tl: {
        hd: 2,
        tl: {
          hd: 3,
          tl: /* [] */0
        }
      }
    }
  },
  {
    hd: 4,
    tl: /* [] */0
  }
]);

$eq$tilde$12(Belt_List.splitAt(a, 3), [
  {
    hd: 0,
    tl: {
      hd: 1,
      tl: {
        hd: 2,
        tl: /* [] */0
      }
    }
  },
  {
    hd: 3,
    tl: {
      hd: 4,
      tl: /* [] */0
    }
  }
]);

$eq$tilde$12(Belt_List.splitAt(a, 2), [
  {
    hd: 0,
    tl: {
      hd: 1,
      tl: /* [] */0
    }
  },
  {
    hd: 2,
    tl: {
      hd: 3,
      tl: {
        hd: 4,
        tl: /* [] */0
      }
    }
  }
]);

$eq$tilde$12(Belt_List.splitAt(a, 1), [
  {
    hd: 0,
    tl: /* [] */0
  },
  {
    hd: 1,
    tl: {
      hd: 2,
      tl: {
        hd: 3,
        tl: {
          hd: 4,
          tl: /* [] */0
        }
      }
    }
  }
]);

$eq$tilde$12(Belt_List.splitAt(a, 0), [
  /* [] */0,
  a
]);

$eq$tilde$12(Belt_List.splitAt(a, -1), undefined);

function succx(x) {
  return x + 1 | 0;
}

function $eq$tilde$13(extra, extra$1) {
  return eq("REMOVEASSOQ", extra, extra$1);
}

function eqx(x, y) {
  return x === y;
}

b("File \"bs_list_test.res\", line 205, characters 4-11", Belt_List.hasAssoc({
  hd: [
    1,
    "1"
  ],
  tl: {
    hd: [
      2,
      "2"
    ],
    tl: {
      hd: [
        3,
        "3"
      ],
      tl: /* [] */0
    }
  }
}, 2, (function (prim0, prim1) {
  return prim0 === prim1;
})));

b("File \"bs_list_test.res\", line 206, characters 4-11", !Belt_List.hasAssoc({
  hd: [
    1,
    "1"
  ],
  tl: {
    hd: [
      2,
      "2"
    ],
    tl: {
      hd: [
        3,
        "3"
      ],
      tl: /* [] */0
    }
  }
}, 4, (function (prim0, prim1) {
  return prim0 === prim1;
})));

b("File \"bs_list_test.res\", line 207, characters 4-11", Belt_List.hasAssoc({
  hd: [
    1,
    "1"
  ],
  tl: {
    hd: [
      2,
      "2"
    ],
    tl: {
      hd: [
        3,
        "3"
      ],
      tl: /* [] */0
    }
  }
}, 4, (function (x, y) {
  return (x + 1 | 0) === y;
})));

$eq$tilde$13(Belt_List.removeAssoc({
  hd: [
    1,
    "1"
  ],
  tl: {
    hd: [
      2,
      "2"
    ],
    tl: {
      hd: [
        3,
        "3"
      ],
      tl: /* [] */0
    }
  }
}, 3, (function (prim0, prim1) {
  return prim0 === prim1;
})), {
  hd: [
    1,
    "1"
  ],
  tl: {
    hd: [
      2,
      "2"
    ],
    tl: /* [] */0
  }
});

$eq$tilde$13(Belt_List.removeAssoc({
  hd: [
    1,
    "1"
  ],
  tl: {
    hd: [
      2,
      "2"
    ],
    tl: {
      hd: [
        3,
        "3"
      ],
      tl: /* [] */0
    }
  }
}, 1, (function (prim0, prim1) {
  return prim0 === prim1;
})), {
  hd: [
    2,
    "2"
  ],
  tl: {
    hd: [
      3,
      "3"
    ],
    tl: /* [] */0
  }
});

$eq$tilde$13(Belt_List.removeAssoc({
  hd: [
    1,
    "1"
  ],
  tl: {
    hd: [
      2,
      "2"
    ],
    tl: {
      hd: [
        3,
        "3"
      ],
      tl: /* [] */0
    }
  }
}, 2, (function (prim0, prim1) {
  return prim0 === prim1;
})), {
  hd: [
    1,
    "1"
  ],
  tl: {
    hd: [
      3,
      "3"
    ],
    tl: /* [] */0
  }
});

$eq$tilde$13(Belt_List.removeAssoc({
  hd: [
    1,
    "1"
  ],
  tl: {
    hd: [
      2,
      "2"
    ],
    tl: {
      hd: [
        3,
        "3"
      ],
      tl: /* [] */0
    }
  }
}, 0, (function (prim0, prim1) {
  return prim0 === prim1;
})), {
  hd: [
    1,
    "1"
  ],
  tl: {
    hd: [
      2,
      "2"
    ],
    tl: {
      hd: [
        3,
        "3"
      ],
      tl: /* [] */0
    }
  }
});

$eq$tilde$13(Belt_List.removeAssoc({
  hd: [
    1,
    "1"
  ],
  tl: {
    hd: [
      2,
      "2"
    ],
    tl: {
      hd: [
        3,
        "3"
      ],
      tl: /* [] */0
    }
  }
}, 3, eqx), {
  hd: [
    1,
    "1"
  ],
  tl: {
    hd: [
      2,
      "2"
    ],
    tl: /* [] */0
  }
});

$eq$tilde$13(Belt_List.removeAssoc({
  hd: [
    1,
    "1"
  ],
  tl: {
    hd: [
      2,
      "2"
    ],
    tl: {
      hd: [
        3,
        "3"
      ],
      tl: /* [] */0
    }
  }
}, 1, eqx), {
  hd: [
    2,
    "2"
  ],
  tl: {
    hd: [
      3,
      "3"
    ],
    tl: /* [] */0
  }
});

$eq$tilde$13(Belt_List.removeAssoc({
  hd: [
    1,
    "1"
  ],
  tl: {
    hd: [
      2,
      "2"
    ],
    tl: {
      hd: [
        3,
        "3"
      ],
      tl: /* [] */0
    }
  }
}, 2, eqx), {
  hd: [
    1,
    "1"
  ],
  tl: {
    hd: [
      3,
      "3"
    ],
    tl: /* [] */0
  }
});

$eq$tilde$13(Belt_List.removeAssoc(/* [] */0, 2, eqx), /* [] */0);

let ll = {
  hd: [
    1,
    "1"
  ],
  tl: {
    hd: [
      2,
      "2"
    ],
    tl: {
      hd: [
        3,
        "3"
      ],
      tl: /* [] */0
    }
  }
};

let ll0 = Belt_List.removeAssoc(ll, 0, eqx);

b("File \"bs_list_test.res\", line 222, characters 4-11", ll === ll0);

let ll1 = Belt_List.setAssoc(ll, 2, "22", (function (prim0, prim1) {
  return prim0 === prim1;
}));

eq("File \"bs_list_test.res\", line 224, characters 5-12", ll1, {
  hd: [
    1,
    "1"
  ],
  tl: {
    hd: [
      2,
      "22"
    ],
    tl: {
      hd: [
        3,
        "3"
      ],
      tl: /* [] */0
    }
  }
});

let ll2 = Belt_List.setAssoc(ll1, 22, "2", (function (prim0, prim1) {
  return prim0 === prim1;
}));

b("File \"bs_list_test.res\", line 226, characters 4-11", Caml_obj.equal(ll2, {
  hd: [
    22,
    "2"
  ],
  tl: ll1
}));

b("File \"bs_list_test.res\", line 227, characters 4-11", Belt_List.tailExn(ll2) === ll1);

b("File \"bs_list_test.res\", line 229, characters 4-11", Caml_obj.equal(Belt_List.setAssoc({
  hd: [
    1,
    "a"
  ],
  tl: {
    hd: [
      2,
      "b"
    ],
    tl: {
      hd: [
        3,
        "c"
      ],
      tl: /* [] */0
    }
  }
}, 2, "x", (function (prim0, prim1) {
  return prim0 === prim1;
})), {
  hd: [
    1,
    "a"
  ],
  tl: {
    hd: [
      2,
      "x"
    ],
    tl: {
      hd: [
        3,
        "c"
      ],
      tl: /* [] */0
    }
  }
}));

b("File \"bs_list_test.res\", line 234, characters 4-11", Caml_obj.equal(Belt_List.setAssoc({
  hd: [
    1,
    "a"
  ],
  tl: {
    hd: [
      3,
      "c"
    ],
    tl: /* [] */0
  }
}, 2, "2", (function (prim0, prim1) {
  return prim0 === prim1;
})), {
  hd: [
    2,
    "2"
  ],
  tl: {
    hd: [
      1,
      "a"
    ],
    tl: {
      hd: [
        3,
        "c"
      ],
      tl: /* [] */0
    }
  }
}));

eq("File \"bs_list_test.res\", line 237, characters 5-12", Belt_List.setAssoc(/* [] */0, 1, "1", (function (prim0, prim1) {
  return prim0 === prim1;
})), {
  hd: [
    1,
    "1"
  ],
  tl: /* [] */0
});

debugger;

eq("File \"bs_list_test.res\", line 239, characters 5-12", Belt_List.setAssoc({
  hd: [
    1,
    "2"
  ],
  tl: /* [] */0
}, 1, "1", (function (prim0, prim1) {
  return prim0 === prim1;
})), {
  hd: [
    1,
    "1"
  ],
  tl: /* [] */0
});

eq("File \"bs_list_test.res\", line 241, characters 5-12", Belt_List.setAssoc({
  hd: [
    0,
    "0"
  ],
  tl: {
    hd: [
      1,
      "2"
    ],
    tl: /* [] */0
  }
}, 1, "1", (function (prim0, prim1) {
  return prim0 === prim1;
})), {
  hd: [
    0,
    "0"
  ],
  tl: {
    hd: [
      1,
      "1"
    ],
    tl: /* [] */0
  }
});

b("File \"bs_list_test.res\", line 242, characters 4-11", Caml_obj.equal(Belt_List.getAssoc({
  hd: [
    1,
    "a"
  ],
  tl: {
    hd: [
      2,
      "b"
    ],
    tl: {
      hd: [
        3,
        "c"
      ],
      tl: /* [] */0
    }
  }
}, 2, (function (prim0, prim1) {
  return prim0 === prim1;
})), "b"));

b("File \"bs_list_test.res\", line 243, characters 4-11", Belt_List.getAssoc({
  hd: [
    1,
    "a"
  ],
  tl: {
    hd: [
      2,
      "b"
    ],
    tl: {
      hd: [
        3,
        "c"
      ],
      tl: /* [] */0
    }
  }
}, 4, (function (prim0, prim1) {
  return prim0 === prim1;
})) === undefined);

eq("File \"bs_list_test.res\", line 248, characters 4-11", [
  Belt_List.head(length_10_id),
  Belt_List.tail(length_10_id)
], [
  0,
  Belt_List.drop(length_10_id, 1)
]);

eq("File \"bs_list_test.res\", line 255, characters 5-12", Belt_List.head(/* [] */0), undefined);

$$throw("File \"bs_list_test.res\", line 256, characters 8-15", (function () {
  Belt_List.headExn(/* [] */0);
}));

$$throw("File \"bs_list_test.res\", line 257, characters 8-15", (function () {
  Belt_List.tailExn(/* [] */0);
}));

$$throw("File \"bs_list_test.res\", line 258, characters 8-15", (function () {
  Belt_List.getExn({
    hd: 0,
    tl: {
      hd: 1,
      tl: /* [] */0
    }
  }, -1);
}));

$$throw("File \"bs_list_test.res\", line 259, characters 8-15", (function () {
  Belt_List.getExn({
    hd: 0,
    tl: {
      hd: 1,
      tl: /* [] */0
    }
  }, 2);
}));

eq("File \"bs_list_test.res\", line 260, characters 5-12", Belt_List.map({
  hd: 0,
  tl: {
    hd: 1,
    tl: /* [] */0
  }
}, (function (i) {
  return Belt_List.getExn({
    hd: 0,
    tl: {
      hd: 1,
      tl: /* [] */0
    }
  }, i);
})), {
  hd: 0,
  tl: {
    hd: 1,
    tl: /* [] */0
  }
});

eq("File \"bs_list_test.res\", line 261, characters 5-12", Belt_List.headExn({
  hd: 1,
  tl: /* [] */0
}), 1);

eq("File \"bs_list_test.res\", line 262, characters 5-12", Belt_List.tailExn({
  hd: 1,
  tl: /* [] */0
}), /* [] */0);

Belt_List.forEachWithIndex(length_10_id, (function (i, x) {
  eq("File \"bs_list_test.res\", line 263, characters 48-55", Belt_List.get(length_10_id, i), x);
}));

eq("File \"bs_list_test.res\", line 264, characters 5-12", Belt_List.tail(/* [] */0), undefined);

eq("File \"bs_list_test.res\", line 265, characters 5-12", Belt_List.drop(/* [] */0, 3), undefined);

eq("File \"bs_list_test.res\", line 266, characters 5-12", Belt_List.mapWithIndex(/* [] */0, (function (i, x) {
  return i + x | 0;
})), /* [] */0);

eq("File \"bs_list_test.res\", line 267, characters 5-12", Belt_List.get(length_10_id, -1), undefined);

eq("File \"bs_list_test.res\", line 268, characters 5-12", Belt_List.get(length_10_id, 12), undefined);

eq("File \"bs_list_test.res\", line 269, characters 5-12", sum(/* [] */0), 0);

eq("File \"bs_list_test.res\", line 270, characters 5-12", sum(length_10_id), 45);

eq("File \"bs_list_test.res\", line 271, characters 5-12", Belt_List.makeBy(0, id), /* [] */0);

eq("File \"bs_list_test.res\", line 273, characters 4-11", Belt_List.reverse(Belt_List.reverse(length_10_id)), length_10_id);

eq("File \"bs_list_test.res\", line 281, characters 4-11", Belt_List.reverse(Belt_List.reverse(length_8_id)), length_8_id);

eq("File \"bs_list_test.res\", line 288, characters 5-12", Belt_List.reverse(/* [] */0), /* [] */0);

eq("File \"bs_list_test.res\", line 289, characters 5-12", Belt_List.reverse(Belt_List.mapReverse(length_10_id, succx)), Belt_List.map(length_10_id, succx));

eq("File \"bs_list_test.res\", line 290, characters 5-12", Belt_List.reduce(length_10_id, 0, add), 45);

eq("File \"bs_list_test.res\", line 291, characters 5-12", Belt_List.reduceReverse(length_10_id, 0, add), 45);

eq("File \"bs_list_test.res\", line 292, characters 5-12", Belt_List.reduceReverse(Belt_List.makeBy(10000, (function (i) {
  return i;
})), 0, (function (prim0, prim1) {
  return prim0 + prim1 | 0;
})), 49995000);

eq("File \"bs_list_test.res\", line 295, characters 5-12", sum2(length_10_id, length_10_id), 90);

eq("File \"bs_list_test.res\", line 296, characters 5-12", sum2(length_8_id, length_10_id), 56);

eq("File \"bs_list_test.res\", line 297, characters 5-12", sum2(length_10_id, length_8_id), 56);

eq("File \"bs_list_test.res\", line 298, characters 5-12", Belt_List.reduce2(length_10_id, length_8_id, 0, (function (acc, x, y) {
  return (acc + x | 0) + y | 0;
})), 56);

eq("File \"bs_list_test.res\", line 299, characters 5-12", Belt_List.reduce2({
  hd: 1,
  tl: {
    hd: 2,
    tl: {
      hd: 3,
      tl: /* [] */0
    }
  }
}, {
  hd: 2,
  tl: {
    hd: 4,
    tl: {
      hd: 6,
      tl: /* [] */0
    }
  }
}, 0, (function (a, b, c) {
  return (a + b | 0) + c | 0;
})), 18);

eq("File \"bs_list_test.res\", line 300, characters 5-12", Belt_List.reduceReverse2(length_10_id, length_8_id, 0, (function (acc, x, y) {
  return (acc + x | 0) + y | 0;
})), 56);

eq("File \"bs_list_test.res\", line 301, characters 5-12", Belt_List.reduceReverse2(length_10_id, length_10_id, 0, (function (acc, x, y) {
  return (acc + x | 0) + y | 0;
})), 90);

eq("File \"bs_list_test.res\", line 302, characters 5-12", Belt_List.reduceReverse2({
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
    tl: /* [] */0
  }
}, 0, (function (acc, x, y) {
  return (acc + x | 0) + y | 0;
})), 6);

eq("File \"bs_list_test.res\", line 303, characters 5-12", Belt_List.every({
  hd: 2,
  tl: {
    hd: 4,
    tl: {
      hd: 6,
      tl: /* [] */0
    }
  }
}, mod2), true);

eq("File \"bs_list_test.res\", line 304, characters 5-12", Belt_List.every({
  hd: 1,
  tl: /* [] */0
}, mod2), false);

eq("File \"bs_list_test.res\", line 305, characters 5-12", Belt_List.every(/* [] */0, mod2), true);

eq("File \"bs_list_test.res\", line 306, characters 5-12", Belt_List.some({
  hd: 1,
  tl: {
    hd: 2,
    tl: {
      hd: 5,
      tl: /* [] */0
    }
  }
}, mod2), true);

eq("File \"bs_list_test.res\", line 307, characters 5-12", Belt_List.some({
  hd: 1,
  tl: {
    hd: 3,
    tl: {
      hd: 5,
      tl: /* [] */0
    }
  }
}, mod2), false);

eq("File \"bs_list_test.res\", line 308, characters 5-12", Belt_List.some(/* [] */0, mod2), false);

eq("File \"bs_list_test.res\", line 309, characters 5-12", Belt_List.has({
  hd: 1,
  tl: {
    hd: 2,
    tl: {
      hd: 3,
      tl: /* [] */0
    }
  }
}, "2", (function (x, s) {
  return String(x) === s;
})), true);

eq("File \"bs_list_test.res\", line 310, characters 5-12", Belt_List.has({
  hd: 1,
  tl: {
    hd: 2,
    tl: {
      hd: 3,
      tl: /* [] */0
    }
  }
}, "0", (function (x, s) {
  return String(x) === s;
})), false);

b("File \"bs_list_test.res\", line 312, characters 4-11", Belt_List.reduceReverse({
  hd: 1,
  tl: {
    hd: 2,
    tl: {
      hd: 3,
      tl: {
        hd: 4,
        tl: /* [] */0
      }
    }
  }
}, 0, (function (prim0, prim1) {
  return prim0 + prim1 | 0;
})) === 10);

b("File \"bs_list_test.res\", line 313, characters 4-11", Belt_List.reduceReverse({
  hd: 1,
  tl: {
    hd: 2,
    tl: {
      hd: 3,
      tl: {
        hd: 4,
        tl: /* [] */0
      }
    }
  }
}, 10, (function (prim0, prim1) {
  return prim0 - prim1 | 0;
})) === 0);

b("File \"bs_list_test.res\", line 314, characters 4-11", Caml_obj.equal(Belt_List.reduceReverse({
  hd: 1,
  tl: {
    hd: 2,
    tl: {
      hd: 3,
      tl: {
        hd: 4,
        tl: /* [] */0
      }
    }
  }
}, /* [] */0, Belt_List.add), {
  hd: 1,
  tl: {
    hd: 2,
    tl: {
      hd: 3,
      tl: {
        hd: 4,
        tl: /* [] */0
      }
    }
  }
}));

b("File \"bs_list_test.res\", line 315, characters 4-11", Belt_List.reduce({
  hd: 1,
  tl: {
    hd: 2,
    tl: {
      hd: 3,
      tl: {
        hd: 4,
        tl: /* [] */0
      }
    }
  }
}, 0, (function (prim0, prim1) {
  return prim0 + prim1 | 0;
})) === 10);

b("File \"bs_list_test.res\", line 316, characters 4-11", Belt_List.reduce({
  hd: 1,
  tl: {
    hd: 2,
    tl: {
      hd: 3,
      tl: {
        hd: 4,
        tl: /* [] */0
      }
    }
  }
}, 10, (function (prim0, prim1) {
  return prim0 - prim1 | 0;
})) === 0);

b("File \"bs_list_test.res\", line 317, characters 4-11", Caml_obj.equal(Belt_List.reduce({
  hd: 1,
  tl: {
    hd: 2,
    tl: {
      hd: 3,
      tl: {
        hd: 4,
        tl: /* [] */0
      }
    }
  }
}, /* [] */0, Belt_List.add), {
  hd: 4,
  tl: {
    hd: 3,
    tl: {
      hd: 2,
      tl: {
        hd: 1,
        tl: /* [] */0
      }
    }
  }
}));

b("File \"bs_list_test.res\", line 318, characters 4-11", Belt_List.reduceWithIndex({
  hd: 1,
  tl: {
    hd: 2,
    tl: {
      hd: 3,
      tl: {
        hd: 4,
        tl: /* [] */0
      }
    }
  }
}, 0, (function (acc, x, i) {
  return (acc + x | 0) + i | 0;
})) === 16);

b("File \"bs_list_test.res\", line 319, characters 4-11", Belt_List.reduceReverse2({
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
    tl: /* [] */0
  }
}, 0, (function (acc, x, y) {
  return (acc + x | 0) + y | 0;
})) === 6);

let a$1 = Belt_List.makeBy(10000, (function (i) {
  return i;
}));

b("File \"bs_list_test.res\", line 322, characters 4-11", Belt_List.reduceReverse2(a$1, {
  hd: 0,
  tl: a$1
}, 0, (function (acc, x, y) {
  return (acc + x | 0) + y | 0;
})) === 99980001);

eq("File \"bs_list_test.res\", line 328, characters 5-12", Belt_List.every2(/* [] */0, {
  hd: 1,
  tl: /* [] */0
}, (function (x, y) {
  return x > y;
})), true);

eq("File \"bs_list_test.res\", line 329, characters 5-12", Belt_List.every2({
  hd: 2,
  tl: {
    hd: 3,
    tl: /* [] */0
  }
}, {
  hd: 1,
  tl: /* [] */0
}, (function (x, y) {
  return x > y;
})), true);

eq("File \"bs_list_test.res\", line 330, characters 5-12", Belt_List.every2({
  hd: 2,
  tl: /* [] */0
}, {
  hd: 1,
  tl: /* [] */0
}, (function (x, y) {
  return x > y;
})), true);

eq("File \"bs_list_test.res\", line 331, characters 5-12", Belt_List.every2({
  hd: 2,
  tl: {
    hd: 3,
    tl: /* [] */0
  }
}, {
  hd: 1,
  tl: {
    hd: 4,
    tl: /* [] */0
  }
}, (function (x, y) {
  return x > y;
})), false);

eq("File \"bs_list_test.res\", line 332, characters 5-12", Belt_List.every2({
  hd: 2,
  tl: {
    hd: 3,
    tl: /* [] */0
  }
}, {
  hd: 1,
  tl: {
    hd: 0,
    tl: /* [] */0
  }
}, (function (x, y) {
  return x > y;
})), true);

eq("File \"bs_list_test.res\", line 333, characters 5-12", Belt_List.some2(/* [] */0, {
  hd: 1,
  tl: /* [] */0
}, (function (x, y) {
  return x > y;
})), false);

eq("File \"bs_list_test.res\", line 334, characters 5-12", Belt_List.some2({
  hd: 2,
  tl: {
    hd: 3,
    tl: /* [] */0
  }
}, {
  hd: 1,
  tl: /* [] */0
}, (function (x, y) {
  return x > y;
})), true);

eq("File \"bs_list_test.res\", line 335, characters 5-12", Belt_List.some2({
  hd: 2,
  tl: {
    hd: 3,
    tl: /* [] */0
  }
}, {
  hd: 1,
  tl: {
    hd: 4,
    tl: /* [] */0
  }
}, (function (x, y) {
  return x > y;
})), true);

eq("File \"bs_list_test.res\", line 336, characters 5-12", Belt_List.some2({
  hd: 0,
  tl: {
    hd: 3,
    tl: /* [] */0
  }
}, {
  hd: 1,
  tl: {
    hd: 4,
    tl: /* [] */0
  }
}, (function (x, y) {
  return x > y;
})), false);

eq("File \"bs_list_test.res\", line 337, characters 5-12", Belt_List.some2({
  hd: 0,
  tl: {
    hd: 3,
    tl: /* [] */0
  }
}, {
  hd: 3,
  tl: {
    hd: 2,
    tl: /* [] */0
  }
}, (function (x, y) {
  return x > y;
})), true);

eq("File \"bs_list_test.res\", line 338, characters 5-12", Belt_List.some2({
  hd: 1,
  tl: {
    hd: 2,
    tl: {
      hd: 3,
      tl: /* [] */0
    }
  }
}, {
  hd: -1,
  tl: {
    hd: -2,
    tl: /* [] */0
  }
}, (function (x, y) {
  return x === y;
})), false);

function makeTest(n) {
  eq("File \"bs_list_test.res\", line 341, characters 23-30", Belt_List.make(n, 3), Belt_List.makeBy(n, (function (param) {
    return 3;
  })));
}

eq("File \"bs_list_test.res\", line 343, characters 12-19", {
  hd: 2,
  tl: {
    hd: 3,
    tl: /* [] */0
  }
}, {
  hd: 2,
  tl: {
    hd: 3,
    tl: /* [] */0
  }
});

b("File \"bs_list_test.res\", line 345, characters 4-11", Belt_List.cmp({
  hd: 1,
  tl: {
    hd: 2,
    tl: {
      hd: 3,
      tl: /* [] */0
    }
  }
}, {
  hd: 0,
  tl: {
    hd: 1,
    tl: {
      hd: 2,
      tl: {
        hd: 3,
        tl: /* [] */0
      }
    }
  }
}, Caml.int_compare) > 0);

b("File \"bs_list_test.res\", line 346, characters 4-11", Belt_List.cmp({
  hd: 1,
  tl: {
    hd: 2,
    tl: {
      hd: 3,
      tl: {
        hd: 4,
        tl: /* [] */0
      }
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
}, Caml.int_compare) > 0);

b("File \"bs_list_test.res\", line 347, characters 4-11", Belt_List.cmp({
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
      tl: {
        hd: 4,
        tl: /* [] */0
      }
    }
  }
}, Caml.int_compare) < 0);

b("File \"bs_list_test.res\", line 348, characters 4-11", Belt_List.cmp({
  hd: 1,
  tl: {
    hd: 2,
    tl: {
      hd: 3,
      tl: /* [] */0
    }
  }
}, {
  hd: 0,
  tl: {
    hd: 1,
    tl: {
      hd: 2,
      tl: /* [] */0
    }
  }
}, Caml.int_compare) > 0);

b("File \"bs_list_test.res\", line 349, characters 4-11", Belt_List.cmp({
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
}, Caml.int_compare) === 0);

b("File \"bs_list_test.res\", line 350, characters 4-11", Belt_List.cmp({
  hd: 1,
  tl: {
    hd: 2,
    tl: {
      hd: 4,
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
}, Caml.int_compare) > 0);

b("File \"bs_list_test.res\", line 351, characters 4-11", Belt_List.cmpByLength(/* [] */0, /* [] */0) === 0);

b("File \"bs_list_test.res\", line 352, characters 4-11", Belt_List.cmpByLength({
  hd: 1,
  tl: /* [] */0
}, /* [] */0) > 0);

b("File \"bs_list_test.res\", line 353, characters 4-11", Belt_List.cmpByLength(/* [] */0, {
  hd: 1,
  tl: /* [] */0
}) < 0);

b("File \"bs_list_test.res\", line 354, characters 4-11", Belt_List.cmpByLength({
  hd: 1,
  tl: {
    hd: 2,
    tl: /* [] */0
  }
}, {
  hd: 1,
  tl: /* [] */0
}) > 0);

b("File \"bs_list_test.res\", line 355, characters 4-11", Belt_List.cmpByLength({
  hd: 1,
  tl: /* [] */0
}, {
  hd: 1,
  tl: {
    hd: 2,
    tl: /* [] */0
  }
}) < 0);

b("File \"bs_list_test.res\", line 356, characters 4-11", Belt_List.cmpByLength({
  hd: 1,
  tl: {
    hd: 3,
    tl: /* [] */0
  }
}, {
  hd: 1,
  tl: {
    hd: 2,
    tl: /* [] */0
  }
}) === 0);

makeTest(0);

makeTest(1);

makeTest(2);

makeTest(3);

function $eq$tilde$14(extra, extra$1) {
  return eq("SORT", extra, extra$1);
}

function cmp(a, b) {
  return a - b | 0;
}

$eq$tilde$14(Belt_List.sort({
  hd: 5,
  tl: {
    hd: 4,
    tl: {
      hd: 3,
      tl: {
        hd: 2,
        tl: /* [] */0
      }
    }
  }
}, cmp), {
  hd: 2,
  tl: {
    hd: 3,
    tl: {
      hd: 4,
      tl: {
        hd: 5,
        tl: /* [] */0
      }
    }
  }
});

$eq$tilde$14(Belt_List.sort({
  hd: 3,
  tl: {
    hd: 9,
    tl: {
      hd: 37,
      tl: {
        hd: 3,
        tl: {
          hd: 1,
          tl: /* [] */0
        }
      }
    }
  }
}, cmp), {
  hd: 1,
  tl: {
    hd: 3,
    tl: {
      hd: 3,
      tl: {
        hd: 9,
        tl: {
          hd: 37,
          tl: /* [] */0
        }
      }
    }
  }
});

b("File \"bs_list_test.res\", line 374, characters 4-11", !Belt_List.eq({
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
    tl: /* [] */0
  }
}, (function (x, y) {
  return x === y;
})));

b("File \"bs_list_test.res\", line 375, characters 4-11", Belt_List.eq({
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
}, (function (x, y) {
  return x === y;
})));

b("File \"bs_list_test.res\", line 376, characters 4-11", !Belt_List.eq({
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
      hd: 4,
      tl: /* [] */0
    }
  }
}, (function (x, y) {
  return x === y;
})));

b("File \"bs_list_test.res\", line 377, characters 4-11", !Belt_List.eq({
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
      tl: {
        hd: 4,
        tl: /* [] */0
      }
    }
  }
}, (function (prim0, prim1) {
  return prim0 === prim1;
})));

let u0 = Belt_List.makeBy(20, (function (x) {
  return x;
}));

let u1 = Belt_List.keepMap(u0, (function (x) {
  if (x % 7 === 0) {
    return x + 1 | 0;
  }
  
}));

eq("File \"bs_list_test.res\", line 388, characters 5-12", u1, {
  hd: 1,
  tl: {
    hd: 8,
    tl: {
      hd: 15,
      tl: /* [] */0
    }
  }
});

b("File \"bs_list_test.res\", line 390, characters 4-11", Caml_obj.equal(Belt_List.keepMap({
  hd: 1,
  tl: {
    hd: 2,
    tl: {
      hd: 3,
      tl: {
        hd: 4,
        tl: /* [] */0
      }
    }
  }
}, (function (x) {
  if (x % 2 === 0) {
    return -x | 0;
  }
  
})), {
  hd: -2,
  tl: {
    hd: -4,
    tl: /* [] */0
  }
}));

b("File \"bs_list_test.res\", line 404, characters 4-11", Belt_List.keepMap({
  hd: 1,
  tl: {
    hd: 2,
    tl: {
      hd: 3,
      tl: {
        hd: 4,
        tl: /* [] */0
      }
    }
  }
}, (function (x) {
  if (x % 5 === 0) {
    return x;
  }
  
})) === /* [] */0);

Mt.from_pair_suites("Bs_list_test", suites.contents);

let N;

let A;

let J;

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.b = b;
exports.$$throw = $$throw;
exports.N = N;
exports.A = A;
exports.J = J;
exports.sum = sum;
exports.sum2 = sum2;
exports.mod2 = mod2;
exports.evenIndex = evenIndex;
exports.id = id;
exports.add = add;
exports.length_10_id = length_10_id;
exports.length_8_id = length_8_id;
exports.succx = succx;
exports.makeTest = makeTest;
/* u Not a pure module */
