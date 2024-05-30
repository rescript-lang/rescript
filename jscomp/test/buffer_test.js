// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

let Mt = require("./mt.js");
let Bytes = require("../../lib/js/bytes.js");
let $$Buffer = require("../../lib/js/buffer.js");
let Caml_bytes = require("../../lib/js/caml_bytes.js");
let Caml_string = require("../../lib/js/caml_string.js");

let v = "gso";

let suites_0 = [
  "equal",
  (function (param) {
    return {
      TAG: "Eq",
      _0: [
        Caml_bytes.get(Bytes.make(3, /* 'a' */97), 0),
        Bytes.make(3, /* 'a' */97)[0]
      ],
      _1: [
        /* 'a' */97,
        /* 'a' */97
      ]
    };
  })
];

let suites_1 = {
  hd: [
    "equal2",
    (function (param) {
      let u = Bytes.make(3, /* 'a' */97);
      u[0] = /* 'b' */98;
      return {
        TAG: "Eq",
        _0: [
          u[0],
          Caml_string.get(v, 0)
        ],
        _1: [
          /* 'b' */98,
          /* 'g' */103
        ]
      };
    })
  ],
  tl: {
    hd: [
      "buffer",
      (function (param) {
        let v = $$Buffer.create(30);
        for(let i = 0; i <= 10; ++i){
          $$Buffer.add_string(v, String(i));
        }
        return {
          TAG: "Eq",
          _0: $$Buffer.contents(v),
          _1: "012345678910"
        };
      })
    ],
    tl: /* [] */0
  }
};

let suites = {
  hd: suites_0,
  tl: suites_1
};

Mt.from_pair_suites("Buffer_test", suites);

exports.v = v;
exports.suites = suites;
/*  Not a pure module */
