'use strict';

var Mt = require("./mt.js");
var Caml = require("../../lib/js/caml.js");
var List = require("../../lib/js/list.js");

function height(param) {
  if (typeof param === "string") {
    return 0;
  } else {
    return param._4;
  }
}

function create(l, x, d, r) {
  var hl = height(l);
  var hr = height(r);
  return /* Node */{
          _0: l,
          _1: x,
          _2: d,
          _3: r,
          _4: hl >= hr ? hl + 1 | 0 : hr + 1 | 0
        };
}

function bal(l, x, d, r) {
  var hl;
  hl = typeof l === "string" ? 0 : l._4;
  var hr;
  hr = typeof r === "string" ? 0 : r._4;
  if (hl > (hr + 2 | 0)) {
    if (typeof l === "string") {
      throw {
            RE_EXN_ID: "Invalid_argument",
            _1: "Map.bal",
            Error: new Error()
          };
    }
    var lr = l._3;
    var ld = l._2;
    var lv = l._1;
    var ll = l._0;
    if (height(ll) >= height(lr)) {
      return create(ll, lv, ld, create(lr, x, d, r));
    }
    if (typeof lr !== "string") {
      return create(create(ll, lv, ld, lr._0), lr._1, lr._2, create(lr._3, x, d, r));
    }
    throw {
          RE_EXN_ID: "Invalid_argument",
          _1: "Map.bal",
          Error: new Error()
        };
  }
  if (hr <= (hl + 2 | 0)) {
    return /* Node */{
            _0: l,
            _1: x,
            _2: d,
            _3: r,
            _4: hl >= hr ? hl + 1 | 0 : hr + 1 | 0
          };
  }
  if (typeof r === "string") {
    throw {
          RE_EXN_ID: "Invalid_argument",
          _1: "Map.bal",
          Error: new Error()
        };
  }
  var rr = r._3;
  var rd = r._2;
  var rv = r._1;
  var rl = r._0;
  if (height(rr) >= height(rl)) {
    return create(create(l, x, d, rl), rv, rd, rr);
  }
  if (typeof rl !== "string") {
    return create(create(l, x, d, rl._0), rl._1, rl._2, create(rl._3, rv, rd, rr));
  }
  throw {
        RE_EXN_ID: "Invalid_argument",
        _1: "Map.bal",
        Error: new Error()
      };
}

function add(x, data, param) {
  if (typeof param === "string") {
    return /* Node */{
            _0: "Empty",
            _1: x,
            _2: data,
            _3: "Empty",
            _4: 1
          };
  }
  var r = param._3;
  var d = param._2;
  var v = param._1;
  var l = param._0;
  var c = Caml.int_compare(x, v);
  if (c === 0) {
    return /* Node */{
            _0: l,
            _1: x,
            _2: data,
            _3: r,
            _4: param._4
          };
  } else if (c < 0) {
    return bal(add(x, data, l), v, d, r);
  } else {
    return bal(l, v, d, add(x, data, r));
  }
}

function find(x, _param) {
  while(true) {
    var param = _param;
    if (typeof param === "string") {
      throw {
            RE_EXN_ID: "Not_found",
            Error: new Error()
          };
    }
    var c = Caml.int_compare(x, param._1);
    if (c === 0) {
      return param._2;
    }
    _param = c < 0 ? param._0 : param._3;
    continue ;
  };
}

var m = List.fold_left((function (acc, param) {
        return add(param[0], param[1], acc);
      }), "Empty", {
      hd: [
        10,
        /* 'a' */97
      ],
      tl: {
        hd: [
          3,
          /* 'b' */98
        ],
        tl: {
          hd: [
            7,
            /* 'c' */99
          ],
          tl: {
            hd: [
              20,
              /* 'd' */100
            ],
            tl: /* [] */0
          }
        }
      }
    });

Mt.from_pair_suites("Inline_map_test", {
      hd: [
        "find",
        (function (param) {
            return {
                    TAG: "Eq",
                    _0: find(10, m),
                    _1: /* 'a' */97
                  };
          })
      ],
      tl: /* [] */0
    });

/* m Not a pure module */
