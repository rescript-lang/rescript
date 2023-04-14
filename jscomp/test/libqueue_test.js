'use strict';

var List = require("../../lib/js/list.js");
var Curry = require("../../lib/js/curry.js");
var Queue = require("../../lib/js/queue.js");
var Caml_obj = require("../../lib/js/caml_obj.js");
var Caml_js_exceptions = require("../../lib/js/caml_js_exceptions.js");

function to_list(q) {
  return List.rev(Queue.fold((function (l, x) {
                    return {
                            hd: x,
                            tl: l
                          };
                  }), /* [] */0, q));
}

var Q = {
  Empty: Queue.Empty,
  create: Queue.create,
  add: Queue.add,
  push: Queue.push,
  take: Queue.take,
  pop: Queue.pop,
  peek: Queue.peek,
  top: Queue.top,
  clear: Queue.clear,
  copy: Queue.copy,
  is_empty: Queue.is_empty,
  length: Queue.length,
  iter: Queue.iter,
  fold: Queue.fold,
  transfer: Queue.transfer,
  to_list: to_list
};

function does_raise(f, q) {
  try {
    Curry._1(f, q);
    return false;
  }
  catch (raw_exn){
    var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
    if (exn.RE_EXN_ID === Queue.Empty) {
      return true;
    }
    throw exn;
  }
}

var q = {
  length: 0,
  first: "Nil",
  last: "Nil"
};

if (!(to_list(q) === /* [] */0 && q.length === 0)) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "libqueue_test.res",
          30,
          2
        ],
        Error: new Error()
      };
}

Queue.add(1, q);

if (!(Caml_obj.equal(to_list(q), {
          hd: 1,
          tl: /* [] */0
        }) && q.length === 1)) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "libqueue_test.res",
          32,
          2
        ],
        Error: new Error()
      };
}

Queue.add(2, q);

if (!(Caml_obj.equal(to_list(q), {
          hd: 1,
          tl: {
            hd: 2,
            tl: /* [] */0
          }
        }) && q.length === 2)) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "libqueue_test.res",
          34,
          2
        ],
        Error: new Error()
      };
}

Queue.add(3, q);

if (!(Caml_obj.equal(to_list(q), {
          hd: 1,
          tl: {
            hd: 2,
            tl: {
              hd: 3,
              tl: /* [] */0
            }
          }
        }) && q.length === 3)) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "libqueue_test.res",
          36,
          2
        ],
        Error: new Error()
      };
}

Queue.add(4, q);

if (!(Caml_obj.equal(to_list(q), {
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
        }) && q.length === 4)) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "libqueue_test.res",
          38,
          2
        ],
        Error: new Error()
      };
}

if (Queue.take(q) !== 1) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "libqueue_test.res",
          39,
          2
        ],
        Error: new Error()
      };
}

if (!(Caml_obj.equal(to_list(q), {
          hd: 2,
          tl: {
            hd: 3,
            tl: {
              hd: 4,
              tl: /* [] */0
            }
          }
        }) && q.length === 3)) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "libqueue_test.res",
          40,
          2
        ],
        Error: new Error()
      };
}

if (Queue.take(q) !== 2) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "libqueue_test.res",
          41,
          2
        ],
        Error: new Error()
      };
}

if (!(Caml_obj.equal(to_list(q), {
          hd: 3,
          tl: {
            hd: 4,
            tl: /* [] */0
          }
        }) && q.length === 2)) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "libqueue_test.res",
          42,
          2
        ],
        Error: new Error()
      };
}

if (Queue.take(q) !== 3) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "libqueue_test.res",
          43,
          2
        ],
        Error: new Error()
      };
}

if (!(Caml_obj.equal(to_list(q), {
          hd: 4,
          tl: /* [] */0
        }) && q.length === 1)) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "libqueue_test.res",
          44,
          2
        ],
        Error: new Error()
      };
}

if (Queue.take(q) !== 4) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "libqueue_test.res",
          45,
          2
        ],
        Error: new Error()
      };
}

if (!(to_list(q) === /* [] */0 && q.length === 0)) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "libqueue_test.res",
          46,
          2
        ],
        Error: new Error()
      };
}

if (!does_raise(Queue.take, q)) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "libqueue_test.res",
          47,
          2
        ],
        Error: new Error()
      };
}

var q$1 = {
  length: 0,
  first: "Nil",
  last: "Nil"
};

Queue.add(1, q$1);

if (Queue.take(q$1) !== 1) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "libqueue_test.res",
          53,
          2
        ],
        Error: new Error()
      };
}

if (!does_raise(Queue.take, q$1)) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "libqueue_test.res",
          54,
          2
        ],
        Error: new Error()
      };
}

Queue.add(2, q$1);

if (Queue.take(q$1) !== 2) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "libqueue_test.res",
          56,
          2
        ],
        Error: new Error()
      };
}

if (!does_raise(Queue.take, q$1)) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "libqueue_test.res",
          57,
          2
        ],
        Error: new Error()
      };
}

if (q$1.length !== 0) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "libqueue_test.res",
          58,
          2
        ],
        Error: new Error()
      };
}

var q$2 = {
  length: 0,
  first: "Nil",
  last: "Nil"
};

Queue.add(1, q$2);

if (Queue.peek(q$2) !== 1) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "libqueue_test.res",
          64,
          2
        ],
        Error: new Error()
      };
}

Queue.add(2, q$2);

if (Queue.peek(q$2) !== 1) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "libqueue_test.res",
          66,
          2
        ],
        Error: new Error()
      };
}

Queue.add(3, q$2);

if (Queue.peek(q$2) !== 1) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "libqueue_test.res",
          68,
          2
        ],
        Error: new Error()
      };
}

if (Queue.peek(q$2) !== 1) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "libqueue_test.res",
          69,
          2
        ],
        Error: new Error()
      };
}

if (Queue.take(q$2) !== 1) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "libqueue_test.res",
          70,
          2
        ],
        Error: new Error()
      };
}

if (Queue.peek(q$2) !== 2) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "libqueue_test.res",
          71,
          2
        ],
        Error: new Error()
      };
}

if (Queue.take(q$2) !== 2) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "libqueue_test.res",
          72,
          2
        ],
        Error: new Error()
      };
}

if (Queue.peek(q$2) !== 3) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "libqueue_test.res",
          73,
          2
        ],
        Error: new Error()
      };
}

if (Queue.take(q$2) !== 3) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "libqueue_test.res",
          74,
          2
        ],
        Error: new Error()
      };
}

if (!does_raise(Queue.peek, q$2)) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "libqueue_test.res",
          75,
          2
        ],
        Error: new Error()
      };
}

if (!does_raise(Queue.peek, q$2)) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "libqueue_test.res",
          76,
          2
        ],
        Error: new Error()
      };
}

var q$3 = {
  length: 0,
  first: "Nil",
  last: "Nil"
};

for(var i = 1; i <= 10; ++i){
  Queue.add(i, q$3);
}

Queue.clear(q$3);

if (q$3.length !== 0) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "libqueue_test.res",
          85,
          2
        ],
        Error: new Error()
      };
}

if (!does_raise(Queue.take, q$3)) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "libqueue_test.res",
          86,
          2
        ],
        Error: new Error()
      };
}

if (!Caml_obj.equal(q$3, {
        length: 0,
        first: "Nil",
        last: "Nil"
      })) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "libqueue_test.res",
          87,
          2
        ],
        Error: new Error()
      };
}

Queue.add(42, q$3);

if (Queue.take(q$3) !== 42) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "libqueue_test.res",
          89,
          2
        ],
        Error: new Error()
      };
}

var q1 = {
  length: 0,
  first: "Nil",
  last: "Nil"
};

for(var i$1 = 1; i$1 <= 10; ++i$1){
  Queue.add(i$1, q1);
}

var q2 = Queue.copy(q1);

if (!Caml_obj.equal(to_list(q1), {
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
                    tl: {
                      hd: 8,
                      tl: {
                        hd: 9,
                        tl: {
                          hd: 10,
                          tl: /* [] */0
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      })) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "libqueue_test.res",
          98,
          2
        ],
        Error: new Error()
      };
}

if (!Caml_obj.equal(to_list(q2), {
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
                    tl: {
                      hd: 8,
                      tl: {
                        hd: 9,
                        tl: {
                          hd: 10,
                          tl: /* [] */0
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      })) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "libqueue_test.res",
          99,
          2
        ],
        Error: new Error()
      };
}

if (q1.length !== 10) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "libqueue_test.res",
          100,
          2
        ],
        Error: new Error()
      };
}

if (q2.length !== 10) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "libqueue_test.res",
          101,
          2
        ],
        Error: new Error()
      };
}

for(var i$2 = 1; i$2 <= 10; ++i$2){
  if (Queue.take(q1) !== i$2) {
    throw {
          RE_EXN_ID: "Assert_failure",
          _1: [
            "libqueue_test.res",
            103,
            4
          ],
          Error: new Error()
        };
  }
  
}

for(var i$3 = 1; i$3 <= 10; ++i$3){
  if (Queue.take(q2) !== i$3) {
    throw {
          RE_EXN_ID: "Assert_failure",
          _1: [
            "libqueue_test.res",
            106,
            4
          ],
          Error: new Error()
        };
  }
  
}

var q$4 = {
  length: 0,
  first: "Nil",
  last: "Nil"
};

if (q$4.length !== 0) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "libqueue_test.res",
          112,
          2
        ],
        Error: new Error()
      };
}

for(var i$4 = 1; i$4 <= 10; ++i$4){
  Queue.add(i$4, q$4);
  if (q$4.length !== i$4) {
    throw {
          RE_EXN_ID: "Assert_failure",
          _1: [
            "libqueue_test.res",
            115,
            4
          ],
          Error: new Error()
        };
  }
  if (q$4.length === 0) {
    throw {
          RE_EXN_ID: "Assert_failure",
          _1: [
            "libqueue_test.res",
            116,
            4
          ],
          Error: new Error()
        };
  }
  
}

for(var i$5 = 10; i$5 >= 1; --i$5){
  if (q$4.length !== i$5) {
    throw {
          RE_EXN_ID: "Assert_failure",
          _1: [
            "libqueue_test.res",
            119,
            4
          ],
          Error: new Error()
        };
  }
  if (q$4.length === 0) {
    throw {
          RE_EXN_ID: "Assert_failure",
          _1: [
            "libqueue_test.res",
            120,
            4
          ],
          Error: new Error()
        };
  }
  Queue.take(q$4);
}

if (q$4.length !== 0) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "libqueue_test.res",
          123,
          2
        ],
        Error: new Error()
      };
}

if (q$4.length !== 0) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "libqueue_test.res",
          124,
          2
        ],
        Error: new Error()
      };
}

var q$5 = {
  length: 0,
  first: "Nil",
  last: "Nil"
};

for(var i$6 = 1; i$6 <= 10; ++i$6){
  Queue.add(i$6, q$5);
}

var i$7 = {
  contents: 1
};

Queue.iter((function (j) {
        if (i$7.contents !== j) {
          throw {
                RE_EXN_ID: "Assert_failure",
                _1: [
                  "libqueue_test.res",
                  134,
                  4
                ],
                Error: new Error()
              };
        }
        i$7.contents = i$7.contents + 1 | 0;
      }), q$5);

var q1$1 = {
  length: 0,
  first: "Nil",
  last: "Nil"
};

var q2$1 = {
  length: 0,
  first: "Nil",
  last: "Nil"
};

if (q1$1.length !== 0) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "libqueue_test.res",
          141,
          2
        ],
        Error: new Error()
      };
}

if (to_list(q1$1) !== /* [] */0) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "libqueue_test.res",
          142,
          2
        ],
        Error: new Error()
      };
}

if (q2$1.length !== 0) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "libqueue_test.res",
          143,
          2
        ],
        Error: new Error()
      };
}

if (to_list(q2$1) !== /* [] */0) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "libqueue_test.res",
          144,
          2
        ],
        Error: new Error()
      };
}

Queue.transfer(q1$1, q2$1);

if (q1$1.length !== 0) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "libqueue_test.res",
          146,
          2
        ],
        Error: new Error()
      };
}

if (to_list(q1$1) !== /* [] */0) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "libqueue_test.res",
          147,
          2
        ],
        Error: new Error()
      };
}

if (q2$1.length !== 0) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "libqueue_test.res",
          148,
          2
        ],
        Error: new Error()
      };
}

if (to_list(q2$1) !== /* [] */0) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "libqueue_test.res",
          149,
          2
        ],
        Error: new Error()
      };
}

var q1$2 = {
  length: 0,
  first: "Nil",
  last: "Nil"
};

var q2$2 = {
  length: 0,
  first: "Nil",
  last: "Nil"
};

for(var i$8 = 1; i$8 <= 4; ++i$8){
  Queue.add(i$8, q1$2);
}

if (q1$2.length !== 4) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "libqueue_test.res",
          157,
          2
        ],
        Error: new Error()
      };
}

if (!Caml_obj.equal(to_list(q1$2), {
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
      })) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "libqueue_test.res",
          158,
          2
        ],
        Error: new Error()
      };
}

if (q2$2.length !== 0) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "libqueue_test.res",
          159,
          2
        ],
        Error: new Error()
      };
}

if (to_list(q2$2) !== /* [] */0) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "libqueue_test.res",
          160,
          2
        ],
        Error: new Error()
      };
}

Queue.transfer(q1$2, q2$2);

if (q1$2.length !== 0) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "libqueue_test.res",
          162,
          2
        ],
        Error: new Error()
      };
}

if (to_list(q1$2) !== /* [] */0) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "libqueue_test.res",
          163,
          2
        ],
        Error: new Error()
      };
}

if (q2$2.length !== 4) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "libqueue_test.res",
          164,
          2
        ],
        Error: new Error()
      };
}

if (!Caml_obj.equal(to_list(q2$2), {
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
      })) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "libqueue_test.res",
          165,
          2
        ],
        Error: new Error()
      };
}

var q1$3 = {
  length: 0,
  first: "Nil",
  last: "Nil"
};

var q2$3 = {
  length: 0,
  first: "Nil",
  last: "Nil"
};

for(var i$9 = 5; i$9 <= 8; ++i$9){
  Queue.add(i$9, q2$3);
}

if (q1$3.length !== 0) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "libqueue_test.res",
          173,
          2
        ],
        Error: new Error()
      };
}

if (to_list(q1$3) !== /* [] */0) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "libqueue_test.res",
          174,
          2
        ],
        Error: new Error()
      };
}

if (q2$3.length !== 4) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "libqueue_test.res",
          175,
          2
        ],
        Error: new Error()
      };
}

if (!Caml_obj.equal(to_list(q2$3), {
        hd: 5,
        tl: {
          hd: 6,
          tl: {
            hd: 7,
            tl: {
              hd: 8,
              tl: /* [] */0
            }
          }
        }
      })) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "libqueue_test.res",
          176,
          2
        ],
        Error: new Error()
      };
}

Queue.transfer(q1$3, q2$3);

if (q1$3.length !== 0) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "libqueue_test.res",
          178,
          2
        ],
        Error: new Error()
      };
}

if (to_list(q1$3) !== /* [] */0) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "libqueue_test.res",
          179,
          2
        ],
        Error: new Error()
      };
}

if (q2$3.length !== 4) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "libqueue_test.res",
          180,
          2
        ],
        Error: new Error()
      };
}

if (!Caml_obj.equal(to_list(q2$3), {
        hd: 5,
        tl: {
          hd: 6,
          tl: {
            hd: 7,
            tl: {
              hd: 8,
              tl: /* [] */0
            }
          }
        }
      })) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "libqueue_test.res",
          181,
          2
        ],
        Error: new Error()
      };
}

var q1$4 = {
  length: 0,
  first: "Nil",
  last: "Nil"
};

var q2$4 = {
  length: 0,
  first: "Nil",
  last: "Nil"
};

for(var i$10 = 1; i$10 <= 4; ++i$10){
  Queue.add(i$10, q1$4);
}

for(var i$11 = 5; i$11 <= 8; ++i$11){
  Queue.add(i$11, q2$4);
}

if (q1$4.length !== 4) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "libqueue_test.res",
          192,
          2
        ],
        Error: new Error()
      };
}

if (!Caml_obj.equal(to_list(q1$4), {
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
      })) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "libqueue_test.res",
          193,
          2
        ],
        Error: new Error()
      };
}

if (q2$4.length !== 4) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "libqueue_test.res",
          194,
          2
        ],
        Error: new Error()
      };
}

if (!Caml_obj.equal(to_list(q2$4), {
        hd: 5,
        tl: {
          hd: 6,
          tl: {
            hd: 7,
            tl: {
              hd: 8,
              tl: /* [] */0
            }
          }
        }
      })) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "libqueue_test.res",
          195,
          2
        ],
        Error: new Error()
      };
}

Queue.transfer(q1$4, q2$4);

if (q1$4.length !== 0) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "libqueue_test.res",
          197,
          2
        ],
        Error: new Error()
      };
}

if (to_list(q1$4) !== /* [] */0) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "libqueue_test.res",
          198,
          2
        ],
        Error: new Error()
      };
}

if (q2$4.length !== 8) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "libqueue_test.res",
          199,
          2
        ],
        Error: new Error()
      };
}

if (!Caml_obj.equal(to_list(q2$4), {
        hd: 5,
        tl: {
          hd: 6,
          tl: {
            hd: 7,
            tl: {
              hd: 8,
              tl: {
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
            }
          }
        }
      })) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "libqueue_test.res",
          200,
          2
        ],
        Error: new Error()
      };
}

console.log("OK");

exports.Q = Q;
exports.does_raise = does_raise;
/* q Not a pure module */
