'use strict';


function blackify(x) {
  if (typeof x !== "object" || x._0 === "Black") {
    return [
            x,
            true
          ];
  } else {
    return [
            {
              TAG: "Node",
              _0: "Black",
              _1: x._1,
              _2: x._2,
              _3: x._3
            },
            false
          ];
  }
}

function is_empty(x) {
  if (typeof x !== "object") {
    return true;
  } else {
    return false;
  }
}

function mem(x, _x_) {
  while(true) {
    var x_ = _x_;
    if (typeof x_ !== "object") {
      return false;
    }
    var y = x_._2;
    if (x === y) {
      return true;
    }
    if (x < y) {
      _x_ = x_._1;
      continue ;
    }
    _x_ = x_._3;
    continue ;
  };
}

function balance_left(l, x, r) {
  var exit = 0;
  var a;
  var x$1;
  var b;
  var y;
  var c;
  var z;
  var d;
  if (typeof l !== "object" || l._0 === "Black") {
    exit = 1;
  } else {
    var a$1 = l._1;
    var exit$1 = 0;
    if (typeof a$1 !== "object" || a$1._0 === "Black") {
      exit$1 = 3;
    } else {
      a = a$1._1;
      x$1 = a$1._2;
      b = a$1._3;
      y = l._2;
      c = l._3;
      z = x;
      d = r;
      exit = 2;
    }
    if (exit$1 === 3) {
      var match = l._3;
      if (typeof match !== "object" || match._0 === "Black") {
        exit = 1;
      } else {
        a = a$1;
        x$1 = l._2;
        b = match._1;
        y = match._2;
        c = match._3;
        z = x;
        d = r;
        exit = 2;
      }
    }
    
  }
  switch (exit) {
    case 1 :
        return {
                TAG: "Node",
                _0: "Black",
                _1: l,
                _2: x,
                _3: r
              };
    case 2 :
        return {
                TAG: "Node",
                _0: "Red",
                _1: {
                  TAG: "Node",
                  _0: "Black",
                  _1: a,
                  _2: x$1,
                  _3: b
                },
                _2: y,
                _3: {
                  TAG: "Node",
                  _0: "Black",
                  _1: c,
                  _2: z,
                  _3: d
                }
              };
    
  }
}

function balance_right(l, x, r) {
  var exit = 0;
  var a;
  var x$1;
  var b;
  var y;
  var c;
  var z;
  var d;
  if (typeof r !== "object" || r._0 === "Black") {
    exit = 1;
  } else {
    var b$1 = r._1;
    var exit$1 = 0;
    if (typeof b$1 !== "object" || b$1._0 === "Black") {
      exit$1 = 3;
    } else {
      a = l;
      x$1 = x;
      b = b$1._1;
      y = b$1._2;
      c = b$1._3;
      z = r._2;
      d = r._3;
      exit = 2;
    }
    if (exit$1 === 3) {
      var match = r._3;
      if (typeof match !== "object" || match._0 === "Black") {
        exit = 1;
      } else {
        a = l;
        x$1 = x;
        b = b$1;
        y = r._2;
        c = match._1;
        z = match._2;
        d = match._3;
        exit = 2;
      }
    }
    
  }
  switch (exit) {
    case 1 :
        return {
                TAG: "Node",
                _0: "Black",
                _1: l,
                _2: x,
                _3: r
              };
    case 2 :
        return {
                TAG: "Node",
                _0: "Red",
                _1: {
                  TAG: "Node",
                  _0: "Black",
                  _1: a,
                  _2: x$1,
                  _3: b
                },
                _2: y,
                _3: {
                  TAG: "Node",
                  _0: "Black",
                  _1: c,
                  _2: z,
                  _3: d
                }
              };
    
  }
}

function singleton(x) {
  return {
          TAG: "Node",
          _0: "Black",
          _1: "Empty",
          _2: x,
          _3: "Empty"
        };
}

function unbalanced_left(x) {
  if (typeof x === "object") {
    if (x._0 === "Black") {
      var match = x._1;
      if (typeof match === "object") {
        if (match._0 === "Black") {
          return [
                  balance_left({
                        TAG: "Node",
                        _0: "Red",
                        _1: match._1,
                        _2: match._2,
                        _3: match._3
                      }, x._2, x._3),
                  true
                ];
        }
        var match$1 = match._3;
        if (typeof match$1 === "object" && match$1._0 === "Black") {
          return [
                  {
                    TAG: "Node",
                    _0: "Black",
                    _1: match._1,
                    _2: match._2,
                    _3: balance_left({
                          TAG: "Node",
                          _0: "Red",
                          _1: match$1._1,
                          _2: match$1._2,
                          _3: match$1._3
                        }, x._2, x._3)
                  },
                  false
                ];
        }
        
      }
      
    } else {
      var match$2 = x._1;
      if (typeof match$2 === "object" && match$2._0 === "Black") {
        return [
                balance_left({
                      TAG: "Node",
                      _0: "Red",
                      _1: match$2._1,
                      _2: match$2._2,
                      _3: match$2._3
                    }, x._2, x._3),
                false
              ];
      }
      
    }
  }
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "rbset.res",
          64,
          9
        ],
        Error: new Error()
      };
}

function unbalanced_right(x) {
  if (typeof x === "object") {
    if (x._0 === "Black") {
      var match = x._3;
      var x$1 = x._2;
      var a = x._1;
      if (typeof match === "object") {
        if (match._0 === "Black") {
          return [
                  balance_right(a, x$1, {
                        TAG: "Node",
                        _0: "Red",
                        _1: match._1,
                        _2: match._2,
                        _3: match._3
                      }),
                  true
                ];
        }
        var match$1 = match._1;
        if (typeof match$1 === "object" && match$1._0 === "Black") {
          return [
                  {
                    TAG: "Node",
                    _0: "Black",
                    _1: balance_right(a, x$1, {
                          TAG: "Node",
                          _0: "Red",
                          _1: match$1._1,
                          _2: match$1._2,
                          _3: match$1._3
                        }),
                    _2: match._2,
                    _3: match._3
                  },
                  false
                ];
        }
        
      }
      
    } else {
      var match$2 = x._3;
      if (typeof match$2 === "object" && match$2._0 === "Black") {
        return [
                balance_right(x._1, x._2, {
                      TAG: "Node",
                      _0: "Red",
                      _1: match$2._1,
                      _2: match$2._2,
                      _3: match$2._3
                    }),
                false
              ];
      }
      
    }
  }
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "rbset.res",
          75,
          9
        ],
        Error: new Error()
      };
}

function lbalance(x1, x2, x3) {
  if (typeof x1 !== "object") {
    return {
            TAG: "Node",
            _0: "Black",
            _1: x1,
            _2: x2,
            _3: x3
          };
  }
  if (x1._0 === "Black") {
    return {
            TAG: "Node",
            _0: "Black",
            _1: x1,
            _2: x2,
            _3: x3
          };
  }
  var r = x1._3;
  var l = x1._1;
  if (typeof l === "object" && l._0 !== "Black") {
    return {
            TAG: "Node",
            _0: "Red",
            _1: {
              TAG: "Node",
              _0: "Black",
              _1: l._1,
              _2: l._2,
              _3: l._3
            },
            _2: x1._2,
            _3: {
              TAG: "Node",
              _0: "Black",
              _1: r,
              _2: x2,
              _3: x3
            }
          };
  }
  if (typeof r !== "object") {
    return {
            TAG: "Node",
            _0: "Black",
            _1: x1,
            _2: x2,
            _3: x3
          };
  }
  if (r._0 === "Black") {
    return {
            TAG: "Node",
            _0: "Black",
            _1: x1,
            _2: x2,
            _3: x3
          };
  }
  var y = r._2;
  return {
          TAG: "Node",
          _0: "Red",
          _1: {
            TAG: "Node",
            _0: "Black",
            _1: l,
            _2: y,
            _3: r._1
          },
          _2: y,
          _3: {
            TAG: "Node",
            _0: "Black",
            _1: r._3,
            _2: x2,
            _3: x3
          }
        };
}

function rbalance(x1, x2, x3) {
  if (typeof x3 === "object" && x3._0 !== "Black") {
    var b = x3._1;
    var exit = 0;
    if (typeof b !== "object") {
      exit = 2;
    } else {
      if (b._0 !== "Black") {
        return {
                TAG: "Node",
                _0: "Red",
                _1: {
                  TAG: "Node",
                  _0: "Black",
                  _1: x1,
                  _2: x2,
                  _3: b._1
                },
                _2: b._2,
                _3: {
                  TAG: "Node",
                  _0: "Black",
                  _1: b._3,
                  _2: x3._2,
                  _3: x3._3
                }
              };
      }
      exit = 2;
    }
    if (exit === 2) {
      var match = x3._3;
      if (typeof match === "object" && match._0 !== "Black") {
        return {
                TAG: "Node",
                _0: "Red",
                _1: {
                  TAG: "Node",
                  _0: "Black",
                  _1: x1,
                  _2: x2,
                  _3: b
                },
                _2: x3._2,
                _3: {
                  TAG: "Node",
                  _0: "Black",
                  _1: match._1,
                  _2: match._2,
                  _3: match._3
                }
              };
      }
      
    }
    
  }
  return {
          TAG: "Node",
          _0: "Black",
          _1: x1,
          _2: x2,
          _3: x3
        };
}

function ins(x, x_) {
  if (typeof x_ !== "object") {
    return {
            TAG: "Node",
            _0: "Red",
            _1: "Empty",
            _2: x,
            _3: "Empty"
          };
  }
  if (x_._0 === "Black") {
    var y = x_._2;
    if (x === y) {
      return x_;
    }
    var b = x_._3;
    var a = x_._1;
    if (x < y) {
      return lbalance(ins(x, a), y, b);
    } else {
      return rbalance(a, y, ins(x, b));
    }
  }
  var y$1 = x_._2;
  if (x === y$1) {
    return x_;
  }
  var b$1 = x_._3;
  var a$1 = x_._1;
  if (x < y$1) {
    return {
            TAG: "Node",
            _0: "Red",
            _1: ins(x, a$1),
            _2: y$1,
            _3: b$1
          };
  } else {
    return {
            TAG: "Node",
            _0: "Red",
            _1: a$1,
            _2: y$1,
            _3: ins(x, b$1)
          };
  }
}

function add(x, s) {
  var s$1 = ins(x, s);
  if (typeof s$1 !== "object" || s$1._0 === "Black") {
    return s$1;
  } else {
    return {
            TAG: "Node",
            _0: "Black",
            _1: s$1._1,
            _2: s$1._2,
            _3: s$1._3
          };
  }
}

function remove_min(x) {
  if (typeof x !== "object") {
    throw {
          RE_EXN_ID: "Assert_failure",
          _1: [
            "rbset.res",
            138,
            4
          ],
          Error: new Error()
        };
  }
  var c = x._0;
  if (c === "Black") {
    var tmp = x._1;
    if (typeof tmp !== "object") {
      var match = x._3;
      var x$1 = x._2;
      if (typeof match !== "object") {
        return [
                "Empty",
                x$1,
                true
              ];
      }
      if (match._0 !== "Black") {
        return [
                {
                  TAG: "Node",
                  _0: "Black",
                  _1: match._1,
                  _2: match._2,
                  _3: match._3
                },
                x$1,
                false
              ];
      }
      throw {
            RE_EXN_ID: "Assert_failure",
            _1: [
              "rbset.res",
              138,
              4
            ],
            Error: new Error()
          };
    }
    
  } else {
    var tmp$1 = x._1;
    if (typeof tmp$1 !== "object") {
      return [
              x._3,
              x._2,
              false
            ];
    }
    
  }
  var match$1 = remove_min(x._1);
  var y = match$1[1];
  var s_1 = match$1[0];
  var s_2 = x._2;
  var s_3 = x._3;
  var s = {
    TAG: "Node",
    _0: c,
    _1: s_1,
    _2: s_2,
    _3: s_3
  };
  if (!match$1[2]) {
    return [
            s,
            y,
            false
          ];
  }
  var match$2 = unbalanced_right(s);
  return [
          match$2[0],
          y,
          match$2[1]
        ];
}

function remove_aux(x, n) {
  if (typeof n !== "object") {
    return [
            "Empty",
            false
          ];
  }
  var r = n._3;
  var y = n._2;
  var l = n._1;
  var c = n._0;
  if (x === y) {
    if (typeof r !== "object") {
      if (c === "Red") {
        return [
                l,
                false
              ];
      } else {
        return blackify(l);
      }
    }
    var match = remove_min(r);
    var n_2 = match[1];
    var n_3 = match[0];
    var n$1 = {
      TAG: "Node",
      _0: c,
      _1: l,
      _2: n_2,
      _3: n_3
    };
    if (match[2]) {
      return unbalanced_left(n$1);
    } else {
      return [
              n$1,
              false
            ];
    }
  }
  if (x < y) {
    var match$1 = remove_aux(x, l);
    var n_1 = match$1[0];
    var n$2 = {
      TAG: "Node",
      _0: c,
      _1: n_1,
      _2: y,
      _3: r
    };
    if (match$1[1]) {
      return unbalanced_right(n$2);
    } else {
      return [
              n$2,
              false
            ];
    }
  }
  var match$2 = remove_aux(x, r);
  var n_3$1 = match$2[0];
  var n$3 = {
    TAG: "Node",
    _0: c,
    _1: l,
    _2: y,
    _3: n_3$1
  };
  if (match$2[1]) {
    return unbalanced_left(n$3);
  } else {
    return [
            n$3,
            false
          ];
  }
}

function remove(x, s) {
  return remove_aux(x, s)[0];
}

function cardinal(x) {
  if (typeof x !== "object") {
    return 0;
  } else {
    return (1 + cardinal(x._1) | 0) + cardinal(x._3) | 0;
  }
}

var empty = "Empty";

exports.blackify = blackify;
exports.empty = empty;
exports.is_empty = is_empty;
exports.mem = mem;
exports.balance_left = balance_left;
exports.balance_right = balance_right;
exports.singleton = singleton;
exports.unbalanced_left = unbalanced_left;
exports.unbalanced_right = unbalanced_right;
exports.lbalance = lbalance;
exports.rbalance = rbalance;
exports.ins = ins;
exports.add = add;
exports.remove_min = remove_min;
exports.remove_aux = remove_aux;
exports.remove = remove;
exports.cardinal = cardinal;
/* No side effect */
