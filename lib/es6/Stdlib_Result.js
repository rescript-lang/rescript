


function getExn(x) {
  if (x.TAG === "Ok") {
    return x._0;
  }
  throw {
    RE_EXN_ID: "Not_found",
    Error: new Error()
  };
}

function mapOr(opt, $$default, f) {
  if (opt.TAG === "Ok") {
    return f(opt._0);
  } else {
    return $$default;
  }
}

function map(opt, f) {
  if (opt.TAG === "Ok") {
    return {
      TAG: "Ok",
      _0: f(opt._0)
    };
  } else {
    return opt;
  }
}

function flatMap(opt, f) {
  if (opt.TAG === "Ok") {
    return f(opt._0);
  } else {
    return opt;
  }
}

function getOr(opt, $$default) {
  if (opt.TAG === "Ok") {
    return opt._0;
  } else {
    return $$default;
  }
}

function isOk(x) {
  return x.TAG === "Ok";
}

function isError(x) {
  return x.TAG !== "Ok";
}

function equal(a, b, f) {
  if (a.TAG === "Ok") {
    if (b.TAG === "Ok") {
      return f(a._0, b._0);
    } else {
      return false;
    }
  } else {
    return b.TAG !== "Ok";
  }
}

function compare(a, b, f) {
  if (a.TAG === "Ok") {
    if (b.TAG === "Ok") {
      return f(a._0, b._0);
    } else {
      return 1;
    }
  } else if (b.TAG === "Ok") {
    return -1;
  } else {
    return 0;
  }
}

function forEach(r, f) {
  if (r.TAG === "Ok") {
    return f(r._0);
  }
  
}

function mapError(r, f) {
  if (r.TAG === "Ok") {
    return r;
  } else {
    return {
      TAG: "Error",
      _0: f(r._0)
    };
  }
}

function all(results) {
  let acc = [];
  let returnValue;
  let index = 0;
  while (returnValue === undefined && index < results.length) {
    let err = results[index];
    if (err.TAG === "Ok") {
      acc.push(err._0);
      index = index + 1 | 0;
    } else {
      returnValue = err;
    }
  };
  let error = returnValue;
  if (error !== undefined) {
    return error;
  } else {
    return {
      TAG: "Ok",
      _0: acc
    };
  }
}

function all2(param) {
  let b = param[1];
  let a = param[0];
  if (a.TAG === "Ok") {
    if (b.TAG === "Ok") {
      return {
        TAG: "Ok",
        _0: [
          a._0,
          b._0
        ]
      };
    } else {
      return {
        TAG: "Error",
        _0: b._0
      };
    }
  } else {
    return {
      TAG: "Error",
      _0: a._0
    };
  }
}

function all3(param) {
  let c = param[2];
  let b = param[1];
  let a = param[0];
  if (a.TAG === "Ok") {
    if (b.TAG === "Ok") {
      if (c.TAG === "Ok") {
        return {
          TAG: "Ok",
          _0: [
            a._0,
            b._0,
            c._0
          ]
        };
      } else {
        return {
          TAG: "Error",
          _0: c._0
        };
      }
    } else {
      return {
        TAG: "Error",
        _0: b._0
      };
    }
  } else {
    return {
      TAG: "Error",
      _0: a._0
    };
  }
}

function all4(param) {
  let d = param[3];
  let c = param[2];
  let b = param[1];
  let a = param[0];
  if (a.TAG === "Ok") {
    if (b.TAG === "Ok") {
      if (c.TAG === "Ok") {
        if (d.TAG === "Ok") {
          return {
            TAG: "Ok",
            _0: [
              a._0,
              b._0,
              c._0,
              d._0
            ]
          };
        } else {
          return {
            TAG: "Error",
            _0: d._0
          };
        }
      } else {
        return {
          TAG: "Error",
          _0: c._0
        };
      }
    } else {
      return {
        TAG: "Error",
        _0: b._0
      };
    }
  } else {
    return {
      TAG: "Error",
      _0: a._0
    };
  }
}

function all5(param) {
  let e = param[4];
  let d = param[3];
  let c = param[2];
  let b = param[1];
  let a = param[0];
  if (a.TAG === "Ok") {
    if (b.TAG === "Ok") {
      if (c.TAG === "Ok") {
        if (d.TAG === "Ok") {
          if (e.TAG === "Ok") {
            return {
              TAG: "Ok",
              _0: [
                a._0,
                b._0,
                c._0,
                d._0,
                e._0
              ]
            };
          } else {
            return {
              TAG: "Error",
              _0: e._0
            };
          }
        } else {
          return {
            TAG: "Error",
            _0: d._0
          };
        }
      } else {
        return {
          TAG: "Error",
          _0: c._0
        };
      }
    } else {
      return {
        TAG: "Error",
        _0: b._0
      };
    }
  } else {
    return {
      TAG: "Error",
      _0: a._0
    };
  }
}

function all6(param) {
  let f = param[5];
  let e = param[4];
  let d = param[3];
  let c = param[2];
  let b = param[1];
  let a = param[0];
  if (a.TAG === "Ok") {
    if (b.TAG === "Ok") {
      if (c.TAG === "Ok") {
        if (d.TAG === "Ok") {
          if (e.TAG === "Ok") {
            if (f.TAG === "Ok") {
              return {
                TAG: "Ok",
                _0: [
                  a._0,
                  b._0,
                  c._0,
                  d._0,
                  e._0,
                  f._0
                ]
              };
            } else {
              return {
                TAG: "Error",
                _0: f._0
              };
            }
          } else {
            return {
              TAG: "Error",
              _0: e._0
            };
          }
        } else {
          return {
            TAG: "Error",
            _0: d._0
          };
        }
      } else {
        return {
          TAG: "Error",
          _0: c._0
        };
      }
    } else {
      return {
        TAG: "Error",
        _0: b._0
      };
    }
  } else {
    return {
      TAG: "Error",
      _0: a._0
    };
  }
}

let mapWithDefault = mapOr;

let getWithDefault = getOr;

export {
  getExn,
  mapOr,
  mapWithDefault,
  map,
  flatMap,
  getOr,
  getWithDefault,
  isOk,
  isError,
  equal,
  compare,
  forEach,
  mapError,
  all,
  all2,
  all3,
  all4,
  all5,
  all6,
}
/* No side effect */
