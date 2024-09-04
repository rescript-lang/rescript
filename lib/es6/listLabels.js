

import * as Caml_obj from "./caml_obj.js";
import * as Pervasives from "./pervasives.js";
import * as Caml_option from "./caml_option.js";
import * as Caml_js_exceptions from "./caml_js_exceptions.js";

function length(l) {
  let _len = 0;
  let _param = l;
  while (true) {
    let param = _param;
    let len = _len;
    if (!param) {
      return len;
    }
    _param = param.tl;
    _len = len + 1 | 0;
    continue;
  };
}

function cons(a, l) {
  return {
    hd: a,
    tl: l
  };
}

function hd(param) {
  if (param) {
    return param.hd;
  }
  throw Caml_js_exceptions.internalFromExtension({
    RE_EXN_ID: "Failure",
    _1: "hd"
  });
}

function tl(param) {
  if (param) {
    return param.tl;
  }
  throw Caml_js_exceptions.internalFromExtension({
    RE_EXN_ID: "Failure",
    _1: "tl"
  });
}

function nth(l, n) {
  if (n < 0) {
    throw Caml_js_exceptions.internalFromExtension({
      RE_EXN_ID: "Invalid_argument",
      _1: "List.nth"
    });
  }
  let _l = l;
  let _n = n;
  while (true) {
    let n$1 = _n;
    let l$1 = _l;
    if (l$1) {
      if (n$1 === 0) {
        return l$1.hd;
      }
      _n = n$1 - 1 | 0;
      _l = l$1.tl;
      continue;
    }
    throw Caml_js_exceptions.internalFromExtension({
      RE_EXN_ID: "Failure",
      _1: "nth"
    });
  };
}

function nth_opt(l, n) {
  if (n < 0) {
    throw Caml_js_exceptions.internalFromExtension({
      RE_EXN_ID: "Invalid_argument",
      _1: "List.nth"
    });
  }
  let _l = l;
  let _n = n;
  while (true) {
    let n$1 = _n;
    let l$1 = _l;
    if (!l$1) {
      return;
    }
    if (n$1 === 0) {
      return Caml_option.some(l$1.hd);
    }
    _n = n$1 - 1 | 0;
    _l = l$1.tl;
    continue;
  };
}

function rev_append(_l1, _l2) {
  while (true) {
    let l2 = _l2;
    let l1 = _l1;
    if (!l1) {
      return l2;
    }
    _l2 = {
      hd: l1.hd,
      tl: l2
    };
    _l1 = l1.tl;
    continue;
  };
}

function rev(l) {
  return rev_append(l, /* [] */0);
}

function init_tailrec_aux(_acc, _i, n, f) {
  while (true) {
    let i = _i;
    let acc = _acc;
    if (i >= n) {
      return acc;
    }
    _i = i + 1 | 0;
    _acc = {
      hd: f(i),
      tl: acc
    };
    continue;
  };
}

function init_aux(i, n, f) {
  if (i >= n) {
    return /* [] */0;
  }
  let r = f(i);
  return {
    hd: r,
    tl: init_aux(i + 1 | 0, n, f)
  };
}

function init(len, f) {
  if (len < 0) {
    throw Caml_js_exceptions.internalFromExtension({
      RE_EXN_ID: "Invalid_argument",
      _1: "List.init"
    });
  }
  if (len > 10000) {
    return rev_append(init_tailrec_aux(/* [] */0, 0, len, f), /* [] */0);
  } else {
    return init_aux(0, len, f);
  }
}

function flatten(param) {
  if (param) {
    return Pervasives.$at(param.hd, flatten(param.tl));
  } else {
    return /* [] */0;
  }
}

function map(f, param) {
  if (!param) {
    return /* [] */0;
  }
  let r = f(param.hd);
  return {
    hd: r,
    tl: map(f, param.tl)
  };
}

function mapi(i, f, param) {
  if (!param) {
    return /* [] */0;
  }
  let r = f(i, param.hd);
  return {
    hd: r,
    tl: mapi(i + 1 | 0, f, param.tl)
  };
}

function mapi$1(f, l) {
  return mapi(0, f, l);
}

function rev_map(f, l) {
  let _accu = /* [] */0;
  let _param = l;
  while (true) {
    let param = _param;
    let accu = _accu;
    if (!param) {
      return accu;
    }
    _param = param.tl;
    _accu = {
      hd: f(param.hd),
      tl: accu
    };
    continue;
  };
}

function iter(f, _param) {
  while (true) {
    let param = _param;
    if (!param) {
      return;
    }
    f(param.hd);
    _param = param.tl;
    continue;
  };
}

function iteri(f, l) {
  let _i = 0;
  let _param = l;
  while (true) {
    let param = _param;
    let i = _i;
    if (!param) {
      return;
    }
    f(i, param.hd);
    _param = param.tl;
    _i = i + 1 | 0;
    continue;
  };
}

function fold_left(f, _accu, _l) {
  while (true) {
    let l = _l;
    let accu = _accu;
    if (!l) {
      return accu;
    }
    _l = l.tl;
    _accu = f(accu, l.hd);
    continue;
  };
}

function fold_right(f, l, accu) {
  if (l) {
    return f(l.hd, fold_right(f, l.tl, accu));
  } else {
    return accu;
  }
}

function map2(f, l1, l2) {
  if (l1) {
    if (l2) {
      let r = f(l1.hd, l2.hd);
      return {
        hd: r,
        tl: map2(f, l1.tl, l2.tl)
      };
    }
    throw Caml_js_exceptions.internalFromExtension({
      RE_EXN_ID: "Invalid_argument",
      _1: "List.map2"
    });
  }
  if (!l2) {
    return /* [] */0;
  }
  throw Caml_js_exceptions.internalFromExtension({
    RE_EXN_ID: "Invalid_argument",
    _1: "List.map2"
  });
}

function rev_map2(f, l1, l2) {
  let _accu = /* [] */0;
  let _l1 = l1;
  let _l2 = l2;
  while (true) {
    let l2$1 = _l2;
    let l1$1 = _l1;
    let accu = _accu;
    if (l1$1) {
      if (l2$1) {
        _l2 = l2$1.tl;
        _l1 = l1$1.tl;
        _accu = {
          hd: f(l1$1.hd, l2$1.hd),
          tl: accu
        };
        continue;
      }
      throw Caml_js_exceptions.internalFromExtension({
        RE_EXN_ID: "Invalid_argument",
        _1: "List.rev_map2"
      });
    }
    if (l2$1) {
      throw Caml_js_exceptions.internalFromExtension({
        RE_EXN_ID: "Invalid_argument",
        _1: "List.rev_map2"
      });
    }
    return accu;
  };
}

function iter2(f, _l1, _l2) {
  while (true) {
    let l2 = _l2;
    let l1 = _l1;
    if (l1) {
      if (l2) {
        f(l1.hd, l2.hd);
        _l2 = l2.tl;
        _l1 = l1.tl;
        continue;
      }
      throw Caml_js_exceptions.internalFromExtension({
        RE_EXN_ID: "Invalid_argument",
        _1: "List.iter2"
      });
    }
    if (!l2) {
      return;
    }
    throw Caml_js_exceptions.internalFromExtension({
      RE_EXN_ID: "Invalid_argument",
      _1: "List.iter2"
    });
  };
}

function fold_left2(f, _accu, _l1, _l2) {
  while (true) {
    let l2 = _l2;
    let l1 = _l1;
    let accu = _accu;
    if (l1) {
      if (l2) {
        _l2 = l2.tl;
        _l1 = l1.tl;
        _accu = f(accu, l1.hd, l2.hd);
        continue;
      }
      throw Caml_js_exceptions.internalFromExtension({
        RE_EXN_ID: "Invalid_argument",
        _1: "List.fold_left2"
      });
    }
    if (l2) {
      throw Caml_js_exceptions.internalFromExtension({
        RE_EXN_ID: "Invalid_argument",
        _1: "List.fold_left2"
      });
    }
    return accu;
  };
}

function fold_right2(f, l1, l2, accu) {
  if (l1) {
    if (l2) {
      return f(l1.hd, l2.hd, fold_right2(f, l1.tl, l2.tl, accu));
    }
    throw Caml_js_exceptions.internalFromExtension({
      RE_EXN_ID: "Invalid_argument",
      _1: "List.fold_right2"
    });
  }
  if (l2) {
    throw Caml_js_exceptions.internalFromExtension({
      RE_EXN_ID: "Invalid_argument",
      _1: "List.fold_right2"
    });
  }
  return accu;
}

function for_all(p, _param) {
  while (true) {
    let param = _param;
    if (!param) {
      return true;
    }
    if (!p(param.hd)) {
      return false;
    }
    _param = param.tl;
    continue;
  };
}

function exists(p, _param) {
  while (true) {
    let param = _param;
    if (!param) {
      return false;
    }
    if (p(param.hd)) {
      return true;
    }
    _param = param.tl;
    continue;
  };
}

function for_all2(p, _l1, _l2) {
  while (true) {
    let l2 = _l2;
    let l1 = _l1;
    if (l1) {
      if (l2) {
        if (!p(l1.hd, l2.hd)) {
          return false;
        }
        _l2 = l2.tl;
        _l1 = l1.tl;
        continue;
      }
      throw Caml_js_exceptions.internalFromExtension({
        RE_EXN_ID: "Invalid_argument",
        _1: "List.for_all2"
      });
    }
    if (!l2) {
      return true;
    }
    throw Caml_js_exceptions.internalFromExtension({
      RE_EXN_ID: "Invalid_argument",
      _1: "List.for_all2"
    });
  };
}

function exists2(p, _l1, _l2) {
  while (true) {
    let l2 = _l2;
    let l1 = _l1;
    if (l1) {
      if (l2) {
        if (p(l1.hd, l2.hd)) {
          return true;
        }
        _l2 = l2.tl;
        _l1 = l1.tl;
        continue;
      }
      throw Caml_js_exceptions.internalFromExtension({
        RE_EXN_ID: "Invalid_argument",
        _1: "List.exists2"
      });
    }
    if (!l2) {
      return false;
    }
    throw Caml_js_exceptions.internalFromExtension({
      RE_EXN_ID: "Invalid_argument",
      _1: "List.exists2"
    });
  };
}

function mem(x, _set) {
  while (true) {
    let set = _set;
    if (!set) {
      return false;
    }
    if (Caml_obj.equal(set.hd, x)) {
      return true;
    }
    _set = set.tl;
    continue;
  };
}

function memq(x, _set) {
  while (true) {
    let set = _set;
    if (!set) {
      return false;
    }
    if (set.hd === x) {
      return true;
    }
    _set = set.tl;
    continue;
  };
}

function assoc(x, _param) {
  while (true) {
    let param = _param;
    if (param) {
      let match = param.hd;
      if (Caml_obj.equal(match[0], x)) {
        return match[1];
      }
      _param = param.tl;
      continue;
    }
    throw Caml_js_exceptions.internalMakeExn("Not_found");
  };
}

function assoc_opt(x, _param) {
  while (true) {
    let param = _param;
    if (!param) {
      return;
    }
    let match = param.hd;
    if (Caml_obj.equal(match[0], x)) {
      return Caml_option.some(match[1]);
    }
    _param = param.tl;
    continue;
  };
}

function assq(x, _param) {
  while (true) {
    let param = _param;
    if (param) {
      let match = param.hd;
      if (match[0] === x) {
        return match[1];
      }
      _param = param.tl;
      continue;
    }
    throw Caml_js_exceptions.internalMakeExn("Not_found");
  };
}

function assq_opt(x, _param) {
  while (true) {
    let param = _param;
    if (!param) {
      return;
    }
    let match = param.hd;
    if (match[0] === x) {
      return Caml_option.some(match[1]);
    }
    _param = param.tl;
    continue;
  };
}

function mem_assoc(x, _map) {
  while (true) {
    let map = _map;
    if (!map) {
      return false;
    }
    if (Caml_obj.equal(map.hd[0], x)) {
      return true;
    }
    _map = map.tl;
    continue;
  };
}

function mem_assq(x, _map) {
  while (true) {
    let map = _map;
    if (!map) {
      return false;
    }
    if (map.hd[0] === x) {
      return true;
    }
    _map = map.tl;
    continue;
  };
}

function remove_assoc(x, param) {
  if (!param) {
    return /* [] */0;
  }
  let l = param.tl;
  let pair = param.hd;
  if (Caml_obj.equal(pair[0], x)) {
    return l;
  } else {
    return {
      hd: pair,
      tl: remove_assoc(x, l)
    };
  }
}

function remove_assq(x, param) {
  if (!param) {
    return /* [] */0;
  }
  let l = param.tl;
  let pair = param.hd;
  if (pair[0] === x) {
    return l;
  } else {
    return {
      hd: pair,
      tl: remove_assq(x, l)
    };
  }
}

function find(p, _param) {
  while (true) {
    let param = _param;
    if (param) {
      let x = param.hd;
      if (p(x)) {
        return x;
      }
      _param = param.tl;
      continue;
    }
    throw Caml_js_exceptions.internalMakeExn("Not_found");
  };
}

function find_opt(p, _param) {
  while (true) {
    let param = _param;
    if (!param) {
      return;
    }
    let x = param.hd;
    if (p(x)) {
      return Caml_option.some(x);
    }
    _param = param.tl;
    continue;
  };
}

function find_all(p, l) {
  let _accu = /* [] */0;
  let _param = l;
  while (true) {
    let param = _param;
    let accu = _accu;
    if (!param) {
      return rev_append(accu, /* [] */0);
    }
    let l$1 = param.tl;
    let x = param.hd;
    if (p(x)) {
      _param = l$1;
      _accu = {
        hd: x,
        tl: accu
      };
      continue;
    }
    _param = l$1;
    continue;
  };
}

function partition(p, l) {
  let _yes = /* [] */0;
  let _no = /* [] */0;
  let _param = l;
  while (true) {
    let param = _param;
    let no = _no;
    let yes = _yes;
    if (!param) {
      return [
        rev_append(yes, /* [] */0),
        rev_append(no, /* [] */0)
      ];
    }
    let l$1 = param.tl;
    let x = param.hd;
    if (p(x)) {
      _param = l$1;
      _yes = {
        hd: x,
        tl: yes
      };
      continue;
    }
    _param = l$1;
    _no = {
      hd: x,
      tl: no
    };
    continue;
  };
}

function split(param) {
  if (!param) {
    return [
      /* [] */0,
      /* [] */0
    ];
  }
  let match = param.hd;
  let match$1 = split(param.tl);
  return [
    {
      hd: match[0],
      tl: match$1[0]
    },
    {
      hd: match[1],
      tl: match$1[1]
    }
  ];
}

function combine(l1, l2) {
  if (l1) {
    if (l2) {
      return {
        hd: [
          l1.hd,
          l2.hd
        ],
        tl: combine(l1.tl, l2.tl)
      };
    }
    throw Caml_js_exceptions.internalFromExtension({
      RE_EXN_ID: "Invalid_argument",
      _1: "List.combine"
    });
  }
  if (!l2) {
    return /* [] */0;
  }
  throw Caml_js_exceptions.internalFromExtension({
    RE_EXN_ID: "Invalid_argument",
    _1: "List.combine"
  });
}

function merge(cmp, l1, l2) {
  if (!l1) {
    return l2;
  }
  if (!l2) {
    return l1;
  }
  let h2 = l2.hd;
  let h1 = l1.hd;
  if (cmp(h1, h2) <= 0) {
    return {
      hd: h1,
      tl: merge(cmp, l1.tl, l2)
    };
  } else {
    return {
      hd: h2,
      tl: merge(cmp, l1, l2.tl)
    };
  }
}

function chop(_k, _l) {
  while (true) {
    let l = _l;
    let k = _k;
    if (k === 0) {
      return l;
    }
    if (l) {
      _l = l.tl;
      _k = k - 1 | 0;
      continue;
    }
    throw Caml_js_exceptions.internalFromExtension({
      RE_EXN_ID: "Assert_failure",
      _1: [
        "listLabels.res",
        420,
        11
      ]
    });
  };
}

function stable_sort(cmp, l) {
  let sort = (n, l) => {
    if (n !== 2) {
      if (n === 3 && l) {
        let match = l.tl;
        if (match) {
          let match$1 = match.tl;
          if (match$1) {
            let x3 = match$1.hd;
            let x2 = match.hd;
            let x1 = l.hd;
            if (cmp(x1, x2) <= 0) {
              if (cmp(x2, x3) <= 0) {
                return {
                  hd: x1,
                  tl: {
                    hd: x2,
                    tl: {
                      hd: x3,
                      tl: /* [] */0
                    }
                  }
                };
              } else if (cmp(x1, x3) <= 0) {
                return {
                  hd: x1,
                  tl: {
                    hd: x3,
                    tl: {
                      hd: x2,
                      tl: /* [] */0
                    }
                  }
                };
              } else {
                return {
                  hd: x3,
                  tl: {
                    hd: x1,
                    tl: {
                      hd: x2,
                      tl: /* [] */0
                    }
                  }
                };
              }
            } else if (cmp(x1, x3) <= 0) {
              return {
                hd: x2,
                tl: {
                  hd: x1,
                  tl: {
                    hd: x3,
                    tl: /* [] */0
                  }
                }
              };
            } else if (cmp(x2, x3) <= 0) {
              return {
                hd: x2,
                tl: {
                  hd: x3,
                  tl: {
                    hd: x1,
                    tl: /* [] */0
                  }
                }
              };
            } else {
              return {
                hd: x3,
                tl: {
                  hd: x2,
                  tl: {
                    hd: x1,
                    tl: /* [] */0
                  }
                }
              };
            }
          }
          
        }
        
      }
      
    } else if (l) {
      let match$2 = l.tl;
      if (match$2) {
        let x2$1 = match$2.hd;
        let x1$1 = l.hd;
        if (cmp(x1$1, x2$1) <= 0) {
          return {
            hd: x1$1,
            tl: {
              hd: x2$1,
              tl: /* [] */0
            }
          };
        } else {
          return {
            hd: x2$1,
            tl: {
              hd: x1$1,
              tl: /* [] */0
            }
          };
        }
      }
      
    }
    let n1 = (n >> 1);
    let n2 = n - n1 | 0;
    let l2 = chop(n1, l);
    let s1 = rev_sort(n1, l);
    let s2 = rev_sort(n2, l2);
    let _l1 = s1;
    let _l2 = s2;
    let _accu = /* [] */0;
    while (true) {
      let accu = _accu;
      let l2$1 = _l2;
      let l1 = _l1;
      if (!l1) {
        return rev_append(l2$1, accu);
      }
      if (!l2$1) {
        return rev_append(l1, accu);
      }
      let h2 = l2$1.hd;
      let h1 = l1.hd;
      if (cmp(h1, h2) > 0) {
        _accu = {
          hd: h1,
          tl: accu
        };
        _l1 = l1.tl;
        continue;
      }
      _accu = {
        hd: h2,
        tl: accu
      };
      _l2 = l2$1.tl;
      continue;
    };
  };
  let rev_sort = (n, l) => {
    if (n !== 2) {
      if (n === 3 && l) {
        let match = l.tl;
        if (match) {
          let match$1 = match.tl;
          if (match$1) {
            let x3 = match$1.hd;
            let x2 = match.hd;
            let x1 = l.hd;
            if (cmp(x1, x2) > 0) {
              if (cmp(x2, x3) > 0) {
                return {
                  hd: x1,
                  tl: {
                    hd: x2,
                    tl: {
                      hd: x3,
                      tl: /* [] */0
                    }
                  }
                };
              } else if (cmp(x1, x3) > 0) {
                return {
                  hd: x1,
                  tl: {
                    hd: x3,
                    tl: {
                      hd: x2,
                      tl: /* [] */0
                    }
                  }
                };
              } else {
                return {
                  hd: x3,
                  tl: {
                    hd: x1,
                    tl: {
                      hd: x2,
                      tl: /* [] */0
                    }
                  }
                };
              }
            } else if (cmp(x1, x3) > 0) {
              return {
                hd: x2,
                tl: {
                  hd: x1,
                  tl: {
                    hd: x3,
                    tl: /* [] */0
                  }
                }
              };
            } else if (cmp(x2, x3) > 0) {
              return {
                hd: x2,
                tl: {
                  hd: x3,
                  tl: {
                    hd: x1,
                    tl: /* [] */0
                  }
                }
              };
            } else {
              return {
                hd: x3,
                tl: {
                  hd: x2,
                  tl: {
                    hd: x1,
                    tl: /* [] */0
                  }
                }
              };
            }
          }
          
        }
        
      }
      
    } else if (l) {
      let match$2 = l.tl;
      if (match$2) {
        let x2$1 = match$2.hd;
        let x1$1 = l.hd;
        if (cmp(x1$1, x2$1) > 0) {
          return {
            hd: x1$1,
            tl: {
              hd: x2$1,
              tl: /* [] */0
            }
          };
        } else {
          return {
            hd: x2$1,
            tl: {
              hd: x1$1,
              tl: /* [] */0
            }
          };
        }
      }
      
    }
    let n1 = (n >> 1);
    let n2 = n - n1 | 0;
    let l2 = chop(n1, l);
    let s1 = sort(n1, l);
    let s2 = sort(n2, l2);
    let _l1 = s1;
    let _l2 = s2;
    let _accu = /* [] */0;
    while (true) {
      let accu = _accu;
      let l2$1 = _l2;
      let l1 = _l1;
      if (!l1) {
        return rev_append(l2$1, accu);
      }
      if (!l2$1) {
        return rev_append(l1, accu);
      }
      let h2 = l2$1.hd;
      let h1 = l1.hd;
      if (cmp(h1, h2) <= 0) {
        _accu = {
          hd: h1,
          tl: accu
        };
        _l1 = l1.tl;
        continue;
      }
      _accu = {
        hd: h2,
        tl: accu
      };
      _l2 = l2$1.tl;
      continue;
    };
  };
  let len = length(l);
  if (len < 2) {
    return l;
  } else {
    return sort(len, l);
  }
}

function sort_uniq(cmp, l) {
  let sort = (n, l) => {
    if (n !== 2) {
      if (n === 3 && l) {
        let match = l.tl;
        if (match) {
          let match$1 = match.tl;
          if (match$1) {
            let x3 = match$1.hd;
            let x2 = match.hd;
            let x1 = l.hd;
            let c = cmp(x1, x2);
            if (c === 0) {
              let c$1 = cmp(x2, x3);
              if (c$1 === 0) {
                return {
                  hd: x2,
                  tl: /* [] */0
                };
              } else if (c$1 < 0) {
                return {
                  hd: x2,
                  tl: {
                    hd: x3,
                    tl: /* [] */0
                  }
                };
              } else {
                return {
                  hd: x3,
                  tl: {
                    hd: x2,
                    tl: /* [] */0
                  }
                };
              }
            }
            if (c < 0) {
              let c$2 = cmp(x2, x3);
              if (c$2 === 0) {
                return {
                  hd: x1,
                  tl: {
                    hd: x2,
                    tl: /* [] */0
                  }
                };
              }
              if (c$2 < 0) {
                return {
                  hd: x1,
                  tl: {
                    hd: x2,
                    tl: {
                      hd: x3,
                      tl: /* [] */0
                    }
                  }
                };
              }
              let c$3 = cmp(x1, x3);
              if (c$3 === 0) {
                return {
                  hd: x1,
                  tl: {
                    hd: x2,
                    tl: /* [] */0
                  }
                };
              } else if (c$3 < 0) {
                return {
                  hd: x1,
                  tl: {
                    hd: x3,
                    tl: {
                      hd: x2,
                      tl: /* [] */0
                    }
                  }
                };
              } else {
                return {
                  hd: x3,
                  tl: {
                    hd: x1,
                    tl: {
                      hd: x2,
                      tl: /* [] */0
                    }
                  }
                };
              }
            }
            let c$4 = cmp(x1, x3);
            if (c$4 === 0) {
              return {
                hd: x2,
                tl: {
                  hd: x1,
                  tl: /* [] */0
                }
              };
            }
            if (c$4 < 0) {
              return {
                hd: x2,
                tl: {
                  hd: x1,
                  tl: {
                    hd: x3,
                    tl: /* [] */0
                  }
                }
              };
            }
            let c$5 = cmp(x2, x3);
            if (c$5 === 0) {
              return {
                hd: x2,
                tl: {
                  hd: x1,
                  tl: /* [] */0
                }
              };
            } else if (c$5 < 0) {
              return {
                hd: x2,
                tl: {
                  hd: x3,
                  tl: {
                    hd: x1,
                    tl: /* [] */0
                  }
                }
              };
            } else {
              return {
                hd: x3,
                tl: {
                  hd: x2,
                  tl: {
                    hd: x1,
                    tl: /* [] */0
                  }
                }
              };
            }
          }
          
        }
        
      }
      
    } else if (l) {
      let match$2 = l.tl;
      if (match$2) {
        let x2$1 = match$2.hd;
        let x1$1 = l.hd;
        let c$6 = cmp(x1$1, x2$1);
        if (c$6 === 0) {
          return {
            hd: x1$1,
            tl: /* [] */0
          };
        } else if (c$6 < 0) {
          return {
            hd: x1$1,
            tl: {
              hd: x2$1,
              tl: /* [] */0
            }
          };
        } else {
          return {
            hd: x2$1,
            tl: {
              hd: x1$1,
              tl: /* [] */0
            }
          };
        }
      }
      
    }
    let n1 = (n >> 1);
    let n2 = n - n1 | 0;
    let l2 = chop(n1, l);
    let s1 = rev_sort(n1, l);
    let s2 = rev_sort(n2, l2);
    let _l1 = s1;
    let _l2 = s2;
    let _accu = /* [] */0;
    while (true) {
      let accu = _accu;
      let l2$1 = _l2;
      let l1 = _l1;
      if (!l1) {
        return rev_append(l2$1, accu);
      }
      if (!l2$1) {
        return rev_append(l1, accu);
      }
      let t2 = l2$1.tl;
      let h2 = l2$1.hd;
      let t1 = l1.tl;
      let h1 = l1.hd;
      let c$7 = cmp(h1, h2);
      if (c$7 === 0) {
        _accu = {
          hd: h1,
          tl: accu
        };
        _l2 = t2;
        _l1 = t1;
        continue;
      }
      if (c$7 > 0) {
        _accu = {
          hd: h1,
          tl: accu
        };
        _l1 = t1;
        continue;
      }
      _accu = {
        hd: h2,
        tl: accu
      };
      _l2 = t2;
      continue;
    };
  };
  let rev_sort = (n, l) => {
    if (n !== 2) {
      if (n === 3 && l) {
        let match = l.tl;
        if (match) {
          let match$1 = match.tl;
          if (match$1) {
            let x3 = match$1.hd;
            let x2 = match.hd;
            let x1 = l.hd;
            let c = cmp(x1, x2);
            if (c === 0) {
              let c$1 = cmp(x2, x3);
              if (c$1 === 0) {
                return {
                  hd: x2,
                  tl: /* [] */0
                };
              } else if (c$1 > 0) {
                return {
                  hd: x2,
                  tl: {
                    hd: x3,
                    tl: /* [] */0
                  }
                };
              } else {
                return {
                  hd: x3,
                  tl: {
                    hd: x2,
                    tl: /* [] */0
                  }
                };
              }
            }
            if (c > 0) {
              let c$2 = cmp(x2, x3);
              if (c$2 === 0) {
                return {
                  hd: x1,
                  tl: {
                    hd: x2,
                    tl: /* [] */0
                  }
                };
              }
              if (c$2 > 0) {
                return {
                  hd: x1,
                  tl: {
                    hd: x2,
                    tl: {
                      hd: x3,
                      tl: /* [] */0
                    }
                  }
                };
              }
              let c$3 = cmp(x1, x3);
              if (c$3 === 0) {
                return {
                  hd: x1,
                  tl: {
                    hd: x2,
                    tl: /* [] */0
                  }
                };
              } else if (c$3 > 0) {
                return {
                  hd: x1,
                  tl: {
                    hd: x3,
                    tl: {
                      hd: x2,
                      tl: /* [] */0
                    }
                  }
                };
              } else {
                return {
                  hd: x3,
                  tl: {
                    hd: x1,
                    tl: {
                      hd: x2,
                      tl: /* [] */0
                    }
                  }
                };
              }
            }
            let c$4 = cmp(x1, x3);
            if (c$4 === 0) {
              return {
                hd: x2,
                tl: {
                  hd: x1,
                  tl: /* [] */0
                }
              };
            }
            if (c$4 > 0) {
              return {
                hd: x2,
                tl: {
                  hd: x1,
                  tl: {
                    hd: x3,
                    tl: /* [] */0
                  }
                }
              };
            }
            let c$5 = cmp(x2, x3);
            if (c$5 === 0) {
              return {
                hd: x2,
                tl: {
                  hd: x1,
                  tl: /* [] */0
                }
              };
            } else if (c$5 > 0) {
              return {
                hd: x2,
                tl: {
                  hd: x3,
                  tl: {
                    hd: x1,
                    tl: /* [] */0
                  }
                }
              };
            } else {
              return {
                hd: x3,
                tl: {
                  hd: x2,
                  tl: {
                    hd: x1,
                    tl: /* [] */0
                  }
                }
              };
            }
          }
          
        }
        
      }
      
    } else if (l) {
      let match$2 = l.tl;
      if (match$2) {
        let x2$1 = match$2.hd;
        let x1$1 = l.hd;
        let c$6 = cmp(x1$1, x2$1);
        if (c$6 === 0) {
          return {
            hd: x1$1,
            tl: /* [] */0
          };
        } else if (c$6 > 0) {
          return {
            hd: x1$1,
            tl: {
              hd: x2$1,
              tl: /* [] */0
            }
          };
        } else {
          return {
            hd: x2$1,
            tl: {
              hd: x1$1,
              tl: /* [] */0
            }
          };
        }
      }
      
    }
    let n1 = (n >> 1);
    let n2 = n - n1 | 0;
    let l2 = chop(n1, l);
    let s1 = sort(n1, l);
    let s2 = sort(n2, l2);
    let _l1 = s1;
    let _l2 = s2;
    let _accu = /* [] */0;
    while (true) {
      let accu = _accu;
      let l2$1 = _l2;
      let l1 = _l1;
      if (!l1) {
        return rev_append(l2$1, accu);
      }
      if (!l2$1) {
        return rev_append(l1, accu);
      }
      let t2 = l2$1.tl;
      let h2 = l2$1.hd;
      let t1 = l1.tl;
      let h1 = l1.hd;
      let c$7 = cmp(h1, h2);
      if (c$7 === 0) {
        _accu = {
          hd: h1,
          tl: accu
        };
        _l2 = t2;
        _l1 = t1;
        continue;
      }
      if (c$7 < 0) {
        _accu = {
          hd: h1,
          tl: accu
        };
        _l1 = t1;
        continue;
      }
      _accu = {
        hd: h2,
        tl: accu
      };
      _l2 = t2;
      continue;
    };
  };
  let len = length(l);
  if (len < 2) {
    return l;
  } else {
    return sort(len, l);
  }
}

function compare_lengths(_l1, _l2) {
  while (true) {
    let l2 = _l2;
    let l1 = _l1;
    if (!l1) {
      if (l2) {
        return -1;
      } else {
        return 0;
      }
    }
    if (!l2) {
      return 1;
    }
    _l2 = l2.tl;
    _l1 = l1.tl;
    continue;
  };
}

function compare_length_with(_l, _n) {
  while (true) {
    let n = _n;
    let l = _l;
    if (!l) {
      if (n === 0) {
        return 0;
      } else if (n > 0) {
        return -1;
      } else {
        return 1;
      }
    }
    if (n <= 0) {
      return 1;
    }
    _n = n - 1 | 0;
    _l = l.tl;
    continue;
  };
}

let append = Pervasives.$at;

let concat = flatten;

let filter = find_all;

let sort = stable_sort;

let fast_sort = stable_sort;

export {
  length,
  hd,
  compare_lengths,
  compare_length_with,
  cons,
  tl,
  nth,
  nth_opt,
  rev,
  init,
  append,
  rev_append,
  concat,
  flatten,
  iter,
  iteri,
  map,
  mapi$1 as mapi,
  rev_map,
  fold_left,
  fold_right,
  iter2,
  map2,
  rev_map2,
  fold_left2,
  fold_right2,
  for_all,
  exists,
  for_all2,
  exists2,
  mem,
  memq,
  find,
  find_opt,
  filter,
  find_all,
  partition,
  assoc,
  assoc_opt,
  assq,
  assq_opt,
  mem_assoc,
  mem_assq,
  remove_assoc,
  remove_assq,
  split,
  combine,
  sort,
  stable_sort,
  fast_sort,
  sort_uniq,
  merge,
}
/* No side effect */
