

import * as List from "./list.js";
import * as Curry from "./curry.js";
import * as Caml_option from "./caml_option.js";

function Make(Ord) {
  let height = function (param) {
    if (typeof param !== "object") {
      return 0;
    } else {
      return param.h;
    }
  };
  let create = function (l, v, r) {
    let hl;
    hl = typeof l !== "object" ? 0 : l.h;
    let hr;
    hr = typeof r !== "object" ? 0 : r.h;
    return {
      TAG: "Node",
      l: l,
      v: v,
      r: r,
      h: hl >= hr ? hl + 1 | 0 : hr + 1 | 0
    };
  };
  let bal = function (l, v, r) {
    let hl;
    hl = typeof l !== "object" ? 0 : l.h;
    let hr;
    hr = typeof r !== "object" ? 0 : r.h;
    if (hl > (hr + 2 | 0)) {
      if (typeof l !== "object") {
        throw new Error("Invalid_argument", {
              cause: {
                RE_EXN_ID: "Invalid_argument",
                _1: "Set.bal"
              }
            });
      }
      let lr = l.r;
      let lv = l.v;
      let ll = l.l;
      if (height(ll) >= height(lr)) {
        return create(ll, lv, create(lr, v, r));
      }
      if (typeof lr === "object") {
        return create(create(ll, lv, lr.l), lr.v, create(lr.r, v, r));
      }
      throw new Error("Invalid_argument", {
            cause: {
              RE_EXN_ID: "Invalid_argument",
              _1: "Set.bal"
            }
          });
    }
    if (hr <= (hl + 2 | 0)) {
      return {
        TAG: "Node",
        l: l,
        v: v,
        r: r,
        h: hl >= hr ? hl + 1 | 0 : hr + 1 | 0
      };
    }
    if (typeof r !== "object") {
      throw new Error("Invalid_argument", {
            cause: {
              RE_EXN_ID: "Invalid_argument",
              _1: "Set.bal"
            }
          });
    }
    let rr = r.r;
    let rv = r.v;
    let rl = r.l;
    if (height(rr) >= height(rl)) {
      return create(create(l, v, rl), rv, rr);
    }
    if (typeof rl === "object") {
      return create(create(l, v, rl.l), rl.v, create(rl.r, rv, rr));
    }
    throw new Error("Invalid_argument", {
          cause: {
            RE_EXN_ID: "Invalid_argument",
            _1: "Set.bal"
          }
        });
  };
  let add = function (x, param) {
    if (typeof param !== "object") {
      return {
        TAG: "Node",
        l: "Empty",
        v: x,
        r: "Empty",
        h: 1
      };
    }
    let r = param.r;
    let v = param.v;
    let l = param.l;
    let c = Curry._2(Ord.compare, x, v);
    if (c === 0) {
      return param;
    }
    if (c < 0) {
      let ll = add(x, l);
      if (l === ll) {
        return param;
      } else {
        return bal(ll, v, r);
      }
    }
    let rr = add(x, r);
    if (r === rr) {
      return param;
    } else {
      return bal(l, v, rr);
    }
  };
  let singleton = function (x) {
    return {
      TAG: "Node",
      l: "Empty",
      v: x,
      r: "Empty",
      h: 1
    };
  };
  let add_min_element = function (x, param) {
    if (typeof param !== "object") {
      return singleton(x);
    } else {
      return bal(add_min_element(x, param.l), param.v, param.r);
    }
  };
  let add_max_element = function (x, param) {
    if (typeof param !== "object") {
      return singleton(x);
    } else {
      return bal(param.l, param.v, add_max_element(x, param.r));
    }
  };
  let join = function (l, v, r) {
    if (typeof l !== "object") {
      return add_min_element(v, r);
    }
    let lh = l.h;
    if (typeof r !== "object") {
      return add_max_element(v, l);
    }
    let rh = r.h;
    if (lh > (rh + 2 | 0)) {
      return bal(l.l, l.v, join(l.r, v, r));
    } else if (rh > (lh + 2 | 0)) {
      return bal(join(l, v, r.l), r.v, r.r);
    } else {
      return create(l, v, r);
    }
  };
  let min_elt = function (_param) {
    while(true) {
      let param = _param;
      if (typeof param !== "object") {
        throw new Error("Not_found", {
              cause: {
                RE_EXN_ID: "Not_found"
              }
            });
      }
      let l = param.l;
      if (typeof l !== "object") {
        return param.v;
      }
      _param = l;
      continue;
    };
  };
  let min_elt_opt = function (_param) {
    while(true) {
      let param = _param;
      if (typeof param !== "object") {
        return;
      }
      let l = param.l;
      if (typeof l !== "object") {
        return Caml_option.some(param.v);
      }
      _param = l;
      continue;
    };
  };
  let max_elt = function (_param) {
    while(true) {
      let param = _param;
      if (typeof param !== "object") {
        throw new Error("Not_found", {
              cause: {
                RE_EXN_ID: "Not_found"
              }
            });
      }
      let r = param.r;
      if (typeof r !== "object") {
        return param.v;
      }
      _param = r;
      continue;
    };
  };
  let max_elt_opt = function (_param) {
    while(true) {
      let param = _param;
      if (typeof param !== "object") {
        return;
      }
      let r = param.r;
      if (typeof r !== "object") {
        return Caml_option.some(param.v);
      }
      _param = r;
      continue;
    };
  };
  let remove_min_elt = function (param) {
    if (typeof param !== "object") {
      throw new Error("Invalid_argument", {
            cause: {
              RE_EXN_ID: "Invalid_argument",
              _1: "Set.remove_min_elt"
            }
          });
    }
    let l = param.l;
    if (typeof l !== "object") {
      return param.r;
    } else {
      return bal(remove_min_elt(l), param.v, param.r);
    }
  };
  let merge = function (t1, t2) {
    if (typeof t1 !== "object") {
      return t2;
    } else if (typeof t2 !== "object") {
      return t1;
    } else {
      return bal(t1, min_elt(t2), remove_min_elt(t2));
    }
  };
  let concat = function (t1, t2) {
    if (typeof t1 !== "object") {
      return t2;
    } else if (typeof t2 !== "object") {
      return t1;
    } else {
      return join(t1, min_elt(t2), remove_min_elt(t2));
    }
  };
  let split = function (x, param) {
    if (typeof param !== "object") {
      return [
        "Empty",
        false,
        "Empty"
      ];
    }
    let r = param.r;
    let v = param.v;
    let l = param.l;
    let c = Curry._2(Ord.compare, x, v);
    if (c === 0) {
      return [
        l,
        true,
        r
      ];
    }
    if (c < 0) {
      let match = split(x, l);
      return [
        match[0],
        match[1],
        join(match[2], v, r)
      ];
    }
    let match$1 = split(x, r);
    return [
      join(l, v, match$1[0]),
      match$1[1],
      match$1[2]
    ];
  };
  let is_empty = function (param) {
    if (typeof param !== "object") {
      return true;
    } else {
      return false;
    }
  };
  let mem = function (x, _param) {
    while(true) {
      let param = _param;
      if (typeof param !== "object") {
        return false;
      }
      let c = Curry._2(Ord.compare, x, param.v);
      if (c === 0) {
        return true;
      }
      _param = c < 0 ? param.l : param.r;
      continue;
    };
  };
  let remove = function (x, param) {
    if (typeof param !== "object") {
      return "Empty";
    }
    let r = param.r;
    let v = param.v;
    let l = param.l;
    let c = Curry._2(Ord.compare, x, v);
    if (c === 0) {
      return merge(l, r);
    }
    if (c < 0) {
      let ll = remove(x, l);
      if (l === ll) {
        return param;
      } else {
        return bal(ll, v, r);
      }
    }
    let rr = remove(x, r);
    if (r === rr) {
      return param;
    } else {
      return bal(l, v, rr);
    }
  };
  let union = function (s1, s2) {
    if (typeof s1 !== "object") {
      return s2;
    }
    let h1 = s1.h;
    let v1 = s1.v;
    if (typeof s2 !== "object") {
      return s1;
    }
    let h2 = s2.h;
    let v2 = s2.v;
    if (h1 >= h2) {
      if (h2 === 1) {
        return add(v2, s1);
      }
      let match = split(v1, s2);
      return join(union(s1.l, match[0]), v1, union(s1.r, match[2]));
    }
    if (h1 === 1) {
      return add(v1, s2);
    }
    let match$1 = split(v2, s1);
    return join(union(match$1[0], s2.l), v2, union(match$1[2], s2.r));
  };
  let inter = function (s1, s2) {
    if (typeof s1 !== "object") {
      return "Empty";
    }
    if (typeof s2 !== "object") {
      return "Empty";
    }
    let r1 = s1.r;
    let v1 = s1.v;
    let l1 = s1.l;
    let match = split(v1, s2);
    let l2 = match[0];
    if (match[1]) {
      return join(inter(l1, l2), v1, inter(r1, match[2]));
    } else {
      return concat(inter(l1, l2), inter(r1, match[2]));
    }
  };
  let diff = function (s1, s2) {
    if (typeof s1 !== "object") {
      return "Empty";
    }
    if (typeof s2 !== "object") {
      return s1;
    }
    let r1 = s1.r;
    let v1 = s1.v;
    let l1 = s1.l;
    let match = split(v1, s2);
    let l2 = match[0];
    if (match[1]) {
      return concat(diff(l1, l2), diff(r1, match[2]));
    } else {
      return join(diff(l1, l2), v1, diff(r1, match[2]));
    }
  };
  let cons_enum = function (_s, _e) {
    while(true) {
      let e = _e;
      let s = _s;
      if (typeof s !== "object") {
        return e;
      }
      _e = {
        TAG: "More",
        _0: s.v,
        _1: s.r,
        _2: e
      };
      _s = s.l;
      continue;
    };
  };
  let compare_aux = function (_e1, _e2) {
    while(true) {
      let e2 = _e2;
      let e1 = _e1;
      if (typeof e1 !== "object") {
        if (typeof e2 !== "object") {
          return 0;
        } else {
          return -1;
        }
      }
      if (typeof e2 !== "object") {
        return 1;
      }
      let c = Curry._2(Ord.compare, e1._0, e2._0);
      if (c !== 0) {
        return c;
      }
      _e2 = cons_enum(e2._1, e2._2);
      _e1 = cons_enum(e1._1, e1._2);
      continue;
    };
  };
  let compare = function (s1, s2) {
    return compare_aux(cons_enum(s1, "End"), cons_enum(s2, "End"));
  };
  let equal = function (s1, s2) {
    return compare(s1, s2) === 0;
  };
  let subset = function (_s1, _s2) {
    while(true) {
      let s2 = _s2;
      let s1 = _s1;
      if (typeof s1 !== "object") {
        return true;
      }
      let r1 = s1.r;
      let v1 = s1.v;
      let l1 = s1.l;
      if (typeof s2 !== "object") {
        return false;
      }
      let r2 = s2.r;
      let l2 = s2.l;
      let c = Curry._2(Ord.compare, v1, s2.v);
      if (c === 0) {
        if (!subset(l1, l2)) {
          return false;
        }
        _s2 = r2;
        _s1 = r1;
        continue;
      }
      if (c < 0) {
        if (!subset({
            TAG: "Node",
            l: l1,
            v: v1,
            r: "Empty",
            h: 0
          }, l2)) {
          return false;
        }
        _s1 = r1;
        continue;
      }
      if (!subset({
          TAG: "Node",
          l: "Empty",
          v: v1,
          r: r1,
          h: 0
        }, r2)) {
        return false;
      }
      _s1 = l1;
      continue;
    };
  };
  let iter = function (f, _param) {
    while(true) {
      let param = _param;
      if (typeof param !== "object") {
        return;
      }
      iter(f, param.l);
      Curry._1(f, param.v);
      _param = param.r;
      continue;
    };
  };
  let fold = function (f, _s, _accu) {
    while(true) {
      let accu = _accu;
      let s = _s;
      if (typeof s !== "object") {
        return accu;
      }
      _accu = Curry._2(f, s.v, fold(f, s.l, accu));
      _s = s.r;
      continue;
    };
  };
  let for_all = function (p, _param) {
    while(true) {
      let param = _param;
      if (typeof param !== "object") {
        return true;
      }
      if (!Curry._1(p, param.v)) {
        return false;
      }
      if (!for_all(p, param.l)) {
        return false;
      }
      _param = param.r;
      continue;
    };
  };
  let exists = function (p, _param) {
    while(true) {
      let param = _param;
      if (typeof param !== "object") {
        return false;
      }
      if (Curry._1(p, param.v)) {
        return true;
      }
      if (exists(p, param.l)) {
        return true;
      }
      _param = param.r;
      continue;
    };
  };
  let filter = function (p, param) {
    if (typeof param !== "object") {
      return "Empty";
    }
    let r = param.r;
    let v = param.v;
    let l = param.l;
    let l$p = filter(p, l);
    let pv = Curry._1(p, v);
    let r$p = filter(p, r);
    if (pv) {
      if (l === l$p && r === r$p) {
        return param;
      } else {
        return join(l$p, v, r$p);
      }
    } else {
      return concat(l$p, r$p);
    }
  };
  let partition = function (p, param) {
    if (typeof param !== "object") {
      return [
        "Empty",
        "Empty"
      ];
    }
    let v = param.v;
    let match = partition(p, param.l);
    let lf = match[1];
    let lt = match[0];
    let pv = Curry._1(p, v);
    let match$1 = partition(p, param.r);
    let rf = match$1[1];
    let rt = match$1[0];
    if (pv) {
      return [
        join(lt, v, rt),
        concat(lf, rf)
      ];
    } else {
      return [
        concat(lt, rt),
        join(lf, v, rf)
      ];
    }
  };
  let cardinal = function (param) {
    if (typeof param !== "object") {
      return 0;
    } else {
      return (cardinal(param.l) + 1 | 0) + cardinal(param.r) | 0;
    }
  };
  let elements_aux = function (_accu, _param) {
    while(true) {
      let param = _param;
      let accu = _accu;
      if (typeof param !== "object") {
        return accu;
      }
      _param = param.l;
      _accu = {
        hd: param.v,
        tl: elements_aux(accu, param.r)
      };
      continue;
    };
  };
  let elements = function (s) {
    return elements_aux(/* [] */0, s);
  };
  let find = function (x, _param) {
    while(true) {
      let param = _param;
      if (typeof param !== "object") {
        throw new Error("Not_found", {
              cause: {
                RE_EXN_ID: "Not_found"
              }
            });
      }
      let v = param.v;
      let c = Curry._2(Ord.compare, x, v);
      if (c === 0) {
        return v;
      }
      _param = c < 0 ? param.l : param.r;
      continue;
    };
  };
  let find_first_aux = function (_v0, f, _param) {
    while(true) {
      let param = _param;
      let v0 = _v0;
      if (typeof param !== "object") {
        return v0;
      }
      let v = param.v;
      if (Curry._1(f, v)) {
        _param = param.l;
        _v0 = v;
        continue;
      }
      _param = param.r;
      continue;
    };
  };
  let find_first = function (f, _param) {
    while(true) {
      let param = _param;
      if (typeof param !== "object") {
        throw new Error("Not_found", {
              cause: {
                RE_EXN_ID: "Not_found"
              }
            });
      }
      let v = param.v;
      if (Curry._1(f, v)) {
        return find_first_aux(v, f, param.l);
      }
      _param = param.r;
      continue;
    };
  };
  let find_first_opt_aux = function (_v0, f, _param) {
    while(true) {
      let param = _param;
      let v0 = _v0;
      if (typeof param !== "object") {
        return Caml_option.some(v0);
      }
      let v = param.v;
      if (Curry._1(f, v)) {
        _param = param.l;
        _v0 = v;
        continue;
      }
      _param = param.r;
      continue;
    };
  };
  let find_first_opt = function (f, _param) {
    while(true) {
      let param = _param;
      if (typeof param !== "object") {
        return;
      }
      let v = param.v;
      if (Curry._1(f, v)) {
        return find_first_opt_aux(v, f, param.l);
      }
      _param = param.r;
      continue;
    };
  };
  let find_last_aux = function (_v0, f, _param) {
    while(true) {
      let param = _param;
      let v0 = _v0;
      if (typeof param !== "object") {
        return v0;
      }
      let v = param.v;
      if (Curry._1(f, v)) {
        _param = param.r;
        _v0 = v;
        continue;
      }
      _param = param.l;
      continue;
    };
  };
  let find_last = function (f, _param) {
    while(true) {
      let param = _param;
      if (typeof param !== "object") {
        throw new Error("Not_found", {
              cause: {
                RE_EXN_ID: "Not_found"
              }
            });
      }
      let v = param.v;
      if (Curry._1(f, v)) {
        return find_last_aux(v, f, param.r);
      }
      _param = param.l;
      continue;
    };
  };
  let find_last_opt_aux = function (_v0, f, _param) {
    while(true) {
      let param = _param;
      let v0 = _v0;
      if (typeof param !== "object") {
        return Caml_option.some(v0);
      }
      let v = param.v;
      if (Curry._1(f, v)) {
        _param = param.r;
        _v0 = v;
        continue;
      }
      _param = param.l;
      continue;
    };
  };
  let find_last_opt = function (f, _param) {
    while(true) {
      let param = _param;
      if (typeof param !== "object") {
        return;
      }
      let v = param.v;
      if (Curry._1(f, v)) {
        return find_last_opt_aux(v, f, param.r);
      }
      _param = param.l;
      continue;
    };
  };
  let find_opt = function (x, _param) {
    while(true) {
      let param = _param;
      if (typeof param !== "object") {
        return;
      }
      let v = param.v;
      let c = Curry._2(Ord.compare, x, v);
      if (c === 0) {
        return Caml_option.some(v);
      }
      _param = c < 0 ? param.l : param.r;
      continue;
    };
  };
  let try_join = function (l, v, r) {
    if ((l === "Empty" || Curry._2(Ord.compare, max_elt(l), v) < 0) && (r === "Empty" || Curry._2(Ord.compare, v, min_elt(r)) < 0)) {
      return join(l, v, r);
    } else {
      return union(l, add(v, r));
    }
  };
  let map = function (f, param) {
    if (typeof param !== "object") {
      return "Empty";
    }
    let r = param.r;
    let v = param.v;
    let l = param.l;
    let l$p = map(f, l);
    let v$p = Curry._1(f, v);
    let r$p = map(f, r);
    if (l === l$p && v === v$p && r === r$p) {
      return param;
    } else {
      return try_join(l$p, v$p, r$p);
    }
  };
  let of_sorted_list = function (l) {
    let sub = function (n, l) {
      switch (n) {
        case 0 :
            return [
              "Empty",
              l
            ];
        case 1 :
            if (l) {
              return [
                {
                  TAG: "Node",
                  l: "Empty",
                  v: l.hd,
                  r: "Empty",
                  h: 1
                },
                l.tl
              ];
            }
            break;
        case 2 :
            if (l) {
              let match = l.tl;
              if (match) {
                return [
                  {
                    TAG: "Node",
                    l: {
                      TAG: "Node",
                      l: "Empty",
                      v: l.hd,
                      r: "Empty",
                      h: 1
                    },
                    v: match.hd,
                    r: "Empty",
                    h: 2
                  },
                  match.tl
                ];
              }
              
            }
            break;
        case 3 :
            if (l) {
              let match$1 = l.tl;
              if (match$1) {
                let match$2 = match$1.tl;
                if (match$2) {
                  return [
                    {
                      TAG: "Node",
                      l: {
                        TAG: "Node",
                        l: "Empty",
                        v: l.hd,
                        r: "Empty",
                        h: 1
                      },
                      v: match$1.hd,
                      r: {
                        TAG: "Node",
                        l: "Empty",
                        v: match$2.hd,
                        r: "Empty",
                        h: 1
                      },
                      h: 2
                    },
                    match$2.tl
                  ];
                }
                
              }
              
            }
            break;
        default:
          
      }
      let nl = n / 2 | 0;
      let match$3 = sub(nl, l);
      let l$1 = match$3[1];
      if (l$1) {
        let match$4 = sub((n - nl | 0) - 1 | 0, l$1.tl);
        return [
          create(match$3[0], l$1.hd, match$4[0]),
          match$4[1]
        ];
      }
      throw new Error("Assert_failure", {
            cause: {
              RE_EXN_ID: "Assert_failure",
              _1: [
                "setLabels.res",
                691,
                20
              ]
            }
          });
    };
    return sub(List.length(l), l)[0];
  };
  let of_list = function (l) {
    if (!l) {
      return "Empty";
    }
    let match = l.tl;
    let x0 = l.hd;
    if (!match) {
      return singleton(x0);
    }
    let match$1 = match.tl;
    let x1 = match.hd;
    if (!match$1) {
      return add(x1, singleton(x0));
    }
    let match$2 = match$1.tl;
    let x2 = match$1.hd;
    if (!match$2) {
      return add(x2, add(x1, singleton(x0)));
    }
    let match$3 = match$2.tl;
    let x3 = match$2.hd;
    if (match$3) {
      if (match$3.tl) {
        return of_sorted_list(List.sort_uniq(Ord.compare, l));
      } else {
        return add(match$3.hd, add(x3, add(x2, add(x1, singleton(x0)))));
      }
    } else {
      return add(x3, add(x2, add(x1, singleton(x0))));
    }
  };
  return {
    height: height,
    create: create,
    bal: bal,
    add: add,
    singleton: singleton,
    add_min_element: add_min_element,
    add_max_element: add_max_element,
    join: join,
    min_elt: min_elt,
    min_elt_opt: min_elt_opt,
    max_elt: max_elt,
    max_elt_opt: max_elt_opt,
    remove_min_elt: remove_min_elt,
    merge: merge,
    concat: concat,
    split: split,
    empty: "Empty",
    is_empty: is_empty,
    mem: mem,
    remove: remove,
    union: union,
    inter: inter,
    diff: diff,
    cons_enum: cons_enum,
    compare_aux: compare_aux,
    compare: compare,
    equal: equal,
    subset: subset,
    iter: iter,
    fold: fold,
    for_all: for_all,
    exists: exists,
    filter: filter,
    partition: partition,
    cardinal: cardinal,
    elements_aux: elements_aux,
    elements: elements,
    choose: min_elt,
    choose_opt: min_elt_opt,
    find: find,
    find_first_aux: find_first_aux,
    find_first: find_first,
    find_first_opt_aux: find_first_opt_aux,
    find_first_opt: find_first_opt,
    find_last_aux: find_last_aux,
    find_last: find_last,
    find_last_opt_aux: find_last_opt_aux,
    find_last_opt: find_last_opt,
    find_opt: find_opt,
    try_join: try_join,
    map: map,
    of_sorted_list: of_sorted_list,
    of_list: of_list
  };
}

export {
  Make,
}
/* No side effect */
