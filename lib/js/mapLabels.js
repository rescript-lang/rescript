'use strict';

let Curry = require("./curry.js");
let Caml_option = require("./caml_option.js");

function Make(Ord) {
  let height = function (param) {
    if (typeof param !== "object") {
      return 0;
    } else {
      return param.h;
    }
  };
  let create = function (l, x, d, r) {
    let hl = height(l);
    let hr = height(r);
    return {
      TAG: "Node",
      l: l,
      v: x,
      d: d,
      r: r,
      h: hl >= hr ? hl + 1 | 0 : hr + 1 | 0
    };
  };
  let singleton = function (x, d) {
    return {
      TAG: "Node",
      l: "Empty",
      v: x,
      d: d,
      r: "Empty",
      h: 1
    };
  };
  let bal = function (l, x, d, r) {
    let hl;
    hl = typeof l !== "object" ? 0 : l.h;
    let hr;
    hr = typeof r !== "object" ? 0 : r.h;
    if (hl > (hr + 2 | 0)) {
      if (typeof l !== "object") {
        throw new Error("Invalid_argument", {
              cause: {
                RE_EXN_ID: "Invalid_argument",
                _1: "Map.bal"
              }
            });
      }
      let lr = l.r;
      let ld = l.d;
      let lv = l.v;
      let ll = l.l;
      if (height(ll) >= height(lr)) {
        return create(ll, lv, ld, create(lr, x, d, r));
      }
      if (typeof lr === "object") {
        return create(create(ll, lv, ld, lr.l), lr.v, lr.d, create(lr.r, x, d, r));
      }
      throw new Error("Invalid_argument", {
            cause: {
              RE_EXN_ID: "Invalid_argument",
              _1: "Map.bal"
            }
          });
    }
    if (hr <= (hl + 2 | 0)) {
      return {
        TAG: "Node",
        l: l,
        v: x,
        d: d,
        r: r,
        h: hl >= hr ? hl + 1 | 0 : hr + 1 | 0
      };
    }
    if (typeof r !== "object") {
      throw new Error("Invalid_argument", {
            cause: {
              RE_EXN_ID: "Invalid_argument",
              _1: "Map.bal"
            }
          });
    }
    let rr = r.r;
    let rd = r.d;
    let rv = r.v;
    let rl = r.l;
    if (height(rr) >= height(rl)) {
      return create(create(l, x, d, rl), rv, rd, rr);
    }
    if (typeof rl === "object") {
      return create(create(l, x, d, rl.l), rl.v, rl.d, create(rl.r, rv, rd, rr));
    }
    throw new Error("Invalid_argument", {
          cause: {
            RE_EXN_ID: "Invalid_argument",
            _1: "Map.bal"
          }
        });
  };
  let is_empty = function (param) {
    if (typeof param !== "object") {
      return true;
    } else {
      return false;
    }
  };
  let add = function (x, data, param) {
    if (typeof param !== "object") {
      return {
        TAG: "Node",
        l: "Empty",
        v: x,
        d: data,
        r: "Empty",
        h: 1
      };
    }
    let r = param.r;
    let d = param.d;
    let v = param.v;
    let l = param.l;
    let c = Curry._2(Ord.compare, x, v);
    if (c === 0) {
      if (d === data) {
        return param;
      } else {
        return {
          TAG: "Node",
          l: l,
          v: x,
          d: data,
          r: r,
          h: param.h
        };
      }
    }
    if (c < 0) {
      let ll = add(x, data, l);
      if (l === ll) {
        return param;
      } else {
        return bal(ll, v, d, r);
      }
    }
    let rr = add(x, data, r);
    if (r === rr) {
      return param;
    } else {
      return bal(l, v, d, rr);
    }
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
      let c = Curry._2(Ord.compare, x, param.v);
      if (c === 0) {
        return param.d;
      }
      _param = c < 0 ? param.l : param.r;
      continue;
    };
  };
  let find_first_aux = function (_v0, _d0, f, _param) {
    while(true) {
      let param = _param;
      let d0 = _d0;
      let v0 = _v0;
      if (typeof param !== "object") {
        return [
          v0,
          d0
        ];
      }
      let v = param.v;
      if (Curry._1(f, v)) {
        _param = param.l;
        _d0 = param.d;
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
        return find_first_aux(v, param.d, f, param.l);
      }
      _param = param.r;
      continue;
    };
  };
  let find_first_opt_aux = function (_v0, _d0, f, _param) {
    while(true) {
      let param = _param;
      let d0 = _d0;
      let v0 = _v0;
      if (typeof param !== "object") {
        return [
          v0,
          d0
        ];
      }
      let v = param.v;
      if (Curry._1(f, v)) {
        _param = param.l;
        _d0 = param.d;
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
        return find_first_opt_aux(v, param.d, f, param.l);
      }
      _param = param.r;
      continue;
    };
  };
  let find_last_aux = function (_v0, _d0, f, _param) {
    while(true) {
      let param = _param;
      let d0 = _d0;
      let v0 = _v0;
      if (typeof param !== "object") {
        return [
          v0,
          d0
        ];
      }
      let v = param.v;
      if (Curry._1(f, v)) {
        _param = param.r;
        _d0 = param.d;
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
        return find_last_aux(v, param.d, f, param.r);
      }
      _param = param.l;
      continue;
    };
  };
  let find_last_opt_aux = function (_v0, _d0, f, _param) {
    while(true) {
      let param = _param;
      let d0 = _d0;
      let v0 = _v0;
      if (typeof param !== "object") {
        return [
          v0,
          d0
        ];
      }
      let v = param.v;
      if (Curry._1(f, v)) {
        _param = param.r;
        _d0 = param.d;
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
        return find_last_opt_aux(v, param.d, f, param.r);
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
      let c = Curry._2(Ord.compare, x, param.v);
      if (c === 0) {
        return Caml_option.some(param.d);
      }
      _param = c < 0 ? param.l : param.r;
      continue;
    };
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
  let min_binding = function (_param) {
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
        return [
          param.v,
          param.d
        ];
      }
      _param = l;
      continue;
    };
  };
  let min_binding_opt = function (_param) {
    while(true) {
      let param = _param;
      if (typeof param !== "object") {
        return;
      }
      let l = param.l;
      if (typeof l !== "object") {
        return [
          param.v,
          param.d
        ];
      }
      _param = l;
      continue;
    };
  };
  let max_binding = function (_param) {
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
        return [
          param.v,
          param.d
        ];
      }
      _param = r;
      continue;
    };
  };
  let max_binding_opt = function (_param) {
    while(true) {
      let param = _param;
      if (typeof param !== "object") {
        return;
      }
      let r = param.r;
      if (typeof r !== "object") {
        return [
          param.v,
          param.d
        ];
      }
      _param = r;
      continue;
    };
  };
  let remove_min_binding = function (param) {
    if (typeof param !== "object") {
      throw new Error("Invalid_argument", {
            cause: {
              RE_EXN_ID: "Invalid_argument",
              _1: "Map.remove_min_elt"
            }
          });
    }
    let l = param.l;
    if (typeof l !== "object") {
      return param.r;
    } else {
      return bal(remove_min_binding(l), param.v, param.d, param.r);
    }
  };
  let merge = function (t1, t2) {
    if (typeof t1 !== "object") {
      return t2;
    }
    if (typeof t2 !== "object") {
      return t1;
    }
    let match = min_binding(t2);
    return bal(t1, match[0], match[1], remove_min_binding(t2));
  };
  let remove = function (x, param) {
    if (typeof param !== "object") {
      return "Empty";
    }
    let r = param.r;
    let d = param.d;
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
        return bal(ll, v, d, r);
      }
    }
    let rr = remove(x, r);
    if (r === rr) {
      return param;
    } else {
      return bal(l, v, d, rr);
    }
  };
  let update = function (x, f, param) {
    if (typeof param !== "object") {
      let data = Curry._1(f, undefined);
      if (data !== undefined) {
        return {
          TAG: "Node",
          l: "Empty",
          v: x,
          d: Caml_option.valFromOption(data),
          r: "Empty",
          h: 1
        };
      } else {
        return "Empty";
      }
    }
    let r = param.r;
    let d = param.d;
    let v = param.v;
    let l = param.l;
    let c = Curry._2(Ord.compare, x, v);
    if (c === 0) {
      let data$1 = Curry._1(f, Caml_option.some(d));
      if (data$1 === undefined) {
        return merge(l, r);
      }
      let data$2 = Caml_option.valFromOption(data$1);
      if (d === data$2) {
        return param;
      } else {
        return {
          TAG: "Node",
          l: l,
          v: x,
          d: data$2,
          r: r,
          h: param.h
        };
      }
    }
    if (c < 0) {
      let ll = update(x, f, l);
      if (l === ll) {
        return param;
      } else {
        return bal(ll, v, d, r);
      }
    }
    let rr = update(x, f, r);
    if (r === rr) {
      return param;
    } else {
      return bal(l, v, d, rr);
    }
  };
  let iter = function (f, _param) {
    while(true) {
      let param = _param;
      if (typeof param !== "object") {
        return;
      }
      iter(f, param.l);
      Curry._2(f, param.v, param.d);
      _param = param.r;
      continue;
    };
  };
  let map = function (f, param) {
    if (typeof param !== "object") {
      return "Empty";
    }
    let l$p = map(f, param.l);
    let d$p = Curry._1(f, param.d);
    let r$p = map(f, param.r);
    return {
      TAG: "Node",
      l: l$p,
      v: param.v,
      d: d$p,
      r: r$p,
      h: param.h
    };
  };
  let mapi = function (f, param) {
    if (typeof param !== "object") {
      return "Empty";
    }
    let v = param.v;
    let l$p = mapi(f, param.l);
    let d$p = Curry._2(f, v, param.d);
    let r$p = mapi(f, param.r);
    return {
      TAG: "Node",
      l: l$p,
      v: v,
      d: d$p,
      r: r$p,
      h: param.h
    };
  };
  let fold = function (f, _m, _accu) {
    while(true) {
      let accu = _accu;
      let m = _m;
      if (typeof m !== "object") {
        return accu;
      }
      _accu = Curry._3(f, m.v, m.d, fold(f, m.l, accu));
      _m = m.r;
      continue;
    };
  };
  let for_all = function (p, _param) {
    while(true) {
      let param = _param;
      if (typeof param !== "object") {
        return true;
      }
      if (!Curry._2(p, param.v, param.d)) {
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
      if (Curry._2(p, param.v, param.d)) {
        return true;
      }
      if (exists(p, param.l)) {
        return true;
      }
      _param = param.r;
      continue;
    };
  };
  let add_min_binding = function (k, x, param) {
    if (typeof param !== "object") {
      return singleton(k, x);
    } else {
      return bal(add_min_binding(k, x, param.l), param.v, param.d, param.r);
    }
  };
  let add_max_binding = function (k, x, param) {
    if (typeof param !== "object") {
      return singleton(k, x);
    } else {
      return bal(param.l, param.v, param.d, add_max_binding(k, x, param.r));
    }
  };
  let join = function (l, v, d, r) {
    if (typeof l !== "object") {
      return add_min_binding(v, d, r);
    }
    let lh = l.h;
    if (typeof r !== "object") {
      return add_max_binding(v, d, l);
    }
    let rh = r.h;
    if (lh > (rh + 2 | 0)) {
      return bal(l.l, l.v, l.d, join(l.r, v, d, r));
    } else if (rh > (lh + 2 | 0)) {
      return bal(join(l, v, d, r.l), r.v, r.d, r.r);
    } else {
      return create(l, v, d, r);
    }
  };
  let concat = function (t1, t2) {
    if (typeof t1 !== "object") {
      return t2;
    }
    if (typeof t2 !== "object") {
      return t1;
    }
    let match = min_binding(t2);
    return join(t1, match[0], match[1], remove_min_binding(t2));
  };
  let concat_or_join = function (t1, v, d, t2) {
    if (d !== undefined) {
      return join(t1, v, Caml_option.valFromOption(d), t2);
    } else {
      return concat(t1, t2);
    }
  };
  let split = function (x, param) {
    if (typeof param !== "object") {
      return [
        "Empty",
        undefined,
        "Empty"
      ];
    }
    let r = param.r;
    let d = param.d;
    let v = param.v;
    let l = param.l;
    let c = Curry._2(Ord.compare, x, v);
    if (c === 0) {
      return [
        l,
        Caml_option.some(d),
        r
      ];
    }
    if (c < 0) {
      let match = split(x, l);
      return [
        match[0],
        match[1],
        join(match[2], v, d, r)
      ];
    }
    let match$1 = split(x, r);
    return [
      join(l, v, d, match$1[0]),
      match$1[1],
      match$1[2]
    ];
  };
  let merge$1 = function (f, s1, s2) {
    if (typeof s1 !== "object") {
      if (typeof s2 !== "object") {
        return "Empty";
      }
      
    } else {
      let v1 = s1.v;
      if (s1.h >= height(s2)) {
        let match = split(v1, s2);
        return concat_or_join(merge$1(f, s1.l, match[0]), v1, Curry._3(f, v1, Caml_option.some(s1.d), match[1]), merge$1(f, s1.r, match[2]));
      }
      
    }
    if (typeof s2 !== "object") {
      throw new Error("Assert_failure", {
            cause: {
              RE_EXN_ID: "Assert_failure",
              _1: [
                "mapLabels.res",
                552,
                11
              ]
            }
          });
    }
    let v2 = s2.v;
    let match$1 = split(v2, s1);
    return concat_or_join(merge$1(f, match$1[0], s2.l), v2, Curry._3(f, v2, match$1[1], Caml_option.some(s2.d)), merge$1(f, match$1[2], s2.r));
  };
  let union = function (f, s1, s2) {
    if (typeof s1 !== "object") {
      return s2;
    }
    let d1 = s1.d;
    let v1 = s1.v;
    if (typeof s2 !== "object") {
      return s1;
    }
    let d2 = s2.d;
    let v2 = s2.v;
    if (s1.h >= s2.h) {
      let match = split(v1, s2);
      let d2$1 = match[1];
      let l = union(f, s1.l, match[0]);
      let r = union(f, s1.r, match[2]);
      if (d2$1 !== undefined) {
        return concat_or_join(l, v1, Curry._3(f, v1, d1, Caml_option.valFromOption(d2$1)), r);
      } else {
        return join(l, v1, d1, r);
      }
    }
    let match$1 = split(v2, s1);
    let d1$1 = match$1[1];
    let l$1 = union(f, match$1[0], s2.l);
    let r$1 = union(f, match$1[2], s2.r);
    if (d1$1 !== undefined) {
      return concat_or_join(l$1, v2, Curry._3(f, v2, Caml_option.valFromOption(d1$1), d2), r$1);
    } else {
      return join(l$1, v2, d2, r$1);
    }
  };
  let filter = function (p, param) {
    if (typeof param !== "object") {
      return "Empty";
    }
    let r = param.r;
    let d = param.d;
    let v = param.v;
    let l = param.l;
    let l$p = filter(p, l);
    let pvd = Curry._2(p, v, d);
    let r$p = filter(p, r);
    if (pvd) {
      if (l === l$p && r === r$p) {
        return param;
      } else {
        return join(l$p, v, d, r$p);
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
    let d = param.d;
    let v = param.v;
    let match = partition(p, param.l);
    let lf = match[1];
    let lt = match[0];
    let pvd = Curry._2(p, v, d);
    let match$1 = partition(p, param.r);
    let rf = match$1[1];
    let rt = match$1[0];
    if (pvd) {
      return [
        join(lt, v, d, rt),
        concat(lf, rf)
      ];
    } else {
      return [
        concat(lt, rt),
        join(lf, v, d, rf)
      ];
    }
  };
  let cons_enum = function (_m, _e) {
    while(true) {
      let e = _e;
      let m = _m;
      if (typeof m !== "object") {
        return e;
      }
      _e = {
        TAG: "More",
        _0: m.v,
        _1: m.d,
        _2: m.r,
        _3: e
      };
      _m = m.l;
      continue;
    };
  };
  let compare = function (cmp, m1, m2) {
    let _e1 = cons_enum(m1, "End");
    let _e2 = cons_enum(m2, "End");
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
      let c$1 = Curry._2(cmp, e1._1, e2._1);
      if (c$1 !== 0) {
        return c$1;
      }
      _e2 = cons_enum(e2._2, e2._3);
      _e1 = cons_enum(e1._2, e1._3);
      continue;
    };
  };
  let equal = function (cmp, m1, m2) {
    let _e1 = cons_enum(m1, "End");
    let _e2 = cons_enum(m2, "End");
    while(true) {
      let e2 = _e2;
      let e1 = _e1;
      if (typeof e1 !== "object") {
        if (typeof e2 !== "object") {
          return true;
        } else {
          return false;
        }
      }
      if (typeof e2 !== "object") {
        return false;
      }
      if (Curry._2(Ord.compare, e1._0, e2._0) !== 0) {
        return false;
      }
      if (!Curry._2(cmp, e1._1, e2._1)) {
        return false;
      }
      _e2 = cons_enum(e2._2, e2._3);
      _e1 = cons_enum(e1._2, e1._3);
      continue;
    };
  };
  let cardinal = function (param) {
    if (typeof param !== "object") {
      return 0;
    } else {
      return (cardinal(param.l) + 1 | 0) + cardinal(param.r) | 0;
    }
  };
  let bindings_aux = function (_accu, _param) {
    while(true) {
      let param = _param;
      let accu = _accu;
      if (typeof param !== "object") {
        return accu;
      }
      _param = param.l;
      _accu = {
        hd: [
          param.v,
          param.d
        ],
        tl: bindings_aux(accu, param.r)
      };
      continue;
    };
  };
  let bindings = function (s) {
    return bindings_aux(/* [] */0, s);
  };
  return {
    height: height,
    create: create,
    singleton: singleton,
    bal: bal,
    empty: "Empty",
    is_empty: is_empty,
    add: add,
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
    mem: mem,
    min_binding: min_binding,
    min_binding_opt: min_binding_opt,
    max_binding: max_binding,
    max_binding_opt: max_binding_opt,
    remove_min_binding: remove_min_binding,
    remove: remove,
    update: update,
    iter: iter,
    map: map,
    mapi: mapi,
    fold: fold,
    for_all: for_all,
    exists: exists,
    add_min_binding: add_min_binding,
    add_max_binding: add_max_binding,
    join: join,
    concat: concat,
    concat_or_join: concat_or_join,
    split: split,
    merge: merge$1,
    union: union,
    filter: filter,
    partition: partition,
    cons_enum: cons_enum,
    compare: compare,
    equal: equal,
    cardinal: cardinal,
    bindings_aux: bindings_aux,
    bindings: bindings,
    choose: min_binding,
    choose_opt: min_binding_opt
  };
}

exports.Make = Make;
/* No side effect */
