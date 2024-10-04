// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

let Belt_List = require("rescript/lib/js/belt_List.js");
let Pervasives = require("rescript/lib/js/pervasives.js");

function Make(Ord) {
  let height = x => {
    if (typeof x !== "object") {
      return 0;
    } else {
      return x._3;
    }
  };
  let create = (l, v, r) => {
    let hl;
    hl = typeof l !== "object" ? 0 : l._3;
    let hr;
    hr = typeof r !== "object" ? 0 : r._3;
    return {
      TAG: "Node",
      _0: l,
      _1: v,
      _2: r,
      _3: hl >= hr ? hl + 1 | 0 : hr + 1 | 0
    };
  };
  let bal = (l, v, r) => {
    let hl;
    hl = typeof l !== "object" ? 0 : l._3;
    let hr;
    hr = typeof r !== "object" ? 0 : r._3;
    if (hl > (hr + 2 | 0)) {
      if (typeof l !== "object") {
        return Pervasives.invalid_arg("Set.bal");
      }
      let lr = l._2;
      let lv = l._1;
      let ll = l._0;
      if (height(ll) >= height(lr)) {
        return create(ll, lv, create(lr, v, r));
      } else if (typeof lr !== "object") {
        return Pervasives.invalid_arg("Set.bal");
      } else {
        return create(create(ll, lv, lr._0), lr._1, create(lr._2, v, r));
      }
    }
    if (hr <= (hl + 2 | 0)) {
      return {
        TAG: "Node",
        _0: l,
        _1: v,
        _2: r,
        _3: hl >= hr ? hl + 1 | 0 : hr + 1 | 0
      };
    }
    if (typeof r !== "object") {
      return Pervasives.invalid_arg("Set.bal");
    }
    let rr = r._2;
    let rv = r._1;
    let rl = r._0;
    if (height(rr) >= height(rl)) {
      return create(create(l, v, rl), rv, rr);
    } else if (typeof rl !== "object") {
      return Pervasives.invalid_arg("Set.bal");
    } else {
      return create(create(l, v, rl._0), rl._1, create(rl._2, rv, rr));
    }
  };
  let add = (x, x_) => {
    if (typeof x_ !== "object") {
      return {
        TAG: "Node",
        _0: "Empty",
        _1: x,
        _2: "Empty",
        _3: 1
      };
    }
    let r = x_._2;
    let v = x_._1;
    let l = x_._0;
    let c = Ord.compare(x, v);
    if (c === 0) {
      return x_;
    } else if (c < 0) {
      return bal(add(x, l), v, r);
    } else {
      return bal(l, v, add(x, r));
    }
  };
  let singleton = x => ({
    TAG: "Node",
    _0: "Empty",
    _1: x,
    _2: "Empty",
    _3: 1
  });
  let add_min_element = (v, x) => {
    if (typeof x !== "object") {
      return singleton(v);
    } else {
      return bal(add_min_element(v, x._0), x._1, x._2);
    }
  };
  let add_max_element = (v, x) => {
    if (typeof x !== "object") {
      return singleton(v);
    } else {
      return bal(x._0, x._1, add_max_element(v, x._2));
    }
  };
  let join = (l, v, r) => {
    if (typeof l !== "object") {
      return add_min_element(v, r);
    }
    let lh = l._3;
    if (typeof r !== "object") {
      return add_max_element(v, l);
    }
    let rh = r._3;
    if (lh > (rh + 2 | 0)) {
      return bal(l._0, l._1, join(l._2, v, r));
    } else if (rh > (lh + 2 | 0)) {
      return bal(join(l, v, r._0), r._1, r._2);
    } else {
      return create(l, v, r);
    }
  };
  let min_elt = _x => {
    while (true) {
      let x = _x;
      if (typeof x !== "object") {
        throw {
          RE_EXN_ID: "Not_found",
          Error: new Error()
        };
      }
      let l = x._0;
      if (typeof l !== "object") {
        return x._1;
      }
      _x = l;
      continue;
    };
  };
  let max_elt = _x => {
    while (true) {
      let x = _x;
      if (typeof x !== "object") {
        throw {
          RE_EXN_ID: "Not_found",
          Error: new Error()
        };
      }
      let r = x._2;
      if (typeof r !== "object") {
        return x._1;
      }
      _x = r;
      continue;
    };
  };
  let remove_min_elt = x => {
    if (typeof x !== "object") {
      return Pervasives.invalid_arg("Set.remove_min_elt");
    }
    let l = x._0;
    if (typeof l !== "object") {
      return x._2;
    } else {
      return bal(remove_min_elt(l), x._1, x._2);
    }
  };
  let merge = (t1, t2) => {
    if (typeof t1 !== "object") {
      return t2;
    } else if (typeof t2 !== "object") {
      return t1;
    } else {
      return bal(t1, min_elt(t2), remove_min_elt(t2));
    }
  };
  let concat = (t1, t2) => {
    if (typeof t1 !== "object") {
      return t2;
    } else if (typeof t2 !== "object") {
      return t1;
    } else {
      return join(t1, min_elt(t2), remove_min_elt(t2));
    }
  };
  let split = (x, x_) => {
    if (typeof x_ !== "object") {
      return [
        "Empty",
        false,
        "Empty"
      ];
    }
    let r = x_._2;
    let v = x_._1;
    let l = x_._0;
    let c = Ord.compare(x, v);
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
  let is_empty = x => {
    if (typeof x !== "object") {
      return true;
    } else {
      return false;
    }
  };
  let mem = (x, _x_) => {
    while (true) {
      let x_ = _x_;
      if (typeof x_ !== "object") {
        return false;
      }
      let c = Ord.compare(x, x_._1);
      if (c === 0) {
        return true;
      }
      _x_ = c < 0 ? x_._0 : x_._2;
      continue;
    };
  };
  let remove = (x, x_) => {
    if (typeof x_ !== "object") {
      return "Empty";
    }
    let r = x_._2;
    let v = x_._1;
    let l = x_._0;
    let c = Ord.compare(x, v);
    if (c === 0) {
      return merge(l, r);
    } else if (c < 0) {
      return bal(remove(x, l), v, r);
    } else {
      return bal(l, v, remove(x, r));
    }
  };
  let union = (s1, s2) => {
    if (typeof s1 !== "object") {
      return s2;
    }
    let h1 = s1._3;
    let v1 = s1._1;
    if (typeof s2 !== "object") {
      return s1;
    }
    let h2 = s2._3;
    let v2 = s2._1;
    if (h1 >= h2) {
      if (h2 === 1) {
        return add(v2, s1);
      }
      let match = split(v1, s2);
      return join(union(s1._0, match[0]), v1, union(s1._2, match[2]));
    }
    if (h1 === 1) {
      return add(v1, s2);
    }
    let match$1 = split(v2, s1);
    return join(union(match$1[0], s2._0), v2, union(match$1[2], s2._2));
  };
  let inter = (s1, s2) => {
    if (typeof s1 !== "object") {
      return "Empty";
    }
    if (typeof s2 !== "object") {
      return "Empty";
    }
    let r1 = s1._2;
    let v1 = s1._1;
    let l1 = s1._0;
    let match = split(v1, s2);
    let l2 = match[0];
    if (match[1]) {
      return join(inter(l1, l2), v1, inter(r1, match[2]));
    } else {
      return concat(inter(l1, l2), inter(r1, match[2]));
    }
  };
  let diff = (s1, s2) => {
    if (typeof s1 !== "object") {
      return "Empty";
    }
    if (typeof s2 !== "object") {
      return s1;
    }
    let r1 = s1._2;
    let v1 = s1._1;
    let l1 = s1._0;
    let match = split(v1, s2);
    let l2 = match[0];
    if (match[1]) {
      return concat(diff(l1, l2), diff(r1, match[2]));
    } else {
      return join(diff(l1, l2), v1, diff(r1, match[2]));
    }
  };
  let cons_enum = (_s, _e) => {
    while (true) {
      let e = _e;
      let s = _s;
      if (typeof s !== "object") {
        return e;
      }
      _e = {
        TAG: "More",
        _0: s._1,
        _1: s._2,
        _2: e
      };
      _s = s._0;
      continue;
    };
  };
  let compare_aux = (_e1, _e2) => {
    while (true) {
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
      let c = Ord.compare(e1._0, e2._0);
      if (c !== 0) {
        return c;
      }
      _e2 = cons_enum(e2._1, e2._2);
      _e1 = cons_enum(e1._1, e1._2);
      continue;
    };
  };
  let compare = (s1, s2) => compare_aux(cons_enum(s1, "End"), cons_enum(s2, "End"));
  let equal = (s1, s2) => compare(s1, s2) === 0;
  let subset = (_s1, _s2) => {
    while (true) {
      let s2 = _s2;
      let s1 = _s1;
      if (typeof s1 !== "object") {
        return true;
      }
      let r1 = s1._2;
      let v1 = s1._1;
      let l1 = s1._0;
      if (typeof s2 !== "object") {
        return false;
      }
      let r2 = s2._2;
      let l2 = s2._0;
      let c = Ord.compare(v1, s2._1);
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
            _0: l1,
            _1: v1,
            _2: "Empty",
            _3: 0
          }, l2)) {
          return false;
        }
        _s1 = r1;
        continue;
      }
      if (!subset({
          TAG: "Node",
          _0: "Empty",
          _1: v1,
          _2: r1,
          _3: 0
        }, r2)) {
        return false;
      }
      _s1 = l1;
      continue;
    };
  };
  let iter = (f, _x_) => {
    while (true) {
      let x_ = _x_;
      if (typeof x_ !== "object") {
        return;
      }
      iter(f, x_._0);
      f(x_._1);
      _x_ = x_._2;
      continue;
    };
  };
  let fold = (f, _s, _accu) => {
    while (true) {
      let accu = _accu;
      let s = _s;
      if (typeof s !== "object") {
        return accu;
      }
      _accu = f(s._1, fold(f, s._0, accu));
      _s = s._2;
      continue;
    };
  };
  let for_all = (p, _x) => {
    while (true) {
      let x = _x;
      if (typeof x !== "object") {
        return true;
      }
      if (!p(x._1)) {
        return false;
      }
      if (!for_all(p, x._0)) {
        return false;
      }
      _x = x._2;
      continue;
    };
  };
  let exists = (p, _x) => {
    while (true) {
      let x = _x;
      if (typeof x !== "object") {
        return false;
      }
      if (p(x._1)) {
        return true;
      }
      if (exists(p, x._0)) {
        return true;
      }
      _x = x._2;
      continue;
    };
  };
  let filter = (p, x) => {
    if (typeof x !== "object") {
      return "Empty";
    }
    let v = x._1;
    let l$p = filter(p, x._0);
    let pv = p(v);
    let r$p = filter(p, x._2);
    if (pv) {
      return join(l$p, v, r$p);
    } else {
      return concat(l$p, r$p);
    }
  };
  let partition = (p, x) => {
    if (typeof x !== "object") {
      return [
        "Empty",
        "Empty"
      ];
    }
    let v = x._1;
    let match = partition(p, x._0);
    let lf = match[1];
    let lt = match[0];
    let pv = p(v);
    let match$1 = partition(p, x._2);
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
  let cardinal = x => {
    if (typeof x !== "object") {
      return 0;
    } else {
      return (cardinal(x._0) + 1 | 0) + cardinal(x._2) | 0;
    }
  };
  let elements_aux = (_accu, _x) => {
    while (true) {
      let x = _x;
      let accu = _accu;
      if (typeof x !== "object") {
        return accu;
      }
      _x = x._0;
      _accu = {
        hd: x._1,
        tl: elements_aux(accu, x._2)
      };
      continue;
    };
  };
  let elements = s => elements_aux(/* [] */0, s);
  let find = (x, _x_) => {
    while (true) {
      let x_ = _x_;
      if (typeof x_ !== "object") {
        throw {
          RE_EXN_ID: "Not_found",
          Error: new Error()
        };
      }
      let v = x_._1;
      let c = Ord.compare(x, v);
      if (c === 0) {
        return v;
      }
      _x_ = c < 0 ? x_._0 : x_._2;
      continue;
    };
  };
  let of_sorted_list = l => {
    let sub = (n, l) => {
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
                _0: "Empty",
                _1: l.hd,
                _2: "Empty",
                _3: 1
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
                  _0: {
                    TAG: "Node",
                    _0: "Empty",
                    _1: l.hd,
                    _2: "Empty",
                    _3: 1
                  },
                  _1: match.hd,
                  _2: "Empty",
                  _3: 2
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
                    _0: {
                      TAG: "Node",
                      _0: "Empty",
                      _1: l.hd,
                      _2: "Empty",
                      _3: 1
                    },
                    _1: match$1.hd,
                    _2: {
                      TAG: "Node",
                      _0: "Empty",
                      _1: match$2.hd,
                      _2: "Empty",
                      _3: 1
                    },
                    _3: 2
                  },
                  match$2.tl
                ];
              }
              
            }
            
          }
          break;
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
      throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "test_set.res",
          497,
          20
        ],
        Error: new Error()
      };
    };
    return sub(Belt_List.length(l), l)[0];
  };
  let of_list = l => {
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
        return of_sorted_list(Belt_List.sort(l, Ord.compare));
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
    max_elt: max_elt,
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
    find: find,
    of_sorted_list: of_sorted_list,
    of_list: of_list
  };
}

let N = {
  a: 3
};

exports.Make = Make;
exports.N = N;
/* No side effect */
