/* ********************************************************************* */
/*  */
/* OCaml */
/*  */
/* Xavier Leroy, projet Cristal, INRIA Rocquencourt */
/*  */
/* Copyright 1996 Institut National de Recherche en Informatique et */
/* en Automatique.  All rights reserved.  This file is distributed */
/* under the terms of the GNU Library General Public License, with */
/* the special exception on linking described in file ../LICENSE. */
/*  */
/* ********************************************************************* */

/* List operations */

let u = Belt.List.length
let rec length_aux = (len, x) =>
  switch x {
  | list{} => len
  | list{a, ...l} => length_aux(len + 1, l)
  }

let length = l => length_aux(0, l)

let hd = x =>
  switch x {
  | list{} => failwith("hd")
  | list{a, ...l} => a
  }

let tl = x =>
  switch x {
  | list{} => failwith("tl")
  | list{a, ...l} => l
  }

let nth = (l, n) =>
  if n < 0 {
    invalid_arg("List.nth")
  } else {
    let rec nth_aux = (l, n) =>
      switch l {
      | list{} => failwith("nth")
      | list{a, ...l} =>
        if n == 0 {
          a
        } else {
          nth_aux(l, n - 1)
        }
      }
    nth_aux(l, n)
  }

let append = \"@"

let rec rev_append = (l1, l2) =>
  switch l1 {
  | list{} => l2
  | list{a, ...l} => rev_append(l, list{a, ...l2})
  }

let rev = l => rev_append(l, list{})

let rec flatten = x =>
  switch x {
  | list{} => list{}
  | list{l, ...r} => \"@"(l, flatten(r))
  }

let concat = flatten

let rec map = (f, x) =>
  switch x {
  | list{} => list{}
  | list{a, ...l} =>
    let r = f(a)
    list{r, ...map(f, l)}
  }

let rec mapi = (i, f, x) =>
  switch x {
  | list{} => list{}
  | list{a, ...l} =>
    let r = f(i, a)
    list{r, ...mapi(i + 1, f, l)}
  }

let mapi = (f, l) => mapi(0, f, l)

let rev_map = (f, l) => {
  let rec rmap_f = (accu, x) =>
    switch x {
    | list{} => accu
    | list{a, ...l} => rmap_f(list{f(a), ...accu}, l)
    }

  rmap_f(list{}, l)
}

let rec iter = (f, x) =>
  switch x {
  | list{} => ()
  | list{a, ...l} =>
    f(a)
    iter(f, l)
  }

let rec iteri = (i, f, x) =>
  switch x {
  | list{} => ()
  | list{a, ...l} =>
    f(i, a)
    iteri(i + 1, f, l)
  }

let iteri = (f, l) => iteri(0, f, l)

let rec fold_left = (f, accu, l) =>
  switch l {
  | list{} => accu
  | list{a, ...l} => fold_left(f, f(accu, a), l)
  }

let rec fold_right = (f, l, accu) =>
  switch l {
  | list{} => accu
  | list{a, ...l} => f(a, fold_right(f, l, accu))
  }

let rec map2 = (f, l1, l2) =>
  switch (l1, l2) {
  | (list{}, list{}) => list{}
  | (list{a1, ...l1}, list{a2, ...l2}) =>
    let r = f(a1, a2)
    list{r, ...map2(f, l1, l2)}
  | (_, _) => invalid_arg("List.map2")
  }

let rev_map2 = (f, l1, l2) => {
  let rec rmap2_f = (accu, l1, l2) =>
    switch (l1, l2) {
    | (list{}, list{}) => accu
    | (list{a1, ...l1}, list{a2, ...l2}) => rmap2_f(list{f(a1, a2), ...accu}, l1, l2)
    | (_, _) => invalid_arg("List.rev_map2")
    }

  rmap2_f(list{}, l1, l2)
}

let rec iter2 = (f, l1, l2) =>
  switch (l1, l2) {
  | (list{}, list{}) => ()
  | (list{a1, ...l1}, list{a2, ...l2}) =>
    f(a1, a2)
    iter2(f, l1, l2)
  | (_, _) => invalid_arg("List.iter2")
  }

let rec fold_left2 = (f, accu, l1, l2) =>
  switch (l1, l2) {
  | (list{}, list{}) => accu
  | (list{a1, ...l1}, list{a2, ...l2}) => fold_left2(f, f(accu, a1, a2), l1, l2)
  | (_, _) => invalid_arg("List.fold_left2")
  }

let rec fold_right2 = (f, l1, l2, accu) =>
  switch (l1, l2) {
  | (list{}, list{}) => accu
  | (list{a1, ...l1}, list{a2, ...l2}) => f(a1, a2, fold_right2(f, l1, l2, accu))
  | (_, _) => invalid_arg("List.fold_right2")
  }

let rec for_all = (p, x) =>
  switch x {
  | list{} => true
  | list{a, ...l} => p(a) && for_all(p, l)
  }

let rec exists = (p, x) =>
  switch x {
  | list{} => false
  | list{a, ...l} => p(a) || exists(p, l)
  }

let rec for_all2 = (p, l1, l2) =>
  switch (l1, l2) {
  | (list{}, list{}) => true
  | (list{a1, ...l1}, list{a2, ...l2}) => p(a1, a2) && for_all2(p, l1, l2)
  | (_, _) => invalid_arg("List.for_all2")
  }

let rec exists2 = (p, l1, l2) =>
  switch (l1, l2) {
  | (list{}, list{}) => false
  | (list{a1, ...l1}, list{a2, ...l2}) => p(a1, a2) || exists2(p, l1, l2)
  | (_, _) => invalid_arg("List.exists2")
  }

let rec mem = (x, x_) =>
  switch x_ {
  | list{} => false
  | list{a, ...l} => compare(a, x) == 0 || mem(x, l)
  }

let rec memq = (x, x_) =>
  switch x_ {
  | list{} => false
  | list{a, ...l} => a === x || memq(x, l)
  }

let rec assoc = (x, x_) =>
  switch x_ {
  | list{} => raise(Not_found)
  | list{(a, b), ...l} =>
    if compare(a, x) == 0 {
      b
    } else {
      assoc(x, l)
    }
  }

let rec assq = (x, x_) =>
  switch x_ {
  | list{} => raise(Not_found)
  | list{(a, b), ...l} =>
    if a === x {
      b
    } else {
      assq(x, l)
    }
  }

let rec mem_assoc = (x, x_) =>
  switch x_ {
  | list{} => false
  | list{(a, b), ...l} => compare(a, x) == 0 || mem_assoc(x, l)
  }

let rec mem_assq = (x, x_) =>
  switch x_ {
  | list{} => false
  | list{(a, b), ...l} => a === x || mem_assq(x, l)
  }

let rec remove_assoc = (x, x_) =>
  switch x_ {
  | list{} => list{}
  | list{(a, b) as pair, ...l} =>
    if compare(a, x) == 0 {
      l
    } else {
      list{pair, ...remove_assoc(x, l)}
    }
  }

let rec remove_assq = (x, x_) =>
  switch x_ {
  | list{} => list{}
  | list{(a, b) as pair, ...l} =>
    if a === x {
      l
    } else {
      list{pair, ...remove_assq(x, l)}
    }
  }

let rec find = (p, x) =>
  switch x {
  | list{} => raise(Not_found)
  | list{x, ...l} =>
    if p(x) {
      x
    } else {
      find(p, l)
    }
  }

let find_all = p => {
  let rec find = (accu, x) =>
    switch x {
    | list{} => rev(accu)
    | list{x, ...l} =>
      if p(x) {
        find(list{x, ...accu}, l)
      } else {
        find(accu, l)
      }
    }
  find(list{}, _)
}

let filter = find_all

let partition = (p, l) => {
  let rec part = (yes, no, x) =>
    switch x {
    | list{} => (rev(yes), rev(no))
    | list{x, ...l} =>
      if p(x) {
        part(list{x, ...yes}, no, l)
      } else {
        part(yes, list{x, ...no}, l)
      }
    }
  part(list{}, list{}, l)
}

let rec split = x =>
  switch x {
  | list{} => (list{}, list{})
  | list{(x, y), ...l} =>
    let (rx, ry) = split(l)
    (list{x, ...rx}, list{y, ...ry})
  }

let rec combine = (l1, l2) =>
  switch (l1, l2) {
  | (list{}, list{}) => list{}
  | (list{a1, ...l1}, list{a2, ...l2}) => list{(a1, a2), ...combine(l1, l2)}
  | (_, _) => invalid_arg("List.combine")
  }

/* sorting */

let rec merge = (cmp, l1, l2) =>
  switch (l1, l2) {
  | (list{}, l2) => l2
  | (l1, list{}) => l1
  | (list{h1, ...t1}, list{h2, ...t2}) =>
    if cmp(h1, h2) <= 0 {
      list{h1, ...merge(cmp, t1, l2)}
    } else {
      list{h2, ...merge(cmp, l1, t2)}
    }
  }

let rec chop = (k, l) =>
  if k == 0 {
    l
  } else {
    switch l {
    | list{x, ...t} => chop(k - 1, t)
    | _ => assert(false)
    }
  }

let stable_sort = (cmp, l) => {
  let rec rev_merge = (l1, l2, accu) =>
    switch (l1, l2) {
    | (list{}, l2) => rev_append(l2, accu)
    | (l1, list{}) => rev_append(l1, accu)
    | (list{h1, ...t1}, list{h2, ...t2}) =>
      if cmp(h1, h2) <= 0 {
        rev_merge(t1, l2, list{h1, ...accu})
      } else {
        rev_merge(l1, t2, list{h2, ...accu})
      }
    }

  let rec rev_merge_rev = (l1, l2, accu) =>
    switch (l1, l2) {
    | (list{}, l2) => rev_append(l2, accu)
    | (l1, list{}) => rev_append(l1, accu)
    | (list{h1, ...t1}, list{h2, ...t2}) =>
      if cmp(h1, h2) > 0 {
        rev_merge_rev(t1, l2, list{h1, ...accu})
      } else {
        rev_merge_rev(l1, t2, list{h2, ...accu})
      }
    }

  let rec sort = (n, l) =>
    switch (n, l) {
    | (2, list{x1, x2, ..._}) =>
      if cmp(x1, x2) <= 0 {
        list{x1, x2}
      } else {
        list{x2, x1}
      }
    | (3, list{x1, x2, x3, ..._}) =>
      if cmp(x1, x2) <= 0 {
        if cmp(x2, x3) <= 0 {
          list{x1, x2, x3}
        } else if cmp(x1, x3) <= 0 {
          list{x1, x3, x2}
        } else {
          list{x3, x1, x2}
        }
      } else if cmp(x1, x3) <= 0 {
        list{x2, x1, x3}
      } else if cmp(x2, x3) <= 0 {
        list{x2, x3, x1}
      } else {
        list{x3, x2, x1}
      }
    | (n, l) =>
      let n1 = asr(n, 1)
      let n2 = n - n1
      let l2 = chop(n1, l)
      let s1 = rev_sort(n1, l)
      let s2 = rev_sort(n2, l2)
      rev_merge_rev(s1, s2, list{})
    }
  and rev_sort = (n, l) =>
    switch (n, l) {
    | (2, list{x1, x2, ..._}) =>
      if cmp(x1, x2) > 0 {
        list{x1, x2}
      } else {
        list{x2, x1}
      }
    | (3, list{x1, x2, x3, ..._}) =>
      if cmp(x1, x2) > 0 {
        if cmp(x2, x3) > 0 {
          list{x1, x2, x3}
        } else if cmp(x1, x3) > 0 {
          list{x1, x3, x2}
        } else {
          list{x3, x1, x2}
        }
      } else if cmp(x1, x3) > 0 {
        list{x2, x1, x3}
      } else if cmp(x2, x3) > 0 {
        list{x2, x3, x1}
      } else {
        list{x3, x2, x1}
      }
    | (n, l) =>
      let n1 = asr(n, 1)
      let n2 = n - n1
      let l2 = chop(n1, l)
      let s1 = sort(n1, l)
      let s2 = sort(n2, l2)
      rev_merge(s1, s2, list{})
    }

  let len = length(l)
  if len < 2 {
    l
  } else {
    sort(len, l)
  }
}

let sort = stable_sort
let fast_sort = stable_sort

/* sorting + removing duplicates */

let sort_uniq = (cmp, l) => {
  let rec rev_merge = (l1, l2, accu) =>
    switch (l1, l2) {
    | (list{}, l2) => rev_append(l2, accu)
    | (l1, list{}) => rev_append(l1, accu)
    | (list{h1, ...t1}, list{h2, ...t2}) =>
      let c = cmp(h1, h2)
      if c == 0 {
        rev_merge(t1, t2, list{h1, ...accu})
      } else if c < 0 {
        rev_merge(t1, l2, list{h1, ...accu})
      } else {
        rev_merge(l1, t2, list{h2, ...accu})
      }
    }

  let rec rev_merge_rev = (l1, l2, accu) =>
    switch (l1, l2) {
    | (list{}, l2) => rev_append(l2, accu)
    | (l1, list{}) => rev_append(l1, accu)
    | (list{h1, ...t1}, list{h2, ...t2}) =>
      let c = cmp(h1, h2)
      if c == 0 {
        rev_merge_rev(t1, t2, list{h1, ...accu})
      } else if c > 0 {
        rev_merge_rev(t1, l2, list{h1, ...accu})
      } else {
        rev_merge_rev(l1, t2, list{h2, ...accu})
      }
    }

  let rec sort = (n, l) =>
    switch (n, l) {
    | (2, list{x1, x2, ..._}) =>
      let c = cmp(x1, x2)
      if c == 0 {
        list{x1}
      } else if c < 0 {
        list{x1, x2}
      } else {
        list{x2, x1}
      }
    | (3, list{x1, x2, x3, ..._}) =>
      let c = cmp(x1, x2)
      if c == 0 {
        let c = cmp(x2, x3)
        if c == 0 {
          list{x2}
        } else if c < 0 {
          list{x2, x3}
        } else {
          list{x3, x2}
        }
      } else if c < 0 {
        let c = cmp(x2, x3)
        if c == 0 {
          list{x1, x2}
        } else if c < 0 {
          list{x1, x2, x3}
        } else {
          let c = cmp(x1, x3)
          if c == 0 {
            list{x1, x2}
          } else if c < 0 {
            list{x1, x3, x2}
          } else {
            list{x3, x1, x2}
          }
        }
      } else {
        let c = cmp(x1, x3)
        if c == 0 {
          list{x2, x1}
        } else if c < 0 {
          list{x2, x1, x3}
        } else {
          let c = cmp(x2, x3)
          if c == 0 {
            list{x2, x1}
          } else if c < 0 {
            list{x2, x3, x1}
          } else {
            list{x3, x2, x1}
          }
        }
      }
    | (n, l) =>
      let n1 = asr(n, 1)
      let n2 = n - n1
      let l2 = chop(n1, l)
      let s1 = rev_sort(n1, l)
      let s2 = rev_sort(n2, l2)
      rev_merge_rev(s1, s2, list{})
    }
  and rev_sort = (n, l) =>
    switch (n, l) {
    | (2, list{x1, x2, ..._}) =>
      let c = cmp(x1, x2)
      if c == 0 {
        list{x1}
      } else if c > 0 {
        list{x1, x2}
      } else {
        list{x2, x1}
      }
    | (3, list{x1, x2, x3, ..._}) =>
      let c = cmp(x1, x2)
      if c == 0 {
        let c = cmp(x2, x3)
        if c == 0 {
          list{x2}
        } else if c > 0 {
          list{x2, x3}
        } else {
          list{x3, x2}
        }
      } else if c > 0 {
        let c = cmp(x2, x3)
        if c == 0 {
          list{x1, x2}
        } else if c > 0 {
          list{x1, x2, x3}
        } else {
          let c = cmp(x1, x3)
          if c == 0 {
            list{x1, x2}
          } else if c > 0 {
            list{x1, x3, x2}
          } else {
            list{x3, x1, x2}
          }
        }
      } else {
        let c = cmp(x1, x3)
        if c == 0 {
          list{x2, x1}
        } else if c > 0 {
          list{x2, x1, x3}
        } else {
          let c = cmp(x2, x3)
          if c == 0 {
            list{x2, x1}
          } else if c > 0 {
            list{x2, x3, x1}
          } else {
            list{x3, x2, x1}
          }
        }
      }
    | (n, l) =>
      let n1 = asr(n, 1)
      let n2 = n - n1
      let l2 = chop(n1, l)
      let s1 = sort(n1, l)
      let s2 = sort(n2, l2)
      rev_merge(s1, s2, list{})
    }

  let len = length(l)
  if len < 2 {
    l
  } else {
    sort(len, l)
  }
}

let isEmpty = x =>
  switch x {
  | list{} => true
  | _ => false
  }

let isEmpty2 = x => x == list{} ? true : false
