// FIXME:
//   This exists for compatibility reason.
//   Move this into Pervasives or Core

// Below is all deprecated and should be removed in v13

type t<'a> = list<'a>

let rec length_aux = (len, param) =>
  switch param {
  | list{} => len
  | list{_, ...l} => length_aux(len + 1, l)
  }

let length = l => length_aux(0, l)

let cons = (a, l) => list{a, ...l}

let hd = param =>
  switch param {
  | list{} => failwith("hd")
  | list{a, ..._} => a
  }

let tl = param =>
  switch param {
  | list{} => failwith("tl")
  | list{_, ...l} => l
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

let nth_opt = (l, n) =>
  if n < 0 {
    invalid_arg("List.nth")
  } else {
    let rec nth_aux = (l, n) =>
      switch l {
      | list{} => None
      | list{a, ...l} =>
        if n == 0 {
          Some(a)
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

let rec init_tailrec_aux = (acc, i, n, f) =>
  if i >= n {
    acc
  } else {
    init_tailrec_aux(list{f(i), ...acc}, i + 1, n, f)
  }

let rec init_aux = (i, n, f) =>
  if i >= n {
    list{}
  } else {
    let r = f(i)
    list{r, ...init_aux(i + 1, n, f)}
  }

let init = (len, f) =>
  if len < 0 {
    invalid_arg("List.init")
  } else if len > 10_000 {
    rev(init_tailrec_aux(list{}, 0, len, f))
  } else {
    init_aux(0, len, f)
  }

let rec flatten = param =>
  switch param {
  | list{} => list{}
  | list{l, ...r} => \"@"(l, flatten(r))
  }

let concat = flatten

let rec map = (f, param) =>
  switch param {
  | list{} => list{}
  | list{a, ...l} =>
    let r = f(a)
    list{r, ...map(f, l)}
  }

let rec mapi = (i, f, param) =>
  switch param {
  | list{} => list{}
  | list{a, ...l} =>
    let r = f(i, a)
    list{r, ...mapi(i + 1, f, l)}
  }

let mapi = (f, l) => mapi(0, f, l)

let rev_map = (f, l) => {
  let rec rmap_f = (accu, param) =>
    switch param {
    | list{} => accu
    | list{a, ...l} => rmap_f(list{f(a), ...accu}, l)
    }

  rmap_f(list{}, l)
}

let rec iter = (f, param) =>
  switch param {
  | list{} => ()
  | list{a, ...l} =>
    f(a)
    iter(f, l)
  }

let rec iteri = (i, f, param) =>
  switch param {
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

let rec for_all = (p, param) =>
  switch param {
  | list{} => true
  | list{a, ...l} => p(a) && for_all(p, l)
  }

let rec exists = (p, param) =>
  switch param {
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

let rec mem = (x, param) =>
  switch param {
  | list{} => false
  | list{a, ...l} => compare(a, x) == 0 || mem(x, l)
  }

let rec memq = (x, param) =>
  switch param {
  | list{} => false
  | list{a, ...l} => a === x || memq(x, l)
  }

let rec assoc = (x, param) =>
  switch param {
  | list{} => throw(Not_found)
  | list{(a, b), ...l} =>
    if compare(a, x) == 0 {
      b
    } else {
      assoc(x, l)
    }
  }

let rec assoc_opt = (x, param) =>
  switch param {
  | list{} => None
  | list{(a, b), ...l} =>
    if compare(a, x) == 0 {
      Some(b)
    } else {
      assoc_opt(x, l)
    }
  }

let rec assq = (x, param) =>
  switch param {
  | list{} => throw(Not_found)
  | list{(a, b), ...l} =>
    if a === x {
      b
    } else {
      assq(x, l)
    }
  }

let rec assq_opt = (x, param) =>
  switch param {
  | list{} => None
  | list{(a, b), ...l} =>
    if a === x {
      Some(b)
    } else {
      assq_opt(x, l)
    }
  }

let rec mem_assoc = (x, param) =>
  switch param {
  | list{} => false
  | list{(a, _), ...l} => compare(a, x) == 0 || mem_assoc(x, l)
  }

let rec mem_assq = (x, param) =>
  switch param {
  | list{} => false
  | list{(a, _), ...l} => a === x || mem_assq(x, l)
  }

let rec remove_assoc = (x, param) =>
  switch param {
  | list{} => list{}
  | list{(a, _) as pair, ...l} =>
    if compare(a, x) == 0 {
      l
    } else {
      list{pair, ...remove_assoc(x, l)}
    }
  }

let rec remove_assq = (x, param) =>
  switch param {
  | list{} => list{}
  | list{(a, _) as pair, ...l} =>
    if a === x {
      l
    } else {
      list{pair, ...remove_assq(x, l)}
    }
  }

let rec find = (p, param) =>
  switch param {
  | list{} => throw(Not_found)
  | list{x, ...l} =>
    if p(x) {
      x
    } else {
      find(p, l)
    }
  }

let rec find_opt = (p, param) =>
  switch param {
  | list{} => None
  | list{x, ...l} =>
    if p(x) {
      Some(x)
    } else {
      find_opt(p, l)
    }
  }

let find_all = (p, l) => {
  let rec find = (accu, param) =>
    switch param {
    | list{} => rev(accu)
    | list{x, ...l} =>
      if p(x) {
        find(list{x, ...accu}, l)
      } else {
        find(accu, l)
      }
    }
  find(list{}, l)
}

let filter = find_all

let partition = (p, l) => {
  let rec part = (yes, no, param) =>
    switch param {
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

let rec split = param =>
  switch param {
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
    | list{_, ...t} => chop(k - 1, t)
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

/* Note: on a list of length between about 100000 (depending on the minor
  heap size and the type of the list) and Sys.max_array_size, it is
  actually faster to use the following, but it might also use more memory
  because the argument list cannot be deallocated incrementally.

  Also, there seems to be a bug in this code or in the
  implementation of obj_truncate.

external obj_truncate : 'a array -> int -> unit = "caml_obj_truncate"

let array_to_list_in_place a =
  let l = Array.length a in
  let rec loop accu n p =
    if p <= 0 then accu else begin
      if p = n then begin
        obj_truncate a p;
        loop (a.(p-1) :: accu) (n-1000) (p-1)
      end else begin
        loop (a.(p-1) :: accu) n (p-1)
      end
    end
  in
  loop [] (l-1000) l


let stable_sort cmp l =
  let a = Array.of_list l in
  Array.stable_sort cmp a;
  array_to_list_in_place a

*/

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

let rec compare_lengths = (l1, l2) =>
  switch (l1, l2) {
  | (list{}, list{}) => 0
  | (list{}, _) => -1
  | (_, list{}) => 1
  | (list{_, ...l1}, list{_, ...l2}) => compare_lengths(l1, l2)
  }

let rec compare_length_with = (l, n) =>
  switch l {
  | list{} =>
    if n == 0 {
      0
    } else if n > 0 {
      -1
    } else {
      1
    }
  | list{_, ...l} =>
    if n <= 0 {
      1
    } else {
      compare_length_with(l, n - 1)
    }
  }
