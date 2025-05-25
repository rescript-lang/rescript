module Array = Ocaml_Array

type rec tree<'a> =
  | Lf
  | Br('a, tree<'a>, tree<'a>)

let rec sub = (tr: tree<_>, k) =>
  switch tr {
  | Lf => throw(Not_found)
  | Br(v, l, r) =>
    if k == 1 {
      v
    } else if mod(k, 2) == 0 {
      sub(l, k / 2)
    } else {
      sub(r, k / 2)
    }
  }

let rec update = (tr: tree<_>, k, w) =>
  switch tr {
  | Lf =>
    if k == 1 {
      Br(w, Lf, Lf)
    } else {
      throw(Not_found)
    }
  | Br(v, l, r) =>
    if k == 1 {
      Br(w, l, r)
    } else if mod(k, 2) == 0 {
      Br(v, update(l, k / 2, w), r)
    } else {
      Br(v, l, update(r, k / 2, w))
    }
  }

let rec delete = (tr: tree<_>, n) =>
  switch tr {
  | Lf => throw(Not_found)
  | Br(v, l, r) =>
    if n == 1 {
      Lf
    } else if mod(n, 2) == 0 {
      Br(v, delete(l, n / 2), r)
    } else {
      Br(v, l, delete(r, n / 2))
    }
  }

/* it is the same as [push_front] */
/** proof:
    left => right 
    right has to be left 
*/
let rec loext = (tr: tree<_>, w) =>
  switch tr {
  | Lf => Br(w, Lf, Lf)
  | Br(v, l, r) =>
    Br(
      w,
      loext(
        r,
        v,
      ) /* v at position 2 
                                   contribute to 4, 6, 8,12, 10, 14
                                   from 3 5 ,...
 */,
      l,
    )
  }

/* pop_back */
let rec lorem = (tr: tree<_>) =>
  switch tr {
  | Lf => throw(Not_found)
  | Br(w, Lf, Lf) => Lf
  /* length = 1 */
  | Br(w, Br(v, ll, lr) as l, r) =>
    /* length >= 2 */
    Br(v, r, lorem(l))
  | _ => assert(false)
  }

module Int_array: {
  type t
  let empty: t
  let get: (t, int) => int
  let set: (t, int, int) => t
  let push_front: (t, int) => t
  let pop_front: t => t
  let push_back: (t, int) => t
  let pop_back: t => t
  let append: (t, t) => t
  let sort: t => t
  let of_array: array<int> => t
  let equal: (t, t) => bool
} = {
  type t = (tree<int>, int)

  let empty = (Lf, 0)

  let length = ((_, n)) => n

  let get = ((tree, k), i) =>
    if i >= 0 && i < k {
      sub(tree, i + 1)
    } else {
      invalid_arg("Array.get")
    }

  let set = ((tree, k), i, v) =>
    if i >= 0 && i < k {
      (update(tree, i + 1, v), k)
    } else {
      invalid_arg("Array.set")
    }

  let push_front = ((tree, k), v) => (loext(tree, v), k + 1)

  let pop_front = ((tree, k)) =>
    if k > 0 {
      (lorem(tree), k - 1)
    } else {
      invalid_arg("Array.pop_front")
    }

  let push_back = ((tree, k), v) => (update(tree, k + 1, v), k + 1)

  let pop_back = ((tree, k)) =>
    if k > 0 {
      (delete(tree, k), k - 1)
    } else {
      invalid_arg("Array.pop_back")
    }

  let filter_from = (i, p, s) => {
    let u = ref(empty)
    for i in i to length(s) - 1 {
      let ele = get(s, i)
      if p(ele) {
        u := push_back(u.contents, ele)
      }
    }
    u.contents
  }

  let append = (a, b) => {
    /* let size = size a + size b in */
    let empty = ref(empty)
    for i in 0 to length(a) - 1 {
      empty := push_back(empty.contents, get(a, i))
    }
    for i in 0 to length(b) - 1 {
      empty := push_back(empty.contents, get(b, i))
    }
    empty.contents
  }

  let rec sort = s => {
    let size = length(s)
    if size <= 1 {
      s
    } else {
      let head = get(s, 0)
      let larger = sort(filter_from(1, x => x > head, s))
      let smaller = sort(filter_from(1, x => x <= head, s))
      append(smaller, push_front(larger, head))
    }
  }
  let of_array = arr => {
    let v = ref(empty)
    for i in 0 to Belt.Array.length(arr) - 1 {
      v := push_back(v.contents, arr[i])
    }
    v.contents
  }

  let equal = (x: t, y: t) => x == y
}

let \"=~" = (x, y) => Int_array.equal(x, Int_array.of_array(y))

let _ = {
  let u = Int_array.of_array([1, 2, 2, 5, 3, 6])
  assert(\"=~"(Int_array.sort(u), [1, 2, 2, 3, 5, 6]))
  let len = 500
  let v = Belt.Array.init(len, i => len - i)
  \"=~"(Int_array.sort(Int_array.of_array(v)), Belt.Array.init(len, i => i + 1))
}
