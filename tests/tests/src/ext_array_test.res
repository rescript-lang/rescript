/* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. */

open Belt

let reverse_range = (a, i, len) =>
  if len == 0 {
    ()
  } else {
    for k in 0 to (len - 1) / 2 {
      let t = Array.getUnsafe(a, i + k)
      Array.setUnsafe(a, i + k, Array.getUnsafe(a, i + len - 1 - k))
      Array.setUnsafe(a, i + len - 1 - k, t)
    }
  }

let reverse_in_place = a => reverse_range(a, 0, Array.length(a))

let reverse = a => {
  let b_len = Array.length(a)
  if b_len == 0 {
    []
  } else {
    let b = Array.copy(a)
    for i in 0 to b_len - 1 {
      Array.setUnsafe(b, i, Array.getUnsafe(a, b_len - 1 - i))
    }
    b
  }
}

let reverse_of_list = x =>
  switch x {
  | list{} => []
  | list{hd, ...tl} as l =>
    let len = List.length(l)
    let a = Array.make(len, hd)
    let rec fill = (i, x) =>
      switch x {
      | list{} => a
      | list{hd, ...tl} =>
        Array.setUnsafe(a, len - i - 2, hd)
        fill(i + 1, tl)
      }
    fill(0, tl)
  }

let filter = (f, a) => {
  let arr_len = Array.length(a)
  let rec aux = (acc, i) =>
    if i == arr_len {
      reverse_of_list(acc)
    } else {
      let v = Array.getUnsafe(a, i)
      if f(v) {
        aux(list{v, ...acc}, i + 1)
      } else {
        aux(acc, i + 1)
      }
    }
  aux(list{}, 0)
}

let filter_map = (f: _ => option<_>, a) => {
  let arr_len = Array.length(a)
  let rec aux = (acc, i) =>
    if i == arr_len {
      reverse_of_list(acc)
    } else {
      let v = Array.getUnsafe(a, i)
      switch f(v) {
      | Some(v) => aux(list{v, ...acc}, i + 1)
      | None => aux(acc, i + 1)
      }
    }
  aux(list{}, 0)
}

let range = (from, to_) =>
  if from > to_ {
    invalid_arg("Ext_array_test.range")
  } else {
    Array.init(to_ - from + 1, i => i + from)
  }

let map2i = (f, a, b) => {
  let len = Array.length(a)
  if len != Array.length(b) {
    invalid_arg("Ext_array_test.map2i")
  } else {
    a->Array.mapWithIndex((i, a) => f(i, a, Array.getUnsafe(b, i)))
  }
}

let rec tolist_aux = (a, f, i, res) =>
  if i < 0 {
    res
  } else {
    let v = Array.getUnsafe(a, i)
    tolist_aux(
      a,
      f,
      i - 1,
      switch f(v) {
      | Some(v) => list{v, ...res}
      | None => res
      },
    )
  }

let to_list_map = (f, a) => tolist_aux(a, f, Array.length(a) - 1, list{})

let to_list_map_acc = (f, a, acc) => tolist_aux(a, f, Array.length(a) - 1, acc)

/* TODO: What would happen if [f] raise, memory leak? */
let of_list_map = (f, a) =>
  switch a {
  | list{} => []
  | list{h, ...tl} =>
    let hd = f(h)
    let len = List.length(tl) + 1
    let arr = Array.make(len, hd)
    let rec fill = (i, x) =>
      switch x {
      | list{} => arr
      | list{hd, ...tl} =>
        Array.setUnsafe(arr, i, f(hd))
        fill(i + 1, tl)
      }
    fill(1, tl)
  }

/**
{[
# rfind_with_index [|1;2;3|] (=) 2;;
- : int = 1
# rfind_with_index [|1;2;3|] (=) 1;;
- : int = 0
# rfind_with_index [|1;2;3|] (=) 3;;
- : int = 2
# rfind_with_index [|1;2;3|] (=) 4;;
- : int = -1
]}
*/
let rfind_with_index = (arr, cmp, v) => {
  let len = Array.length(arr)
  let rec aux = i =>
    if i < 0 {
      i
    } else if cmp(Array.getUnsafe(arr, i), v) {
      i
    } else {
      aux(i - 1)
    }
  aux(len - 1)
}

type split<'a> = [#No_split | #Split(array<'a>, array<'a>)]
let rfind_and_split = (arr, cmp, v): split<_> => {
  let i = rfind_with_index(arr, cmp, v)
  if i < 0 {
    #No_split
  } else {
    #Split(
      Array.slice(arr, ~offset=0, ~len=i),
      Array.slice(arr, ~offset=i + 1, ~len=Array.length(arr) - i - 1),
    )
  }
}

let find_with_index = (arr, cmp, v) => {
  let len = Array.length(arr)
  let rec aux = (i, len) =>
    if i >= len {
      -1
    } else if cmp(Array.getUnsafe(arr, i), v) {
      i
    } else {
      aux(i + 1, len)
    }
  aux(0, len)
}

let find_and_split = (arr, cmp, v): split<_> => {
  let i = find_with_index(arr, cmp, v)
  if i < 0 {
    #No_split
  } else {
    #Split(
      Array.slice(arr, ~offset=0, ~len=i),
      Array.slice(arr, ~offset=i + 1, ~len=Array.length(arr) - i - 1),
    )
  }
}

let exists = (p, a) => {
  let n = Array.length(a)
  let rec loop = i =>
    if i == n {
      false
    } else if p(Array.getUnsafe(a, i)) {
      true
    } else {
      loop(succ(i))
    }
  loop(0)
}

let is_empty = arr => Array.length(arr) == 0

let rec unsafe_loop = (index, len, p, xs, ys) =>
  if index >= len {
    true
  } else {
    p(Array.getUnsafe(xs, index), Array.getUnsafe(ys, index)) &&
    unsafe_loop(succ(index), len, p, xs, ys)
  }

let for_all2_no_exn = (p, xs, ys) => {
  let len_xs = Array.length(xs)
  let len_ys = Array.length(ys)
  len_xs == len_ys && unsafe_loop(0, len_xs, p, xs, ys)
}
