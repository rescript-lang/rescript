// Array module
include Belt.Array

let at = get

let includes = Js.Array2.includes

let head = t => t->get(0)

let take = (t, n) => slice(~offset=0, ~len=n, t)

let last = t => t->at(length(t) - 1)

let isEmpty = t => t->length == 0

let isNotEmpty = t => t->length > 0

let append = (t, v) => t->concat([v])

let prepend = (t, v) => [v]->concat(t)

let flatMap = (t, fn) => t->map(fn)->concatMany

let mapi = Js.Array2.mapi

let flatten = t => t->flatMap(x => x)

let find = (t, fn) => Js.Array.find(fn, t)

let findIndex = (t, fn) => Js.Array.findIndex(fn, t)

let filter = Js.Array2.filter

let reject = (t, fn) => t->filter(el => !fn(el))

let sortBy = (t, fn) =>
  Belt.SortArray.stableSortBy(t, (a, b) => {
    switch fn(a, b) {
    | #less_than => -1
    | #equal_to => 0
    | #greater_than => 1
    }
  })

let sortByRaw = Belt.SortArray.stableSortBy

module String = {
  let joinWith = Js.Array2.joinWith
  let join = joinWith(_, "")
}

let rec eqBy = (xs, ys, ~fn) => {
  let tailOrEmpty = sliceToEnd(_, 1)
  switch (head(xs), head(ys)) {
  | (None, None) => true
  | (Some(x), Some(y)) if fn(x, y) => eqBy(tailOrEmpty(xs), tailOrEmpty(ys), ~fn)
  | _ => false
  }
}

let takeWhile = (t, fn) => {
  let a = ref([])

  let maxLength = t->length - 1
  let rec iter = idx => {
    if idx < maxLength {
      let item = t->getUnsafe(idx)
      if fn(item) {
        a := concat(a.contents, [item])
        iter(idx + 1)
      }
    }
  }
  iter(0)

  a.contents
}

let distinct = (t, eq) => {
  let maxIdx = t->length
  let rec aux = (acc, idx) => {
    if idx < maxIdx {
      let y = t->getUnsafe(idx)
      let acc = if !(acc->some(x => eq(x, y))) {
        acc->concat([y])
      } else {
        acc
      }
      aux(acc, idx + 1)
    } else {
      acc
    }
  }
  aux([], 0)
}

let partition = (t, fn) => {
  let maxLength = t->length
  let rec iter = (a, b, idx) => {
    if idx < maxLength {
      let item = t->getUnsafe(idx)
      let idx = idx + 1
      if fn(item) {
        iter(concat(a, [item]), b, idx)
      } else {
        iter(a, concat(b, [item]), idx)
      }
    } else {
      (a, b)
    }
  }
  iter([], [], 0)
}

let replaceAt = (t: array<'a>, idx: int, item: 'a) =>
  t->mapWithIndex((idx', el) =>
    if idx == idx' {
      item
    } else {
      el
    }
  )

let indexOfBy = (t, fn, value) => {
  let rec aux = idx => {
    switch at(t, idx) {
    | None => None
    | Some(value') if fn(value, value') => Some(idx)
    | _ => aux(idx + 1)
    }
  }
  aux(0)
}

let swapAt = (t, i, j) => {
  switch (at(t, i), at(t, j)) {
  | (Some(a), Some(b)) => t->mapWithIndex((k, x) => i == k ? b : j == k ? a : x)
  | _ => t
  }
}

let splitAt = (t, i) =>
  if i < 0 || i > length(t) {
    None
  } else {
    let a = t->slice(~offset=0, ~len=i)
    let b = t->sliceToEnd(i)
    Some((a, b))
  }

let insertAt = (t, idx, x) =>
  switch t->splitAt(idx) {
  | Some((before, after)) => before->concat([x]->concat(after))
  | None => t
  }

let flatMap = (t, fn) => t->map(fn)->concatMany

let removeAt = (t, idx) => t->keepWithIndex((_, i) => i != idx)

let drop = (t, i) => {
  let l = t->length
  let start = i < 0 ? 0 : l < i ? l : i
  t->sliceToEnd(start)
}

let unsafePop = Js.Array.pop

module Int = {
  let sum = xs => reduce(xs, 0, (a, b) => a + b)
}

module Float = {
  let sum = xs => reduce(xs, 0., (a, b) => a +. b)
}
let clear = t => truncateToLengthUnsafe(t, 0)
