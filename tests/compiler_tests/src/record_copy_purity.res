// A record copy (`{...r, a: 1}`) compiles to an object spread, but reads the
// record's fields like plain data, so an unused copy is dropped. A dict spread
// may run getters and stays.

type r = {a: int, b?: int}

let unusedRecordCopy = (r: r) => {
  let _ = {...r, a: 1}
  r.a
}

let unusedDictSpread = (d: dict<int>) => {
  let _ = dict{...d, "a": 1}
  1
}
