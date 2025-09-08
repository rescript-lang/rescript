let v = ref(0)

let (reset, incr) = (_ => v := 0, _ => incr(v))

let (reset2, incr2) = {
  let vv = ref(0)
  (() => vv := 0, () => incr(vv))
}

let f = (a, b, d, e) => {
  let (u, v) = {
    let h = a(b)
    (d(h), e(h))
  }
  u + v
}

let kf = (cb, v) => {
  cb(v)
  v + v
}

let ikf = v => kf(ignore, v)
