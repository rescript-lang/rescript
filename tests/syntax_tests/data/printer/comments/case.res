let rec mergeU = (s1, s2, f) =>
  switch {
    open N
    (toOpt(s1), toOpt(s2))
  } {
  | (None, None) => N.empty
  | (Some(n) /* (Node (l1, v1, d1, r1, h1), _) */, _)
    when {
      open N
      heightGet(n) >=
      switch N.toOpt(s2) {
      | None => 0
      | Some(n) => N.heightGet(n)
      }
    } =>
    let (l1, v1, d1, r1) = {
      open N
      (leftGet(n), keyGet(n), valueGet(n), rightGet(n))
    }
    let (l2, d2, r2) = split(v1, s2)
    N.concatOrJoin(mergeU(l1, l2, f), v1, f(. v1, Some(d1), d2), mergeU(r1, r2, f))
  | (_, Some(n)) /* Node (l2, v2, d2, r2, h2) */ =>
    let (l2, v2, d2, r2) = {
      open N
      (leftGet(n), keyGet(n), valueGet(n), rightGet(n))
    }
    let (l1, d1, r1) = split(v2, s1)
    N.concatOrJoin(mergeU(l1, l2, f), v2, f(. v2, d1, Some(d2)), mergeU(r1, r2, f))
  | _ => assert(false)
  }
