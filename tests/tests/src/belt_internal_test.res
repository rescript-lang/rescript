type rec node<'value> = {
  mutable value: 'value,
  mutable height: int,
  mutable left: t<'value>,
  mutable right: t<'value>,
}
and t<'value> = option<node<'value>>

let treeHeight = (n: t<_>) =>
  switch n {
  | None => 0
  | Some(n) => n.height
  }

let rec copy = n =>
  switch n {
  | None => n
  | Some({left: l, right: r, value: v, height: h}) =>
    Some({
      left: copy(l),
      right: copy(r),
      value: v,
      height: h,
    })
  }
