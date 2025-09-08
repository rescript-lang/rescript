@@uncurried

// test.res
type rec tt = [
  | #List(list<tt>)
]
type sexp = tt

/* {2 Serialization (encoding)} */

let rec expr_starting_with = (c, k, t) =>
  switch c {
  | '(' => expr_list(list{}, k, t)
  | c => atom(k, t)
  }

/* parse list */
and expr_list = (acc, k, t) => {
  switch assert(false) {
  | ')' => k(None, #List(acc))
  | c =>
    expr_starting_with(
      c,
      (last, e) =>
        switch last {
        | _ => expr_list(list{e, ...acc}, k, t)
        },
      t,
    )
  }
}
/* parse atom */
and atom = (k, t) => {
  let _ = atom(k)
  assert(false)
}
