- Fix code after `let {a, ...rest} = r` confusing `a` with `r.a`: an
  assignment to `r.a` was lost, a read of `r.a` could return an old value, and
  `a` could change when `r.a` was written later.
