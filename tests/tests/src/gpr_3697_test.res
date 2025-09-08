type rec t<'a> = Fix(Lazy.t<t<'a>>)

let rec fix = () => Fix(Lazy.from_fun(fix))

let rec unfixLeak = (Fix(f)) => unfixLeak(Lazy.force(f))

let unfix = p =>
  while true {
    p :=
      switch p.contents {
      | Fix(h) => Lazy.force(h)
      }
  }
/* ;; unfixLeak (fix ()) */

/* ;; unfix (ref (fix ())) */
