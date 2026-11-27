type rec t<'a> = Fix(Lazy.t<t<'a>>)

let rec fix = () => Fix(Lazy.make(fix))

let rec unfixLeak = (Fix(f)) => unfixLeak(Lazy.get(f))

let unfix = p =>
  while true {
    p :=
      switch p.contents {
      | Fix(h) => Lazy.get(h)
      }
  }
/* ;; unfixLeak (fix ()) */

/* ;; unfix (ref (fix ())) */
