type rec any = Any('a, 'a => string): any

// `@warning` on a let destructuring a GADT and on a case pattern covers the
// pattern's variables; only `unsilenced` is reported.
let get = value => {
  /** Destructures the existential. */
  @warning("-27")
  let Any(x, f) = value
  ignore(f)
  "s"
}

let silenced = o =>
  switch o {
  | @warning("-27") Some(x) => 1
  | None => 0
  }

let unsilenced = o =>
  switch o {
  | Some(y) => 1
  | None => 0
  }
