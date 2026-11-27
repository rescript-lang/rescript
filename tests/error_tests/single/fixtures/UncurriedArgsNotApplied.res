let apply = (fn: unit => option<int>) => fn()

let _ = apply(Some(1))
