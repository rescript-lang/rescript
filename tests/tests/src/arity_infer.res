let f0 = x =>
  (
    if x > 3 {
      x => x + 1
    } else {
      throw(Not_found)
    }
  )(3)

let f1 = x => (throw(Not_found): _ => _)(x)

let f3 = x =>
  (
    switch x {
    | 0 => x => x + 1
    | 1 => x => x + 2
    | 2 => x => x + 3
    | 3 => x => x + 4
    | _ => throw(Not_found)
    }
  )(3)
