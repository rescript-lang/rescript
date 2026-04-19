let flag = true
let useValue = (~value=?, ()) => value

let direct = useValue(~value=?Some(1), ())
let nested = useValue(
  ~value=?Some(
    if flag {
      1
    } else {
      2
    },
  ),
  (),
)
let fromComputation = useValue(~value=?Some(String.length("abc")), ())
