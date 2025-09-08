open Js
let (aa, bb, cc) = (eqNull, eqUndefined, eqNullable)

let suites = ref(list{})
let test_id = ref(0)

let eq = (loc, x, y) => Mt.eq_suites(~suites, ~test_id, loc, x, y)
let b = (loc, x) => Mt.bool_suites(~suites, ~test_id, loc, x)

let f = () => None
let shouldBeNull = () => Js.null

let () = {
  b(__LOC__, !eqNull(3, Js.null))
  b(__LOC__, !eqNull(None, Js.null))
  b(__LOC__, !eqNull("3", Js.null))
  b(__LOC__, !eqNull('3', Js.null))
  b(__LOC__, !eqNull(0, Js.null))
  b(__LOC__, !eqNull(0., Js.null))
  b(__LOC__, !eqNull(f(), Js.null))
  b(__LOC__, eqNull(shouldBeNull(), Js.null))
  b(__LOC__, !eqNull(1, Js.Null.return(3)))
  b(__LOC__, eqNull(None, Js.Null.return(None)))
  b(__LOC__, !eqNull(Some(3), Js.Null.return(None)))
}

let () = {
  let v = Nullable.null
  b(__LOC__, !eqNullable(3, v))
  b(__LOC__, !eqNullable(None, v))
  b(__LOC__, !eqNullable("3", v))
  b(__LOC__, !eqNullable('3', v))
  b(__LOC__, !eqNullable(0, v))
  b(__LOC__, !eqNullable(0., v))
  b(__LOC__, !eqNullable(f(), v))
  b(__LOC__, eqNullable(shouldBeNull(), v))
  b(__LOC__, !eqNullable(1, Nullable.return(3)))
  b(__LOC__, eqNullable(None, Nullable.return(None)))
  b(__LOC__, !eqNullable(Some(3), Nullable.return(None)))
}

let () = {
  let v = Undefined.empty

  b(__LOC__, !eqUndefined(3, v))
  b(__LOC__, eqUndefined(None, v))
  b(__LOC__, !eqUndefined("3", v))
  b(__LOC__, !eqUndefined('3', v))
  b(__LOC__, !eqUndefined(0, v))
  b(__LOC__, !eqUndefined(0., v))
  b(__LOC__, eqUndefined(f(), v))
  /* [ None === undefined]
     [ None === Js.Undefined.return None]
 */
  b(__LOC__, !eqUndefined(shouldBeNull(), v))
  b(__LOC__, !eqUndefined(1, Undefined.return(3)))
  b(__LOC__, eqUndefined(None, Undefined.return(None)))
  b(__LOC__, !eqUndefined(Some(3), Undefined.return(None)))
}

Mt.from_pair_suites(__LOC__, suites.contents)
