let suites: ref<Mt.pair_suites> = ref(list{})
let test_id = ref(0)
let eq = (loc, x, y) => Mt.eq_suites(~suites, ~test_id, loc, x, y)
let b = (loc, v) => Mt.bool_suites(~suites, ~test_id, loc, v)

type u<'a> = option<'a> =
  private
  | None
  | Some('a)

let f0 = x =>
  switch x {
  | (_, Some(true)) => 1
  | (_, _) => 2
  }

type x = A(int, int) | None

type x0 = Some(int) | None
let f1 = u =>
  switch u {
  | A(_) => 0
  | None => 1
  }

let f2 = (~x=?, ~y: option<int>=?, ~z=3, ()) => {
  Js.log(x)
  switch y {
  | None => 0
  | Some(y) => y + z
  }
}

let f3 = x =>
  switch x {
  | None => 0
  | Some(_) => 1
  }

let f4 = x =>
  switch x {
  | None => 0
  | Some(x) => x + 1
  }

type t<'a> =
  | None
  | Some('a)
let f5 = a => Some(a) == None

let f6 = a => Some(a) != None

let f7 = None

let f8 = Some(None)

let f9 = Some(Some(None))

let f10 = Some(Some(Some(Some(None))))

let f11 = Some(f10)

let f12 = Some(Some(Some(Some(list{(1, 2)}))))

let randomized = ref(false)

let create = (~random=randomized.contents, ()) =>
  if random {
    2
  } else {
    1
  }

let ff = create(~random=false, ())

let f13 = (~x=3, ~y=4, ()) => x + y

let a = f13(~x=2, ())

let f12 = (x: list<_>) => Some(x)

module N = Belt.List

let length_8_id: list<int> = N.makeBy(8, x => x)
let length_10_id: list<int> = N.makeBy(10, x => x)

type xx<'a> = option<'a> =
  | None
  | Some('a)
let f13 = () => N.take(length_10_id, 8) == (Some(list{1, 2, 3}): option<_>)

let () = {
  b(__LOC__, None < Some(Js.null))
  b(__LOC__, !(None > Some(Js.null)))
  b(__LOC__, Some(Js.null) > None)
  b(__LOC__, None < Some(Js.undefined))
  b(__LOC__, Some(Js.undefined) > None)
}

@val
external log3: (
  ~req: @unwrap
  [
    | #String(string)
    | #Int(int)
  ],
  ~opt: @unwrap
  [
    | #String(string)
    | #Bool(bool)
  ]=?,
  unit,
) => unit = "console.log"

let none_arg = None
let _ = log3(~req=#Int(6), ~opt=?none_arg, ())

let ltx = (a, b) => a < b && b > a
let gtx = (a, b) => a > b && b < a
let eqx = (a, b) => a == b && b == a
let neqx = (a, b) => a != b && b != a

let all_true = xs => Belt.List.every(xs, x => x)

b(__LOC__, ...)(all_true(list{gtx(Some(Some(Js.null)), Some(None))}))

b(__LOC__, ...)(
  all_true(list{
    ltx(Some(None), Some(Some(3))),
    ltx(Some(None), Some(Some(None))),
    ltx(Some(None), Some(Some("3"))),
    ltx(Some(None), Some(Some(true))),
    ltx(Some(None), Some(Some(false))),
    ltx(Some(false), Some(true)),
    ltx(Some(Some(false)), Some(Some(true))),
    ltx(None, Some(None)),
    ltx(None, Some(Js.null)),
    ltx(None, Some(x => x)),
    ltx(Some(Js.null), Some(Js.Null.return(3))),
  }),
)

b(__LOC__, ...)(
  all_true(list{
    eqx(None, None),
    neqx(None, Some(Js.null)),
    eqx(Some(None), Some(None)),
    eqx(Some(Some(None)), Some(Some(None))),
    neqx(Some(Some(Some(None))), Some(Some(None))),
  }),
)

module N0 = {
  type record = {x: int, mutable y: string}
  type t =
    | None
    | Some(record)

  let v = (x: record): t => Some(x)
  let v0 = (x: record): option<record> => Some(x)
  /* [v] and [v0] should be just an identity function */
}
Mt.from_pair_suites(__MODULE__, suites.contents)
