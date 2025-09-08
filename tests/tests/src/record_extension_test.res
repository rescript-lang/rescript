let suites: ref<Mt.pair_suites> = ref(list{})
let test_id = ref(0)
let eq = (loc, x, y) => Mt.eq_suites(~test_id, ~suites, loc, x, y)

/* Record_extension */
type t0 = ..
type t0 += Inline_record({x: int, y: string})
type t0 += SinglePayload(string) | TuplePayload(int, string)

let f = x =>
  switch x {
  | Inline_record({x, y}) =>
    switch Belt.Int.fromString(y) {
    | Some(y) => Some(x + y)
    | _ => None
    }
  | SinglePayload(v) => Belt.Int.fromString(v)
  | TuplePayload(v0, v1) =>
    switch Belt.Int.fromString(v1) {
    | Some(v1) => Some(v0 + v1)
    | _ => None
    }
  | _ => None
  }

eq(__LOC__, f(Inline_record({x: 3, y: "4"})), Some(7))
eq(__LOC__, f(SinglePayload("1")), Some(1))
eq(__LOC__, f(TuplePayload(1, "2")), Some(3))

/* Record_unboxed */
type t1 = | @unboxed A({x: int})

/* Record_inlined */
type t2 =
  | B
  | C({x: int, y: string})
  | D({w: int})
let f2 = x =>
  switch x {
  | D(_)
  | B => 0

  | C({x}) => x
  }

let f2_with = x =>
  switch x {
  | D(_)
  | B => x
  | C(u) => C({...u, x: 0})
  }

exception A({name: int, x: int})
exception B(int, int)
exception C({name: int})

let u = f =>
  try f() catch {
  | A({name, x}) => name + x
  | B(a, b) => a + b
  | C(x) => x.name
  | _ => -1
  }

eq(__LOC__, u(() => throw(A({name: 1, x: 1}))), 2)
eq(__LOC__, u(() => throw(B(1, 2))), 3)
eq(__LOC__, u(() => throw(C({name: 4}))), 4)

let () = Mt.from_pair_suites(__LOC__, suites.contents)
