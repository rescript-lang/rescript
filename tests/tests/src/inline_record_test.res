let suites: ref<Mt.pair_suites> = ref(list{})

let test_id = ref(0)

let eq = (loc, x, y): unit => Mt.eq_suites(~test_id, loc, ~suites, x, y)

type t0 =
  | A0({lbl: int, more: list<int>})
  | A1({more: list<int>})

let v: t0 = A0({lbl: 3, more: list{}})
let v1 = A1({more: list{1, 2}})

let f = (x: t0) =>
  switch x {
  | A0({lbl, more}) => more->Belt.List.reduceReverse(lbl, \"+")
  | A1({more}) => more->Belt.List.reduceReverse(0, \"+")
  }
eq(__LOC__, f(v), 3)
eq(__LOC__, f(v1), 3)
Js.log(f(v))
Js.log(f(v1))

/* let foo ?(bar= 1) baz = bar + baz */
type t1 =
  | A0({lbl: int, more: list<int>})
  | A1

let v2: t1 = A0({lbl: 3, more: list{}})

type t2 = ..

type t2 += A0({lbl: int, more: list<int>})

let v3: t2 = A0({lbl: 3, more: list{}})

type t3 =
  | A0({lbl: int, more: list<int>})
  | A1

let vvv: t3 = A0({lbl: 3, more: list{}})

eq(
  __LOC__,
  switch v3 {
  | A0({lbl}) => lbl
  | _ => assert(false)
  },
  3,
)

type t4 =
  | A0({mutable x: int, y: int, mutable z: int})
  | A1({mutable z: int})

let ff = (x: t4) =>
  switch x {
  | A0(u) => u.x = u.x + 1
  | A1(u) => u.z = u.z + 2
  }

let v4: t4 = A0({x: 0, y: 0, z: 0})
let v5: t4 = A1({z: 0})

for i in 0 to 10 {
  ff(v4)
  ff(v5)
}

eq(
  __LOC__,
  switch v4 {
  | A0(u) => u.x
  | _ => assert(false)
  },
  11,
)

eq(
  __LOC__,
  switch v5 {
  | A1(u) => u.z
  | _ => assert(false)
  },
  22,
)

exception A4({mutable x: int, y: int, mutable z: int})

let v6: exn = A4({x: 0, y: 0, z: 0})

let ff0 = (x: exn) =>
  switch x {
  | A4(u) =>
    u.x = u.x + 1
    u.z = u.z + 1
  | _ => ()
  }

for i in 0 to 10 {
  ff0(v6)
}

eq(
  __LOC__,
  switch v6 {
  | A4(u) => u.x
  | _ => assert(false)
  },
  11,
)

let ff1 = (x: t1): t1 =>
  switch x {
  | A0(u) => A0({...u, lbl: u.lbl + 1})
  | A1 => A1
  }

let () = Mt.from_pair_suites(__MODULE__, suites.contents)

type emptyRecord = A | B({})

let b = B({})

let () = switch b {
| A => Js.log("A!")
| B({}) => Js.log("B")
}

type r = {y: int}
let r = {y: 10}

switch r {
| {y: 10} => Js.log("10!")
| {} => Js.log("Catch all?")
}
