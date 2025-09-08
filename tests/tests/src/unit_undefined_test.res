@@config({
  flags: [
    /* "-bs-diagnose" */
    /* ; "-drawlambda" */
    /* ; "-dtypedtree" */
  ],
})

let suites: ref<Mt.pair_suites> = ref(list{})
let test_id = ref(0)
let eq = (loc, x, y) => Mt.eq_suites(~test_id, ~suites, loc, x, y)

@val external hi: (unit => unit) => int = "hi"

let f_01 = () =>
  hi((() as x) =>
    if x == () {
      Js.log("x")
    }
  ) /* FIXME: not inlined */

let u = x =>
  switch () {
  | () if x > 3 => 1
  | () if x < 2 => 2
  | () if x > 4 => 0
  | () => 3
  }

let fx = () => ()

let u0 = (x: unit) => Some(x)

let u1 = Some()
type t = unit

let u2 = (x: t) => Some(x)
let u3: option<t> = Some()
let u4: t = ()

eq(__LOC__, u0(), Obj.magic(Some(None)))
eq(__LOC__, u1, Obj.magic(Some(None)))
eq(__LOC__, u2(), Obj.magic(Some(None)))
eq(__LOC__, u3, Obj.magic(Some(None)))
eq(__LOC__, u4, Obj.magic(None))

Mt.from_pair_suites(__FILE__, suites.contents)
