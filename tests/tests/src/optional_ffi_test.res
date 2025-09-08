let suites: ref<Mt.pair_suites> = ref(list{})
let test_id = ref(0)
let eq = (loc, (x, y)) => {
  incr(test_id)
  suites :=
    list{
      (loc ++ (" id " ++ Js.Int.toString(test_id.contents)), _ => Mt.Eq(x, y)),
      ...suites.contents,
    }
}

%%raw(`
function hey(x, y) {
    if (x === void 0) { x = 3; }
    return x + y;
  }
`)
@val external xx: (~x: int=?, ~y: int, unit) => int = "hey"

let u = xx(~y=3, ())

let z = xx(~x=2 + 3, ~y=3, ())

let () = eq(__LOC__, ((u, z), (6, 8)))

let counter = ref(0)
let side_effect = x => {
  incr(x)
  x.contents
}

let bug_to_fix = (f, x) => xx(~x=f(x), ~y=3, ()) /* : [f x] is done once */

let bug_to_fix2 = (f, x) => xx(~x=?f(x), ~y=3, ()) /* : [f x] is done once */

let counter2 = ref(0)
let side_effect2 = x => {
  incr(x)
  Some(x.contents)
}

let () = {
  let v = bug_to_fix(side_effect, counter)
  let pair = ((v, counter.contents), (4, 1))
  let v2 = bug_to_fix2(side_effect2, counter2)
  let pair2 = ((v2, counter.contents), (4, 1))
  /* Js.log (pair,pair2) */
  eq(__LOC__, pair)
  eq(__LOC__, pair2)
}

%%raw(`
function heystr(x, y) {
    if (x === void 0) { x = "3"; }
    return x + y;
  }
  `)

@val external kk: (~name: string=?, string) => string = "heystr"

let () = {
  let pair = ("name4", kk(~name="name", "4"))
  /* Js.log pair ; */
  eq(__LOC__, pair)
}

Mt.from_pair_suites(__MODULE__, suites.contents)
