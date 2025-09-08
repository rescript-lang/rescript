let suites: ref<Mt.pair_suites> = ref(list{})
let test_id = ref(0)
let eq = (loc, x, y) => {
  incr(test_id)
  suites :=
    list{
      (loc ++ (" id " ++ Js.Int.toString(test_id.contents)), _ => Mt.Eq(x, y)),
      ...suites.contents,
    }
}

let f = x => (x["_003"], x["_50"], x["_50x"], x["__50"], x["__50x"], x["_50x'"], x["x'"])
/* x##_ */ /* TODO: should have a syntax error */

let v = f({
  "_003": 0,
  "_50": 1,
  "_50x": 2,
  "__50": 3,
  "__50x": 4,
  "_50x'": 5,
  "x'": 6,
  /* _  = 6 */
})

eq(__LOC__, (0, 1, 2, 3, 4, 5, 6), v)
Mt.from_pair_suites(__MODULE__, suites.contents)
