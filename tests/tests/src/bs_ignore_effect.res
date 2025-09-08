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

%%raw(`
function add(x,y){
  return x + y
}
`)
type rec kind<_> =
  | Float: kind<float>
  | String: kind<string>
@val external add: (@ignore kind<'a>, 'a, 'a) => 'a = "add"

let v = ref(0)

@obj external config: (~hi: int, ~lo: int, unit) => _ = ""

let h = config(~hi=2, ~lo=0, ignore(incr(v)))
let z = add(
  {
    incr(v)
    Float
  },
  3.0,
  2.0,
)

let () = {
  eq(__LOC__, v.contents, 2)
  eq(__LOC__, z, 5.0)
}

let () = Mt.from_pair_suites(__MODULE__, suites.contents)
