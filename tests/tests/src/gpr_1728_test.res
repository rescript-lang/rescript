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

@scope("Number") external parseInt: string => int = "parseInt"

let foo = x => parseInt(x) !== 3

let badInlining = obj =>
  if foo(obj["field"]) {
    ()
  }

eq(__LOC__, badInlining({"field": "3"}), ())

eq(__LOC__, parseInt("-13"), -13)
eq(__LOC__, parseInt("+13"), 13)
eq(__LOC__, parseInt("13"), 13)
eq(__LOC__, parseInt("+0x32"), 50)
eq(__LOC__, parseInt("-0x32"), -50)
eq(__LOC__, parseInt("0x32"), 50)
Mt.from_pair_suites(__MODULE__, suites.contents)
