let suites: ref<Mt.pair_suites> = ref(list{})
let test_id = ref(0)
let eq = (loc, x, y) => Mt.eq_suites(~test_id, ~suites, loc, x, y)

%%raw("function foo(a){return a()}")

@val("foo") external foo: (unit => int) => int = ""
let fn = () => {
  Js.log("hi")
  1
}

let () = eq(__LOC__, foo(fn), 1)

let () = Mt.from_pair_suites(__FILE__, suites.contents)
