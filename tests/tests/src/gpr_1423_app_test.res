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

let foo = f => Js.log(f(~a1="a1", ()))

let _ = foo(Gpr_1423_nav.busted(~a2="a2", ...))

let foo2 = f => f(~a1="a1", ())

let () = eq(__LOC__, foo2(Gpr_1423_nav.busted(~a2="a2", ...)), "a1a2")

let () = Mt.from_pair_suites(__MODULE__, suites.contents)
