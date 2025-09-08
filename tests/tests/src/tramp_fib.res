@@config({flags: ["-w", "a", "-bs-noassertfalse"]})

let suites: ref<Mt.pair_suites> = ref(list{})
let test_id = ref(0)
let eq = (loc, x, y) => Mt.eq_suites(~test_id, ~suites, loc, x, y)

type rec bounce<'a> = Continue('a) | Suspend(unit => bounce<'a>)
/* https://eli.thegreenplace.net/2017/on-recursion-continuations-and-trampolines/ */
/* http://gallium.inria.fr/seminaires/transparents/20141027.Frederic.Bour.pdf */
/* http://www.usrsb.in/blog/blog/2012/08/12/bouncing-pythons-generators-with-a-trampoline/ */
/* http://glat.info/jscheck/tomrec.html */
let rec fib = (n, k) =>
  switch n {
  | 0 | 1 =>
    /* k (Continue 1) [@bs] */
    /* Suspend (fun [@bs]() -> k (Continue 1 ) [@bs]) */
    k(1)
  | _ =>
    Suspend(
      () =>
        fib(n - 1, v0 => fib(n - 2, v1 => k(v0 + v1))),
        /* match v0,v1 with
         | Continue v0, Continue v1 -> */
        /* k (Continue (v0 + v1)) [@bs] */
        /* Suspend (fun [@bs]() -> k (Continue (v0 + v1)) [@bs]) */
        /* | _ -> assert false */
        /* FIXME: this branch completly gone */
    )
  }

let u = fib(10, x => Continue(x))

let rec iter = (bounce: bounce<'a>): 'a =>
  switch bounce {
  | Continue(v) => v
  | Suspend(f) => iter(f())
  }

/* first it needs to be tailcall */
let rec isEven = n =>
  switch n {
  | 0 => Continue(true)
  | 1 => Continue(false)
  | _ => Suspend(() => isOdd(n - 1))
  }
and isOdd = n =>
  switch n {
  | 0 => Continue(false)
  | 1 => Continue(true)
  | _ => isEven(n - 1)
  }
/* Suspend (fun [@bs] () -> isEven (n - 1)) */
eq(__LOC__, iter(u), 89)

eq(__LOC__, isEven(20_000)->iter, true)

Mt.from_pair_suites(__LOC__, suites.contents)
