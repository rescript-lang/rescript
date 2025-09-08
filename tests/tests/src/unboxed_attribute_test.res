let suites: ref<Mt.pair_suites> = ref(list{})
let test_id = ref(0)
let eq = (loc, x, y) => Mt.eq_suites(~test_id, ~suites, loc, x, y)

@unboxed type t = A(int)

let v0 = A(3)

let make = x => A(x)

let get = (A(x)) => x

/* For concrete types */
@unboxed type t1 = A(string)

{
  let x = A("foo")
  eq(
    __LOC__,
    Obj.repr(x),
    Obj.repr(
      switch x {
      | A(s) => s
      },
    ),
  )
}

/* For records */
@unboxed type t2 = {f: string}

{
  let x = {f: "foo"}
  eq(__LOC__, Obj.repr(x), Obj.repr(x.f))
}

/* For inline records */
@unboxed type t3 = B({g: string})

{
  let x = B({g: "foo"})
  eq(
    __LOC__,
    Obj.repr(x),
    Obj.repr(
      switch x {
      | B({g}) => g
      },
    ),
  )
}

type rec r = A(r)
let rec y = A(y)

let () = Mt.from_pair_suites(__FILE__, suites.contents)
