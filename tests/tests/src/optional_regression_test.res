let suites: ref<Mt.pair_suites> = ref(list{})
let test_id = ref(0)
let eq = (loc, x, y) => Mt.eq_suites(~test_id, ~suites, loc, x, y)

@deriving(abstract)
type test = {
  @optional s: string,
  @optional b: bool,
  @optional i: int,
}

let make = (~s=?, ~b=?, ~i=?, ()) => test(~s?, ~b?, ~i?, ())

let hh = make(~s="", ~b=false, ~i=0, ())

eq(__LOC__, hh->sGet, Some(""))
eq(__LOC__, hh->bGet, Some(false))
eq(__LOC__, hh->iGet, Some(0))
Js.log(hh)
Mt.from_pair_suites(__MODULE__, suites.contents)
