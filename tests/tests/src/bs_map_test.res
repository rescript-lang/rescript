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

let b = (loc, v) => {
  incr(test_id)
  suites :=
    list{(loc ++ (" id " ++ Js.Int.toString(test_id.contents)), _ => Mt.Ok(v)), ...suites.contents}
}

module M = Belt.Map.Int
module N = Belt.Set.Int
module A = Belt.Array

let mapOfArray = x => M.fromArray(x)
let setOfArray = x => N.fromArray(x)
let emptyMap = () => M.empty

let () = {
  let v = A.makeByAndShuffle(1_000_000, i => (i, i))
  let u = M.fromArray(v)
  M.checkInvariantInternal(u)
  let firstHalf = A.slice(v, ~offset=0, ~len=2_000)
  let xx = A.reduce(firstHalf, u, (acc, (x, _)) => M.remove(acc, x))
  M.checkInvariantInternal(u)
  M.checkInvariantInternal(xx)
}

Mt.from_pair_suites(__MODULE__, suites.contents)
