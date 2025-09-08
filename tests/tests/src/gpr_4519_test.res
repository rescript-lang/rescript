let suites: ref<Mt.pair_suites> = ref(list{})
let test_id = ref(0)
let eq = (loc, x, y) => Mt.eq_suites(~test_id, ~suites, loc, x, y)

type t =
  | Required
  | Optional

let nextFor = (x: option<t>) =>
  switch x {
  | Some(Required) => Some(Optional)
  | Some(Optional) => None
  | None => Some(Required)
  }

eq(__LOC__, nextFor(Some(Required)), Some(Optional))
let () = Mt.from_pair_suites(__MODULE__, suites.contents)
