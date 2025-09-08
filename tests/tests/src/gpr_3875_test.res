let result = ref("")
/** TODO: 
  pattern match over (Some \"xx\") could be simplified
*/
module Xx = {
  let log = x => result := x
}

/** TODO: 
  pattern match over (Some \"xx\") could be simplified
*/
let compilerBug = (a, b, c, f) =>
  switch (a, b) {
  | (Some("x"), _)
  | (_, Some("x")) =>
    if f() {
      Xx.log("Some x, f returns true")
    } else {
      Xx.log("Some x, f returns false")
    }
  | _ =>
    if c {
      Xx.log("No x, c is true")
    } else {
      Xx.log("No x, c is false")
    }
  }

let suites: ref<Mt.pair_suites> = ref(list{})
let test_id = ref(0)
let eq = (loc, x, y) => Mt.eq_suites(~test_id, ~suites, loc, x, y)

let _ = {
  compilerBug(Some("x"), None, true, () => true)
  eq(__LOC__, result.contents, "Some x, f returns true")
}

let () = Mt.from_pair_suites(__FILE__, suites.contents)
