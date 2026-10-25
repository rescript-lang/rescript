// A finally block without catch handles no exception
let throwsThroughFinally = () =>
  try throw(Not_found) finally {
    ()
  }

let throwsInFinally = () =>
  try () catch {
  | _ => ()
  } finally {
    throw(Not_found)
  }

let catchesBeforeFinally = () =>
  try throw(Not_found) catch {
  | Not_found => ()
  } finally {
    ()
  }
