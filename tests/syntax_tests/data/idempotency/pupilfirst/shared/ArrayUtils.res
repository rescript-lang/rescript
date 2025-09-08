exception UnsafeFindFailed(string)

let copyAndSort = (f, t) => {
  let cp = t->Array.copy
  cp->Array.sort(f)
  cp
}

let copyAndPush = (e, t) => {
  let copy = t->Array.copy
  copy->Js.Array.push(e)->ignore
  copy
}

let isEmpty = a =>
  switch a {
  | [] => true
  | _ => false
  }

let isNotEmpty = a => !(a->isEmpty)

let unsafeFind = (p, message, l) =>
  switch Js.Array.find(p, l) {
  | Some(e) => e
  | None =>
    Rollbar.error(message)
    Notification.error(
      "An unexpected error occurred",
      "Our team has been notified about this error. Please try reloading this page.",
    )
    throw(UnsafeFindFailed(message))
  }

let flatten = t => t->Array.to_list->List.flatten->Array.of_list

let distinct = t => t->Array.to_list->ListUtils.distinct->Array.of_list

let sort_uniq = (f, t) => t->Array.to_list->List.sort_uniq(f)->Array.of_list

let getOpt = (a, i) =>
  try Some(a->Array.get(i)) catch {
  | Not_found => None
  }

let swapUp = (i, t) =>
  if i <= 0 || i >= (t->Array.length) {
    Rollbar.warning("Index to swap out of bounds in array!")
    t
  } else {
    let copy = t->Array.copy
    copy[i] = t[i - 1]
    copy[i - 1] = t[i]
    copy
  }

let swapDown = (i, t) => swapUp(i + 1, t)
