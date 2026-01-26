open Mocha
open Test_utils

type rec cell<'a> = {
  content: 'a,
  mutable next: cell<'a>,
}

let rec rec_cell = {
  content: 3,
  next: rec_cell,
} /* over records */

let f0 = x => {
  let rec rec_cell = {
    content: x * x - 6,
    next: rec_cell,
  }
  rec_cell
}

let a0 = x => x.content + x.next.content + x.next.next.content

type rec cell2 =
  | Nil
  | Cons({content: int, mutable next: cell2})

let rec rec_cell2 = Cons({content: 3, next: rec_cell2})
/* over inline records */
let f2 = x => {
  let rec rec_cell2 = Cons({content: x * x - 6, next: rec_cell2})
  rec_cell2
}

let hd = x =>
  switch x {
  | Nil => 0
  | Cons(x) => x.content
  }

let tl_exn = x =>
  switch x {
  | Nil => assert(false)
  | Cons(x) => x.next
  }

let rec rec_cell3 = list{3, ...rec_cell3} /* over variant */

let f3 = x => {
  let rec rec_cell3 = list{x * x - 6, ...rec_cell3} /* over variant */
  rec_cell3
}

describe(__MODULE__, () => {
  test("recursive record operations", () => {
    eq(__LOC__, a0(rec_cell), 9)
    eq(__LOC__, a0(f0(3)), 9)
  })

  test("recursive inline record operations", () => {
    eq(__LOC__, hd(rec_cell2) + hd(tl_exn(rec_cell2)) + hd(tl_exn(tl_exn(rec_cell2))), 9)
    let rec_cell2 = f2(3)
    eq(__LOC__, hd(rec_cell2) + hd(tl_exn(rec_cell2)) + hd(tl_exn(tl_exn(rec_cell2))), 9)
  })

  test("recursive variant list operations", () => {
    let hd = Belt.List.headExn
    let tl = Belt.List.tailExn
    eq(__LOC__, hd(rec_cell3) + hd(tl(rec_cell3)) + hd(tl(tl(rec_cell3))), 9)
    let rec_cell3 = f3(3)
    eq(__LOC__, hd(rec_cell3) + hd(tl(rec_cell3)) + hd(tl(tl(rec_cell3))), 9)
  })
})
