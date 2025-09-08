let suites: ref<Mt.pair_suites> = ref(list{})
let test_id = ref(0)
let eq = (loc, x, y) => Mt.eq_suites(~test_id, ~suites, loc, x, y)

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

let () = {
  eq(__LOC__, a0(rec_cell), 9)
  eq(__LOC__, a0(f0(3)), 9)
}

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

let () = {
  eq(__LOC__, hd(rec_cell2) + hd(tl_exn(rec_cell2)) + hd(tl_exn(tl_exn(rec_cell2))), 9)
  let rec_cell2 = f2(3)
  eq(__LOC__, hd(rec_cell2) + hd(tl_exn(rec_cell2)) + hd(tl_exn(tl_exn(rec_cell2))), 9)
}

let rec rec_cell3 = list{3, ...rec_cell3} /* over variant */

let f3 = x => {
  let rec rec_cell3 = list{x * x - 6, ...rec_cell3} /* over variant */
  rec_cell3
}

let () = {
  eq(
    __LOC__,
    {
      let hd = Belt.List.headExn
      let tl = Belt.List.tailExn
      hd(rec_cell3) + hd(tl(rec_cell3)) + hd(tl(tl(rec_cell3)))
    },
    9,
  )
  let rec_cell3 = f3(3)
  eq(
    __LOC__,
    {
      let hd = Belt.List.headExn
      let tl = Belt.List.tailExn
      hd(rec_cell3) + hd(tl(rec_cell3)) + hd(tl(tl(rec_cell3)))
    },
    9,
  )
}

let () = Mt.from_pair_suites(__FILE__, suites.contents)
