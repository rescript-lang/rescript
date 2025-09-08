let l = list{1, 2, 3}->List.map (i => i+1, _)->List.filter(i => i > 0, _)

let l = (i => i+1)->List.map(_, list{1,2,3})

let x = List.length(_)

let nested = x => List.length(_)

let incr = (~v) => v+1

let l1 = list{1, 2, 3}->List.map(incr(~v=_))->List.length

let l2 = list{1, 2, 3}->List.map(incr(~v=_))->List.length

let optParam = (~v=?, ()) => v == None ? 0 : 1

let l1 =
  list{Some(1), None, Some(2)}->List.map(optParam(~v=?_, ()))->List.length

let l2 =
  list{Some(1), None, Some(2)}->List.map(optParam(~v=?_, ()))->List.length

underscoreWithComments(
  // Comment 1
  x => {
    // Comment 2
    something()
  },
  _,
)
