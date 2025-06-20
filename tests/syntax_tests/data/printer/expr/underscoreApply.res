let x = f(a, _, c)
let x = f(a, g(x, _, z), _)
let x = f(_, x, c)
let x = f(_, _, _)
let x = f(g(_))
let x = f(~a=1, ~b=_, ~c=2)
let x = f(~a=_, ~b=_, ~c=_)
let x = f(~a=_, ~b=_, ~c=g(~x=2, ~y=_, ~z=_))

let nested = x => List.length(_);
let nested2 = (x, y, z) => List.length(_);

let l = [1,2,3]->List.map (i => i+1, _)->List.filter (i => i>0, _);

let l = (i => i+1)->List.map(_, [1,2,3]);

let x = List.length(_);

let incr = (~v) => v+1;

let l1 = [1,2,3]->List.map(incr(~v=_))->List.length;

let l2 = [1,2,3]->List.map(incr(~v =_))->List.length;

let optParam = (~v=?, ()) => v == None ? 0 : 1;

let l1 =
 [Some(1), None, Some(2)]->List.map(optParam(~v=?_, ()))->List.length;

let l2 =
 [Some(1), None, Some(2)]->List.map(optParam(~v =?_, ()))->List.length;

// callback in last position
f(a, b, (a, b) => List.length(_))
// callback in first position
f((a, b) => List.length(_), a, b)

f(a, b, _)(x, y)
-f(a, b, _)
f(a, b, _) + g(x, _, z)
f(a, b, _) + g(x, _, z) + h(alpha, beta, _)

assert(f(a, b, _))

getDirector(a, b, _).name

f(a, b, _) ? g(x, y, _) : h(alpha, beta, _)

<div onClick=f(a, b, _) />
<div> {f(a, b, _)} </div>

f(a, b, _)[ix]
f(a, b, _)[ix] = 2

getDirector(a, b, _).name = "Steve"

filterNone->Array.get(_, 0)
filterNone->Array.get(_, 0)
Array.get(_, 0)
1 + Array.get(_, 0)
Array.get(_, 1) + Array.get(_, 0)
let f = Array.get(_, 0)

let photo = pricedRoom["room"]["photos"]->filterNone->Array.get(_, 0)

underscoreWithComments(
  // Comment 1
  x => {
    // Comment 2
    something()
  },
  _,
)

let status =
    json
    ->optional(field("status", string, _), _)
    ->Option.mapOr(Status.Active, Status.fromString)
