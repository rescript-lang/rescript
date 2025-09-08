type t<'a> = {
  x: 'a,
  y: 'a,
}
module Common = {
  let create = (x, y) => {x: x, y: y}
  let toPair = ({x, y}) => (x, y)
  let fromPair = ((x, y)) => {x: x, y: y}
  let map = (~f, {x, y}) => {x: f(x), y: f(y)}
}
include Common

module Int = {
  include Common
  type t = t<int>
  let zero = {x: 0, y: 0}
  let add = ({x: x1, y: y1}, {x: x2, y: y2}) => {x: x1 + x2, y: y1 + y2}
  let sub = ({x: x1, y: y1}, {x: x2, y: y2}) => {x: x1 - x2, y: y1 - y2}
  let addScalar = ({x, y}, s) => {x: x + s, y: y + s}
  let divScalar = ({x, y}, s) => {x: x / s, y: y / s}
  let neg = x => sub(zero, x)
  let mag = ({x, y}) => sqrt(float_of_int(x * x + y * y))
  let string = t => Printf.sprintf("{x:%d,y:%d}", t.x, t.y)
  let print = t => print_endline(string(t))
  let ofFloatPt = t => map(t, ~f=int_of_float)
  let \"+" = add
  let \"-" = sub
  let \"/@" = divScalar
  let \"+@" = addScalar
}

module Float = {
  include Common
  type t = t<float>
  let zero = {x: 0., y: 0.}
  let add = ({x: x1, y: y1}, {x: x2, y: y2}) => {x: x1 +. x2, y: y1 +. y2}
  let sub = ({x: x1, y: y1}, {x: x2, y: y2}) => {x: x1 -. x2, y: y1 -. y2}
  let addScalar = ({x, y}, s) => {x: x +. s, y: y +. s}
  let multScalar = ({x, y}, s) => {x: x *. s, y: y *. s}
  let divScalar = ({x, y}, s) => {x: x /. s, y: y /. s}
  let neg = x => sub(zero, x)
  let mag = ({x, y}) => sqrt(x *. x +. y *. y)
  let string = t => Printf.sprintf("{x:%f,y:%f}", t.x, t.y)
  let print = t => print_endline(string(t))
  let ofIntPt = t => map(t, ~f=float_of_int)
  let \"+" = add
  let \"-" = sub
  let \"/@" = divScalar
  let \"+@" = addScalar
  let \"*@" = multScalar
}
