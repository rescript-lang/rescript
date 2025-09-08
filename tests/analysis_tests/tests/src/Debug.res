// turn on by adding this comment // ^db+

let _ = ShadowedBelt.List.map
//                         ^def

open Js
module Before = {
  open Belt
  let _ = Id.getCmpInternal
}
module Inner = {
  // eqN
  //    ^com
  open List
  let _ = map
}
// ^db-
