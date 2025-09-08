module F = (A: X, B: Y) => A 
// trailing comma
module F = (A: X, B: Y,) => A 
module F = (@attr1 A: X, @attr2 B: Y) => A 
include (X: Map, Y: Set) => Z
// trailing comma
include (X: Map, Y: Set,) => Z

module F = @functorAttr (A: X, B: Y) => A 
include @functorAttr (X: Map, Y: Set) => Z
include @functorAttr (@functorAttr2 X: Map, @attr3 Y: Set) => Z

// constraint on return type
module Make = (A: X, B: Y) :Set => {let a = A.a + B.b}
// with attributes
module Make = @functorAttr (A: X, B: Y) :Set => {let a = A.a + B.b}

module F = () => Map
module F = @functorAttr () => Map
include () => Map
include @functorAttr () => Map

module Make = (
  Cmp: {
    type t;
    let eq: (t, t) => bool;
  }) : {
 type key = Cmp.t;
 type coll;
 let empty: coll;
 let add: (coll, key) => coll;
} => {
  open Cmp;
  type key = t;
  type coll = list<key>;
  let empty = list{};
  let add = (y: coll, e: key) =>
    if (List.exists(x => eq(x, e), y)) {
      y;
    } else {
      list{e, ...y};
    };
}

module Gen1 = (P: Primitive, ()) => {
  type t = P.t
  type internal = P.t
  let inject = t => t
}

module DistinctString = (()) : StringBased  => {
  type t = string
  let inject = t => t
}

module DistinctString = (): StringBased  => {
  type t = string
  let inject = t => t
}
