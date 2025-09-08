@@config({
  flags: [
    /* "-drawlambda"; */
    /* "-dlambda"; */
    /* "-dtypedtree"; */
    /* "-bs-diagnose" */

    "-bs-no-cross-module-opt",
  ],
})

module rec A: {
  type t = Leaf(string) | Node(AComparable.t)
  let cmp: (t, t) => int
} = {
  type t = Leaf(string) | Node(AComparable.t)
  let cmp = (t1, t2) =>
    switch (t1, t2) {
    | (Leaf(s1), Leaf(s2)) => Pervasives.compare(s1, s2)
    | (Leaf(_), Node(_)) => 1
    | (Node(_), Leaf(_)) => -1
    | (Node(n1), Node(n2)) => Obj.magic(AComparable.cmp)(n1, n2)
    }
  let hello = x => x
}
and AComparable: Belt.Id.Comparable with type t = A.t = Belt.Id.MakeComparable(A)

module rec X: {} = X

module rec X0: {
  type t
} = {
  type t
}
and Y0: {
  type t
} = {
  type t
}
module type S = {
  let f: int => int
}

module rec X1: S = {
  let f = x => x + 1
}
and Y1: S = {
  let f = x => x + 2
}
