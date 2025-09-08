type rec stack
  | Empty

// name cannot contain module access paths
type Foo.bar = string

// missing type
type t =

// missing type
type state =

// prevent last error
;

// The name must start with a lowercase
type T1 = D1

type M.T2 += D2

type M1.M2.T3 += D3

type T3 += Tid: Tid.t<t>

type T4<_> = D4

type M1.M2.T5<_> += D5

type X.Y z += D6
