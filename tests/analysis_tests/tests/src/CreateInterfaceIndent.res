// ^int

module Make = (X: {type t}) => {
  type u = X.t
}

module M = {type t = int}

module N = {type r = {
  a: int,
  b: string,
}}

module P = {
  @val external now: unit => float = "Date.now"
  type v =
    | A
    | B
}

module Q = {
  type w = {
    @as("first
second") field: int,
  }
  @module("a
b") external multi: unit => unit = "default"
  type x = {
    @as(`one
  two`) other: int,
  }
}
