module rec A: {
  type t

  @send external child: t => B.t = "child"
} = A

and B: {
  type t

  @send external parent: t => A.t = "parent"
} = B

module C = {
  type t

  @send external createA: t => A.t = "createA"
}

module MC = C
//          ^hov
module MA = A
//          ^hov
