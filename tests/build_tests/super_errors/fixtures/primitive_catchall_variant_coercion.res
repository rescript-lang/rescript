module A = {
  @tag("kind")
  type t = | @catch(string) Other({kind: string, value: int})
}

module B = {
  @tag("kind")
  type t = Other({value: int})
}

let f = (x: A.t): B.t => (x :> B.t)
