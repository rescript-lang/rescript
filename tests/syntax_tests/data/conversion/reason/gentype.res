module M: {
  @genType @after
  type t

  @genType @after
  let x: int

  @foo
  type e = ..
} = {
  type t
  let x = 34
  type e = ..
}

module type MT = {
  @genType @after
  type t

  @genType @after
  let x: int

  @foo
  type e = ..
}

@genType("ddd")
let x = 42
