type t = Foo

module M = {
  type t = Foo
}

let _: t = Foo
open M
let _: t = Foo
