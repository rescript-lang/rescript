type rec any = First('a): any | Second('a): any

let get = (v, w) =>
  switch v {
  | First(x) =>
    module M = {
      let inner = switch w {
      | Second(y) => ignore(y)
      | First(_) => ()
      }
    }
    x
  | Second(_) => assert(false)
  }
