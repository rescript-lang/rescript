module Inner = {
  type t<'a>
}

module A = Inner
let m = (x: A.t<[#X]>) => (x :> A.t<[#X | #Y]>)
