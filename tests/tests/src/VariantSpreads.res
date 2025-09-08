module S = {
  type x = Foo | Bar
  type s = Five(int) | Six
}

type a = One(bool, S.x) | Two

type b = | ...a | Three | Four | ...S.s

let b1: b = Two
let b2: b = One(true, Bar)

let c: b = Five(2)

let ddd: b = Six

type f = One({name: string, age?: int}) | Two
type q = | ...f | Three

let q: q = One({name: "hello"})
