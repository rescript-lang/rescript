let p1 = Js.Promise.resolve(1)
let p2 = Js.Promise.reject(Failure("err"))

let all1 = Js.Promise.all([Js.Promise.resolve(1), Js.Promise.resolve(2)])
let all2 = Js.Promise.all2((Js.Promise.resolve(1), Js.Promise.resolve(2)))
let all3 = Js.Promise.all3((Js.Promise.resolve(1), Js.Promise.resolve(2), Js.Promise.resolve(3)))
let all4 = Js.Promise.all4((
  Js.Promise.resolve(1),
  Js.Promise.resolve(2),
  Js.Promise.resolve(3),
  Js.Promise.resolve(4),
))
let all5 = Js.Promise.all5((
  Js.Promise.resolve(1),
  Js.Promise.resolve(2),
  Js.Promise.resolve(3),
  Js.Promise.resolve(4),
  Js.Promise.resolve(5),
))
let all6 = Js.Promise.all6((
  Js.Promise.resolve(1),
  Js.Promise.resolve(2),
  Js.Promise.resolve(3),
  Js.Promise.resolve(4),
  Js.Promise.resolve(5),
  Js.Promise.resolve(6),
))

let race1 = Js.Promise.race([Js.Promise.resolve(10), Js.Promise.resolve(20)])

// let thenPipe = Js.Promise.resolve(1)->Js.Promise.then_(x => Js.Promise.resolve(x + 1), _)
// let thenDirect = Js.Promise.then_(x => Js.Promise.resolve(x + 1), Js.Promise.resolve(1))

// Type alias migration
external p: Js.Promise.t<int> = "p"

// let catchPipe = Js.Promise.resolve(1)->Js.Promise.catch(_e => Js.Promise.resolve(0), _)
// let catchDirect = Js.Promise.catch(_e => Js.Promise.resolve(0), Js.Promise.resolve(1))
let make1 = Js.Promise.make((~resolve, ~reject) => resolve(1))
