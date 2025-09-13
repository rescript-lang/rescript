let p1 = Js.Promise2.resolve(1)
let p2 = Js.Promise2.reject(Failure("err"))

let all1 = Js.Promise2.all([Js.Promise2.resolve(1), Js.Promise2.resolve(2)])
let all2 = Js.Promise2.all2((Js.Promise2.resolve(1), Js.Promise2.resolve(2)))
let all3 = Js.Promise2.all3((
  Js.Promise2.resolve(1),
  Js.Promise2.resolve(2),
  Js.Promise2.resolve(3),
))

let all4 = Js.Promise2.all4((
  Js.Promise2.resolve(1),
  Js.Promise2.resolve(2),
  Js.Promise2.resolve(3),
  Js.Promise2.resolve(4),
))
let all5 = Js.Promise2.all5((
  Js.Promise2.resolve(1),
  Js.Promise2.resolve(2),
  Js.Promise2.resolve(3),
  Js.Promise2.resolve(4),
  Js.Promise2.resolve(5),
))
let all6 = Js.Promise2.all6((
  Js.Promise2.resolve(1),
  Js.Promise2.resolve(2),
  Js.Promise2.resolve(3),
  Js.Promise2.resolve(4),
  Js.Promise2.resolve(5),
  Js.Promise2.resolve(6),
))

let race1 = Js.Promise2.race([Js.Promise2.resolve(10), Js.Promise2.resolve(20)])

let thenPipe = Js.Promise2.resolve(1)->Js.Promise2.then(x => Js.Promise2.resolve(x + 1))
let thenDirect = Js.Promise2.then(Js.Promise2.resolve(1), x => Js.Promise2.resolve(x + 1))

// Type alias migration
external p2: Js.Promise2.t<int> = "p2"

let catchPipe = Js.Promise2.resolve(1)->Js.Promise2.catch(_e => Js.Promise2.resolve(0))
let catchDirect = Js.Promise2.catch(Js.Promise2.resolve(1), _e => Js.Promise2.resolve(0))
let make1 = Js.Promise2.make((~resolve, ~reject) => resolve(1))

let _ = p2->Js.Promise2.then(x => Js.Promise2.resolve(x + 1))
