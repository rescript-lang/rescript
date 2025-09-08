module(Three)
module(Three: X_int)

let three = module(Three: X_int)
let three = module(Three)

let x = @attr module(Foo)
let x = @attr module(Foo: FirstClass)

module(Teenager)[0]
module(Teenager)->age->Js.log
module(Teenager)[0]->Js.log
module(Teenager)->age->isAdult ? Js.log("has responsibilities")  : Js.log("can play in the playground")
module(Streets)[0]->isExpensive ? Js.log("big money") : Js.log("affordable")


let () = {
  module(Teenager)->age->Js.log
}
let () = {
  module(Teenager)[0]
}
let () = {
  module(Teenager)->age->isAdult ? Js.log("has responsibilities")  : Js.log("can play in the playground")
}
let () = {
  module(Streets)[0]->isExpensive ? Js.log("big money") : Js.log("affordable")
}

let () = {
  let a = 1
  let b = 2
  module(Teenager)[0]
  module(Teenager)->age->Js.log
}


let () = {
  let a = 1
  let b = 2
  module(Teenager)->age->Js.log
  module(Teenager)[0]->age->isAdult ? Js.log("has responsibilities")  : Js.log("can play in the playground")
}
