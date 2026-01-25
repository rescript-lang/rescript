let make1 = "hello"->Js.Undefined.return
let make2 = Js.Undefined.return("hello")

let empty1 = Js.Undefined.empty

let getUnsafe1 = Js.Undefined.return(1)->Js.Undefined.getUnsafe
let getUnsafe2 = Js.Undefined.getUnsafe(Js.Undefined.return(1))

let getExn1 = Js.Undefined.return(1)->Js.Undefined.getExn
let getExn2 = Js.Undefined.getExn(Js.Undefined.return(1))

let map1 = Js.Undefined.return(2)->Js.Undefined.bind(x => x + 1)
let map2 = Js.Undefined.bind(Js.Undefined.return(2), x => x + 1)

let forEach1 = Js.Undefined.return(2)->Js.Undefined.iter(x => ignore(x))
let forEach2 = Js.Undefined.iter(Js.Undefined.return(2), x => ignore(x))

let fromOption1 = Some("x")->Js.Undefined.fromOption
let fromOption2 = Js.Undefined.fromOption(None)

let from_opt1 = Some("y")->Js.Undefined.from_opt
let from_opt2 = Js.Undefined.from_opt(None)

let toOption1 = Js.Undefined.return(3)->Js.Undefined.toOption
let toOption2 = Js.Undefined.toOption(Js.Undefined.return(3))

let to_opt1 = Js.Undefined.return(4)->Js.Undefined.to_opt
let to_opt2 = Js.Undefined.to_opt(Js.Undefined.return(4))

let test1 = Js.Undefined.empty->Js.Undefined.test
let test2 = Js.Undefined.test(Js.Undefined.empty)
let test3 = Js.Undefined.return(5)->Js.Undefined.bind(v => v)->Js.Undefined.test

let testAny1 = Js.Undefined.testAny(Js.Undefined.empty)
let testAny2 = Js.Undefined.empty->Js.Undefined.testAny
