let make1 = "hello"->Js.Null.return
let make2 = Js.Null.return("hello")

let empty1 = Js.Null.empty

let getUnsafe1 = Js.Null.return(1)->Js.Null.getUnsafe
let getUnsafe2 = Js.Null.getUnsafe(Js.Null.return(1))

let getExn1 = Js.Null.return(1)->Js.Null.getExn
let getExn2 = Js.Null.getExn(Js.Null.return(1))

let map1 = Js.Null.return(2)->Js.Null.bind(x => x + 1)
let map2 = Js.Null.bind(Js.Null.return(2), x => x + 1)

let forEach1 = Js.Null.return(2)->Js.Null.iter(x => ignore(x))
let forEach2 = Js.Null.iter(Js.Null.return(2), x => ignore(x))

let fromOption1 = Some("x")->Js.Null.fromOption
let fromOption2 = Js.Null.fromOption(None)

let from_opt1 = Some("y")->Js.Null.from_opt
let from_opt2 = Js.Null.from_opt(None)

let toOption1 = Js.Null.return(3)->Js.Null.toOption
let toOption2 = Js.Null.toOption(Js.Null.return(3))

let to_opt1 = Js.Null.return(4)->Js.Null.to_opt
let to_opt2 = Js.Null.to_opt(Js.Null.return(4))

let test1 = Js.Null.empty->Js.Null.test
let test2 = Js.Null.test(Js.Null.empty)
let test3 = Js.Null.return(5)->Js.Null.bind(v => v)->Js.Null.test

// Type alias migration
let nullT: Js.Null.t<int> = Js.Null.return(1)
