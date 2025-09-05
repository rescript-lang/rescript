let make1 = "hello"->Js.Null_undefined.return
let make2 = Js.Null_undefined.return("hello")

let null1 = Js.Null_undefined.null
let undefined1 = Js.Null_undefined.undefined

let isNullable1 = Js.Null_undefined.null->Js.Null_undefined.isNullable
let isNullable2 = Js.Null_undefined.isNullable(Js.Null_undefined.null)

let map1 = Js.Null_undefined.return(2)->Js.Null_undefined.bind(x => x + 1)
let map2 = Js.Null_undefined.bind(Js.Null_undefined.return(2), x => x + 1)

let forEach1 = Js.Null_undefined.return(2)->Js.Null_undefined.iter(x => ignore(x))
let forEach2 = Js.Null_undefined.iter(Js.Null_undefined.return(2), x => ignore(x))

let fromOption1 = Some("x")->Js.Null_undefined.fromOption
let fromOption2 = Js.Null_undefined.fromOption(None)

let from_opt1 = Some("y")->Js.Null_undefined.from_opt
let from_opt2 = Js.Null_undefined.from_opt(None)

let toOption1 = Js.Null_undefined.return(3)->Js.Null_undefined.toOption
let toOption2 = Js.Null_undefined.toOption(Js.Null_undefined.return(3))

let to_opt1 = Js.Null_undefined.return(4)->Js.Null_undefined.to_opt
let to_opt2 = Js.Null_undefined.to_opt(Js.Null_undefined.return(4))
