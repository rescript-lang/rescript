exception Local(int)

let f = () => throw(Local(3))

let g = () => throw(Not_found)

let h = () => throw(Test_common.U(3))
let x = () => throw(Test_common.H)

let xx = () => throw(Invalid_argument("x"))

exception Nullary

let a = Nullary
