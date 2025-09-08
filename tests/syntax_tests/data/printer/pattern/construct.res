let () = 1 

let Constructor = 1

let Foo() = 1
let Foo(()) = 1

let Rgb(r, g, b) = 1

let Rgb(rrrrrrrrrrrrrrrrrrrrrrrrr, ggggggggggggggggggggggggg, bbbbbbbbbbbbbbbbbbbbbbbbb) = 1
let Units((), (), ()) = 1

switch truth {
| true => Js.log("true")
| false => Js.log("false")
}

switch sphere->intersect(~ray) {
| None => assert(false)
| Some((x, y)) => assert(true)
}


// huggable patterns
let Foo({superLongIdent: x, superLongIdent2: y, superLongIdent3: z, superLongIdent4: a}) = x 
let Foo((superLongIdent, superLongIdent2, superLongIdent, superLongIdent4, superLongIdent5)) = x 
let Foo([superLongIdent, superLongIdent2, superLongIdent, superLongIdent4, superLongIdent5]) = x 
let Foo(list{superLongIdent, superLongIdent2, superLongIdent, superLongIdent4, superLongIdent5}) = x 
