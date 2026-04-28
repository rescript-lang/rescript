@@warning("-22")
/* [@@@config no_export] */
/* let for_each n =
  ([%raw{|
	for (var key in n){
      console.log(key)
    }
  |}] : unit ); () */

let sum: (int, int) => int = %raw(`(a,b) => a + b
`)

let v = sum(1, 2)

let f = a => a + %raw(`3`)

let b = f(1)
let c = f(2)

let forIn = %raw(`(o,foo)=> {
  for (var i in o){
    foo(o)
  }
  }`)

let forIn: ('a, string => unit) => unit = forIn

/*
let%raw forIn : 'a -> (string -> unit [@bs]) -> unit = fun o foo -> {|
  for (var i in o){
    foo(o)
  }
|}
*/
module N: {
  let log2: string => unit
} = {
  let log = x => Console.log(x)

  let log2: 'a => unit = log
}

/* let log : 'a -> unit = fun%raw x -> {|console.log (x)|} */

forIn({"x": 3}, x => Console.log(x))
forIn({"x": 3, "y": 3}, x => Console.log(x))

let f3: unit => bool = %raw("()=>true")

let bbbb = f3()

assert(bbbb)
