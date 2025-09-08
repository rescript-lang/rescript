@send external map: (array<'a>, 'a => 'b) => array<'b> = "map"
@send external mapi: (array<'a>, ('a, int) => 'b) => array<'b> = "map"

@val external parseInt: string => int = "parseInt"
@val external parseInt_radix: (string, int) => int = "parseInt"

let f = v =>
  if mod(v, 2) == 0 {
    v => v * v
  } else {
    v => v + v
  }

let v = mapi([1, 2, 3], (a, b) => f(a)(b))

let vv = mapi([1, 2, 3], (a, b) => a + b)

let hh = map(["1", "2", "3"], x => parseInt(x))

let u = () => 3

let vvv = ref(0)
let fff = () => {
  /* No inline */
  Js.log("x")
  Js.log("x")
  incr(vvv)
}

let g = () => fff()
/* will be compiled into 
  var g = function () { fff (0)}
  not {[ var g = fff ]}
*/
let abc = (x, y, z) => {
  Js.log("xx")
  Js.log("yy")
  x + y + z
}

let abc_u = (x, y, z) => abc(x, y, z)
/* cool, it will be compiled into 
{[ var absc_u = abc ]}
*/
let () = g()
Mt.from_pair_suites(
  __MODULE__,
  {
    open Mt
    list{
      (__LOC__, _ => Eq(v, [0, 1, 4])),
      (__LOC__, _ => Eq(vv, [1, 3, 5])),
      (__LOC__, _ => Eq(hh, [1, 2, 3])),
      /* __LOC__, (fun _ -> Eq(  
         
         map (map [| 1;2;3|]  ( (fun [@bs] x -> fun y -> x + y))) 
          ( fun [@bs] y -> (y 0)  * (y 1) ), [|2; 6 ; 12|]
      )); */
      /* __LOC__, (fun _ -> Eq(
        mapi [|1;2;3|] (Js.Internal.fn_mk2 (fun x  -> let y =  x * x in fun i -> y + i )), 
        [|1; 5 ; 11|]        
      )) */
    }
  },
)

/* FIXME: */
let bar = fn => fn()
/* let hh0001 = fun%raw a b -> {| a + b|}
 let hh0002 = fun%raw () -> {| console.log ("forgiving arity")|} */
bar(%raw(`function(){console.log("forgiving arity")}`))
