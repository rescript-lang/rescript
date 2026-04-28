/*
type process 

external on : process -> 
  [
  `beforeExit 
  | `exit 
  ] ->  unit Js.fn -> unit = "on" [@@send]


external p : process = "process" [@@val]


external on_hi : process -> 
  [
  `hello
  | `xx
  ] ->  (unit*unit) Js.fn -> unit = "on" [@@send]

type 'a t 

external (!) : 'a t -> 'a = "identity"

let f x = 
  !x # hey 3  + !x # v

let () = 
  on p `exit (Js.Internal.fn_mk0 (fun _ -> prerr_endline "hello world"));
  on_hi p `xx (Js.Internal.fn_mk1 (fun _ -> prerr_endline "hello world"))

*/

let h0 = x => x()
/* {[
     function h0 (x){
         return x ()
       }
   ]}
*/
let h00 = x => x()

let h1 = (x, y) => x(y) /* weird case */
let h10 = x => x(3)

let h30 = x => a => x(3, 3, a)
let h33 = x => x(1, 2, 3)
let h34 = x => x(1, 2, 3)(4)

let ocaml_run = (b, c) => ((x, y, z) => x + y + z)(1, b, c)

let a0 = () => Console.log("hi")
let a1 = () => x => x
let a2 = (x, y) => x + y
let a3 = (x, y, z) => x + y + z
/* let a4 = Js.Internal.fn_mk4 (fun x y z -> let u = x * x + y * y + z * z in fun d -> u + d) */

/* let a44 = Js.Internal.fn_mk4 (fun x y z d -> let u = x * x + y * y + z * z in  u + d) */

/* let b44 () = Js.Internal.fn_mk4 (fun x y z d -> (x,y,z,d)) */
/* polymoprhic restriction */

let test_as: (_ as 'b, 'a => 'a) => 'b = Belt.List.map

let xx: unit => _ => unit = () => _ => Console.log(3)

/* let test_hihi = hihi _ [@bs] */
