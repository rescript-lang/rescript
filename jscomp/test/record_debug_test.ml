
(* [@@@bs.config {flags = [|"-dsource"|]}] *)
let suites :  Mt.pair_suites ref  = ref []
let test_id = ref 0
let eq loc x y = Mt.eq_suites ~test_id ~suites loc x y 

type t = {
  a : int option; 
  b : y 
}
and y = {
  xx : int ; 
  yy : int 
}




let v = { a = Some 3 ; b = {xx = 2; yy = 3}}

let u = {v with a = Some 2}


let h = [1;2;3;4]

exception A of int 

type u = ..

type u += B of int * int 

let v0 = A 3 
let v1 = B (3,2)
let v2 = `C 2 
let v3 = `C (2,3)

module N = struct 
  let a = 0 
  let b = 1  
  external f : int -> int = "%identity"
end

module N0 : sig 
  val a : int 
  val b : int 
  val f : int -> int 
end = struct 
    let a = 0 
  let b = 1  
  external f : int -> int = "%identity"
end

;; Js.log2 "hei" v

let a, c =  (1,2,2,4,3) , [|1;2;3;4;5|]
;; Js.log2 a c

let%private i = 3
let%private a = 
  {js||js},{js|a|js} 
;;


eq __LOC__ a ("","a")
;;
Mt.from_pair_suites __FILE__ !suites