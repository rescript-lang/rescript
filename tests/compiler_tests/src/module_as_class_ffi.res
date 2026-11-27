type t

@new @module external mk: int => t = "xx/foo_class"

let f = () => mk(3)

/* external mk2 : int -> t = "xx/foo_class" [@@new "x"] [@@module] 

File "module_as_class_ffi.ml", line 9, characters 0-69:
conflict attributes found: (new should not carry payload here)
*/
/*
TODO: more error checking
1. [@@module] can only be used once
2. here [new] should not have any payload
3. consolidate all [module] 


let ff () =
  mk2 3  
*/

@module("xx/foo_class") external ff: int => t = "ff"

let v = () => ff(3)
