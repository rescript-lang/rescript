

val h1 : < p : 'a; .. > -> 'a
val h3 : < hi : int -> int -> 'a; .. > -> 'a
val g5 : < hi : int [@bs.set]; .. > -> unit
val h5 : < hi : int [@bs.set]; .. > -> unit
(* The inferred type is 
   val h5 : < hi#= : (int -> unit [@bs.meth]); .. > -> unit
   We should propose the rescript syntax:
   { mutable "hi" : int  }
*)
val h6 : < p : 'a; .. > -> 'a
val h7 : < m : (int -> int -> 'a [@bs]); .. > -> 'a
val h8 : < hi : int -> int -> 'a; .. > -> 'a

val chain_g : < x : < y : < z : int > > > -> int 