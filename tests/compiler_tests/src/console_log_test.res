@val @scope("Math") external min_int: (int, int) => int = "min"

/* ATTENTION: only built-in runtime would simplify it
   as
   {[
     var min_int = Math.min
   ]}
   otherwise it has to be expanded as
   {[
     var min_int = function(x,y){
       return Math.min(x,y)
     }
   ]}
   There are other things like [@send] which does not like eta reduction
   
*/
let min_int = min_int

type t
@send external say: (t, int) => int = "say"

let say = say

@@warning("-102")
let v = Pervasives.compare
