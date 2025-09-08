let f_list = x =>
  switch x {
  | list{a0, a1, a2, a3, a4, a5, ..._} => a0 + a1 + a2 + a3 + a4 + a5
  | _ => 0
  }

/*
if(!match) {return 0}
else {
   var match$1 = match[1]
   if (!match$1) {return 0}
}


Ideal code output:
{[
var match, match$1, match$2, match$3, match$4

if(x && (match$1 = match[1]) && (match$2 = match$1[1]) && match$3 = match$2[1] && match$4 = match$3[1]){
    reutrn x[0] + match[0] + match$1[0] +match$2[0]
           + match$3[0] + match$4[0]
}else {
  return 0
}

]}

{[
var b0, b1, b2, b3, b4, b5

if(!x || !(b0 = x[1] ) || !(b1 = b0[1]) || !(b2=b1[1]) || !(b3 = b2[1]) || !(b4 = b3[1]) || !(b5=b4[1])){
    return 0
} else {
  var a0 = b0[0],
      a1 = b1[0],
      a2 = b2[0],
      a3 = b3[0],
      a4 = b4[0]
      a5 = b5[0];
  return a0 + a1 + a2 + a3 + a4 + a5
}
]}
*/

let f_arr = x =>
  switch x {
  | [a0, a1, a2, a3, a4, a5] => a0 + a1 + a2 + a3 + a4 + a5
  | _ => 0
  }

type t = {
  hi: int,
  lo: list<option<int>>,
}
let f_opion = x =>
  switch x {
  | {hi: 3, lo: list{None, None, Some(2), Some(1), Some(_), ..._}} => 2
  | _ => 0
  }

let f_opion = x =>
  switch x {
  | {hi: 3, lo: list{None, None, Some(2), Some(1), Some(_), ..._}} => 2
  | {hi: 2, lo: list{None, None, Some(2), Some(1), Some(_), ..._}} => 3
  | _ => 0
  }
