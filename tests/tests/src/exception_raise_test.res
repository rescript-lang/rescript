@@warning("-22")

exception Local
exception B(list<int>)
exception C(int, int)
exception D((int, int))
let appf = (g, x) => {
  module U = {
    exception A(int)
  }
  try g(x) catch {
  | Local => 3
  | Not_found => 2
  | U.A(32) => 3
  | U.A(_) => 3
  | B(list{_, _, x, ..._}) => x
  | C(x, _)
  | D(x, _) => x
  | _ => 4
  }
}

/*
TODO:
{[
    else if (exn[0] === B) {
      var match = exn[1];
      if (match) {
        var match$1 = match[1];
        if (match$1) {
          var match$2 = match$1[1];
          if (match$2) {
            return match$2[0];
          }
          else {
            exit = 1;
          }
        }
        else {
          exit = 1;
        }
      }
      else {
        exit = 1;
      }
    }

]}

can be simplified as 

{[
var match, match$1, match$2 ; 

else if (exn[0] === B) {
  if (match = exn[1] && match$1 = match[1] && match$2 =  match$1[1]) {
      return match$2[0];
    }
  else {
    exit = 1;
  }
}

]}

peepwhole rules like 
{[
var x = e ;
if (x) {
 ..
}
]}

can be translated into 

{[
var x ; 
if (x = e){
}
]}
*/

exception A(int)

let f = try %raw(` function () {throw (new Error ("x"))} ()`) catch {
| A(x) => x
| _ => 2
}

let ff = try %raw(` function () {throw 3} ()`) catch {
| A(x) => x
| _ => 2
}

let fff = try %raw(` function () {throw 2} ()`) catch {
| A(x) => x
| _ => 2
}

let a0 = try %raw(` function (){throw 2} () `) catch {
/* throw is a statement */
| A(x) => x
| Js.Exn.Error(v) => Obj.magic(v)
| _ => assert(false)
}

let a1: exn = try %raw(` function (){throw 2} () `) catch {
/* throw is a statement */
| e => e
}

let a2: exn = try %raw(` function (){throw (new Error("x"))} () `) catch {
/* throw is a statement */
| e => e
}

let suites = ref({
  open Mt
  list{
    (__LOC__, _ => Eq((f, ff, fff, a0), (2, 2, 2, 2))),
    /* __LOC__, (fun _ -> Eq (Js.Exn.Error (Obj.magic 2) , a1)) */
    (
      __LOC__,
      _ =>
        switch a1 {
        | Js.Exn.Error(v) => Eq(Obj.magic(v), 2)
        | _ => assert(false)
        },
    ),
  }
})

let test_id = ref(0)
let eq = (loc, x, y) => Mt.eq_suites(~test_id, ~suites, loc, x, y)

let () = try %raw(`()=>{throw 2}`)() catch {
| e => eq(__LOC__, Js.Exn.asJsExn(e) != None, true)
}

let () = try throw(Not_found) catch {
| e => eq(__LOC__, Js.Exn.asJsExn(e) != None, false)
}

let fff0 = (x, g) =>
  switch x() {
  | exception _ => 1
  | _ => g()
  }
type in_channel
@val external input_line: in_channel => string = "input_line"
let rec input_lines = (ic, acc) =>
  switch input_line(ic) {
  | exception _ => Belt.List.reverse(acc)
  | line => input_lines(ic, list{line, ...acc})
  }

let () = eq(__LOC__, (%raw(`(a,b,c,_) => a + b + c `): (_, _, _, _) => _)(1, 2, 3, 4), 6)
Mt.from_pair_suites(__MODULE__, suites.contents)
