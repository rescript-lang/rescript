%%raw(`
function add(x,y){
  return x + y
}
`)
type rec kind<_> =
  | Float: kind<float>
  | String: kind<string>
@val external add: (@ignore kind<'a>, 'a, 'a) => 'a = "add"

let () = {
  Js.log(add(Float, 3.0, 2.0))
  Js.log(add(String, "x", "y"))
}

%%raw(`
function add_dyn(kind,x,y){
  switch(kind){
  case "string" : return x + y;
  case "float" : return x + y;
  }
}
`)

let string_of_kind = (type t, kind: kind<t>) =>
  switch kind {
  | Float => "float"
  | String => "string"
  }

@val external add_dyn: (@ignore kind<'a>, string, 'a, 'a) => 'a = "add_dyn"

let add2 = (k, x, y) => add_dyn(k, string_of_kind(k), x, y)

let () = {
  Js.log(add2(Float, 3.0, 2.0))
  Js.log(add2(String, "x", "y"))
}
