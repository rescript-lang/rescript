let suites: ref<Mt.pair_suites> = ref(list{})
let test_id = ref(0)
let eq = (loc, x, y) => {
  incr(test_id)
  suites :=
    list{
      (loc ++ (" id " ++ Js.Int.toString(test_id.contents)), _ => Mt.Eq(x, y)),
      ...suites.contents,
    }
}

%%raw(`
function Make (){
  this.data = []
  for(var i = 0; i < arguments.length; ++i){
   this.data[i] = arguments[i]
}
}

Make.prototype.sum = function(){
  var result  = 0;
  for(var k = 0; k < this.data.length; ++k){
    result = result + this.data[k]
  };
  return result
}  

Make.prototype.add = function(){
  
} 
`)

type t

@new external make: (int, int, int, int) => t = "Make"

@send external sum: (t, unit) => int = "sum"

/* compile error */
/* external join : string  -> string = "" [@@module "path"] [@@variadic] */
@module("path") @variadic external join: array<string> => string = "join"

@send @variadic external test: (t, array<string>) => t = "test" /* FIXME */

/* compile error */
let u = ["x", "d"]
let f = x => x->test(["a", "b"])->test(["a", "b"])
/* |> test u */

let v = make(1, 2, 3, 4)

let u = sum(v, ())

let () = eq(__LOC__, u, 10)

Mt.from_pair_suites(__MODULE__, suites.contents)
