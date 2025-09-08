let keys: Obj.t => array<string> = %raw(" function (x){return Object.keys(x)}")

%%raw(`
  function $$higher_order(x){
   return function(y,z){
      return x + y + z
   }
  }
`)
@val external higher_order: int => (int, int) => int = "$$higher_order"

let suites: ref<Mt.pair_suites> = ref(list{})
let test_id = ref(0)
let eq = (loc, (x, y)) => {
  incr(test_id)
  suites :=
    list{
      (loc ++ (" id " ++ Js.Int.toString(test_id.contents)), _ => Mt.Eq(x, y)),
      ...suites.contents,
    }
}

type rec kind<_> =
  | Int: kind<int>
  | Str: kind<string>

@obj external config: (~kind: @ignore kind<'a>, ~hi: int, ~low: 'a) => _ = ""

let int_config = config(~kind=Int, ~hi=3, ~low=32)

let string_config = config(~kind=Str, ~hi=3, ~low="32")

let () = eq(__LOC__, (6, higher_order(1)(2, 3)))

let same_type = (
  list{int_config, {"hi": 3, "low": 32}},
  list{string_config, {"hi": 3, "low": "32"}},
)

let () = {
  eq(__LOC__, (Belt.Array.length(Js_obj.keys(int_config)), 2))
  eq(__LOC__, (Belt.Array.length(Js_obj.keys(string_config)), 2))
}

let u = ref(3)

let side_effect_config = config(
  ~kind={
    incr(u)
    Int
  },
  ~hi=3,
  ~low=32,
)

let () = eq(__LOC__, (u.contents, 4))

type null_obj

@send external hh: null_obj => int = "hh" /* it also work */
@send external ff: (null_obj, unit) => int = "ff"
@send external ff_pipe: (null_obj, unit) => int = "ff_pipe"
@send external ff_pipe2: null_obj => int = "ff_pipe2" /* FIXME */
let vv = z => hh(z)

let v = z => ff(z, ())

let vvv = z => z->ff_pipe()

let vvvv = z => z->ff_pipe2
let create_prim = () => {"x'": 3, "x''": 3, "x''''": 2}

type t
@set external setGADT: (t, @ignore kind<'a>, 'a) => unit = "setGADT"
@set external setGADT2: (t, @ignore kind<'a>, @ignore kind<'b>, ('a, 'b)) => unit = "setGADT2"

@get external getGADT: (t, @ignore kind<'a>) => 'a = "getGADT"

@get external getGADT2: (t, @ignore kind<'a>, @ignore kind<'b>) => ('a, 'b) = "getGADT2"

@get_index external getGADTI2: (t, @ignore kind<'a>, @ignore kind<'b>, int) => ('a, 'b) = ""

@get_index external getGADTI3: (t, @ignore kind<'a>, @ignore kind<'b>, @as(3) _) => ('a, 'b) = ""

@set_index external setGADTI2: (t, @ignore kind<'a>, @ignore kind<'b>, int, ('a, 'b)) => unit = ""

@set_index
external setGADTI3: (t, @ignore kind<'a>, @ignore kind<'b>, @as(3) _, ('a, 'b)) => unit = ""

let ffff = x => {
  setGADT(x, Int, 3)
  setGADT2(x, Int, Str, (3, "3"))
  setGADT2(x, Str, Int, ("3", 3))
  switch getGADTI3(x, Int, Str) {
  | (cc, dd) => Js.log((cc, dd))
  }
  Js.log(getGADT(x, Int))
  switch getGADT2(x, Int, Str) {
  | (a: int, b: string) => Js.log2(a, b)
  }
  switch getGADTI2(x, Int, Str, 0) {
  | (a: int, b: string) => Js.log2(a, b)
  }
  setGADTI2(x, Int, Str, 0, (1, "x"))
  setGADTI3(x, Int, Str, (3, "x"))
}
let () = Mt.from_pair_suites(__MODULE__, suites.contents)
