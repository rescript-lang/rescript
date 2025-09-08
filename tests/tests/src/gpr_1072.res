/*
external ice_cream:
    ?flavor:([`vanilla | `chocolate ] [@string]) -> 
    num:int ->
    unit -> 
    _ =  ""
[@@obj]


let my_scoop = ice_cream ~flavor:`vanilla ~num:3 ()
*/
/*
external ice_cream_2:
    flavor:([`vanilla | `chocolate ] [@string]) -> 
    num:int ->
    unit -> 
    _ =  ""
[@@obj]

let my_scoop2 = ice_cream_2 ~flavor:`vanilla ~num:3 ()
*/

type opt_test = {"x": Js.Undefined.t<int>, "y": Js.Undefined.t<int>}
@obj external opt_test: (~x: int=?, ~y: int=?, unit) => _ = ""

let u: opt_test = opt_test(~y=3, ())

@obj
external ice_cream3: (~flavor: @string [#vanilla | @as("x") #chocolate]=?, ~num: int, unit) => _ =
  "" /* TODO: warn when [_] happens in any place except `obj` */
type ice_cream3_expect = {"flavor": Js.undefined<string>, "num": int}

let v_ice_cream3: list<ice_cream3_expect> = list{
  ice_cream3(~flavor=#vanilla, ~num=3, ()),
  ice_cream3(~flavor=#chocolate, ~num=3, ()),
  ice_cream3(~flavor=#vanilla, ~num=3, ()),
}

type u
@obj
external ice_cream4: (~flavor: @string [#vanilla | @as("x") #chocolate]=?, ~num: int, unit) => u =
  ""

let v_ice_cream4: list<u> = list{
  ice_cream4(~flavor=#vanilla, ~num=3, ()),
  ice_cream4(~flavor=#chocolate, ~num=3, ()),
}

@obj external label_test: (~x__ignore: int, unit) => _ = ""

/** here the type label should be the same, 
    when get the object, it will be mangled */
type label_expect = {"x__ignore": int}

let vv: label_expect = label_test(~x__ignore=3, ())

@obj external int_test: (~x__ignore: @int [#a | #b], unit) => _ = ""
/* translate [`a] to 0, [`b] to 1 */
type int_expect = {"x__ignore": int}

let int_expect: int_expect = int_test(~x__ignore=#a, ())

@obj external int_test2: (~x__ignore: @int [#a | #b]=?, unit) => _ = ""

type int_expect2 = {"x__ignore": Js.Undefined.t<int>}

let int_expect2: int_expect2 = int_test2(~x__ignore=#a, ())

@obj external int_test3: (~x__ignore: @int [@as(2) #a | #b]=?, unit) => _ = ""

let int_expects: list<int_expect2> = list{
  int_test3(),
  int_test3(~x__ignore=#a, ()),
  int_test3(~x__ignore=#b, ()),
}

type flavor = [#vanilla | #chocolate]
@obj external ice: (~flavour: flavor, ~num: int, unit) => _ = ""

let mk_ice: {"flavour": flavor, "num": int} = ice(~flavour=#vanilla, ~num=3, ())

@obj external ice2: (~flavour: flavor=?, ~num: int, unit) => _ = ""

let my_ice2: {"flavour": Js.Undefined.t<flavor>, "num": int} = ice2(~flavour=#vanilla, ~num=1, ())

let my_ice3: {"flavour": Js.Undefined.t<flavor>, "num": int} = ice2(~num=2, ())

@obj external mk4: (~x__ignore: @ignore [#a | #b], ~y: int, unit) => _ = ""

let v_mk4: {"y": int} = mk4(~x__ignore=#a, ~y=3, ())

@obj external mk5: (~x: unit, ~y: int, unit) => _ = ""

let v_mk5: {"x": unit, "y": int} = mk5(~x=(), ~y=3, ())

@obj external mk6: (~x: unit=?, ~y: int, unit) => _ = ""

let v_mk6: {"x": Js.Undefined.t<unit>, "y": int} = mk6(~y=3, ())

let v_mk6_1 = mk6(~x=(), ~y=3, ())
type mk
@obj external mk: (~x__ignore: @int [#a | #b]=?, unit) => _ = ""

/* TODO: fix me */
let mk_u: {"x__ignore": Js.Undefined.t<int>} = mk(~x__ignore=#a, ())

@obj external mk7: (~x: @ignore [#a | #b]=?, ~y: int, unit) => _ = ""

let v_mk7: list<{"y": int}> = list{mk7(~x=#a, ~y=3, ()), mk7(~x=#b, ~y=2, ()), mk7(~y=2, ())}

@val external again: (~x__ignore: [#a | #b]=?, int) => unit = "again"

let () = {
  again(~x__ignore=#a, 3)
  again(3)
  again(~x__ignore=?None, 3)
  again(
    ~x__ignore=?{
      ignore(3)
      None
    },
    3,
  )
}

@val external again2: (~x__ignore: [#a | #b], int) => unit = "again2"

let () = again2(~x__ignore=#a, 3)

@val external again3: (~x__ignore: @ignore [#a | #b], int) => unit = "again3"

let () = {
  again3(~x__ignore=#a, 3)
  again3(~x__ignore=#b, 2)
}

@val external again4: (~x: unit=?, ~y: unit, int, unit) => unit = "again4"

let side_effect = ref(0)
let () = {
  again4(~y=(), __LINE__, ())
  again4(~x=?None, ~y=(), __LINE__, ())
  again4(~x=?Some(), ~y=(), __LINE__, ())
  again4(~x=(), ~y=(), __LINE__, ())
  again4(~y=(), __LINE__, ())
  again4(
    ~x={
      incr(side_effect)
      ()
    },
    ~y=(),
    __LINE__,
    (),
  )
  again4(
    ~x={
      incr(side_effect)
      ()
    },
    ~y={
      decr(side_effect)
      ()
    },
    __LINE__,
    (),
  )
  again4(
    ~y={
      decr(side_effect)
      ()
    },
    __LINE__,
    (),
  )
  again4(~x=incr(side_effect), ~y=(), __LINE__, ())
}

/* external again5 : ?x__ignore:([`a of unit -> int | `b of string -> int ] [@string]) */
/* -> int -> unit = "" [@@val] */

/* let v = again5 3 */
