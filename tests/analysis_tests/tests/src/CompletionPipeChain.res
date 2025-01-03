module Integer: {
  type t
  let increment: (t, int) => t
  let decrement: (t, int => int) => t
  let make: int => t
  let toInt: t => int
} = {
  type t = int
  let increment = (t, num) => t + num
  let decrement = (t, decrementer) => decrementer(t)
  let make = t => t
  let toInt = t => t
}

module SuperFloat: {
  type t
  let fromInteger: Integer.t => t
  let toInteger: t => Integer.t
} = {
  type t = float
  let fromInteger = t => t->Integer.toInt->Belt.Float.fromInt
  let toInteger = t => t->Belt.Float.toInt->Integer.make
}

let toFlt = i => i->SuperFloat.fromInteger
let int = Integer.make(1)
let f = int->Integer.increment(2)
// let _ = int->
//              ^com

// let _ = int->toFlt->
//                     ^com

// let _ = int->Integer.increment(2)->
//                                    ^com

// let _ = Integer.increment(int, 2)->
//                                    ^com

// let _ = int->Integer.decrement(t => t - 1)->
//                                             ^com

// let _ = int->Integer.increment(2)->Integer.decrement(t => t - 1)->
//                                                                   ^com

// let _ = int->Integer.increment(2)->SuperFloat.fromInteger->
//                                                            ^com

// let _ = int->Integer.increment(2)->SuperFloat.fromInteger->t
//                                                             ^com

// let _ = int->Integer.increment(2)->Integer.toInt->CompletionSupport.Test.make->
//                                                                                ^com

// let _ = CompletionSupport.Test.make(1)->CompletionSupport.Test.addSelf(2)->
//                                                                            ^com

let _ = [123]->Js.Array2.forEach(v => Js.log(v))
// ->
//   ^com

let _ = [123]->Belt.Array.reduce(0, (acc, curr) => acc + curr)
// ->t
//    ^com

type aliasedType = CompletionSupport.Test.t

let aliased: aliasedType = {name: 123}
let notAliased: CompletionSupport.Test.t = {name: 123}

// aliased->
//          ^com

// notAliased->
//             ^com

let renderer = CompletionSupport2.makeRenderer(
  ~prepare=() => "hello",
  ~render=props => {
    ignore(props)

    // Doesn't work when tried through this chain. Presumably because it now goes through multiple different files.
    // props.support.root->ren
    //                        ^com
    let root = props.support.root
    ignore(root)

    // Works here though when it's lifted out. Probably because it only goes through one file...?
    // root->ren
    //          ^com
    React.null
  },
  (),
)

// Console.log(int->)
//                  ^com

// Console.log(int->t)
//                   ^com

let r = %re("/t/g")

// r->la
//      ^com

module Xyz = {
  type xx = One
  let do = (_: xx) => ""
}

let xx = Xyz.One
// xx->
//     ^com
