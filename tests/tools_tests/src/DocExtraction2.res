type t = string

let getStr = () => "123"

let make = getStr

module InnerModule = {
  type t = unit
  let make = () => ()
}

// ^dex

let log = msg => Console.log(msg)
